import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import numpy as np
import re

def heatmap_points_from_observations(stations_csv, observations_csv, var_col,
                                     title=None, outfile=None):
    """
    Calcule la moyenne de `var_col` par station à partir du fichier d'observations
    et trace des points (taille+couleur = valeur moyenne) sur la carte des régions
    de France métropolitaine.
    """
    REGIONS_URL = "https://france-geojson.gregoiredavid.fr/repo/regions.geojson"
    OM_CODES = {"01","02","03","04","06"}  # DOM à exclure
    title = title or f"Moyenne de {var_col} par station (observations)"
    outfile = outfile or f"fig/heatmap_points_{re.sub(r'[^A-Za-z0-9_-]+','_',var_col)}.png"

    # ---------- Helpers ----------
    def autodetect_read_csv(path):
        df = pd.read_csv(path, sep=None, engine="python")
        if df.shape[1] == 1:
            df = pd.read_csv(path, sep=",")
        if df.shape[1] == 1:
            df = pd.read_csv(path, sep=";")
        # Nettoyage léger des entêtes et des strings
        df.columns = (
            df.columns
              .str.strip()
              .str.replace(r'^"|"$', "", regex=True)
              .str.replace(r"\s+", " ", regex=True)
        )
        for c in df.select_dtypes(include="object").columns:
            df[c] = df[c].astype(str).str.strip().str.replace(r'^"|"$', "", regex=True)
        return df

    def find_col(df, candidates):
        """trouve une colonne en normalisant (minuscules, sans ponctuation)"""
        norm = {c: re.sub(r"[^\w]+", "", c.lower()) for c in df.columns}
        for wanted in candidates:
            wn = re.sub(r"[^\w]+", "", wanted.lower())
            for col, cn in norm.items():
                if cn == wn:
                    return col
        for wanted in candidates:
            wn = re.sub(r"[^\w]+", "", wanted.lower())
            for col, cn in norm.items():
                if wn in cn:
                    return col
        raise KeyError(f"Impossible de trouver {candidates} dans {list(df.columns)}")

    def safe_to_numeric(s):
        return pd.to_numeric(s, errors="coerce")

    def scale_sizes(x, smin=40, smax=300):
        x = x.astype(float)
        if x.max() == x.min() or not np.isfinite(x).all():
            return np.full_like(x, (smin + smax)/2.0, dtype=float)
        z = (x - x.min()) / (x.max() - x.min())
        return smin + z * (smax - smin)

    # ---------- 1) Lire les CSV ----------
    st = autodetect_read_csv(stations_csv)
    obs = autodetect_read_csv(observations_csv)

    # ---------- 2) Détection des colonnes clés ----------
    # ID station (présent dans les deux fichiers)
    id_candidates = ["ID.OMM.station", "id_omm_station", "id station", "id", "station"]
    id_st = find_col(st, id_candidates)
    id_obs = find_col(obs, id_candidates)

    # Lat/Lon dans le CSV stations
    lat_st = find_col(st, ["Latitude","lat"])
    lon_st = find_col(st, ["Longitude","lon","lng"])
    
    try:
        var_obs = find_col(obs, [var_col])

        # Conversion en numérique
        obs[var_obs] = pd.to_numeric(obs[var_obs], errors="coerce")

        # Si c'est la température en Kelvin → on convertit en °C
        if var_col.lower().startswith("temp"):
            # Seulement si les valeurs semblent être en Kelvin (>150)
            if obs[var_obs].mean(skipna=True) > 150:
                obs[var_obs] = obs[var_obs] - 273.15
                print("Conversion de la température en °C")

    except KeyError:
        print("Variable non trouvée :", var_col)

    # Optionnel: nom de commune pour l’annotation
    commune_col = None
    try:
        commune_col = find_col(st, ["communes..name","commune","nom commune"])
    except KeyError:
        pass

    # ---------- 3) Nettoyage / types ----------
    st[lat_st] = safe_to_numeric(st[lat_st])
    st[lon_st] = safe_to_numeric(st[lon_st])
    obs[var_obs] = safe_to_numeric(obs[var_obs])

    # ---------- 4) Agrégation par station (moyenne sur toutes les observations) ----------
    agg = (
        obs[[id_obs, var_obs]]
        .dropna(subset=[var_obs])
        .groupby(id_obs, as_index=False)[var_obs].mean()
        .rename(columns={var_obs: f"{var_obs}_mean"})
    )

    # ---------- 5) Merge avec coordonnées des stations ----------
    st_coords = st[[id_st, lat_st, lon_st] + ([commune_col] if commune_col else [])].drop_duplicates(subset=[id_st])
    df_mean = pd.merge(agg, st_coords, left_on=id_obs, right_on=id_st, how="inner")

    # retirer stations sans coords valides
    df_mean = df_mean.dropna(subset=[lat_st, lon_st])

    gdf = gpd.GeoDataFrame(
        df_mean,
        geometry=gpd.points_from_xy(df_mean[lon_st].astype(float), df_mean[lat_st].astype(float)),
        crs="EPSG:4326",
    )
    regions = gpd.read_file(REGIONS_URL)
    regions_metropole = regions[~regions["code"].isin(OM_CODES)].copy()

    # garder uniquement les points en métropole (utile si le CSV contient outre-mer)
    gdf_in = gpd.sjoin(
        gdf,
        regions_metropole[["code","nom","geometry"]],
        how="inner", predicate="within"
    )

    # ---------- 7) Tracé ----------
    value_col = f"{var_obs}_mean"
    fig, ax = plt.subplots(figsize=(9, 10))
    regions_metropole.boundary.plot(ax=ax, linewidth=1, color="black")
    regions_metropole.plot(ax=ax, edgecolor="black", facecolor="white", alpha=0.08)

    sizes = scale_sizes(gdf_in[value_col])
    gdf_in.plot(
        ax=ax,
        column=value_col,
        cmap="coolwarm",          # bleu -> rouge
        markersize=sizes,
        alpha=0.9,
        legend=True,
        legend_kwds={"label": f"Moyenne de {var_col}"}
    )

    # Petites annotations lisibles si peu de points
    if len(gdf_in) <= 80:
        for _, r in gdf_in.iterrows():
            label = None
            if commune_col and pd.notna(r.get(commune_col, None)) and str(r[commune_col]).strip():
                label = str(r[commune_col])
            else:
                label = str(r[id_st])
            ax.annotate(label, (r.geometry.x, r.geometry.y), fontsize=7, ha="left", va="bottom")

    ax.set_axis_off()
    ax.set_title(title, fontsize=14)
    plt.tight_layout()
    plt.savefig(outfile, dpi=220, bbox_inches="tight")
    print(f"Carte enregistrée sous '{outfile}'")
    
if __name__ == "__main__":
    stations_csv = "csv/stations.csv"
    observations_csv = "csv/observations_cleaned.csv"  # (toutes les mesures)
    heatmap_points_from_observations(
        stations_csv, observations_csv,
        var_col="Hauteur.totale.de.la.couche.de.neige..glace..autre.au.sol"  # mets ici le nom attendu côté observations
    )