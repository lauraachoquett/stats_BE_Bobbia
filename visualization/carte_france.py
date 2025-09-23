import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import re

CSV_PATH = "stations.csv"
REGIONS_URL = "https://france-geojson.gregoiredavid.fr/repo/regions.geojson"
OM_CODES = {"01","02","03","04","06"}  # DOM à exclure

def autodetect_read_csv(path):
    df = pd.read_csv(path, sep=None, engine="python")
    if df.shape[1] == 1:
        df = pd.read_csv(path, sep=",")
    if df.shape[1] == 1:
        df = pd.read_csv(path, sep=";")

    df.columns = (
        df.columns
        .str.strip()
        .str.replace(r"^\"|\"$", "", regex=True)  # guillemets en début/fin
        .str.replace(r"\s+", " ", regex=True)
    )

    for col in df.select_dtypes(include="object").columns:
        df[col] = df[col].str.strip().str.replace(r"^\"|\"$", "", regex=True)

    return df

def find_col(df, candidates):
    norm = {c: re.sub(r"[^\w]+", "", c.lower()) for c in df.columns}
    for wanted in candidates:
        wanted_norm = re.sub(r"[^\w]+", "", wanted.lower())
        for col, col_norm in norm.items():
            if col_norm == wanted_norm:
                return col
    for wanted in candidates:
        wanted_norm = re.sub(r"[^\w]+", "", wanted.lower())
        for col, col_norm in norm.items():
            if wanted_norm in col_norm:
                return col
    raise KeyError(f"Impossible de trouver {candidates} dans {list(df.columns)}")

# --- 1) Lire le CSV
df = autodetect_read_csv(CSV_PATH)
print("Colonnes détectées :", list(df.columns))

lat_col = find_col(df, ["Latitude"])
lon_col = find_col(df, ["Longitude"])
commune_col = find_col(df, ["communes..name"]) if any("communes" in c.lower() for c in df.columns) else None

# --- 2) GeoDataFrame
gdf_pts = gpd.GeoDataFrame(
    df,
    geometry=gpd.points_from_xy(df[lon_col].astype(float), df[lat_col].astype(float)),
    crs="EPSG:4326",
)

# --- 3) Régions métropole
regions = gpd.read_file(REGIONS_URL)
regions_metropole = regions[~regions["code"].isin(OM_CODES)].copy()

# --- 4) Points seulement en métropole
gdf_pts_in = gpd.sjoin(
    gdf_pts,
    regions_metropole[["code", "nom", "geometry"]],
    how="inner", predicate="within"
)

# --- 5) Carte statique avec noms de communes
fig, ax = plt.subplots(figsize=(9, 10))
regions_metropole.boundary.plot(ax=ax, linewidth=1, color="black")
regions_metropole.plot(ax=ax, edgecolor="black", facecolor="white", alpha=0.08)
gdf_pts_in.plot(ax=ax, markersize=8, alpha=0.85, color="red")

# Ajouter les labels (communes si dispo, sinon ID OMM)
for _, row in gdf_pts_in.iterrows():
    name = ""
    if commune_col and pd.notna(row[commune_col]) and str(row[commune_col]).strip():
        name = str(row[commune_col])
    elif "ID.OMM.station" in gdf_pts_in.columns:
        name = str(row["ID.OMM.station"])
    if name:
        ax.annotate(name, (row.geometry.x, row.geometry.y), fontsize=8, ha="left", va="bottom")

ax.set_axis_off()
ax.set_title("Stations en France métropolitaine", fontsize=14)
plt.tight_layout()
plt.savefig('fig/carte_france.png',dpi=200,bbox_inches='tight')
print("Carte enregistrée sous 'fig/carte_france.png'")