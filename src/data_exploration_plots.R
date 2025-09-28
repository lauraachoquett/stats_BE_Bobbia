

load_data <- function(file_path, nrows = 1000) {
    data <- read.csv(file_path, header = TRUE, sep = ",", nrows = nrows)
    print(dim(data))  # Print the dimensions of the data frame
    return(data)
}

boxplot_column <- function(data, column_name, output_file) {
    png(output_file, width = 400, height = 600)
    boxplot(data[[column_name]], main = paste("Boxplot of", column_name),
            ylab = column_name, col = "lightblue", border = "darkblue")
    dev.off()
}

if (sys.nframe() == 0) {
    # This block runs if the script is executed directly (like 'main')
    data <- load_data("observations.csv")
    boxplot_column(data, "Pression.au.niveau.mer", "boxplot_pression_mer.png")
}