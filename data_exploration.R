# Load the CSV file into a data frame and choosing how many rows and columns

data <- read.csv("meteo.csv", header = TRUE, sep = ";", nrows = 100)

# Display the first few rows of the data frame
head(data[, 1:5])  # Display first 5 columns for brevity

# Display the structure of the data frame to understand its contents
str(data) 

# Summary statistics of the data frame
summary(data)

# Visualize the distribution of a specific variable, e.g., "Pression au niveau de la mer" and save the plot as a PNG file
png("pression_boxplot.png", width = 1200, height = 1500)
boxplot(data$Pression.au.niveau.mer, main = "Boxplot of Pression au niveau de la mer", ylab = "Pression (hPa)")