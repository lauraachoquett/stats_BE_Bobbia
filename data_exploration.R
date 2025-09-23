# Load the CSV file into a data frame and choosing how many rows and columns

data <- read.csv("meteo.csv", header = TRUE, sep = ";", )

# Display the first few rows of the data frame
head(data[, 1:5])  # Display first 5 columns for brevity