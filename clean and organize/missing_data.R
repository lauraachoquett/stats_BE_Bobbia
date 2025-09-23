# Load the CSV file into a data frame and choosing how many rows and columns

data <- read.csv("observations.csv", header = TRUE, sep = ",")
print(dim(data))  # Print the dimensions of the data frame

# Check for missing values in the dataset per column and get a the missing values per column
missing_values <- colSums(is.na(data))
cat("Total missing values in the dataset per column:\n")
print(missing_values[missing_values > 0])
print("hello")