#  Load the CSV file into a data frame and choosing how many rows and columns

data <- read.csv("observations.csv", header = TRUE, sep = ",")
print(dim(data))  # Print the dimensions of the data frame

# Check for missing values in the dataset per column and get a the missing values per column
missing_values <- colSums(is.na(data))
cat("Total missing values in the dataset per column:\n")
print(missing_values[missing_values > 0])

# Drop the columns if there is more than 30% of missing values
threshold <-  0.3 * nrow(data)
cols_to_drop <- names(missing_values[missing_values > threshold])
data_cleaned <- data[, !(names(data) %in% cols_to_drop)]
cat("Dropped columns with more than 30% missing values:\n")


write.csv(data_cleaned, "observations_cleaned.csv", row.names = FALSE)
