# Load the CSV file into a data frame and choosing how many rows and columns

data <- read.csv("csv/observations.csv", header = TRUE, sep = ",")
print(dim(data))  # Print the dimensions of the data frame

# Check for missing values in the dataset per column and get a the missing values per column
missing_values <- colSums(is.na(data))
cat("Total missing values in the dataset per column:\n")
print(missing_values[missing_values > 0])

# Function to drop columns with more than a specified threshold of missing values
drop_columns_with_missing <- function(data, missing_values, threshold_ratio = 0.3) {
    threshold <- threshold_ratio * nrow(data)
    cols_to_drop <- names(missing_values[missing_values > threshold])
    data_cleaned <- data[, !(names(data) %in% cols_to_drop)]
    cat("Dropped columns with more than", threshold_ratio * 100, "% missing values:\n")
    print(cols_to_drop)
    # Save the cleaned data to a new CSV file
    write.csv(data_cleaned, "csv/observations_cleaned.csv", row.names = FALSE)
    return(data_cleaned)
}

# Example usage:
data_cleaned <- drop_columns_with_missing(data, missing_values)
