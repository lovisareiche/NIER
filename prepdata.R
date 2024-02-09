
## Housekeeping

# Install and load required packages
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
library(readxl)
library(dplyr)

# Clear the environment
rm(list = ls())


# Set the file path and name
# this can be updated as new releases appear
file_path_survey <- "00_raw_data/individual-responses-2023_11.xlsx"
file_path_cpi <- "00_raw_data/series-201223.xls"
sheet_name <- "Dataset"

## Retrieve data

# Read the survey data file into a data frame
tryCatch({
  # Read the Excel file to automatically infer column types for other columns
  df <- readxl::read_excel(file_path_survey, sheet = sheet_name, col_types = "numeric")
  cat("File loaded successfully!\n")
}, error = function(e) {
  cat("Error:", e$message, "\n")
})

# Read the cpi data file into a data frame
tryCatch({
  # Read the Excel file to automatically infer column types for other columns
  cpi <- readxl::read_excel(file_path_cpi, col_types = "numeric")
  cat("File loaded successfully!\n")
}, error = function(e) {
  cat("Error:", e$message, "\n")
})

## Merge data

# Merge dataframes based on 'yyyyqq'
df <- merge(cpi, df, by = "yyyyqq") 


## Create variable "point" which denotes the upper bound of the respondents point forecast
# Note: lower bound is the integer below


# Create a new variable "point" based on specified conditions

df <- df %>%
  mutate(
    ppoint = case_when(
      q1 == 2 ~ 0,
      q1 == 3 ~ 1,
      q1 == 4 ~ 2,
      q1 == 5 ~ 3,
      q1 == 6 ~ 4,
      q1 == 7 ~ 5,
      q1a == 1 ~ 6,
      q1a == 2 ~ 7,
      q1a == 3 ~ 8,
      q1a == 4 ~ 9,
      q1a == 5 ~ 10,
      q1a4 == 1 ~ 11,
      q1a4 == 2 ~ 12,
      q1a4 == 3 ~ 13,
      q1a4 == 4 ~ 14,
      q1a4 == 5 ~ 15,
      q1a4 == 6 ~ 16,
      q1a2 == 1 ~ -1,
      q1a2 == 2 ~ -2,
      q1a2 == 3 ~ -3,
      q1a2 == 4 ~ -4,
      q1a2 == 5 ~ -5,
      q1a2 == 6 ~ -6,
      q1 == 9 ~ -888,
      TRUE ~ -999
    )
  )


df <- df %>%
  mutate(
    epoint = case_when(
      q2 == 2 ~ 0,
      q2 == 3 ~ 1,
      q2 == 4 ~ 2,
      q2 == 5 ~ 3,
      q2 == 6 ~ 4,
      q2 == 7 ~ 5,
      q2a == 1 ~ 6,
      q2a == 2 ~ 7,
      q2a == 3 ~ 8,
      q2a == 4 ~ 9,
      q2a == 5 ~ 10,
      q2a4 == 1 ~ 11,
      q2a4 == 2 ~ 12,
      q2a4 == 3 ~ 13,
      q2a4 == 4 ~ 14,
      q2a4 == 5 ~ 15,
      q2a4 == 6 ~ 16,
      q2a2 == 1 ~ -1,
      q2a2 == 2 ~ -2,
      q2a2 == 3 ~ -3,
      q2a2 == 4 ~ -4,
      q2a2 == 5 ~ -5,
      q2a2 == 6 ~ -6,
      q2 == 9 ~ -888,
      TRUE ~ -999
    )
  )


# save dataframe

output_path <- "01_tidy_data/data.csv"

# Save the dataframe as a CSV file
write.csv(df, file = output_path, row.names = FALSE)

# plot in histogram

point_counts <- table(df$epoint)
total_observations <- length(df$epoint)

# Calculate frequencies (counts divided by total observations)
point_frequencies <- point_counts / total_observations

# Plot the barplot
dev.new()
barplot(point_frequencies, names.arg = names(point_frequencies),
        col = "skyblue", main = "Point Frequencies",
        xlab = "Point Values", ylab = "Frequency")

