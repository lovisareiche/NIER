
####################################
## Has rounding changed over time?##
####################################


## Housekeeping

# Install and load required packages

library(dplyr)
library(ggplot2)

# Clear the environment
rm(list = ls())


# Set the file path
file_path <- "01_tidy_data/data.csv"

# Load the CSV file
df <- read.csv(file_path)

## Plot histograms

dev.new()
ggplot(df, aes(x = epoint, fill = factor(yyyyqq))) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
  labs(title = "Histogram of Point Forecasts",
       x = "Point Forecast", y = "Frequency") +
  theme_minimal() +
  scale_x_continuous(limits = c(-6, 16))


## Plot share of unchanged over time

# Convert 'yyyyqq' to a factor for better visualization
df$yyyyqq <- factor(df$yyyyqq)

# Calculate the share of point=0 and average point for each yyyyqq
summary_data <- df %>%
  group_by(yyyyqq) %>%
  summarize(
    share_point_zero = mean(q2 == 2, na.rm = TRUE),
    cpi = mean(pi, na.rm = TRUE)
  )

dev.new()
ggplot(summary_data, aes(x = yyyyqq, y = share_point_zero*100, fill = yyyyqq)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_line(aes(x = yyyyqq, y = cpi, group = 1), color = "red", size = 1) +
  geom_point(aes(x = yyyyqq, y = cpi), color = "red", size = 3) +
  labs(title = "Share of Point = 0 and Average Point for Each Quarter",
       x = "Quarter", y = "Share") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## Plot share of don't know

# Calculate the share of point=0 and average point for each yyyyqq
summary_data <- df %>%
  group_by(yyyyqq) %>%
  summarize(
    share_point_dk = mean(q2 == 9, na.rm = TRUE),
    cpi = mean(pi, na.rm = TRUE)
  )

dev.new()
ggplot(summary_data, aes(x = yyyyqq, y = share_point_dk*100, fill = yyyyqq)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_line(aes(x = yyyyqq, y = cpi, group = 1), color = "red", size = 1) +
  geom_point(aes(x = yyyyqq, y = cpi), color = "red", size = 3) +
  labs(title = "Share of Don't Know and CPI Inflation for Each Quarter",
       x = "Quarter", y = "Share") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## look at May 2020

df_202002 <- df[df$yyyyqq==202002 & df$epoint != -999,]

point_counts <- table(df_202002$epoint) 
total_observations <- length(df_202002$epoint)

# Calculate frequencies (counts divided by total observations)
point_frequencies <- point_counts / total_observations

# Plot the barplot
dev.new()
barplot(point_frequencies, names.arg = names(point_frequencies),
        col = "skyblue", main = "2020 Q2",
        xlab = "Point Values", ylab = "Frequency")


## look at Feb 2017


df_201701 <- df[df$yyyyqq==201701,]

point_counts <- table(df_201701$epoint)
total_observations <- length(df_201701$epoint)

# Calculate frequencies (counts divided by total observations)
point_frequencies <- point_counts / total_observations

# Plot the barplot
dev.new()
barplot(point_frequencies, names.arg = names(point_frequencies),
        col = "skyblue", main = "2017 Q1",
        xlab = "Point Values", ylab = "Frequency")

## look at Feb 2023


df_202301 <- df[df$yyyyqq==202301,]

point_counts <- table(df_202301$epoint)
total_observations <- length(df_202301$epoint)

# Calculate frequencies (counts divided by total observations)
point_frequencies <- point_counts / total_observations

# Plot the barplot
dev.new()
barplot(point_frequencies, names.arg = names(point_frequencies),
        col = "skyblue", main = "2023 Q1",
        xlab = "Point Values", ylab = "Frequency")

