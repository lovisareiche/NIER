
####################################
## Has source chnaged over time?  ##
####################################


## Housekeeping

# Install and load required packages

library(dplyr)
library(ggplot2)

# Clear the environment
rm(list = ls())


# Set the file path
file_path <- "01_tidy_data/data.csv"


## Source about Inflation

# Load the CSV file
df <- read.csv(file_path) %>%
  subset(yyyyqq %in% c("201701", "202301")) %>%
  mutate(female = sex-1) %>% # female dummy
  subset(female <= 1) # remove undefined


df_summary <- df %>%
  group_by(yyyyqq) %>%
  summarise(
    female_share = mean(female),
    q2aiv1_female_share = mean(q2aiv1 * female, na.rm = TRUE),
    q2aiv2_female_share = mean(q2aiv2 * female, na.rm = TRUE),
    q2aiv3_female_share = mean(q2aiv3 * female, na.rm = TRUE),
    q2aiv4_female_share = mean(q2aiv4 * female, na.rm = TRUE),
    q2aiv5_female_share = mean(q2aiv5 * female, na.rm = TRUE),
    q2aiv6_female_share = mean(q2aiv6 * female, na.rm = TRUE),
    q2aiv7_female_share = mean(q2aiv7 * female, na.rm = TRUE),
    q2aiv8_female_share = mean(q2aiv8 * female, na.rm = TRUE),
    q2aiv9_female_share = mean(q2aiv9 * female, na.rm = TRUE),
    q2aiv10_female_share = mean(q2aiv10 * female, na.rm = TRUE),
    q2aiv11_female_share = mean(q2aiv11 * female, na.rm = TRUE),
    q2aiv12_female_share = mean(q2aiv12 * female, na.rm = TRUE),
    q2aiv13_female_share = mean(q2aiv13 * female, na.rm = TRUE),
    male_share = 1 - female_share,
    q2aiv1_male_share = mean(q2aiv1 * (1 - female), na.rm = TRUE),
    q2aiv2_male_share = mean(q2aiv2 * (1 - female), na.rm = TRUE),
    q2aiv3_male_share = mean(q2aiv3 * (1 - female), na.rm = TRUE),
    q2aiv4_male_share = mean(q2aiv4 * (1 - female), na.rm = TRUE),
    q2aiv5_male_share = mean(q2aiv5 * (1 - female), na.rm = TRUE),
    q2aiv6_male_share = mean(q2aiv6 * (1 - female), na.rm = TRUE),
    q2aiv7_male_share = mean(q2aiv7 * (1 - female), na.rm = TRUE),
    q2aiv8_male_share = mean(q2aiv8 * (1 - female), na.rm = TRUE),
    q2aiv9_male_share = mean(q2aiv9 * (1 - female), na.rm = TRUE),
    q2aiv10_male_share = mean(q2aiv10 * (1 - female), na.rm = TRUE),
    q2aiv11_male_share = mean(q2aiv11 * (1 - female), na.rm = TRUE),
    q2aiv12_male_share = mean(q2aiv12 * (1 - female), na.rm = TRUE),
    q2aiv13_male_share = mean(q2aiv13 * (1 - female), na.rm = TRUE)
  ) %>%
  ungroup()

# Print the summary dataframe
print(df_summary)

# Mapping variable names to new labels
variable_labels <- c(
  'Groceries (recently)', 'Groceries (long)', 'Media (current)', 
  'Media (future)', 'Interest Rates', 'Inflation Target', 'UK Economy', 
  'UK Expectations', 'Exchange Rate', 'Other', 'None', 'Energy', 'Petrol'
)
# Specify the order of the factor levels
variable_order <- c('q2aiv1', 'q2aiv2', 'q2aiv3', 'q2aiv4', 'q2aiv5', 'q2aiv6', 'q2aiv7', 'q2aiv8', 'q2aiv9', 'q2aiv10', 'q2aiv11', 'q2aiv12', 'q2aiv13')



# Melt the data to long format for easier plotting
df_melted <- df_summary %>%
  tidyr::gather(key, value, -yyyyqq) %>%
  tidyr::separate(key, into = c("variable", "gender"), sep = "_") %>%
  subset(variable != "female") %>%
  subset(variable != "male") %>%
  mutate(variable = factor(variable, levels = variable_order))

# weighting according to gender shares
df_melted$value[df_melted$gender=="female" & df_melted$yyyyqq==201701] <- df_melted$value[df_melted$gender=="female" & df_melted$yyyyqq==201701]*df_summary$female_share[df_summary$yyyyqq==201701]
df_melted$value[df_melted$gender=="female" & df_melted$yyyyqq==202301] <- df_melted$value[df_melted$gender=="female" & df_melted$yyyyqq==202301]*df_summary$female_share[df_summary$yyyyqq==202301]

df_melted$value[df_melted$gender=="male" & df_melted$yyyyqq==201701] <- df_melted$value[df_melted$gender=="male" & df_melted$yyyyqq==201701]*df_summary$male_share[df_summary$yyyyqq==201701]
df_melted$value[df_melted$gender=="male" & df_melted$yyyyqq==202301] <- df_melted$value[df_melted$gender=="male" & df_melted$yyyyqq==202301]*df_summary$male_share[df_summary$yyyyqq==202301]

dev.new()
ggplot(df_melted, aes(x = variable, y = value, fill = gender)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(yyyyqq ~ ., scales = "fixed", switch = "y") +
  labs(x = "",
       y = "Share") +
  scale_fill_manual(values = c("female" = "blue", "male" = "red")) +
  scale_x_discrete(labels = variable_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Source about BOE

# Load the CSV file
df <- read.csv(file_path) %>%
  mutate(female = sex-1) %>% # female dummy
  subset(female <= 1) # remove undefined


df_summary <- df %>%
  summarise(
    female_share = mean(female),
    q12a1_female_share = mean(q2aiv1 * female, na.rm = TRUE),
    q12a2_female_share = mean(q2aiv2 * female, na.rm = TRUE),
    q12a3_female_share = mean(q2aiv3 * female, na.rm = TRUE),
    q12a4_female_share = mean(q2aiv4 * female, na.rm = TRUE),
    q12a5_female_share = mean(q2aiv5 * female, na.rm = TRUE),
    q12a6_female_share = mean(q2aiv6 * female, na.rm = TRUE),
    q12a7_female_share = mean(q2aiv7 * female, na.rm = TRUE),
    q12a8_female_share = mean(q2aiv8 * female, na.rm = TRUE),
    
    male_share = 1 - female_share,
    q12a1_male_share = mean(q12a1 * (1 - female), na.rm = TRUE),
    q12a2_male_share = mean(q12a2 * (1 - female), na.rm = TRUE),
    q12a3_male_share = mean(q12a3 * (1 - female), na.rm = TRUE),
    q12a4_male_share = mean(q12a4 * (1 - female), na.rm = TRUE),
    q12a5_male_share = mean(q12a5 * (1 - female), na.rm = TRUE),
    q12a6_male_share = mean(q12a6 * (1 - female), na.rm = TRUE),
    q12a7_male_share = mean(q12a7 * (1 - female), na.rm = TRUE),
    q12a8_male_share = mean(q12a8 * (1 - female), na.rm = TRUE),
  
  ) %>%
  ungroup()

# Print the summary dataframe
print(df_summary)

# Mapping variable names to new labels
variable_labels <- c(
  'Newspaper', 'TV Radio', ' BoE Website', 
  'Other Website', 'BoE Publication', 'BoE Enquiries', 'None', "Don't Know"
)
# Specify the order of the factor levels
variable_order <- c('q12a1', 'q12a2', 'q12a3', 'q12a4', 'q12a5', 'q12a6', 'q12a7', 'q12a8')



# Melt the data to long format for easier plotting
df_melted <- df_summary %>%
  tidyr::gather(key, value) %>%
  tidyr::separate(key, into = c("variable", "gender"), sep = "_") %>%
  subset(variable != "female") %>%
  subset(variable != "male") %>%
  mutate(variable = factor(variable, levels = variable_order))

# weighting according to gender shares
df_melted$value[df_melted$gender=="female"] <- df_melted$value[df_melted$gender=="female"]*df_summary$female_share
df_melted$value[df_melted$gender=="male"] <- df_melted$value[df_melted$gender=="male"]*df_summary$male_share

dev.new()
ggplot(df_melted, aes(x = variable, y = value, fill = gender)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "",
       y = "Share") +
  scale_fill_manual(values = c("female" = "blue", "male" = "red")) +
  scale_x_discrete(labels = variable_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


