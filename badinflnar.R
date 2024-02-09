
####################################
## Bad inflation narrative time  ##
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

## Aggregate

agg <- df %>%
  select(q3, yyyyqq) %>%
  #subset(q3 != 4) %>% # remove don't know obs
  group_by(yyyyqq) %>%
  summarise(mean_q3 = mean(q3, na.rm = TRUE), median_q3 = median(q3, na.rm = TRUE), Stronger = mean(q3 == 1, na.rm = TRUE), Little_difference = mean(q3 == 2, na.rm = TRUE), Weaker = mean(q3 == 3, na.rm = TRUE), DK = mean(q3 == 4, na.rm = TRUE))

  
  # Gather the data into a long format
  long_agg <- agg %>%
    tidyr::gather(key = "q3_category", value = "value", -yyyyqq) %>%
    subset(q3_category != "mean_q3") %>%
    subset(q3_category != "median_q3")
  
  dev.new()
  ggplot(long_agg, aes(x = factor(yyyyqq), y = value, fill = q3_category)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = "If prices started to rise faster than they do now, do you think Britain's economy would end up stronger, weaker or would it make little difference?",
         x = "Quarter",
         y = "Share of respondents",
         fill = "q3 Category") +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

  
  ## By demographics

  gender <- df %>%
    mutate(female = sex-1) %>%
    select(q3, yyyyqq, female) %>%
    subset(female <= 1) %>% # remove unspecified
    group_by(yyyyqq, female) %>%
    summarise(mean_q3 = mean(q3, na.rm = TRUE), median_q3 = median(q3, na.rm = TRUE), Stronger = mean(q3 == 1, na.rm = TRUE), Little_difference = mean(q3 == 2, na.rm = TRUE), Weaker = mean(q3 == 3, na.rm = TRUE), DK = mean(q3 == 4, na.rm = TRUE))
  
  
  # Gather the data into a long format
  long_gender <- gender %>%
    tidyr::gather(key = "q3_category", value = "value", -yyyyqq, -female) %>%
    subset(q3_category != "mean_q3") %>%
    subset(q3_category != "median_q3") %>%
    subset(q3_category != "DK")
  
  sum_female <- gender[gender$female==1,]
  sum_male <- gender[gender$female==0,]
  
  dev.new()
  plots <- lapply(c("Stronger", "Weaker", "Little_difference", "DK"), function(variable) {
    ggplot() +
      geom_line(data = sum_female, aes(x = factor(yyyyqq), y = get(variable), group = 1), color = "blue") +
      geom_line(data = sum_male, aes(x = factor(yyyyqq), y = get(variable), group = 1), color = "red") +
      geom_vline(xintercept = factor(202002), linetype = "dashed", color = "green", size = 1) +
      labs(x="",
           y = variable,
           color = "Gender") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
gridExtra::grid.arrange(grobs = plots, ncol = 1)
  

  