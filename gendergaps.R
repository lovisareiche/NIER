
##################################
## DK and Female participation  ##
##################################


## Housekeeping

# Install and load required packages

library(dplyr)
library(ggplot2)

# Clear the environment
rm(list = ls())


# Set the file path
file_path <- "01_tidy_data/data.csv"

# Load the CSV file
df <- read.csv(file_path) %>%
  mutate(female = sex-1) %>% # female dummy
  subset(female <= 1) %>% # remove undefined
  subset(epoint >= -888) %>% # remove -999
  subset(ppoint >= -888) # remove -999

# define -888 as na
df[df$epoint == -888,"epoint"] <- NA
df[df$ppoint == -888,"ppoint"] <- NA
df[df$q14 == 6,"q14"] <- NA


## Gender gap in means

summary <- df %>%
  #subset(yyyyqq %in% c("201901","201902","201903","201904","202001","202002", "202003", "202004","202101","202102", "202103", "202104")) %>%
  select(yyyyqq,q2, epoint, q1, ppoint,female,pi) %>%
  group_by(yyyyqq, female) %>%
  summarise(mean_pi = mean(pi), mean_epoint = mean(epoint, na.rm = TRUE), share_edk = mean(q2 == 9, na.rm = TRUE), share_ehigher5 = mean(q2 == 8, na.rm = TRUE), share_e0 = mean(q2 == 2, na.rm = TRUE),mean_ppoint = mean(ppoint, na.rm = TRUE), mean_ppoint_error = mean(ppoint-pi, na.rm = TRUE), share_pdk = mean(q1 == 9, na.rm = TRUE))

sum_female <- summary[summary$female==1,]
sum_male <- summary[summary$female==0,]

dev.new()
plots <- lapply(c("mean_epoint", "mean_ppoint"), function(variable) {
  ggplot() +
    geom_line(data = sum_female, aes(x = factor(yyyyqq), y = get(variable), group = 1), color = "blue") +
    geom_line(data = sum_male, aes(x = factor(yyyyqq), y = get(variable), group = 1), color = "red") +
    geom_vline(xintercept = factor(202002), linetype = "dashed", color = "green", size = 1) +
    labs(x="",
         y = ifelse(variable == "mean_epoint", "Average Expectations", "Average Perceptions"),
         color = "Gender") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
})

# Add a new plot for the timeseries of pi below the existing plots
pi_plot <- ggplot() +
  geom_line(data = summary, aes(x = factor(yyyyqq), y = mean_pi, group = 1), color = "black") +
  labs(x="", y = "CPI Inflation", color = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Combine the plots into a grid
gridExtra::grid.arrange(grobs = c(plots, list(pi_plot)), ncol = 1)


## Misperceptions


dev.new()
plots <- lapply(c("mean_ppoint_error","share_pdk","share_edk"), function(variable) {
  ggplot() +
    geom_line(data = sum_female, aes(x = factor(yyyyqq), y = get(variable), group = 1), color = "blue") +
    geom_line(data = sum_male, aes(x = factor(yyyyqq), y = get(variable), group = 1), color = "red") +
    geom_vline(xintercept = factor(202002), linetype = "dashed", color = "green", size = 1) +
    labs(x="",
         y = ifelse(variable == "mean_ppoint_error", "Average Misperceptions",
                    ifelse(variable == "share_pdk", "Share Don't Know (P)", "Share Don't Know (E)")),
         color = "Gender") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
})

# Combine the plots into a grid
gridExtra::grid.arrange(grobs = c(plots, list(pi_plot)), ncol = 1)


## Inflation bad

# Mapping function
mutate_function <- function(x) {
  ifelse(x == 1, 1,
         ifelse(x == 2, 0,
                ifelse(x == 3, -1, NA)))
}

summary <- df %>%
  select(yyyyqq,q3,female,pi) %>%
  group_by(yyyyqq, female) %>%
  mutate(q3 = mutate_function(q3)) %>%
  summarise(mean_bad = mean(q3, na.rm = TRUE), share_dk = mean(is.na(q3)), mean_pi=mean(pi))

sum_female <- summary[summary$female==1,]
sum_male <- summary[summary$female==0,]

dev.new()
plots <- lapply(c("mean_bad", "share_dk"), function(variable) {
  ggplot() +
    geom_line(data = sum_female, aes(x = factor(yyyyqq), y = get(variable), group = 1), color = "blue") +
    geom_line(data = sum_male, aes(x = factor(yyyyqq), y = get(variable), group = 1), color = "red") +
    geom_vline(xintercept = factor(202002), linetype = "dashed", color = "green", size = 1) +
    labs(
      x = "",
      y = ifelse(variable == "mean_bad", "Average Assessment","Share Don't Know"),
                        
      color = "Gender") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
})

# Combine the plots into a grid
gridExtra::grid.arrange(grobs = c(plots, list(pi_plot)), ncol = 1)



## BOE satisfaction

# Mapping function
mutate_function <- function(x) {
  ifelse(x == 1, 1,
         ifelse(x == 2, 0.5,
                ifelse(x == 3, 0,
                       ifelse(x == 4, -0.5,
                              ifelse(x == 5, -1, NA)))))
}

summary <- df %>%
  select(yyyyqq,q14,female) %>%
  group_by(yyyyqq, female) %>%
  mutate(q14 = mutate_function(q14)) %>%
  summarise(mean_sat = mean(q14, na.rm = TRUE), share_dk = mean(is.na(q14)))

sum_female <- summary[summary$female==1,]
sum_male <- summary[summary$female==0,]

dev.new()
plots <- lapply(c("mean_sat", "share_dk"), function(variable) {
  ggplot() +
    geom_line(data = sum_female, aes(x = factor(yyyyqq), y = get(variable), group = 1), color = "blue") +
    geom_line(data = sum_male, aes(x = factor(yyyyqq), y = get(variable), group = 1), color = "red") +
    geom_vline(xintercept = factor(202002), linetype = "dashed", color = "green", size = 1) +
    labs(
         x = "",
         y = ifelse(variable == "mean_sat", "Average Assessment","Share Don't Know"),
         color = "Gender") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
})

# Combine the plots into a grid
gridExtra::grid.arrange(grobs = c(plots, list(pi_plot)), ncol = 1)



## BOE Knowledge

summary <- df %>%
  select(yyyyqq,q11, q12, q13,female, pi) %>%
  mutate(q11_correct = 2*(q11==1), q12_correct = 2*(q12==3), q13_correct = 2*(q13==3),q11_dk= (q11==7), q12_dk = (q12==6), q13_dk = (q13==5)) %>%
  mutate(test_score_3E = q11_correct + q12_correct + q13_correct + q11_dk + q12_dk + q13_dk) %>%
  mutate(test_score_me = 0.5*q11_correct + 0.5*q12_correct + 0.5*q13_correct) %>%
  group_by(yyyyqq, female) %>%
  summarise(mean_pi = mean(pi), mean_score_3E = mean(test_score_3E, na.rm = TRUE), mean_score_me = mean(test_score_me, na.rm = TRUE), mean_score_me_rob = mean(test_score_me[q11!=7 & q12!=6 & q13!=5], na.rm = TRUE), share_dk = mean(q11==7 | q12==6 | q13==5))

sum_female <- summary[summary$female==1,]
sum_male <- summary[summary$female==0,]

# main plot
dev.new()
plots <- lapply(c( "mean_score_me", "share_dk"), function(variable) {
  ggplot() +
    geom_line(data = sum_female, aes(x = factor(yyyyqq), y = get(variable), group = 1), color = "blue") +
    geom_point(data = sum_female, aes(x = factor(yyyyqq), y = get(variable)), color = "blue", size = 3) +
    geom_line(data = sum_male, aes(x = factor(yyyyqq), y = get(variable), group = 1), color = "red") +
    geom_point(data = sum_male, aes(x = factor(yyyyqq), y = get(variable)), color = "red", size = 3) +
    geom_vline(xintercept = factor(202002), linetype = "dashed", color = "green", size = 1) +
    labs(
         x = "",
         y = ifelse(variable == "mean_score_me", "Average Knowledge (out of 3)", "Share Don't Know"),
         color = "Gender") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
})

# Combine the plots into a grid
gridExtra::grid.arrange(grobs = c(plots, list(pi_plot)), ncol = 1)

dev.new()
plots <- lapply(c( "mean_score_3E", "mean_score_me_rob"), function(variable) {
  ggplot() +
    geom_line(data = sum_female, aes(x = factor(yyyyqq), y = get(variable), group = 1), color = "blue") +
    geom_point(data = sum_female, aes(x = factor(yyyyqq), y = get(variable)), color = "blue", size = 3) +
    geom_line(data = sum_male, aes(x = factor(yyyyqq), y = get(variable), group = 1), color = "red") +
    geom_point(data = sum_male, aes(x = factor(yyyyqq), y = get(variable)), color = "red", size = 3) +
    geom_vline(xintercept = factor(202002), linetype = "dashed", color = "green", size = 1) +
    labs(
      x = "",
      y = ifelse(variable == "mean_score_3E", "Average Knowledge (out of 6)", "Average Knowledge (out of 3, excl. DK)"),
      color = "Gender") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
})

# Combine the plots into a grid
gridExtra::grid.arrange(grobs = c(plots, list(pi_plot)), ncol = 1)



## Inflation Behaviour

summary <- df %>%
  select(yyyyqq,q17_1, q17_2, q17_3, q17_4, q17_5, q17_6, q17_7, q17_8,female) %>%
  subset(yyyyqq >= as.numeric(201201)) %>%
  group_by(yyyyqq, female) %>%
  summarise(spend_major = mean(q17_1, na.rm = TRUE),save = mean(q17_2, na.rm = TRUE),shop_around = mean(q17_3, na.rm = TRUE),push_wage = mean(q17_4, na.rm = TRUE),other_income = mean(q17_5, na.rm = TRUE),move_assets = mean(q17_6, na.rm = TRUE),no_action = mean(q17_7, na.rm = TRUE),dk = mean(q17_8, na.rm = TRUE))

sum_female <- summary[summary$female==1,]
sum_male <- summary[summary$female==0,]

dev.new()
plots <- lapply(c("spend_major", "save","shop_around","push_wage","other_income","move_assets","no_action",  "dk"), function(variable) {
  ggplot() +
    geom_line(data = sum_female, aes(x = factor(yyyyqq), y = get(variable), group = 1), color = "blue") +
    geom_point(data = sum_female, aes(x = factor(yyyyqq), y = get(variable)), color = "blue", size = 3) +
    geom_line(data = sum_male, aes(x = factor(yyyyqq), y = get(variable), group = 1), color = "red") +
    geom_point(data = sum_male, aes(x = factor(yyyyqq), y = get(variable)), color = "red", size = 3) +
    geom_vline(xintercept = factor(202002), linetype = "dashed", color = "green", size = 1) +
    labs(
         x = "",
         y = variable,
         color = "Gender") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    coord_cartesian(xlim = c(factor(201101), NA))  # Set x-axis limits
})

# Combine the plots into a grid
gridExtra::grid.arrange(grobs = plots, ncol = 2)


## Source BOE Knowledge

summary <- df %>%
  select(yyyyqq,q12a1, q12a2, q12a3, q12a4, q12a5, q12a6, q12a7, q12a8,female) %>%
  subset(yyyyqq >= as.numeric(201002)) %>%
  subset(yyyyqq <= as.numeric(201501)) %>%
  group_by(yyyyqq, female) %>%
  summarise(newspaper = mean(q12a1, na.rm = TRUE),tv_radio = mean(q12a2, na.rm = TRUE),boe_website = mean(q12a3, na.rm = TRUE),other_website = mean(q12a4, na.rm = TRUE),boe_publication = mean(q12a5, na.rm = TRUE),boe_enquiries = mean(q12a6, na.rm = TRUE),none = mean(q12a7, na.rm = TRUE),dk = mean(q12a8, na.rm = TRUE))

sum_female <- summary[summary$female==1,]
sum_male <- summary[summary$female==0,]

dev.new()
plots <- lapply(c("newspaper", "tv_radio","boe_website","other_website","boe_publication","boe_enquiries","none",  "dk"), function(variable) {
  ggplot() +
    geom_line(data = sum_female, aes(x = factor(yyyyqq), y = get(variable), group = 1), color = "blue") +
    geom_point(data = sum_female, aes(x = factor(yyyyqq), y = get(variable)), color = "blue", size = 3) +
    geom_line(data = sum_male, aes(x = factor(yyyyqq), y = get(variable), group = 1), color = "red") +
    geom_point(data = sum_male, aes(x = factor(yyyyqq), y = get(variable)), color = "red", size = 3) +
    geom_vline(xintercept = factor(202002), linetype = "dashed", color = "green", size = 1) +
    labs(
      x = "",
      y = variable,
      color = "Gender") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    coord_cartesian(xlim = c(factor(201101), NA))  # Set x-axis limits
})

# Combine the plots into a grid
gridExtra::grid.arrange(grobs = plots, ncol = 2)


