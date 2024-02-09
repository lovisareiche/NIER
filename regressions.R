# Install and load required packages

library(dplyr)
library(ggplot2)
library(broom)
library(stargazer)

# Clear the environment
rm(list = ls())


# Set the file path
file_path <- "01_tidy_data/data.csv"

# Load the CSV file
df <- read.csv(file_path) %>%
  mutate(female = sex-1) %>% # female dummy
  subset(female <= 1) %>% # remove undefined
  subset(epoint >= -888) %>% # remove -999
  subset(ppoint >= -888) %>% # remove -999
  mutate(employed = work==1) %>% # make employed dummy
  subset(income<=11) %>% # remove no answer to income
  mutate(own_house = tenure==1, mortgage = tenure==2, council_house = tenure==3, scotland = sreg==1, north = sreg==2, midlands = sreg==3, wales = sreg==4) 
  

# define -888 as na
df[df$epoint == -888,"epoint"] <- NA
df[df$ppoint == -888,"ppoint"] <- NA


## BASE
#######



# Run OLS regression with time fixed effects
basee <- lm(epoint ~ female + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq), data = df)
basep <- lm(ppoint ~ female + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq) , data = df)


# Extract coefficients and covariance matrix
coefficients <- tidy(basee, conf.int = TRUE)
cov_matrix <- vcov(basee)

# Define the coefficients of interest
coef_female <- coefficients[coefficients$term == "female", ]
coef_interaction <- coefficients[98:181, ]

ints <- paste("female:factor(yyyyqq)",unique(df$yyyyqq[df$yyyyqq>200101]),sep = "")

# Calculate the standard error for the sum of female and yyyyqq:female
se_sum <- sqrt(coef_female$std.error^2 + coef_interaction$std.error^2 + 2 * cov_matrix["female", ints])

# Create a dataframe for plotting
plot_data <- data.frame(
  yyyyqq = factor(unique(df$yyyyqq[df$yyyyqq > 200101])),
  estimate = coef_female$estimate + coef_interaction$estimate,
  low =  (coef_female$estimate + coef_interaction$estimate)-1.96*se_sum,
  high = (coef_female$estimate + coef_interaction$estimate)+1.96*se_sum
)


# Plot
dev.new()
ggplot(plot_data, aes(x = factor(yyyyqq), y = estimate, group = 1)) +
  geom_line(color = "black") +
  geom_vline(xintercept = factor(202002), linetype = "dashed", color = "green", linewidth = 1) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) +  # Thick line at y=0
  labs(x="",
       y ="Estimated Gender Gap") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Knowledge
############

df <- df %>%
  mutate(q11_correct = 2*(q11==1), q12_correct = 2*(q12==3), q13_correct = 2*(q13==3),q11_dk= (q11==7), q12_dk = (q12==6), q13_dk = (q13==5)) %>%
  mutate(test_score_3E = q11_correct + q12_correct + q13_correct + q11_dk + q12_dk + q13_dk) %>%
  mutate(test_score_me = 0.5*q11_correct + 0.5*q12_correct + 0.5*q13_correct)

# decide if you want to use don't know 
df_sub <- subset(df, q11_dk==FALSE & q12_dk==FALSE & q13_dk==FALSE)

# Run OLS regression with time fixed effects
base <- lm(test_score_me ~ female + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq) + factor(yyyyqq):female, data = df)
base_2 <- lm(test_score_me ~ female + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq), data = df)
# with boe info source control
sourceboe <- lm(test_score_me ~ female + q12a1 + q12a2 + q12a3 + q12a4 + q12a5 + q12a6 + q12a7 + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq) + factor(yyyyqq):female, data = df)
sourceboe_2 <- lm(test_score_me ~ female + q12a1 + q12a2 + q12a3 + q12a4 + q12a5 + q12a6 + q12a7 + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq), data = df)
# with pi info source control
sourcepi <- lm(test_score_me ~ female + q2aiv1 + q2aiv2 + q2aiv3 + q2aiv4 + q2aiv5 + q2aiv6 + q2aiv7 + q2aiv8 + q2aiv9 + q2aiv11 + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales , data = df)
base3E <- lm(test_score_3E ~ female + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq) + factor(yyyyqq):female, data = df)
baserob <- lm(test_score_me ~ female + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq) + factor(yyyyqq):female, data = df_sub)
base3E_2 <- lm(test_score_3E ~ female + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq), data = df)
baserob_2 <- lm(test_score_me ~ female + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq), data = df_sub)

# base

# Extract coefficients and covariance matrix
coefficients <- tidy(base, conf.int = TRUE)
cov_matrix <- vcov(base)

# Define the coefficients of interest
coef_female <- coefficients[coefficients$term == "female", ]
coef_interaction <- coefficients[39:63, ]

ints <- paste("female:factor(yyyyqq)",unique(df$yyyyqq[df$yyyyqq>200101 & !is.na(df$test_score_me)]),sep = "")

# Calculate the standard error for the sum of female and yyyyqq:female
se_sum <- sqrt(coef_female$std.error^2 + coef_interaction$std.error^2 + 2 * cov_matrix["female", ints])

# Create a dataframe for plotting
plot_data_base <- data.frame(
  yyyyqq = factor(unique(df$yyyyqq[df$yyyyqq > 200101 & !is.na(df$test_score_me)])),
  estimate = coef_female$estimate + coef_interaction$estimate,
  low =  (coef_female$estimate + coef_interaction$estimate)-1.96*se_sum,
  high = (coef_female$estimate + coef_interaction$estimate)+1.96*se_sum
)

# source boe control

# Extract coefficients and covariance matrix
coefficients <- tidy(sourceboe, conf.int = TRUE)
cov_matrix <- vcov(sourceboe)

# Define the coefficients of interest
coef_female <- coefficients[coefficients$term == "female", ]
coef_interaction <- coefficients[28:34, ]

ints <- paste("female:factor(yyyyqq)",unique(df$yyyyqq[df$yyyyqq>201002 & !is.na(df$test_score_me) & !is.na(df$q12a1)]),sep = "")

# Calculate the standard error for the sum of female and yyyyqq:female
se_sum <- sqrt(coef_female$std.error^2 + coef_interaction$std.error^2 + 2 * cov_matrix["female", ints])

# Create a dataframe for plotting
plot_data_sourceboe <- data.frame(
  yyyyqq = factor(unique(df$yyyyqq[df$yyyyqq>201002 & !is.na(df$test_score_me) & !is.na(df$q12a1)])),
  estimate = coef_female$estimate + coef_interaction$estimate,
  low =  (coef_female$estimate + coef_interaction$estimate)-1.96*se_sum,
  high = (coef_female$estimate + coef_interaction$estimate)+1.96*se_sum
)

# source pi control

# Extract coefficients and covariance matrix
coefficients <- tidy(sourcepi, conf.int = TRUE)
cov_matrix <- vcov(sourcepi)

# Define the coefficients of interest
coef_female <- coefficients[coefficients$term == "female", ]

# note: no interaction since we didn't use factor

# Calculate the standard error for the sum of female and yyyyqq:female
se_sum <- coef_female$std.error

# Create a dataframe for plotting
plot_data_sourcepi <- data.frame(
  yyyyqq = seq(201701,202301,100),
  estimate = rep(coef_female$estimate,7) ,
  low =  rep((coef_female$estimate) -1.96*se_sum,7),
  high = rep((coef_female$estimate) +1.96*se_sum,7)
)


# Combine the two data frames and create a grouping variable
plot_data_combined <- rbind(
  transform(plot_data_base, Model = "Base"),
  transform(plot_data_sourceboe, Model = "SourceBOE"),
  transform(plot_data_sourcepi, Model = "SourcePi")
)

# Plot
dev.new()
ggplot(plot_data_combined[plot_data_combined$yyyyqq != 201002 & plot_data_combined$yyyyqq != 201003 & plot_data_combined$yyyyqq != 201004,], aes(x = factor(yyyyqq), y = estimate, group = Model, color = Model)) +
  geom_line() + 
  geom_ribbon(aes(ymin = low, ymax = high, fill = Model), alpha = 0.1, color = NA) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) +
  #geom_hline(yintercept = plot_data_sourcepi$estimate, linetype = "solid", color = "green", size = 1) +
  #geom_ribbon(aes(ymin = plot_data_sourcepi$low, ymax = plot_data_sourcepi$high), alpha = 0.05, color = NA) +
  labs(x = "", y = "Estimated Gender Gap") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(color = "Model") +
  scale_color_manual(values = c("Base" = "black", "SourceBOE" = "blue","SourcePi" = "green")) +
  scale_fill_manual(values = c("Base" = "black", "SourceBOE" = "blue","SourcePi" = "green"))


# --- Write output

# settings for stargazer
omit <- c("factor","q12a","q2ai")
omit.labels <- c("Time fixed effects","BoE Information Source","Inflation Information Source")
title <- "Drivers of Knowledge about the BoE"
label <- "tab:boeknowreg"
dep.var.labels <- c("Test Score","(as in 3E)","(excl. DK)")


# in which order
desiredOrder <- c("Constant","female","age","single","educ","income","employed","own_house","mortgage","council_house","scotland","north","wales","midlands")

writeLines(capture.output(stargazer(base,base_2,sourceboe_2,sourcepi,base3E_2,baserob_2,
                                    title = title, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels, no.space = FALSE)), 
           file.path('04_regtables/code_tabboeknow.tex'))




## Inflation DK
############

df <- df %>%
  mutate(epoint_dk = q2==9) %>%
  mutate(d201701 = yyyyqq==201701)

# Run OLS regression with time fixed effects
base <- glm(epoint_dk ~ female + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq) + factor(yyyyqq):female, data = df, family = "binomial")
base_2 <- glm(epoint_dk ~ female + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq), data = df, family = "binomial")
# with boe info source control
sourceboe <- glm(epoint_dk ~ female + q12a1 + q12a2 + q12a3 + q12a4 + q12a5 + q12a6 + q12a7 + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq) + factor(yyyyqq):female, data = df, family = "binomial")
sourceboe_2 <- glm(epoint_dk ~ female + q12a1 + q12a2 + q12a3 + q12a4 + q12a5 + q12a6 + q12a7 + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq) , data = df, family = "binomial")
# with pi info source control
sourcepi <- glm(epoint_dk ~ female + q2aiv1 + q2aiv2 + q2aiv3 + q2aiv4 + q2aiv5 + q2aiv6 + q2aiv7 + q2aiv8 + q2aiv9 + q2aiv11 + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales, data = df, family = "binomial")

# base

# Extract coefficients and covariance matrix
#coefficients <- tidy(base, conf.int = TRUE)
# Extract coefficients and confidence intervals
coefficients <- summary(base)$coefficients[, c("Estimate", "Std. Error")]
names(coefficients) <- c("estimate", "std.error")

cov_matrix <- vcov(base)

# Define the coefficients of interest
coef_female <- coefficients["female", ]
coef_interaction <- coefficients[98:181, ]

ints <- paste("female:factor(yyyyqq)",unique(df$yyyyqq[df$yyyyqq>200101]),sep = "")

# Calculate the standard error for the sum of female and yyyyqq:female
se_sum <- sqrt(coef_female["Std. Error"]^2 + coef_interaction[,"Std. Error"]^2 + 2 * cov_matrix["female", ints])

# Create a dataframe for plotting
plot_data_base <- data.frame(
  yyyyqq = factor(unique(df$yyyyqq[df$yyyyqq > 200101 ])),
  estimate = coef_female["Estimate"] + coef_interaction[,"Estimate"],
  low =  (coef_female["Estimate"] + coef_interaction[,"Estimate"])-1.96*se_sum,
  high = (coef_female["Estimate"] + coef_interaction[,"Estimate"])+1.96*se_sum
)

# source boe control

# Extract coefficients and covariance matrix
coefficients <- summary(sourceboe)$coefficients[, c("Estimate", "Std. Error")]
cov_matrix <- vcov(sourceboe)

# Define the coefficients of interest
coef_female <- coefficients["female", ]
coef_interaction <- coefficients[28:34, ]

ints <- paste("female:factor(yyyyqq)",unique(df$yyyyqq[df$yyyyqq>201002 & !is.na(df$q12a1)]),sep = "")

# Calculate the standard error for the sum of female and yyyyqq:female
se_sum <- sqrt(coef_female["Std. Error"]^2 + coef_interaction[,"Std. Error"]^2 + 2 * cov_matrix["female", ints])

# Create a dataframe for plotting
plot_data_sourceboe <- data.frame(
  yyyyqq = factor(unique(df$yyyyqq[df$yyyyqq>201002 & !is.na(df$q12a1)])),
  estimate = coef_female["Estimate"] + coef_interaction[,"Estimate"],
  low =  (coef_female["Estimate"] + coef_interaction[,"Estimate"])-1.96*se_sum,
  high = (coef_female["Estimate"] + coef_interaction[,"Estimate"])+1.96*se_sum
)



# source pi control

# Extract coefficients and covariance matrix
coefficients <- summary(sourcepi)$coefficients[, c("Estimate", "Std. Error")]
cov_matrix <- vcov(sourcepi)

# Define the coefficients of interest
coef_female <- coefficients[ "female", ]

# note: no interaction since we didn't use factor

# Calculate the standard error for the sum of female and yyyyqq:female
se_sum <- coef_female["Std. Error"]

# Create a dataframe for plotting
plot_data_sourcepi <- data.frame(
  yyyyqq = seq(201701,202301,100),
  estimate = rep(coef_female["Estimate"],7) ,
  low =  rep((coef_female["Estimate"]) -1.96*se_sum,7),
  high = rep((coef_female["Estimate"]) +1.96*se_sum,7)
)


# Combine the two data frames and create a grouping variable
plot_data_combined <- rbind(
  transform(plot_data_base, Model = "Base"),
  transform(plot_data_sourceboe, Model = "SourceBOE") #,
  #transform(plot_data_sourcepi, Model = "SourcePi")
)

# Plot
dev.new()
ggplot(plot_data_combined[plot_data_combined$yyyyqq != 201002 & plot_data_combined$yyyyqq != 201003 & plot_data_combined$yyyyqq != 201004,], aes(x = factor(yyyyqq), y = estimate, group = Model, color = Model)) +
  geom_line() + 
  geom_ribbon(aes(ymin = low, ymax = high, fill = Model), alpha = 0.1, color = NA) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) +
  #geom_hline(yintercept = plot_data_sourcepi$estimate, linetype = "solid", color = "green", size = 1) +
  #geom_ribbon(aes(ymin = plot_data_sourcepi$low, ymax = plot_data_sourcepi$high), alpha = 0.05, color = NA) +
  labs(x = "", y = "Estimated Gender Gap") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(color = "Model") +
  scale_color_manual(values = c("Base" = "black", "SourceBOE" = "blue","SourcePi" = "green")) +
  scale_fill_manual(values = c("Base" = "black", "SourceBOE" = "blue","SourcePi" = "green"))


# --- Write output

# settings for stargazer
omit <- c("factor","q12a")
omit.labels <- c("Time Fixed Effects","BoE Information Source")
title <- "Drivers of Inflation Expectations"
label <- "tab:pireg"
dep.var.labels <- c("Inflation Perception","Inflation Expectation","Don't Know")


# in which order
desiredOrder <- c("Constant","female","age","single","educ","income","employed","own_house","mortgage","council_house","scotland","north","wales","midlands")

writeLines(capture.output(stargazer(basep,basee,base,base_2,sourceboe_2,
                                    title = title, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels, no.space = FALSE)), 
           file.path('04_regtables/code_tabpireg.tex'))



## Inflation Response and Bad
############################

mutate_function <- function(x) {
  ifelse(x == 1, 1,
         ifelse(x == 2, 0,
                ifelse(x == 3, -1, NA)))
}

df <- df %>%
  mutate(q3 = mutate_function(q3))

# Run OLS regression with time fixed effects
base <- lm(q3 ~ female + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq) + factor(yyyyqq):female, data = df)
base_2 <- lm(q3 ~ female + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq), data = df)
# with boe info source control
responsepi <- lm(q3 ~ female + q17_1 + q17_2 + q17_3 + q17_4 + q17_5 + q17_6 + q17_7 + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq) + factor(yyyyqq):female, data = df)
responsepisourceboe <- lm(q3 ~ female + q12a1 + q12a2 + q12a3 + q12a4 + q12a5 + q12a6 + q12a7 + q17_1 + q17_2 + q17_3 + q17_4 + q17_5 + q17_6 + q17_7 + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq) + factor(yyyyqq):female, data = df)
responsepi_2 <- lm(q3 ~ female + q17_1 + q17_2 + q17_3 + q17_4 + q17_5 + q17_6 + q17_7 + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq), data = df)
responsepisourceboe_2 <- lm(q3 ~ female + q12a1 + q12a2 + q12a3 + q12a4 + q12a5 + q12a6 + q12a7 + q17_1 + q17_2 + q17_3 + q17_4 + q17_5 + q17_6 + q17_7 + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq) , data = df)

# base

# Extract coefficients and covariance matrix
#coefficients <- tidy(base, conf.int = TRUE)
# Extract coefficients and confidence intervals
coefficients <- summary(base)$coefficients[, c("Estimate", "Std. Error")]
names(coefficients) <- c("estimate", "std.error")

cov_matrix <- vcov(base)

# Define the coefficients of interest
coef_female <- coefficients["female", ]
coef_interaction <- coefficients[98:181, ]

ints <- paste("female:factor(yyyyqq)",unique(df$yyyyqq[df$yyyyqq>200101]),sep = "")

# Calculate the standard error for the sum of female and yyyyqq:female
se_sum <- sqrt(coef_female["Std. Error"]^2 + coef_interaction[,"Std. Error"]^2 + 2 * cov_matrix["female", ints])

# Create a dataframe for plotting
plot_data_base <- data.frame(
  yyyyqq = factor(unique(df$yyyyqq[df$yyyyqq > 200101 ])),
  estimate = coef_female["Estimate"] + coef_interaction[,"Estimate"],
  low =  (coef_female["Estimate"] + coef_interaction[,"Estimate"])-1.96*se_sum,
  high = (coef_female["Estimate"] + coef_interaction[,"Estimate"])+1.96*se_sum
)

# responsepi control

# Extract coefficients and covariance matrix
coefficients <- summary(responsepi)$coefficients[, c("Estimate", "Std. Error")]
cov_matrix <- vcov(responsepi)

# Define the coefficients of interest
coef_female <- coefficients["female", ]
coef_interaction <- coefficients[32:42, ]

ints <- paste("female:factor(yyyyqq)",unique(df$yyyyqq[df$yyyyqq>201204 & !is.na(df$q17_1)]),sep = "")

# Calculate the standard error for the sum of female and yyyyqq:female
se_sum <- sqrt(coef_female["Std. Error"]^2 + coef_interaction[,"Std. Error"]^2 + 2 * cov_matrix["female", ints])

# Create a dataframe for plotting
plot_data_sourceboe <- data.frame(
  yyyyqq = factor(unique(df$yyyyqq[df$yyyyqq>201204 & !is.na(df$q17_1)])),
  estimate = coef_female["Estimate"] + coef_interaction[,"Estimate"],
  low =  (coef_female["Estimate"] + coef_interaction[,"Estimate"])-1.96*se_sum,
  high = (coef_female["Estimate"] + coef_interaction[,"Estimate"])+1.96*se_sum
)

# responsepi + sourceboe control

# Extract coefficients and covariance matrix
coefficients <- summary(responsepisourceboe)$coefficients[, c("Estimate", "Std. Error")]
cov_matrix <- vcov(responsepisourceboe)

# Define the coefficients of interest
coef_female <- coefficients["female", ]
coef_interaction <- coefficients[31:33, ]

ints <- paste("female:factor(yyyyqq)",unique(df$yyyyqq[df$yyyyqq>201204 & !is.na(df$q17_1) & !is.na(df$q12a1)]) ,sep = "")

# Calculate the standard error for the sum of female and yyyyqq:female
se_sum <- sqrt(coef_female["Std. Error"]^2 + coef_interaction[,"Std. Error"]^2 + 2 * cov_matrix["female", ints])

# Create a dataframe for plotting
plot_data_responsepisourceboe <- data.frame(
  yyyyqq = factor(unique(df$yyyyqq[df$yyyyqq>201204 & !is.na(df$q17_1) & !is.na(df$q12a1)])),
  estimate = coef_female["Estimate"] + coef_interaction[,"Estimate"],
  low =  (coef_female["Estimate"] + coef_interaction[,"Estimate"])-1.96*se_sum,
  high = (coef_female["Estimate"] + coef_interaction[,"Estimate"])+1.96*se_sum
)



# Combine the two data frames and create a grouping variable
plot_data_combined <- rbind(
  transform(plot_data_base, Model = "Base"),
  transform(plot_data_sourceboe, Model = "ResponsePi"),
  transform(plot_data_responsepisourceboe, Model = "ResponsePi_SourceBOE")
)

# Plot
dev.new()
ggplot(plot_data_combined, aes(x = factor(yyyyqq), y = estimate, group = Model, color = Model)) +
  geom_line() +
  geom_ribbon(aes(ymin = low, ymax = high, fill = Model), alpha = 0.1, color = NA) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) +
  labs(x = "", y = "Estimated Gender Gap") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(color = "Model") +
  scale_color_manual(values = c("Base" = "black", "ResponsePi" = "red", "ResponsePi_SourceBOE" = "purple")) +
  scale_fill_manual(values = c("Base" = "black", "ResponsePi" = "red", "ResponsePi_SourceBOE" = "purple"))

# --- Write output

# settings for stargazer
omit <- c("factor","q17","q12a")
omit.labels <- c("Time Fixed Effects","Response to Inflation","BoE Information Source")
title <- "Drivers of Inflation Expectations"
label <- "tab:pibadreg"
dep.var.labels <- c("Inflation Assessment")


# in which order
desiredOrder <- c("Constant","female","age","single","educ","income","employed","own_house","mortgage","council_house","scotland","north","wales","midlands")

writeLines(capture.output(stargazer(base,base_2,responsepi_2,responsepisourceboe_2,
                                    title = title, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels, no.space = FALSE)), 
           file.path('04_regtables/code_tabpibadreg.tex'))



## Inflation Response and BoE Rating
############################

mutate_function <- function(x) {
  ifelse(x == 1, 1,
         ifelse(x == 2, 0.5,
                ifelse(x == 3, 0,
                       ifelse(x == 4, -0.5,
                              ifelse(x == 5, -1, NA)))))
}

df <- df %>%
  mutate(q14 = mutate_function(q14))

# Run OLS regression with time fixed effects
base <- lm(q14 ~ female + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq) + factor(yyyyqq):female, data = df)
base_2 <- lm(q14 ~ female + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq) , data = df)
# with boe info source control
responseboe <- lm(q14 ~ female + q17_1 + q17_2 + q17_3 + q17_4 + q17_5 + q17_6 + q17_7 + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq) + factor(yyyyqq):female, data = df)
responsepisourceboe <- lm(q14 ~ female + q12a1 + q12a2 + q12a3 + q12a4 + q12a5 + q12a6 + q12a7 + q17_1 + q17_2 + q17_3 + q17_4 + q17_5 + q17_6 + q17_7 + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq) + factor(yyyyqq):female, data = df)
responseboe_2 <- lm(q14 ~ female + q17_1 + q17_2 + q17_3 + q17_4 + q17_5 + q17_6 + q17_7 + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq), data = df)
responsepisourceboe_2 <- lm(q14 ~ female + q12a1 + q12a2 + q12a3 + q12a4 + q12a5 + q12a6 + q12a7 + q17_1 + q17_2 + q17_3 + q17_4 + q17_5 + q17_6 + q17_7 + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq) , data = df)
inflationbad <- lm(q14 ~ female + q3 + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq) + factor(yyyyqq):female, data = df)
inflationbad_2 <- lm(q14 ~ female + q3 + employed + income + educ + age + own_house + mortgage + council_house + scotland + north + midlands + wales + factor(yyyyqq) + factor(yyyyqq):female, data = df)

# base

# Extract coefficients and covariance matrix
#coefficients <- tidy(base, conf.int = TRUE)
# Extract coefficients and confidence intervals
coefficients <- summary(base)$coefficients[, c("Estimate", "Std. Error")]
names(coefficients) <- c("estimate", "std.error")

cov_matrix <- vcov(base)

# Define the coefficients of interest
coef_female <- coefficients["female", ]
coef_interaction <- coefficients[98:181, ]

ints <- paste("female:factor(yyyyqq)",unique(df$yyyyqq[df$yyyyqq>200101]),sep = "")

# Calculate the standard error for the sum of female and yyyyqq:female
se_sum <- sqrt(coef_female["Std. Error"]^2 + coef_interaction[,"Std. Error"]^2 + 2 * cov_matrix["female", ints])

# Create a dataframe for plotting
plot_data_base <- data.frame(
  yyyyqq = factor(unique(df$yyyyqq[df$yyyyqq > 200101 ])),
  estimate = coef_female["Estimate"] + coef_interaction[,"Estimate"],
  low =  (coef_female["Estimate"] + coef_interaction[,"Estimate"])-1.96*se_sum,
  high = (coef_female["Estimate"] + coef_interaction[,"Estimate"])+1.96*se_sum
)

# source boe control

# Extract coefficients and covariance matrix
coefficients <- summary(responseboe)$coefficients[, c("Estimate", "Std. Error")]
cov_matrix <- vcov(responseboe)

# Define the coefficients of interest
coef_female <- coefficients["female", ]
coef_interaction <- coefficients[32:42, ]

ints <- paste("female:factor(yyyyqq)",unique(df$yyyyqq[df$yyyyqq>201204 & !is.na(df$q17_1)]),sep = "")

# Calculate the standard error for the sum of female and yyyyqq:female
se_sum <- sqrt(coef_female["Std. Error"]^2 + coef_interaction[,"Std. Error"]^2 + 2 * cov_matrix["female", ints])

# Create a dataframe for plotting
plot_data_sourceboe <- data.frame(
  yyyyqq = factor(unique(df$yyyyqq[df$yyyyqq>201204 & !is.na(df$q17_1)])),
  estimate = coef_female["Estimate"] + coef_interaction[,"Estimate"],
  low =  (coef_female["Estimate"] + coef_interaction[,"Estimate"])-1.96*se_sum,
  high = (coef_female["Estimate"] + coef_interaction[,"Estimate"])+1.96*se_sum
)

# responsepi + sourceboe control

# Extract coefficients and covariance matrix
coefficients <- summary(responsepisourceboe)$coefficients[, c("Estimate", "Std. Error")]
cov_matrix <- vcov(responsepisourceboe)

# Define the coefficients of interest
coef_female <- coefficients["female", ]
coef_interaction <- coefficients[31:33, ]

ints <- paste("female:factor(yyyyqq)",unique(df$yyyyqq[df$yyyyqq>201204 & !is.na(df$q17_1) & !is.na(df$q12a1)]) ,sep = "")

# Calculate the standard error for the sum of female and yyyyqq:female
se_sum <- sqrt(coef_female["Std. Error"]^2 + coef_interaction[,"Std. Error"]^2 + 2 * cov_matrix["female", ints])

# Create a dataframe for plotting
plot_data_responsepisourceboe <- data.frame(
  yyyyqq = factor(unique(df$yyyyqq[df$yyyyqq>201204 & !is.na(df$q17_1) & !is.na(df$q12a1)])),
  estimate = coef_female["Estimate"] + coef_interaction[,"Estimate"],
  low =  (coef_female["Estimate"] + coef_interaction[,"Estimate"])-1.96*se_sum,
  high = (coef_female["Estimate"] + coef_interaction[,"Estimate"])+1.96*se_sum
)

# inflationbad

# Extract coefficients and covariance matrix

coefficients <- summary(inflationbad)$coefficients[, c("Estimate", "Std. Error")]
names(coefficients) <- c("estimate", "std.error")

cov_matrix <- vcov(inflationbad)

# Define the coefficients of interest
coef_female <- coefficients["female", ]
coef_interaction <- coefficients[98:181, ]

ints <- paste("female:factor(yyyyqq)",unique(df$yyyyqq[df$yyyyqq>200101]),sep = "")

# Calculate the standard error for the sum of female and yyyyqq:female
se_sum <- sqrt(coef_female["Std. Error"]^2 + coef_interaction[,"Std. Error"]^2 + 2 * cov_matrix["female", ints])

# Create a dataframe for plotting
plot_data_inflationbad <- data.frame(
  yyyyqq = factor(unique(df$yyyyqq[df$yyyyqq > 200101 ])),
  estimate = coef_female["Estimate"] + coef_interaction[,"Estimate"],
  low =  (coef_female["Estimate"] + coef_interaction[,"Estimate"])-1.96*se_sum,
  high = (coef_female["Estimate"] + coef_interaction[,"Estimate"])+1.96*se_sum
)

plot_data_inflationbad <- plot_data_inflationbad[2:84,]



# Combine the two data frames and create a grouping variable
plot_data_combined <- rbind(
  transform(plot_data_base, Model = "Base"),
  transform(plot_data_sourceboe, Model = "ResponsePi"),
  transform(plot_data_responsepisourceboe, Model = "ResponsePi_SourceBOE"),
  transform(plot_data_inflationbad, Model = "Inflation_Assessment")
)

# Plot
dev.new()
ggplot(plot_data_combined, aes(x = factor(yyyyqq), y = estimate, group = Model, color = Model)) +
  geom_line() +
  geom_ribbon(aes(ymin = low, ymax = high, fill = Model), alpha = 0.1, color = NA) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) +
  labs(x = "", y = "Estimated Gender Gap") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(color = "Model") +
  scale_color_manual(values = c("Base" = "black", "ResponsePi" = "red", "ResponsePi_SourceBOE" = "purple", "Inflation_Assessment" = "yellow")) +
  scale_fill_manual(values = c("Base" = "black", "ResponsePi" = "red", "ResponsePi_SourceBOE" = "purple", "Inflation_Assessment" = "yellow"))


# --- Write output

# settings for stargazer
omit <- c("factor","q17","q12a")
omit.labels <- c("Time Fixed Effects","Response to Inflation","BoE Information Source")
title <- "Drivers of BoE Satisfaction"
label <- "tab:BOEsatreg"
dep.var.labels <- c("Inflation Assessment")


# in which order
desiredOrder <- c("Constant","female","age","single","educ","income","employed","own_house","mortgage","council_house","scotland","north","wales","midlands")

writeLines(capture.output(stargazer(base,base_2,responseboe_2,responsepisourceboe_2,inflationbad_2,
                                    title = title, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels, no.space = FALSE)), 
           file.path('04_regtables/code_tabboesatreg.tex'))