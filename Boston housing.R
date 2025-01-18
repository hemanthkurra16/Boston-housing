install.packages("data.table")   #installing package data.table library
install.packages("plyr")   #installing package plyr library
install.packages("FSA")   #installing package FSA library
install.packages("FSAdata")   #installing package FSA data library
install.packages("magrittr")   #installing package magrittr library
install.packages("dplyr")   #installing package dplyr library
install.packages("plotrix")   #installing package plotrix library
install.packages("ggplot2")   #installing package ggplot2 library
install.packages("moments")   #installing package moments library
install.packages("lifecycle")  # Install the lifecycle package
install.packages("tidyverse")
install.packages("gtsummary")
library(glmnet)
library(caret)  # For creating train and test data
library(glmnet)  # For logistic regression
library(tidyverse)
library(lifecycle)            # Load the lifecycle packagel
library(dplyr)                # Load the dplyr package
library(plyr)  # importing plyr library
library(data.table) #importing data.table library
library(FSA)  # import FSA library
library(FSAdata)  # import FSAdata library
library(magrittr)  # import magrittr library
library(dplyr)  # import dplyr library
library(plotrix)  # import plotrix library
library(ggplot2)  # import ggplot2 library
library(moments)  # import moments library(package)
library(caret)
library(knitr)
library(plyr)
library(dplyr)
library(Hmisc)
library(finalfit)
library(ISLR)
library(psych)
library(glmnet)
library(readxl)
library(gtsummary)


# Specify the path to your XLSX file
xlsx_file <- file.choose()

# Read the XLSX file into a data frame
data <- read_xlsx(xlsx_file)

summary(data)

missing_values <- data %>%
  summarise_all(funs(sum(is.na(.))))


# Create summary statistics using psych
psych_summary <- psych::describe(data)
# Print summary statistics using psych
print(psych_summary)


# Create a summary statistics table
selected_variables <- c("AV_TOTAL", "LAND_SF", "R_BDRMS", "YR_BUILT", "NUM_FLOORS")
summary_table <- stargazer(data[, selected_variables], type = "text")
cat(summary_table)
# Preliminary descriptive analysis
desc_table <- data %>% 
  select(AV_TOTAL, YR_BUILT, LIVING_AREA, NUM_FLOORS, PTYPE) %>% 
  tbl_summary(by = PTYPE) %>% 
  add_p()

# Displaying the table
desc_table


# Descriptive statistics grouped by PTYPE
desc_stats_ptype <- data %>%
  group_by(PTYPE) %>%
  summarise(
    mean_AV_TOTAL = mean(AV_TOTAL, na.rm = TRUE),
    sd_AV_TOTAL = sd(AV_TOTAL, na.rm = TRUE),
    min_AV_TOTAL = min(AV_TOTAL, na.rm = TRUE),
    max_AV_TOTAL = max(AV_TOTAL, na.rm = TRUE)
  )

summary(desc_stats_ptype)

# Categorizing YR_BUILT into periods and summarizing AV_TOTAL
dataset <- data %>%
  mutate(
    BUILD_PERIOD = case_when(
      YR_BUILT <= 1950 ~ "Before 1950",
      YR_BUILT > 1950 & YR_BUILT <= 2000 ~ "1951-2000",
      YR_BUILT > 2000 ~ "After 2001",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(BUILD_PERIOD) %>%
  summarise(
    mean_AV_TOTAL = mean(AV_TOTAL, na.rm = TRUE),
    sd_AV_TOTAL = sd(AV_TOTAL, na.rm = TRUE),
    min_AV_TOTAL = min(AV_TOTAL, na.rm = TRUE),
    max_AV_TOTAL = max(AV_TOTAL, na.rm = TRUE)
  )

# Creating PROPERTY_AGE
data <- data %>%
  mutate(PROPERTY_AGE = as.numeric(format(Sys.Date(), "%Y")) - YR_BUILT)

# Creating SIZE_CATEGORY
data <- data %>%
  mutate(SIZE_CATEGORY = case_when(
    LIVING_AREA <= 1000 ~ "Small",
    LIVING_AREA > 1000 & LIVING_AREA <= 2000 ~ "Medium",
    LIVING_AREA > 2000 ~ "Large",
    TRUE ~ "Unknown"
  ))

# Creating LOG_AV_TOTAL
data <- data %>%
  mutate(LOG_AV_TOTAL = log(AV_TOTAL + 1))

# Creating AGE_SIZE_INTERACTION
data <- data %>%
  mutate(AGE_SIZE_INTERACTION = PROPERTY_AGE * LIVING_AREA)

data

lm_model <- lm(AV_TOTAL ~ YR_BUILT + LIVING_AREA + as.factor(PTYPE), data = data)
summary(lm_model)

model2 <- lm(AV_TOTAL ~ NUM_FLOORS + LAND_SF, data = data)
summary(model2)

data <- data %>%
  mutate(
    BUILD_PERIOD = case_when(
      YR_BUILT <= 1950 ~ "Before 1950",
      YR_BUILT > 1950 & YR_BUILT <= 2000 ~ "1951-2000",
      YR_BUILT > 2000 ~ "After 2001",
      TRUE ~ "Unknown"
    )
  )

subset_analysis <- data %>%
  group_by(BUILD_PERIOD, PTYPE) %>%
  summarise(
    mean_AV_TOTAL = mean(AV_TOTAL, na.rm = TRUE),
    sd_AV_TOTAL = sd(AV_TOTAL, na.rm = TRUE),
    min_AV_TOTAL = min(AV_TOTAL, na.rm = TRUE),
    max_AV_TOTAL = max(AV_TOTAL, na.rm = TRUE)
  )

# t-test
# Comparing property values for owner-occupied and non-owner-occupied properties
owner_occupied_values <- data$AV_TOTAL[data$OWN_OCC == "Y"]
non_owner_occupied_values <- data$AV_TOTAL[data$OWN_OCC == "N"]
# Perform an independent t-test
t_test_result <- t.test(owner_occupied_values, non_owner_occupied_values)
# Display the t-test results
print(t_test_result)

# Chi-square Test
# Chi-square test for property type and ownership status
contingency_table <- table(data$PTYPE, data$OWN_OCC)
# Perform a chi-square test
chi_square_result <- chisq.test(contingency_table)
# Display the chi-square test results
print(chi_square_result)

# Correlation Table
library(corrplot)
# Create a correlation matrix for selected numerical variables
numerical_variables <- data[, c("AV_TOTAL", "LAND_SF", "R_BDRMS", "YR_BUILT", "NUM_FLOORS")]
correlation_matrix <- cor(numerical_variables, use = "complete.obs")
print(correlation_matrix)

# Define custom axis labels
custom_labels <- c("Total Value", "Land Area", "Number of Bedrooms", "Year Built",
                   "Number of Floors")

# Set custom column and row names
colnames(correlation_matrix) <- custom_labels
rownames(correlation_matrix) <- custom_labels

# Customize the correlation plot
corrplot(correlation_matrix,
         method = "color",  # Color representation
         type = "upper",    # Display the upper triangle
         order = "hclust",  # Reorder variables
         tl.col = "black",  # Label color
         tl.srt = 45         # Label rotation
)

# LASSO
# Check for missing values and handle them
complete_cases <- complete.cases(data[c("GROSS_AREA", "YR_BUILT", "U_BDRMS", "LAND_SF", "NUM_FLOORS")])
data_clean <- data[complete_cases, ]

# Correlation Analysis
size_price_correlation <- cor.test(data_clean$GROSS_AREA, data_clean$LAND_SF, method = "pearson")

# Perform LASSO regression
x <- as.matrix(data_clean[, c("GROSS_AREA", "YR_BUILT", "U_BDRMS")])
y <- as.matrix(data_clean[, c("LAND_SF", "NUM_FLOORS")])
lasso_model <- cv.glmnet(x, y)

# Display the results
print(size_price_correlation)
lasso_model

# Plot the LASSO regression results with adjusted axis values
plot(lasso_model, log = "x", xlab = "Log(lambda)", ylab = "Coefficients")

# Correlation
cor(data$AV_TOTAL, data$LIVING_AREA, use = "complete.obs")
