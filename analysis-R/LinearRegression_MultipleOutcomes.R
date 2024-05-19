##### DESCRIPTION ######
# Author: Hitesh Pradhan
# Date: 19 May 2024
# About: The R code was developed within the SANSCOG study for conducting linear
# regression. In this code, you can specify your IV and confounders and DV(s).
# The DV(s) here represent the neurocognitive domain task scores from COGNITIO
# battery. The code loops through each neurocognitive domain task score (DVs), adjusts
# for covariates and outputs ".docx" file ready for research publication. 


##### ANALYSIS ######
# Load libraries
library(dplyr)
library(gtsummary)
library(flextable)


# Load the data
data <- read.csv("path/to/your/analysis/file/.csv")

# Convert neurocognitive task columns to numeric
# Since my DVs are continuous variable, z-score (mean-centering) is required
# for easy interpretation in standard deviation units. So, it will be, one unit
# change in my IV, my DV will increase or decrease by "this many" standard deviations.
cols_to_scale <- c("Visuospatial",
                    "Attention",
                   "Language",
                    "Memory")

# Scale the outcome variables (COGNITO)
data <- data %>%
  mutate_at(vars(cols_to_scale), as.numeric) %>%
  # Apply scale only to numeric COGNITO columns
  mutate_at(vars(cols_to_scale), scale)

# After scaling, those columns turn into matrix
# Hence, we will save the dataframe and load it again
write.csv(data, "Scaled_Data.csv")
data <- read.csv("Scaled_Data.csv")

# Auto-convert all categorical variables into factors
# All my categorical variables are coded into 0,1,2  and so on ...
binary_columns <- sapply(data, function(x) all(x %in% c(0, 1, 2)))
for (col in names(data)[binary_columns]) {
  data[[col]] <- as.factor(data[[col]])
}

# Check number of missing variables [OPTIONAL]
data %>% summarise_all(~ sum(is.na(.)))

# Function to perform multiple imputation and regression analysis
perform_regression <- function(outcome_variable) {
  print(outcome_variable)
  formula <- as.formula(paste(outcome_variable, "~ IV + Covariate_1 + Covariate_2 + ..."))
  lm_result <- lm(formula, data = data)
  tbl_regression(lm_result, exponentiate = FALSE) %>% bold_labels() %>% bold_p()
}


# Loop over outcome variables, perform regression analysis, and merge tables
outcome_variables <- c("Visuospatial", "Attention", "Language", "Memory","Composite_Score")
merged_tables <- list()

for (outcome_var in outcome_variables) {
  tbl <- perform_regression(outcome_var)
  merged_tables[[outcome_var]] <- tbl
}

# Merge tables for each outcome variable
merged_tbl <- tbl_merge(tbls = merged_tables,
                        tab_spanner = c("**Visuospatial**",
                                        "**Attention**",
                                        "**Language**",
                                        "**Memory**",
                                        "**Composite**"))


# Save merged table to Word document
merged_tbl %>% as_flex_table() %>% save_as_docx(path = "Merged_Model.docx")
