#Version history
#
#
########## S T A R T   C O D E ##########


# load libraries

library(caret)


library(pROC)


library(tidyverse)


library(conflicted)


library(dplyr)


conflicts_prefer(dplyr::filter)

library(smotefamily)

library(corrplot)

library(MASS)

#################################################

# data prep

#################################################

# set working directory
setwd("~/XXXXX")

# load data


combinedDF <- read.csv('master_knee_hip_wide.dta (after esKOA exclusions).csv', header=T, na.strings=c(""," ","NA"))

# select columns
columns <- c("study", 
             # Demographic.Demographic #
             "sex", "bmiv_baseline", "age_baseline",  "BP_cat_baseline",
             # Demographic.Socioeconomic #
             "smoking_packyr_baseline", "ethnicity", "marst_baseline", "living_status_baseline", "employ_baseline",
             "edu_baseline",       
             # Medicalhistory.Comorbidities #
             "heart_failure_baseline", "heart_attack_baseline", "stroke_baseline", "asthma_baseline", "COPD_baseline",             #        "                     #
             "ulcer_baseline", "diabetes_baseline", "kidney_baseline", 
             # Medicalhistory.Arthritis-specific #
             "arthritis_pmh_baseline", "injury_left_baseline", "freq_pain_left_baseline", "limit_activity_baseline",
             # Medicalhistory.Scoring systems.Mental #
             "cesd_baseline", "sf12_mental_baseline",
             # Medicalhistory.Scoring systems.Physical #
             "sf12_physical_baseline", "womactot_left_baseline", "pasev_baseline",
             # Medicalhistory.Clinical examination #
             "w20m_timet1_baseline", "cstime1_baseline", 
             # Osteoarthritis severity.Imaging Assessments #
             "KLGLEFT_baseline", 
             "JSLLEFT_baseline", "JSMLEFT_baseline",
             # History of intervention.Medication #
             "osteop_med_baseline", "analgesics_baseline", "arth_med_baseline", "steroid_inj_left_baseline",
             # History of intervention.Knee-related surgical intervention #
             "knee_arth_left_baseline", "knee_men_left_baseline", "knee_ligament_left_baseline", 
             "knee_other_surg_left_baseline",
             
             #LEFT KNEE variables
             "injury_right_baseline", "freq_pain_right_baseline", 
             "womactot_right_baseline", 
             "KLGRIGHT_baseline", "JSLRIGHT_baseline", "JSMRIGHT_baseline",  
             "steroid_inj_right_baseline",
             "knee_arth_right_baseline", "knee_men_right_baseline", "knee_ligament_right_baseline", 
             "knee_other_surg_right_baseline",
             
             # Outcomes #
             "knee_esKOA_new_y2_left",
             "knee_esKOA_new_y5_left"
)

# Select the features using the column names
combinedDF_sub_temp <- combinedDF[, columns]



#Pattern of Missing Data Exploration
p_missing <- unlist(lapply(combinedDF_sub_temp, function(x) sum(is.na(x))))/nrow(combinedDF_sub_temp)
sort(p_missing[p_missing > 0], decreasing = TRUE)


# Find columns with missing values
columns_with_missing <- colSums(is.na(combinedDF_sub_temp)) > 0
print(names(columns_with_missing[columns_with_missing]))

# Count the number of observations before removing rows with missing values
original_count <- nrow(combinedDF_sub_temp)
study_counts <- table(combinedDF_sub_temp$study)
print(paste("Original count: ", original_count))
print(study_counts)


# Calculate the number of missing rows stratified by study variable
missing_counts <- table(combinedDF_sub_temp$study, apply(combinedDF_sub_temp, 1, function(x) any(is.na(x))))

# Print the missing counts
print(missing_counts)


combinedDF_sub <- combinedDF_sub_temp


# Count the number of observations after removing rows with missing values
final_count <- nrow(combinedDF_sub)
# Print the results
print(paste("Final count: ", final_count))
final_study_counts <- table(combinedDF_sub$study)
print(final_study_counts)






#Correlations

# code snippet that converts the character variables to factors first and then to numeric values, because direct num conversion does not work 
# as i converts them as NA
combinedDF_sub_num <- combinedDF_sub %>%
  mutate_if(is.character, ~ as.factor(ifelse(.x == "NA", NA, .x))) %>%
  mutate_if(is.factor, as.numeric)



# Columns to exclude
exclude_cols <- c("bmiv_baseline", "age_baseline", "smoking_packyr_baseline", "womactot_left_baseline", "womactot_right_baseline", "sf12_mental_baseline", "sf12_physical_baseline",
                  "pasev_baseline", "w20m_timet1_baseline")

# Get the columns that were converted from factor to numeric, excluding the specified columns
converted_cols <- colnames(combinedDF_sub_num)[sapply(combinedDF_sub_num, is.numeric) & 
                                                 !grepl(paste(exclude_cols, collapse = "|"), colnames(combinedDF_sub_num))]

# Display the converted columns and their mapping
for (col in converted_cols) {
  cat(paste("Column '", col, "' was converted from factor to numeric.\n"))
  unique_values <- unique(combinedDF_sub[[col]])
  mapped_values <- unique(combinedDF_sub_num[[col]], na.rm = TRUE)
  for (i in seq_along(unique_values)) {
    cat(paste("  ", unique_values[i], " -> ", mapped_values[i], "\n"))
  }
  cat("\n")
}







r <- cor(combinedDF_sub_num[,3:42])




# Clear the current plot window
{plot.new(); dev.off()}

# Specify the file name and start the PNG graphics device
png("correlation_plot_left.png", width=800, height=800)

# Create the correlation plot (heatmap)
corrplot(
  r, type = "lower",
  tl.col = "black", tl.srt = 15
)

# Close the PNG graphics device, saving the plot to the file
dev.off()


#Check the extreme correlations:
extreme_cor <- sum(abs(r[upper.tri(r)]) > .999)
extreme_cor

summary(r[upper.tri(r)])



# Find indices of correlation values greater than 0.75 or less than -0.75
indices <- which(abs(r) > 0.75 & abs(r) < 1.0, arr.ind = TRUE)

# Extract the row and column names for the selected correlation values
cor_variables <- rownames(indices)

# Extract the correlation values
cor_values <- r[indices]

# Combine the variable names and correlation values into a data frame
cor_df <- data.frame(Variables = cor_variables, Correlation = cor_values)

# Filter the correlation data frame based on the condition
filtered_cor <- cor_df[abs(cor_df$Correlation) > 0.75 & abs(cor_df$Correlation) < 1, ]

dim(filtered_cor)  # Check dimensions of filtered_cor
str(filtered_cor)  # Check structure of filtered_cor


if (nrow(cor_df) == 0) {
  print("No correlations greater than 0.75 or less than -0.75 found.")
} else {
  filtered_cor <- cor_df[abs(cor_df$Correlation) > 0.75 & abs(cor_df$Correlation) < 1, ]
  if (nrow(filtered_cor) > 0) {
    # Group the variable pairs with the same correlation values
    grouped_cor <- aggregate(Variables ~ Correlation, filtered_cor, paste, collapse = " - ")
    # Display the grouped correlation values
    print(grouped_cor)
  } else {
    print("No correlations greater than 0.75 or less than -0.75 found.")
  }
}


######## BEGIN EXCLUDING DUMMY TRANSFORMATION

# Check for variables with no variation
no_variation_vars <- c()
for (col in names(combinedDF_sub_num)) {
  if (length(unique(combinedDF_sub_num[[col]])) == 1) {
    no_variation_vars <- c(no_variation_vars, col)
  }
}

# Print the variables with no variation
print(no_variation_vars)



#Check  "Zero and Near-Zero Variance Feature Variables"
# a) our data is balanced, with only a small number of unique values (if any) for each feature variable
# b) There are no samples that might have an excessive influence on the model

nearZeroVar(combinedDF_sub_num, saveMetrics = F)

#If the output is not integer(0) then there are variables with problems.

#In the case of integer is not 0 then try this:
#In the event that a feature variable has both a high freqRatio value and a low percentUnique value, 
#and both these values exceed the specified cut-offs, 
#then it would be reasonable to remove this feature variable (assuming it is not a categorical variable).

nearZeroVar(combinedDF_sub_num, saveMetrics = T)





# generalize outcome and predictor variables

outcomeName <- c('knee_esKOA')


outcomeNames <- c('knee_esKOA_new_y2_left', 'knee_esKOA_new_y5_left')


predictorsNames <- names(combinedDF_sub_num)[!(names(combinedDF_sub_num) %in% outcomeNames) & names(combinedDF_sub_num) != "study"]


#################################################
#
# 3. Split the Data into Training and Test Sets
#
#################################################
#
#We will use OAI as validation and internal test datasets, and MOST as external test dataset

set.seed(5701)


# Create a new column to represent the combined outcome of interest
combinedDF_sub_num$knee_esKOA_combined <- ifelse(
    combinedDF_sub_num$knee_esKOA_new_y2_left == 1 |
    combinedDF_sub_num$knee_esKOA_new_y5_left == 1, 1, 0
)


# Filter the data for the OAI study dataset
OAIDF_sub <- combinedDF_sub_num[combinedDF_sub_num$study == 2, !(names(combinedDF_sub_num) %in% c("study"))]

OAIDF_sub_original_count <- nrow(OAIDF_sub)

# Remove rows with missing values or NaNs in the knee_esKOA_combined variable
OAIDF_sub <- OAIDF_sub[complete.cases(OAIDF_sub$knee_esKOA_combined), ]

OAIDF_sub_cleaned_count <- nrow(OAIDF_sub)

# Calculate the number of removed rows
OAIDF_sub_removed_count <- OAIDF_sub_original_count - OAIDF_sub_cleaned_count

# Print the information
cat("Number of rows before removing missing values:", OAIDF_sub_original_count, "\n")
cat("Number of rows removed:", OAIDF_sub_removed_count , "\n")
cat("Number of rows after removing missing values:", OAIDF_sub_cleaned_count, "\n")


# Create a balanced split for the combined outcome variable
splitIndex <- createDataPartition(OAIDF_sub$knee_esKOA_combined, p = .80, list = FALSE, times = 1)


# Create train and test datasets for the combined outcome variable
train_OAI_DF <- OAIDF_sub[ splitIndex,]

test_OAI_DF_raw  <- OAIDF_sub[-splitIndex,]


# Count the number of observations after splitting
train_OAI_DF_count <- nrow(train_OAI_DF)
print(train_OAI_DF_count)

test_OAI_DF_raw_count <- nrow(test_OAI_DF_raw)
print(test_OAI_DF_raw_count)



# Remove rows with missing values or NaNs in the test_OAI_DF
test_OAI_DF <- na.omit(test_OAI_DF_raw)
test_OAI_DF_count <- nrow(test_OAI_DF)
print(test_OAI_DF_count)

# Calculate the number of removed rows
removed_count_test_OAI_DF_raw <- test_OAI_DF_raw_count - test_OAI_DF_count

# Print the number of removed rows
print(removed_count_test_OAI_DF_raw)

#Impute mode/mean value (for categorical/continuous variables) to training dataset
# Identify categorical and continuous variables based on the number of unique values
categorical_vars <- character()
continuous_vars <- character()

for (var in names(train_OAI_DF)) {
  unique_values <- unique(train_OAI_DF[[var]])
  if (length(unique_values) <= 10) {
    categorical_vars <- c(categorical_vars, var)
  } else {
    continuous_vars <- c(continuous_vars, var)
  }
}


# Impute mode value for categorical variables
for (var in categorical_vars) {
  # Find the mode value
  mode_val <- names(table(train_OAI_DF[[var]]))[which.max(table(train_OAI_DF[[var]]))]
  
  # Replace missing values with the mode value
  train_OAI_DF[[var]][is.na(train_OAI_DF[[var]])] <- as.numeric(mode_val)
}


# Impute mean value for continuous variables
for (var in continuous_vars) {
  train_OAI_DF[[var]] <- ifelse(is.na(train_OAI_DF[[var]]), mean(train_OAI_DF[[var]], na.rm = TRUE), train_OAI_DF[[var]])
}



# Percentage and number of outcome Y2 in training dataset
train_OAI_outcome_percent_y2 <- prop.table(table(train_OAI_DF$knee_esKOA_new_y2_left)) * 100
print(table(train_OAI_DF$knee_esKOA_new_y2_left))
print(train_OAI_outcome_percent_y2)

# Percentage and number of outcome Y2 in internal test dataset
test_OAI_outcome_percent_y2 <- prop.table(table(test_OAI_DF$knee_esKOA_new_y2_left)) * 100
print(table(test_OAI_DF$knee_esKOA_new_y2_left))
print(test_OAI_outcome_percent_y2)


# Percentage and number of outcome Y5 in training dataset
train_OAI_outcome_percent_y5 <- prop.table(table(train_OAI_DF$knee_esKOA_new_y5_left)) * 100
print(table(train_OAI_DF$knee_esKOA_new_y5_left))
print(train_OAI_outcome_percent_y5)

# Percentage and number of outcome Y5 in internal test dataset
test_OAI_outcome_percent_y5 <- prop.table(table(test_OAI_DF$knee_esKOA_new_y5_left)) * 100
print(table(test_OAI_DF$knee_esKOA_new_y5_left))
print(test_OAI_outcome_percent_y5)


# Filter the data for the MOST study dataset
test_MOST_DF_raw <- combinedDF_sub_num[combinedDF_sub_num$study == 1, !(names(combinedDF_sub_num) %in% c("study"))]
test_MOST_DF_raw_count <- nrow(test_MOST_DF_raw)

#######
# Remove rows with missing values or NaNs in the knee_esKOA_combined variable in MOST
test_MOST_DF_raw_sub <- test_MOST_DF_raw[complete.cases(test_MOST_DF_raw$knee_esKOA_combined), ]

test_MOST_DF_raw_sub_cleaned_count <- nrow(test_MOST_DF_raw_sub)

# Calculate the number of removed rows
test_MOST_DF_raw_sub_removed_count <- test_MOST_DF_raw_count - test_MOST_DF_raw_sub_cleaned_count

# Print the information
cat("Number of rows in MOST before removing missing outcomes:", test_MOST_DF_raw_count, "\n")
cat("Number of rows in MOST removed as missing outcomes:", test_MOST_DF_raw_sub_removed_count , "\n")
cat("Number of rows in MOST after removing missing outcomes:", test_MOST_DF_raw_sub_cleaned_count, "\n")

############

# Remove rows with missing or NaNs in the features in MOST 
test_MOST_DF <- na.omit(test_MOST_DF_raw_sub)
test_MOST_DF_count <- nrow(test_MOST_DF)

# Calculate the number of removed rows
removed_count_test_MOST_DF_raw <- test_MOST_DF_raw_sub_cleaned_count - test_MOST_DF_count

# Print the information
cat("Number of rows in MOST before removing missing features:", test_MOST_DF_raw_sub_cleaned_count, "\n")
cat("Number of rows in MOST removed as missing features:", removed_count_test_MOST_DF_raw , "\n")
cat("Number of rows in MOST after removing missing features:", test_MOST_DF_count, "\n")



# Percentage and number of outcome Y2 in external test dataset (MOST)
test_MOST_outcome_percent_y2 <- prop.table(table(test_MOST_DF$knee_esKOA_new_y2_left)) * 100
print(table(test_MOST_DF$knee_esKOA_new_y2_left))
print(test_MOST_outcome_percent_y2)

# Percentage and number of outcome Y5 in external test dataset (MOST)
test_MOST_outcome_percent_y5 <- prop.table(table(test_MOST_DF$knee_esKOA_new_y5_left)) * 100
print(table(test_MOST_DF$knee_esKOA_new_y5_left))
print(test_MOST_outcome_percent_y5)


##################### BEGIN BASELINE CHARACTERISTICS

# Define the datasets and their corresponding names
datasets <- list(
  "train_OAI_DF" = "train_OAI",
  "test_OAI_DF" = "test_OAI",
  "test_MOST_DF" = "test_MOST"
)

# Create empty lists to store the summaries
category_summary_list <- list()
continuous_summary_list <- list()

# Loop through each dataset
for (dataset_name in names(datasets)) {
  study_name <- datasets[[dataset_name]]
  study_df <- get(dataset_name)
  print(paste("Processing dataset:", study_name))
  
  # Categorical variables
  category_levels <- data.frame(
    Study = character(0),
    Variable = character(0),
    Level_Statistic = character(0),
    Value = character(0),
    stringsAsFactors = FALSE
  )
  
  # Loop through each variable in categorical_vars
  for (var in categorical_vars) {
    # Get the levels and their counts
    levels_counts <- table(study_df[[var]])
    
    # Compute the percentage of each level
    levels_percentage <- prop.table(levels_counts) * 100
    
    # Get the level names
    level_names <- names(levels_counts)
    
    # Store the results in the data frame
    for (i in seq_along(level_names)) {
      category_levels <- rbind(
        category_levels,
        data.frame(
          Study = study_name,
          Variable = var,
          Level_Statistic = level_names[i],
          Value = paste0(levels_counts[i], " (", round(levels_percentage[i], 1), "%)"),
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
  # Add the categorical summary to the list
  category_summary_list[[study_name]] <- category_levels
  
  # Continuous variables
  continuous_summary <- data.frame(
    Study = character(0),
    Variable = character(0),
    Level_Statistic = character(0),
    Value = character(0),
    stringsAsFactors = FALSE
  )
  
  # Loop through each variable in continuous_vars
  for (var in continuous_vars) {
    # Extract the values for the variable
    values <- study_df[[var]]
    
    # Calculate the statistics
    n <- length(values)
    mean_val <- mean(values, na.rm = TRUE)
    sd_val <- sd(values, na.rm = TRUE)
    median_val <- median(values, na.rm = TRUE)
    range_val <- range(values, na.rm = TRUE)
    
    # Format the range values with 2 decimal points
    range_formatted <- paste0(format(range_val[1], nsmall = 2), " – ", format(range_val[2], nsmall = 2))
    
    # Store the results in the data frame
    continuous_summary <- rbind(
      continuous_summary,
      data.frame(
        Study = study_name,
        Variable = var,
        Level_Statistic = "N",
        Value = n,
        stringsAsFactors = FALSE
      ),
      data.frame(
        Study = study_name,
        Variable = var,
        Level_Statistic = "Mean (SD)",
        Value = paste0(format(round(mean_val, 2), nsmall = 2), " (", format(round(sd_val, 2), nsmall = 2), ")"),
        stringsAsFactors = FALSE
      ),
      data.frame(
        Study = study_name,
        Variable = var,
        Level_Statistic = "Median",
        Value = median_val,
        stringsAsFactors = FALSE
      ),
      data.frame(
        Study = study_name,
        Variable = var,
        Level_Statistic = "Range",
        Value = range_formatted,
        stringsAsFactors = FALSE
      )
    )
  }
  
  # Add the continuous summary to the list
  continuous_summary_list[[study_name]] <- continuous_summary
}

# Combine the categorical summaries into a single data frame
category_combined <- do.call(rbind, category_summary_list)

# Combine the continuous summaries into a single data frame
continuous_combined <- do.call(rbind, continuous_summary_list)

# Rename the columns in the categorical summary
colnames(category_combined) <- c("Study", "Variable", "Level/Statistic", "Value")

# Rename the columns in the continuous summary
colnames(continuous_combined) <- c("Study", "Variable", "Level/Statistic", "Value")

# Combine the categorical and continuous summaries
summary_combined <- rbind(category_combined, continuous_combined)

# Save the combined summary to CSV
paper_path <- "YYYYY"
write.csv(summary_combined, file = paste(paper_path, "Table_1_characteristics.csv"), row.names = FALSE, fileEncoding = "UTF-8")



##################### END BASELINE CHARACTERISTICS



#Remove the column knee_esKOA_combined
train_OAI_DF$knee_esKOA_combined <- NULL
test_OAI_DF$knee_esKOA_combined <- NULL
test_MOST_DF$knee_esKOA_combined <- NULL


# Keep the original training and test datasets because when we loop for each outcome we need to remove outcome columns, therefore 
# we need to preserver the original datasets to start from original datasets for each outcome in the loop.
train_OAI_DF_orig <- train_OAI_DF
test_OAI_DF_orig <- test_OAI_DF
test_MOST_DF_orig <- test_MOST_DF


# Open a file connection for writing the outputs
data_path <- "YYYYY"
tool_path <- "ZZZZZ"

# Loop over each outcome (remember outcomeNames <- c('knee_esKOA_new_y2_left', 'knee_esKOA_new_y5_left'))
for (outcomes in outcomeNames) {
  file_conn <- file(paste(data_path, "output.txt", sep = ""), "a")
  cat(paste("Start of the analysis for the outcome: ", outcomes, "\n"), file = file_conn)
  
  train_OAI_DF <- train_OAI_DF_orig
  test_OAI_DF <- test_OAI_DF_orig
  test_MOST_DF <- test_MOST_DF_orig
  
  train_OAI_DF$knee_esKOA <- train_OAI_DF[,outcomes]
  test_OAI_DF$knee_esKOA <- test_OAI_DF[,outcomes]
  test_MOST_DF$knee_esKOA <- test_MOST_DF[,outcomes]
  
  #Remove the columns
  train_OAI_DF$knee_esKOA_new_y2_left <- NULL
  train_OAI_DF$knee_esKOA_new_y5_left <- NULL
  
  test_OAI_DF$knee_esKOA_new_y2_left <- NULL
  test_OAI_DF$knee_esKOA_new_y5_left <- NULL 

  test_MOST_DF$knee_esKOA_new_y2_left <- NULL
  test_MOST_DF$knee_esKOA_new_y5_left <- NULL 
  
  

#
  # Determine the set seed based on the outcome
  if (outcomes == "knee_esKOA_new_y2_left") {
    set.seed(5702)
  } else if (outcomes == "knee_esKOA_new_y5_left") {
    set.seed(5703)
  }
  
#
#######################################################
#
# 5.Imbalanced Data: Train models
#
#######################################################
#
#Global options that we will use across all of our trained models
#
ctrl <- trainControl(method = 'cv',
                     number = 10,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)


# Convert outcome to a factor and ensure valid R variable names for levels
train_OAI_DF[,outcomeName] <- factor(train_OAI_DF[,outcomeName])
levels(train_OAI_DF[,outcomeName]) <- make.names(levels(train_OAI_DF[,outcomeName]))


#############################################################
# 5.1 Extreme Gradient Boosting (XGB) Model: Imbalanced data
#############################################################

xgb_orig <- train(
  knee_esKOA ~ .,
  data = train_OAI_DF,
  method = "xgbTree",
  trControl = ctrl,
  verbosity = 0,
  verbose = FALSE,
  metric = "ROC"
)

importance_df <- varImp(xgb_orig, scale = FALSE)$importance

if (outcomes == "knee_esKOA_new_y2_left") {
  write.csv(importance_df, "feature_importances_xgb_orig_y2_left.csv", row.names = TRUE)
} else if (outcomes == "knee_esKOA_new_y5_left") {
  write.csv(importance_df, "feature_importances_xgb_orig_y5_left.csv", row.names = TRUE)
}

# Extract the maximum ROC value and its index
max_roc <- max(xgb_orig$results$ROC)
max_index <- which.max(xgb_orig$results$ROC)

# Extract the tuning parameters corresponding to the maximum ROC
optimal_params <- xgb_orig$results[max_index, 
                                   c("eta", "max_depth", "gamma", "colsample_bytree","min_child_weight", "subsample", "nrounds" )]

# Print the maximum ROC value and the corresponding tuning parameters
cat(paste("Optimal Parameters for xgb orig model: ", optimal_params, "\n\n"), file = file_conn)


#AUC and Confidence Intervals
# Perform predictions on the training data
train_predictions <- predict(xgb_orig, newdata = train_OAI_DF, type = "prob")

# Calculate the ROC curve
roc_obj <- roc(train_OAI_DF$knee_esKOA, train_predictions$X1)

# Calculate the AUC
auc <- auc(roc_obj)

# Calculate the confidence intervals for the ROC curve
ci <- ci(roc_obj, method = "bootstrap", boot.n = 1000, boot.stratified = TRUE)

# Print the AUC and confidence intervals
cat(paste("AUC for xgb_orig model: ", auc, "\n"), file = file_conn)
cat(paste("95% Confidence Intervals for ROC for xgb_orig model: ", "\n"), file = file_conn)
cat(paste("Lower bound: ", ci[1], "\n"), file = file_conn)
cat(paste("Upper bound: ", ci[3], "\n"), file = file_conn)


# Define the hyperparameter grid for tuning


#Comment out for quick run and use the next code that has the optimization parameters.
xgb_grid <-  expand.grid(max_depth = c(3, 5, 7, 9), 
                        nrounds = (1:10)*50,    # number of trees
                        eta = c(0.2, 0.3, 0.4),     
                        gamma = c(0, 1, 2), 
                        colsample_bytree = c(0.5, 0.8, 1),
                        subsample = c(0.5, 0.8, 1),
                        min_child_weight = c(1, 3, 5) 
                        )


#If I need to use the already optimized parameters for quick run
#xgb_grid <-  expand.grid(max_depth = c(3), 
#                        nrounds = 50,    # number of trees
#                        eta = c(0.2),     
#                        gamma = c(1), 
#                        colsample_bytree = c(0.8),
#                        subsample = c(0.8),
#                        min_child_weight = c(5) 
#                        )

# Create the control object for cross-validation
ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  search = "grid"
)


# Determine the set seed based on the outcome
if (outcomes == "knee_esKOA_new_y2_left") {
  set.seed(5704)
} else if (outcomes == "knee_esKOA_new_y5_left") {
  set.seed(5705)
}


# Perform the tuning
xgb_tuned <- train(
  knee_esKOA ~ .,
  data = train_OAI_DF,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = xgb_grid,
  verbosity = 0,
  verbose = FALSE,
  metric = "ROC"
)

importance_df2 <- varImp(xgb_tuned, scale = FALSE)$importance

if (outcomes == "knee_esKOA_new_y2_left") {
  write.csv(importance_df2, "feature_importances_xgb_tuned_y2_left.csv", row.names = TRUE)
} else if (outcomes == "knee_esKOA_new_y5_left") {
  write.csv(importance_df2, "feature_importances_xgb_tuned_y5_left.csv", row.names = TRUE)
}



# Print the best model after grid search
cat(paste("Best Tune from tuning for xgb orig model: ", xgb_tuned$bestTune, "\n"), file = file_conn)

# Extract the maximum ROC value and its index
max_roc_tuned <- max(xgb_tuned$results$ROC)
max_index_tuned <- which.max(xgb_tuned$results$ROC)

# Extract the tuning parameters corresponding to the maximum ROC
optimal_params_tuned <- xgb_tuned$results[max_index_tuned, 
                                   c("eta", "max_depth", "gamma", "colsample_bytree","min_child_weight", "subsample", "nrounds" )]

# Print the maximum ROC value and the corresponding tuning parameters
cat(paste("Optimal Parameters Tuned for xgb orig model: ", optimal_params_tuned, "\n\n"), file = file_conn)


# Create a data frame with the tuning parameter values and ROC values
tuning_results <- xgb_tuned$results
tuning_results$Iteration <- seq_len(nrow(tuning_results))

# Create the line plot
ggplot(tuning_results, aes(x = Iteration, y = ROC)) +
  geom_line() +
  labs(x = "Iteration", y = "ROC") +
  ggtitle("Performance of Tuned XGBoost Model")



#AUC and Confidence Intervals
# Perform predictions on the training data
train_predictions_tuned <- predict(xgb_tuned, newdata = train_OAI_DF, type = "prob")

# Calculate the ROC curve
roc_obj_tuned <- roc(train_OAI_DF$knee_esKOA, train_predictions_tuned$X1)

# Calculate the AUC
auc_tuned <- auc(roc_obj_tuned)

# Calculate the confidence intervals for the ROC curve
ci_tuned <- ci(roc_obj_tuned, method = "bootstrap", boot.n = 1000, boot.stratified = TRUE)

# Print the AUC and confidence intervals
cat(paste("AUC for xgb_tuned model: ", auc_tuned, "\n"), file = file_conn)
cat(paste("95% Confidence Intervals for ROC for xgb_tuned model: ", "\n"), file = file_conn)
cat(paste("Lower bound: ", ci_tuned[1], "\n"), file = file_conn)
cat(paste("Upper bound: ", ci_tuned[3], "\n"), file = file_conn)


# Initialize a list to store the models and their ROC values
model_list <- list()

# Check if the optimized ROC is better than the original ROC
if (auc_tuned > auc) {
  
  # If the optimized ROC is better, use the optimized model
  cat(paste("The optimized (tuned) XGB is better than xgb orig model. AUC Tuned: ", auc_tuned, "\n\n"), file = file_conn)
  
  xgb_orig <- xgb_tuned
  auc_orig <- auc_tuned
  lci_orig <- ci_tuned[1]
  uci_orig <- ci_tuned[3]
  
  # Store the tuned model and ROC value in the list
  model_list$original     <- xgb_orig
  model_list$original_auc <- auc_orig
  model_list$original_lci <- lci_orig
  model_list$original_uci <- uci_orig

  # Print the AUC and confidence intervals
  cat(paste("AUC for selected optimized (tuned) model: ", auc_orig, "\n"), file = file_conn)
  cat(paste("95% Confidence Intervals for selected optimized (tuned) model: ", "\n"), file = file_conn)
  cat(paste("Lower bound: ", lci_orig, "\n"), file = file_conn)
  cat(paste("Upper bound: ", uci_orig, "\n"), file = file_conn)
  
  
    
} else {
  # If the optimized ROC is not better, keep the original model
  cat(paste("The optimized (tuned) XGB is NOT better than xgb orig model, keeping the XGB_ORIG. AUC Original: ", auc, "\n\n"), file = file_conn)
  
  xgb_orig <- xgb_orig
  auc_orig <- auc
  lci_orig <- ci[1]
  uci_orig <- ci[3]
  
  # Store the tuned model and ROC value in the list
  model_list$original     <- xgb_orig
  model_list$original_auc <- auc_orig
  model_list$original_lci <- lci_orig
  model_list$original_uci <- uci_orig
 
    # Print the AUC and confidence intervals
  cat(paste("AUC for selected original XGB model: ", auc_orig, "\n"), file = file_conn)
  cat(paste("95% Confidence Intervals for selected original model: ", "\n"), file = file_conn)
  cat(paste("Lower bound: ", lci_orig, "\n"), file = file_conn)
  cat(paste("Upper bound: ", uci_orig, "\n"), file = file_conn)
   
}

if (outcomes == "knee_esKOA_new_y2_left") {
  saveRDS(xgb_orig, file.path(tool_path, "xgb_orig_y2_model.rds"))
  
} else if (outcomes == "knee_esKOA_new_y5_left") {
  saveRDS(xgb_orig, file.path(tool_path, "xgb_orig_y5_model.rds"))
}



#################################################
#
# 6. evaluate model in internal test (OAI) dataset
#
#################################################


selected_threshold <- 0.5

# Convert test_OAI_DF[, outcomeName] to a factor with levels "X0" and "X1"
observed <- factor(test_OAI_DF[, outcomeName], levels = c(0, 1), labels = c("X0", "X1"))

#Extreme Gradient Boosting (XGB) Model predictions on internal test (OAI) dataset
xgb_orig_pred <- predict(object=xgb_orig, test_OAI_DF[,predictorsNames], type='prob')

#Extreme Gradient Boosting Machine (GBM) Assign class  to probabilities
xgb_orig_test <- ifelse(xgb_orig_pred[,2] > selected_threshold, 1, 0)

#Convert predictions to a factor with appropriate levels
xgb_orig_test_f <- factor(xgb_orig_test, levels = c(0, 1), labels = c("X0", "X1"))

#Check the levels of the predictions and observed variables
levels(xgb_orig_test_f)
levels(observed)

#Calculate confusion matrix
confusionMatrix(xgb_orig_test_f, observed)
#the below is for F1 score
conf_mat <- caret::confusionMatrix(data = xgb_orig_test_f, reference = observed)
# Access the F1 scores, accuracy and kappa values
f1_score <- conf_mat$byClass["F1"]
accuracy <- conf_mat$overall["Accuracy"]
kappa <- conf_mat$overall["Kappa"]


# Print the F1 score, accuracy and kappa values
cat(paste("F1 score from internal test dataset for xgb_orig: ", f1_score, "\n"), file = file_conn)
cat(paste("Accuracy from internal test dataset for xgb_orig: ", accuracy, "\n"), file = file_conn)
cat(paste("Kappa score from internal test dataset for xgb_orig :", kappa, "\n\n"), file = file_conn)

#Calculate and Save Precision/Recall/F1 score*
# *F1 score as the harmonic mean of precision and recall (sensitivity), which is a measure of negative predictive power
#
# For Negative Class
cat(paste("START calculations for negative class in the internal (OAI) test data: ", "\n"), file = file_conn)

# Precision for Negative Class
precision_neg_xgb_orig <- conf_mat$byClass["Neg Pred Value"]
cat(paste("XGB ORIG Precision for negative class, using internal (OAI) test data: ", precision_neg_xgb_orig, "\n"), file = file_conn)

# Recall (Sensitivity) for Negative Class = Specificity
recall_neg_xgb_orig <- conf_mat$byClass["Specificity"]
cat(paste("XGB ORIG Recall (Specificity) for negative class, using internal (OAI) test data: ", recall_neg_xgb_orig, "\n"), file = file_conn)

# F1 Score for Negative Class
f1_score_neg_xgb_orig <- 2 * (precision_neg_xgb_orig * recall_neg_xgb_orig) / (precision_neg_xgb_orig + recall_neg_xgb_orig)
cat(paste("XGB ORIG F1 Score for negative class, using internal (OAI) test data: ", f1_score_neg_xgb_orig, "\n"), file = file_conn)


# For Positive Class
cat(paste("START calculations for positive class in the internal (OAI) test data: ", "\n"), file = file_conn)

# Precision for Positive Class
precision_pos_xgb_orig <- conf_mat$byClass["Pos Pred Value"]
cat(paste("XGB ORIG Precision for positive class, using internal (OAI) test data: ", precision_pos_xgb_orig, "\n"), file = file_conn)

# Recall (Sensitivity) for Positive Class
recall_pos_xgb_orig <- conf_mat$byClass["Sensitivity"]
cat(paste("XGB ORIG Recall (Sensitivity) for positive class, using internal (OAI) test data: ", recall_pos_xgb_orig, "\n"), file = file_conn)

# F1 Score for Positive Class
f1_score_pos_xgb_orig <- 2 * (precision_pos_xgb_orig * recall_pos_xgb_orig) / (precision_pos_xgb_orig + recall_pos_xgb_orig)
cat(paste("XGB ORIG F1 Score for positive class, using internal (OAI) test data: ", f1_score_pos_xgb_orig, "\n"), file = file_conn)


# AUC 
auc_xgb_orig <- roc(test_OAI_DF[, outcomeName], xgb_orig_pred[[2]])
cat(paste("XGB ORIG AUC, using internal test data: ", auc_xgb_orig$auc, "\n\n"), file = file_conn)

# Calculate the confidence intervals for the ROC curve
ci_tuned <- ci(auc_xgb_orig$auc, method = "bootstrap", boot.n = 1000, boot.stratified = TRUE)
cat(paste("95% Confidence Intervals for ROC for xgb_tuned model with internal test data: ", "\n"), file = file_conn)
cat(paste("Lower bound: ", ci_tuned[1], "\n"), file = file_conn)
cat(paste("Upper bound: ", ci_tuned[3], "\n"), file = file_conn)

#######################################################
#
# 13. Summarize the model performance on internal test (OAI)
#
#######################################################
#Lets reset the chart settings so we see one chart at a time
par(mfrow = c(1,1))

# 13.1 Compare the Precision of the models: TP / TP + FP. To do that, we'll need to combine our results into a dataframe

model_compare_precision <- data.frame(Model = c('XGB-ORIG'
                                             ),
                                   Precision = c(precision_neg_xgb_orig
                                              ))
ggplot(aes(x = reorder(Model,-Precision),y = Precision),data = model_compare_precision) +
  geom_bar(stat = 'identity',fill = 'light green') +
  ggtitle('Comparative Precision of Models on Test Data') +
  xlab('Models')  +
  ylab('Precision Measure')+
  geom_text(aes(label = round(Precision,2))) + theme_bw() + 
  theme(axis.text.x = element_text(angle = 40))


# Order the models based on F1 score in descending order
ordered_models <- model_compare_precision[order(model_compare_precision$Precision, decreasing = TRUE), ]

# Print the ordered models
print(ordered_models)


#Lets reset the chart settings so we see one chart at a time
par(mfrow = c(1,1))

# 13.2 Compare the Recall of the models: TP / TP + FN. To do that, we'll need to combine our results into a dataframe
model_compare_recall <- data.frame(
  Model = c('XGB-ORIG'
  ),
  Recall = c(
    recall_neg_xgb_orig
    )
)

ggplot(aes(x = reorder(Model, -Recall), y = Recall), data = model_compare_recall) +
  geom_bar(stat = 'identity', fill = 'light green') +
  ggtitle('Comparative Recall of Models on Test Data') +
  xlab('Models') +
  ylab('Recall Measure') +
  geom_text(aes(label = round(Recall, 2))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 40))


# Order the models based on F1 score in descending order
ordered_models <- model_compare_recall[order(model_compare_recall$Recall, decreasing = TRUE), ]

# Print the ordered models
print(ordered_models)


#Lets reset the chart settings so we see one chart at a time
par(mfrow = c(1,1))

# 13.3 Compare the F1 of the models: 2*((Precision*Recall) / (Precision + Recall))
model_compare_f1_score <- data.frame(
  Model = c('XGB-ORIG'
  ),
  F1_Score = c(
    f1_score_neg_xgb_orig
    )
)

ggplot(aes(x = reorder(Model, -F1_Score), y = F1_Score), data = model_compare_f1_score) +
  geom_bar(stat = 'identity', fill = 'light green') +
  ggtitle('Comparative F1 Score of Models on Test Data') +
  xlab('Models') +
  ylab('F1 Score') +
  geom_text(aes(label = round(F1_Score, 2))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 40))

# Order the models based on F1 score in descending order
ordered_models <- model_compare_f1_score[order(model_compare_f1_score$F1_Score, decreasing = TRUE), ]

# Print the ordered models
print(ordered_models)



#Lets reset the chart settings so we see one chart at a time
par(mfrow = c(1,1))

# 13.4 Compare the AUC of the models. To do that, we'll need to combine our results into a dataframe

model_compare_AUC <- data.frame(Model = c('XGB-ORIG'
                                          ),
                              AUC = c(auc_xgb_orig$auc
                                ))
ggplot(aes(x = reorder(Model,-AUC),y = AUC),data = model_compare_AUC) +
  geom_bar(stat = 'identity',fill = 'light green') +
  ggtitle('Comparative AUC of Models on Test Data') +
  xlab('Models')  +
  ylab('AUC Measure')+
  geom_text(aes(label = round(AUC,2))) + theme_bw() + 
  theme(axis.text.x = element_text(angle = 40))


# Order the models based on AUC score in descending order
ordered_models <- model_compare_AUC[order(model_compare_AUC$AUC, decreasing = TRUE), ]


#orig model output
cat(paste("train dataset - XGB orig", "\n"), file = file_conn)
cat(paste("XGB orig AUC: ", model_list$original_auc, "\n"), file = file_conn)
cat(paste("XGB orig AUC Lower CI: ", model_list$original_lci, "\n"), file = file_conn)
cat(paste("XGB orig AUC Upper CI: ", model_list$original_uci, "\n"), file = file_conn)

# Get the best tune values from model_list$orig$bestTune
best_tune <- model_list$original$bestTune
# Create the complete text to write
text_to_write <- paste("nrounds:", best_tune$nrounds)
text_to_write <- paste(text_to_write, "max_depth:", best_tune$max_depth)
text_to_write <- paste(text_to_write, "eta:", best_tune$eta)
text_to_write <- paste(text_to_write, "gamma:", best_tune$gamma)
text_to_write <- paste(text_to_write, "colsample_bytree:", best_tune$colsample_bytree)
text_to_write <- paste(text_to_write, "min_child_weight:", best_tune$min_child_weight)
text_to_write <- paste(text_to_write, "subsample:", best_tune$subsample)
# Append the content to the text file
cat(paste(text_to_write, "\n\n"), file = file_conn)


# Let's draw the ROC curves for each model.

# A. XGB_ORIG FOR INTERNAL TEST DATA (OAI)

# Determine the file path based on the outcome
if (outcomes == "knee_esKOA_new_y2_left") {
  pdf_file_xgb_orig <- "roc_curve_plot_xgb_orig_INT_TEST_OAI_y2.pdf"
} else if (outcomes == "knee_esKOA_new_y5_left") {
  pdf_file_xgb_orig <- "roc_curve_plot_xgb_orig_INT_TEST_OAI_y5.pdf"
}

# Open the PDF device
pdf(pdf_file_xgb_orig)

# Calculate ROC curve values
roc_values <- roc(test_OAI_DF[, outcomeName], xgb_orig_pred[[2]])

# List ROC curve values
roc_values


# Create a data frame with sensitivity and specificity values
roc_data <- data.frame(Sensitivity = roc_values$sensitivities,
                       Specificity = 1 - roc_values$specificities)


# Plot the ROC curve
plot(roc_data$Specificity, roc_data$Sensitivity, type = "l",
     main = "ROC Curve", xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)", xlim = c(0, 1), ylim = c(0, 1))

# Add a diagonal line representing random classifier (AUC = 0.500)
abline(0, 1, col = "gray", lty = 2)


# Calculate the Youden's index
youden_index <- roc_values$sensitivities + roc_values$specificities - 1

# Find the optimal threshold based on the maximum Youden's index
optimal_threshold <- roc_values$thresholds[which.max(youden_index)]
optimal_threshold_xgb_orig_testOAI <- optimal_threshold


# Retrieve the TPR and FPR at the selected threshold (0.5)
index_nearest_threshold <- which.min(abs(roc_values$thresholds - selected_threshold))

cat("index_nearest_threshold: ", index_nearest_threshold, "\n", file = file_conn)

selected_tpr <- roc_values$sensitivities[index_nearest_threshold]
selected_fpr <- 1 - roc_values$specificities[index_nearest_threshold]

# Print the selected TPR and FPR
cat("Selected TPR: ", selected_tpr, "\n", file = file_conn)
cat("Selected FPR: ", selected_fpr, "\n", file = file_conn)

# Print the optimal threshold
cat("Selected threshold: ", selected_threshold, "\n\n", file = file_conn)


# Calculate predicted probabilities using optimal threshold
pred_prob_optimal <- ifelse(xgb_orig_pred[[2]] >= selected_threshold, 1, 0)

# Calculate ROC curve values for optimal threshold
roc_optimal <- roc(test_OAI_DF[, outcomeName], pred_prob_optimal)



# Create a data frame with sensitivity and specificity values for the optimal roc
roc_data_optimal <- data.frame(Sensitivity = roc_optimal$sensitivities,
                       Specificity = 1 - roc_optimal$specificities)
#


#Add point for the selected TPR and FPR on the ROC curve
points(selected_fpr, selected_tpr, pch = 20, col = "red")

text(selected_fpr + 0.1, selected_tpr, 
     labels = paste("Threshold =", round(selected_threshold, 3), 
                    "\nTPR =", round(selected_tpr, 3), 
                    "\nFPR =", round(selected_fpr, 3)),
     cex = 1.2, col = "blue", adj = c(0, 0))



#########

cat("roc_data_optimal$Sensitivity: ", roc_data_optimal$Sensitivity, "\n\n", file = file_conn)
cat("roc_data_optimal$Specificity: ", roc_data_optimal$Specificity, "\n\n", file = file_conn)



# Add legend with AUC and 95% CI and F1-score for positive class
# Get the rounded value of F1 score and auc_value
f1_value_str <- sprintf("%.3f", f1_score_pos_xgb_orig)
auc_value <- round(roc_values$auc, 3)
ci <- ci_tuned 

# Construct the legend text to include AUC, its 95% CI, and the F1 score
legend_text <- c(
  paste("AUC: ", auc_value, " (", round(ci[1], 3), " to ", round(ci[3], 3), ")", sep = ""),
  paste("F1 (positive class): ", f1_value_str)
)

# Add the legend to the plot
legend("bottomright", legend = legend_text,
       lty = c(0, 0), col = "black", bty = "n", cex = 1.2)


# Close the PDF device and save the plot
dev.off()


########  USE OPTIMAL THRESHOLD FOR INTERNAL TEST ####################

# get predictions on the internal testing data

selected_threshold <- optimal_threshold_xgb_orig_testOAI

observed <- factor(test_OAI_DF[, outcomeName], levels = c(0, 1), labels = c("X0", "X1"))

#Extreme Gradient Boosting (XGB) Model predictions

xgb_orig_pred <- predict(object=xgb_orig, test_OAI_DF[,predictorsNames], type='prob')

#Extreme Gradient Boosting Machine (GBM) Assign class  to probabilities
xgb_orig_test <- ifelse(xgb_orig_pred[,2] > selected_threshold, 1, 0)

#Convert predictions to a factor with appropriate levels
xgb_orig_test_f <- factor(xgb_orig_test, levels = c(0, 1), labels = c("X0", "X1"))

#Check the levels of the predictions and observed variables
levels(xgb_orig_test_f)
levels(observed)

#Calculate confusion matrix
confusionMatrix(xgb_orig_test_f, observed)
#the below is for F1 score
conf_mat <- caret::confusionMatrix(data = xgb_orig_test_f, reference = observed)
# Access the F1 scores, accuracy and kappa values
f1_score <- conf_mat$byClass["F1"]
accuracy <- conf_mat$overall["Accuracy"]
kappa <- conf_mat$overall["Kappa"]


# Print the F1 score, accuracy and kappa values
cat(paste("The below is using the threshold from internal dataset: ",selected_threshold, "\n"), file = file_conn)
cat(paste("F1 score from internal (OAI) test dataset for xgb_orig: ", f1_score, "\n"), file = file_conn)
cat(paste("Accuracy from internal (OAI) test dataset for xgb_orig: ", accuracy, "\n"), file = file_conn)
cat(paste("Kappa score from external (OAI) test dataset for xgb_orig: ", kappa, "\n\n"), file = file_conn)

# Calculate and Save Precision/Recall/F1 score*
# *F1 score as the harmonic mean of precision and recall (sensitivity), which is a measure of negative predictive power
#
# For Negative Class
cat(paste("START calculations for negative class in the internal (OAI) test data using optimum threshold: ", "\n"), file = file_conn)

# Precision for Negative Class
precision_neg_xgb_orig <- conf_mat$byClass["Neg Pred Value"]
cat(paste("XGB ORIG Precision for negative class, using internal (OAI) test data using optimum threshold: ", precision_neg_xgb_orig, "\n"), file = file_conn)

# Recall (Sensitivity) for Negative Class = Specificity
recall_neg_xgb_orig <- conf_mat$byClass["Specificity"]
cat(paste("XGB ORIG Recall (Specificity) for negative class, using internal (OAI) test data using optimum threshold: ", recall_neg_xgb_orig, "\n"), file = file_conn)

# F1 Score for Negative Class
f1_score_neg_xgb_orig <- 2 * (precision_neg_xgb_orig * recall_neg_xgb_orig) / (precision_neg_xgb_orig + recall_neg_xgb_orig)
cat(paste("XGB ORIG F1 Score for negative class, using internal (OAI) test data using optimum threshold: ", f1_score_neg_xgb_orig, "\n"), file = file_conn)


# For Positive Class
cat(paste("START calculations for positive class in the internal (OAI) test data using optimum threshold: ", "\n"), file = file_conn)

# Precision for Positive Class
precision_pos_xgb_orig <- conf_mat$byClass["Pos Pred Value"]
cat(paste("XGB ORIG Precision for positive class, using internal (OAI) test data using optimum threshold: ", precision_pos_xgb_orig, "\n"), file = file_conn)

# Recall (Sensitivity) for Positive Class
recall_pos_xgb_orig <- conf_mat$byClass["Sensitivity"]
cat(paste("XGB ORIG Recall (Sensitivity) for positive class, using internal (OAI) test data using optimum threshold: ", recall_pos_xgb_orig, "\n"), file = file_conn)

# F1 Score for Positive Class
f1_score_pos_xgb_orig <- 2 * (precision_pos_xgb_orig * recall_pos_xgb_orig) / (precision_pos_xgb_orig + recall_pos_xgb_orig)
cat(paste("XGB ORIG F1 Score for positive class, using internal (OAI) test data using optimum threshold: ", f1_score_pos_xgb_orig, "\n"), file = file_conn)


# AUC 
auc_xgb_orig <- roc(test_OAI_DF[, outcomeName], xgb_orig_pred[[2]])
cat(paste("XGB ORIG AUC, using internal (OAI) test data using optimum threshold : ", auc_xgb_orig$auc, "\n\n"), file = file_conn)

# Calculate the confidence intervals for the ROC curve
ci_tuned <- ci(auc_xgb_orig$auc, method = "bootstrap", boot.n = 1000, boot.stratified = TRUE)
cat(paste("95% Confidence Intervals for ROC for xgb_tuned model with internal test data using optimum threshold: ", "\n"), file = file_conn)
cat(paste("Lower bound: ", ci_tuned[1], "\n"), file = file_conn)
cat(paste("Upper bound: ", ci_tuned[3], "\n"), file = file_conn)


# Let's draw the ROC curve

# Determine the file path based on the outcome
if (outcomes == "knee_esKOA_new_y2_left") {
  pdf_file_xgb_orig_OAI_opt <- "roc_curve_plot_xgb_orig_INT_TEST_OAI_opt_y2.pdf"
} else if (outcomes == "knee_esKOA_new_y5_left") {
  pdf_file_xgb_orig_OAI_opt <- "roc_curve_plot_xgb_orig_INT_TEST_OAI_opt_y5.pdf"
}


# Open the PDF device
pdf(pdf_file_xgb_orig_OAI_opt)

# Calculate ROC curve values
roc_values <- roc(test_OAI_DF[, outcomeName], xgb_orig_pred[[2]])

# List ROC curve values
roc_values


# Create a data frame with sensitivity and specificity values
roc_data <- data.frame(Sensitivity = roc_values$sensitivities,
                       Specificity = 1 - roc_values$specificities)

# Plot the ROC curve
#plot(roc_data$Specificity, roc_data$Sensitivity, type = "l",
#     main = "ROC Curve", xlab = "False Positive Rate (1 - Specificity)",
#     ylab = "True Positive Rate (Sensitivity)")

# Plot the ROC curve
plot(roc_data$Specificity, roc_data$Sensitivity, type = "l",
     main = "ROC Curve", xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)", xlim = c(0, 1), ylim = c(0, 1))

# Add a diagonal line representing random classifier (AUC = 0.500)
abline(0, 1, col = "gray", lty = 2)


# Retrieve the TPR and FPR at the optimal threshold
selected_tpr <- roc_values$sensitivities[roc_values$thresholds == selected_threshold]
selected_fpr <- 1 - roc_values$specificities[roc_values$thresholds == selected_threshold]


# Print the selected TPR and FPR
cat("Selected TPR: ", selected_tpr, "\n", file = file_conn)
cat("Selected FPR: ", selected_fpr, "\n", file = file_conn)

# Print the optimal threshold
cat("Optimal threshold: ", selected_threshold, "\n\n", file = file_conn)

# Calculate predicted probabilities using optimal threshold
pred_prob_optimal <- ifelse(xgb_orig_pred[[2]] >= optimal_threshold, 1, 0)

# Calculate ROC curve values for optimal threshold
roc_optimal <- roc(test_OAI_DF[, outcomeName], pred_prob_optimal)



# Create a data frame with sensitivity and specificity values for the optimal roc
roc_data_optimal <- data.frame(Sensitivity = roc_optimal$sensitivities,
                               Specificity = 1 - roc_optimal$specificities)

# Add point for the selected TPR and FPR on the ROC curve
points(roc_data_optimal$Specificity[roc_data_optimal$Specificity == selected_fpr],
       roc_data_optimal$Sensitivity[roc_data_optimal$Sensitivity == selected_tpr],
       pch = 20, col = "red")

text(selected_fpr + 0.10, selected_tpr - 0.10, 
     labels = paste("Threshold =", round(selected_threshold, 3), 
                    "\nTPR =", round(selected_tpr, 3), 
                    "\nFPR =", round(selected_fpr, 3)),
     cex = 1.2, col = "blue", adj = c(0, 0))



cat("roc_data_optimal$Sensitivity: ", roc_data_optimal$Sensitivity, "\n\n", file = file_conn)
cat("roc_data_optimal$Specificity: ", roc_data_optimal$Specificity, "\n\n", file = file_conn)



# Add legend with AUC and 95% CI and F1-score for positive class
# Get the rounded value of F1 score and auc_value
f1_value_str <- sprintf("%.3f", f1_score_pos_xgb_orig)
auc_value <- round(roc_values$auc, 3)
ci <- ci_tuned 

# Construct the legend text to include AUC, its 95% CI, and the F1 score
legend_text <- c(
  paste("AUC: ", auc_value, " (", round(ci[1], 3), " to ", round(ci[3], 3), ")", sep = ""),
  paste("F1 (positive class): ", f1_value_str)
)

# Add the legend to the plot
legend("bottomright", legend = legend_text,
       lty = c(0, 0), col = "black", bty = "n", cex = 1.2)



#legend_text <- paste("AUC: ", auc_value, " (", round(ci[1], 3), " to ", round(ci[3], 3), ")", sep = "")
#legend("bottomright", legend = legend_text,
#       lty = 1, col = "black", bty = "n", cex = 1.2)

# Close the PDF device and save the plot
dev.off()



#################################################
#
# evaluate model in external test (MOST) dataset
#
#################################################

# get predictions on the MOST external testing data

###### A. Use default threshold 0.5 #############

selected_threshold <- 0.5

observed <- factor(test_MOST_DF[, outcomeName], levels = c(0, 1), labels = c("X0", "X1"))

#Extreme Gradient Boosting (XGB) Model predictions on external test (MOST) dataset
xgb_orig_pred <- predict(object=xgb_orig, test_MOST_DF[,predictorsNames], type='prob')

#Extreme Gradient Boosting Machine (GBM) Assign class  to probabilities
xgb_orig_test <- ifelse(xgb_orig_pred[,2] > selected_threshold, 1, 0)

#Convert predictions to a factor with appropriate levels
xgb_orig_test_f <- factor(xgb_orig_test, levels = c(0, 1), labels = c("X0", "X1"))

#Check the levels of the predictions and observed variables
levels(xgb_orig_test_f)
levels(observed)

#Calculate confusion matrix
confusionMatrix(xgb_orig_test_f, observed)
#the below is for F1 score
conf_mat <- caret::confusionMatrix(data = xgb_orig_test_f, reference = observed)
# Access the F1 scores, accuracy and kappa values
f1_score <- conf_mat$byClass["F1"]
accuracy <- conf_mat$overall["Accuracy"]
kappa <- conf_mat$overall["Kappa"]


# Print the F1 score, accuracy and kappa values
cat(paste("The below is using the default 0.5  threshold: ", selected_threshold, "\n"), file = file_conn)
cat(paste("F1 score from external (MOST) test dataset for xgb_orig: ", f1_score, "\n"), file = file_conn)
cat(paste("Accuracy from external (MOST) test dataset for xgb_orig: ", accuracy, "\n"), file = file_conn)
cat(paste("Kappa score from external (MOST) test dataset for xgb_orig: ", kappa, "\n\n"), file = file_conn)



#Calculate and Save Precision/Recall/F1 score*
# *F1 score as the harmonic mean of precision and recall (sensitivity), which is a measure of negative predictive power
#
# For Negative Class
cat(paste("START calculations for negative class in the external (MOST) test data: ", "\n"), file = file_conn)

# Precision for Negative Class
precision_neg_xgb_orig <- conf_mat$byClass["Neg Pred Value"]
cat(paste("XGB ORIG Precision for negative class, using external (MOST) test data: ", precision_neg_xgb_orig, "\n"), file = file_conn)

# Recall (Sensitivity) for Negative Class = Specificity
recall_neg_xgb_orig <- conf_mat$byClass["Specificity"]
cat(paste("XGB ORIG Recall (Specificity) for negative class, using external (MOST) test data: ", recall_neg_xgb_orig, "\n"), file = file_conn)

# F1 Score for Negative Class
f1_score_neg_xgb_orig <- 2 * (precision_neg_xgb_orig * recall_neg_xgb_orig) / (precision_neg_xgb_orig + recall_neg_xgb_orig)
cat(paste("XGB ORIG F1 Score for negative class, using external (MOST) test data: ", f1_score_neg_xgb_orig, "\n"), file = file_conn)


# For Positive Class
cat(paste("START calculations for positive class in the external (MOST) test data: ", "\n"), file = file_conn)

# Precision for Positive Class
precision_pos_xgb_orig <- conf_mat$byClass["Pos Pred Value"]
cat(paste("XGB ORIG Precision for positive class, using external (MOST) test data: ", precision_pos_xgb_orig, "\n"), file = file_conn)

# Recall (Sensitivity) for Positive Class
recall_pos_xgb_orig <- conf_mat$byClass["Sensitivity"]
cat(paste("XGB ORIG Recall (Sensitivity) for positive class, using external (MOST) test data: ", recall_pos_xgb_orig, "\n"), file = file_conn)

# F1 Score for Positive Class
f1_score_pos_xgb_orig <- 2 * (precision_pos_xgb_orig * recall_pos_xgb_orig) / (precision_pos_xgb_orig + recall_pos_xgb_orig)
cat(paste("XGB ORIG F1 Score for positive class, using external (MOST) test data: ", f1_score_pos_xgb_orig, "\n"), file = file_conn)

# AUC 
auc_xgb_orig <- roc(test_MOST_DF[, outcomeName], xgb_orig_pred[[2]])
cat(paste("XGB ORIG AUC, using external (MOST) test data using threshold 0.5: ", auc_xgb_orig$auc, "\n\n"), file = file_conn)


# Calculate the confidence intervals for the ROC curve
ci_tuned <- ci(auc_xgb_orig$auc, method = "bootstrap", boot.n = 1000, boot.stratified = TRUE)
cat(paste("95% Confidence Intervals for ROC for xgb_tuned model with external (MOST) test data using threshold 0.5 : ", "\n"), file = file_conn)
cat(paste("Lower bound: ", ci_tuned[1], "\n"), file = file_conn)
cat(paste("Upper bound: ", ci_tuned[3], "\n"), file = file_conn)




# Let's draw the ROC curve


# Determine the file path based on the outcome
if (outcomes == "knee_esKOA_new_y2_left") {
  pdf_file_xgb_orig_MOST <- "roc_curve_plot_xgb_orig_MOST_y2.pdf"
} else if (outcomes == "knee_esKOA_new_y5_left") {
  pdf_file_xgb_orig_MOST <- "roc_curve_plot_xgb_orig_MOST_y5.pdf"
}


# Open the PDF device
pdf(pdf_file_xgb_orig_MOST)

# Calculate ROC curve values
roc_values <- roc(test_MOST_DF[, outcomeName], xgb_orig_pred[[2]])

# List ROC curve values
roc_values


# Create a data frame with sensitivity and specificity values
roc_data <- data.frame(Sensitivity = roc_values$sensitivities,
                       Specificity = 1 - roc_values$specificities)

# Plot the ROC curve
#plot(roc_data$Specificity, roc_data$Sensitivity, type = "l",
#     main = "ROC Curve", xlab = "False Positive Rate (1 - Specificity)",
#     ylab = "True Positive Rate (Sensitivity)")

# Plot the ROC curve
plot(roc_data$Specificity, roc_data$Sensitivity, type = "l",
     main = "ROC Curve", xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)", xlim = c(0, 1), ylim = c(0, 1))

# Add a diagonal line representing random classifier (AUC = 0.500)
abline(0, 1, col = "gray", lty = 2)


# Retrieve the TPR and FPR at the selected threshold (0.5)
index_nearest_threshold <- which.min(abs(roc_values$thresholds - selected_threshold))

cat("index_nearest_threshold: ", index_nearest_threshold, "\n", file = file_conn)

selected_tpr <- roc_values$sensitivities[index_nearest_threshold]
selected_fpr <- 1 - roc_values$specificities[index_nearest_threshold]



# Print the selected TPR and FPR
cat("Selected TPR: ", selected_tpr, "\n", file = file_conn)
cat("Selected FPR: ", selected_fpr, "\n", file = file_conn)

# Print the selected threshold
cat("selected_threshold: ", selected_threshold, "\n\n", file = file_conn)


# Calculate predicted probabilities using optimal threshold
pred_prob_optimal <- ifelse(xgb_orig_pred[[2]] >= selected_threshold, 1, 0)

# Calculate ROC curve values for optimal threshold
roc_optimal <- roc(test_MOST_DF[, outcomeName], pred_prob_optimal)



# Create a data frame with sensitivity and specificity values for the optimal roc
roc_data_optimal <- data.frame(Sensitivity = roc_optimal$sensitivities,
                               Specificity = 1 - roc_optimal$specificities)

# Add point for the selected TPR and FPR on the ROC curve
points(selected_fpr, selected_tpr, pch = 20, col = "red")

text(selected_fpr + 0.10, selected_tpr - 0.10, 
     labels = paste("Threshold =", round(selected_threshold, 3), 
                    "\nTPR =", round(selected_tpr, 3), 
                    "\nFPR =", round(selected_fpr, 3)),
     cex = 1.2, col = "blue", adj = c(0, 0))


cat("roc_data_optimal$Sensitivity: ", roc_data_optimal$Sensitivity, "\n\n", file = file_conn)
cat("roc_data_optimal$Specificity: ", roc_data_optimal$Specificity, "\n\n", file = file_conn)


# Add legend with AUC and 95% CI and F1-score for positive class
# Get the rounded value of F1 score and auc_value
f1_value_str <- sprintf("%.3f", f1_score_pos_xgb_orig)
auc_value <- round(roc_values$auc, 3)
ci <- ci_tuned 

# Construct the legend text to include AUC, its 95% CI, and the F1 score
legend_text <- c(
  paste("AUC: ", auc_value, " (", round(ci[1], 3), " to ", round(ci[3], 3), ")", sep = ""),
  paste("F1 (positive class): ", f1_value_str)
)

# Add the legend to the plot
legend("bottomright", legend = legend_text,
       lty = c(0, 0), col = "black", bty = "n", cex = 1.2)


#legend_text <- paste("AUC: ", auc_value, " (", round(ci[1], 3), " to ", round(ci[3], 3), ")", sep = "")
#legend("bottomright", legend = legend_text,
#       lty = 1, col = "black", bty = "n", cex = 1.2)

# Close the PDF device and save the plot
dev.off()


########  B. USE THRESHOLD FROM INTERNAL TEST ####################


# get predictions on the MOST external testing data

selected_threshold <- optimal_threshold_xgb_orig_testOAI

observed <- factor(test_MOST_DF[, outcomeName], levels = c(0, 1), labels = c("X0", "X1"))

#Extreme Gradient Boosting (XGB) Model predictions

xgb_orig_pred <- predict(object=xgb_orig, test_MOST_DF[,predictorsNames], type='prob')

#Extreme Gradient Boosting Machine (GBM) Assign class  to probabilities
xgb_orig_test <- ifelse(xgb_orig_pred[,2] > selected_threshold, 1, 0)

#Convert predictions to a factor with appropriate levels
xgb_orig_test_f <- factor(xgb_orig_test, levels = c(0, 1), labels = c("X0", "X1"))

#Check the levels of the predictions and observed variables
levels(xgb_orig_test_f)
levels(observed)

#Calculate confusion matrix
confusionMatrix(xgb_orig_test_f, observed)
#the below is for F1 score
conf_mat <- caret::confusionMatrix(data = xgb_orig_test_f, reference = observed)
# Access the F1 scores, accuracy and kappa values
f1_score <- conf_mat$byClass["F1"]
accuracy <- conf_mat$overall["Accuracy"]
kappa <- conf_mat$overall["Kappa"]


# Print the F1 score, accuracy and kappa values
cat(paste("The below is using the threshold from internal dataset: ",selected_threshold, "\n"), file = file_conn)
cat(paste("F1 score from external (MOST) test dataset for xgb_orig: ", f1_score, "\n"), file = file_conn)
cat(paste("Accuracy from external (MOST) test dataset for xgb_orig: ", accuracy, "\n"), file = file_conn)
cat(paste("Kappa score from external (MOST) test dataset for xgb_orig: ", kappa, "\n\n"), file = file_conn)

# Calculate and Save Precision/Recall/F1 score*
# *F1 score as the harmonic mean of precision and recall (sensitivity), which is a measure of negative predictive power
#
# For Negative Class
cat(paste("START calculations for negative class in the external (MOST) test data using optimum threshold: ", "\n"), file = file_conn)

# Precision for Negative Class
precision_neg_xgb_orig <- conf_mat$byClass["Neg Pred Value"]
cat(paste("XGB ORIG Precision for negative class, using external (MOST) test data using optimum threshold: ", precision_neg_xgb_orig, "\n"), file = file_conn)

# Recall (Sensitivity) for Negative Class = Specificity
recall_neg_xgb_orig <- conf_mat$byClass["Specificity"]
cat(paste("XGB ORIG Recall (Specificity) for negative class, using external (MOST) test data using optimum threshold: ", recall_neg_xgb_orig, "\n"), file = file_conn)

# F1 Score for Negative Class
f1_score_neg_xgb_orig <- 2 * (precision_neg_xgb_orig * recall_neg_xgb_orig) / (precision_neg_xgb_orig + recall_neg_xgb_orig)
cat(paste("XGB ORIG F1 Score for negative class, using external (MOST) test data using optimum threshold: ", f1_score_neg_xgb_orig, "\n"), file = file_conn)


# For Positive Class
cat(paste("START calculations for positive class in the external (MOST) test data using optimum threshold: ", "\n"), file = file_conn)

# Precision for Positive Class
precision_pos_xgb_orig <- conf_mat$byClass["Pos Pred Value"]
cat(paste("XGB ORIG Precision for positive class, using external (MOST) test data using optimum threshold: ", precision_pos_xgb_orig, "\n"), file = file_conn)

# Recall (Sensitivity) for Positive Class
recall_pos_xgb_orig <- conf_mat$byClass["Sensitivity"]
cat(paste("XGB ORIG Recall (Sensitivity) for positive class, using external (MOST) test data using optimum threshold: ", recall_pos_xgb_orig, "\n"), file = file_conn)

# F1 Score for Positive Class
f1_score_pos_xgb_orig <- 2 * (precision_pos_xgb_orig * recall_pos_xgb_orig) / (precision_pos_xgb_orig + recall_pos_xgb_orig)
cat(paste("XGB ORIG F1 Score for positive class, using external (MOST) test data using optimum threshold: ", f1_score_pos_xgb_orig, "\n"), file = file_conn)


# AUC 
auc_xgb_orig <- roc(test_MOST_DF[, outcomeName], xgb_orig_pred[[2]])
cat(paste("XGB ORIG AUC, using external (MOST) test data using optimum threshold : ", auc_xgb_orig$auc, "\n\n"), file = file_conn)

# Calculate the confidence intervals for the ROC curve
ci_tuned <- ci(auc_xgb_orig$auc, method = "bootstrap", boot.n = 1000, boot.stratified = TRUE)
cat(paste("95% Confidence Intervals for ROC for xgb_tuned model with external test data using optimum threshold: ", "\n"), file = file_conn)
cat(paste("Lower bound: ", ci_tuned[1], "\n"), file = file_conn)
cat(paste("Upper bound: ", ci_tuned[3], "\n"), file = file_conn)


# Let's draw the ROC curve

# Determine the file path based on the outcome
if (outcomes == "knee_esKOA_new_y2_left") {
  pdf_file_xgb_orig_MOST_opt <- "roc_curve_plot_xgb_orig_MOST_opt_y2.pdf"
} else if (outcomes == "knee_esKOA_new_y5_left") {
  pdf_file_xgb_orig_MOST_opt <- "roc_curve_plot_xgb_orig_MOST_opt_y5.pdf"
}


# Open the PDF device
pdf(pdf_file_xgb_orig_MOST_opt)

# Calculate ROC curve values
roc_values <- roc(test_MOST_DF[, outcomeName], xgb_orig_pred[[2]])

# List ROC curve values
roc_values


# Create a data frame with sensitivity and specificity values
roc_data <- data.frame(Sensitivity = roc_values$sensitivities,
                       Specificity = 1 - roc_values$specificities)

# Plot the ROC curve
#plot(roc_data$Specificity, roc_data$Sensitivity, type = "l",
#     main = "ROC Curve", xlab = "False Positive Rate (1 - Specificity)",
#     ylab = "True Positive Rate (Sensitivity)")

# Plot the ROC curve
plot(roc_data$Specificity, roc_data$Sensitivity, type = "l",
     main = "ROC Curve", xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)", xlim = c(0, 1), ylim = c(0, 1))

# Add a diagonal line representing random classifier (AUC = 0.500)
abline(0, 1, col = "gray", lty = 2)



# Retrieve the TPR and FPR at the selected threshold (0.5)
index_nearest_threshold <- which.min(abs(roc_values$thresholds - selected_threshold))

cat("index_nearest_threshold: ", index_nearest_threshold, "\n", file = file_conn)

selected_tpr <- roc_values$sensitivities[index_nearest_threshold]
selected_fpr <- 1 - roc_values$specificities[index_nearest_threshold]


# Print the optimal threshold
cat("Optimal threshold: ", selected_threshold, "\n\n", file = file_conn)

# Calculate predicted probabilities using optimal threshold
pred_prob_optimal <- ifelse(xgb_orig_pred[[2]] >= selected_threshold, 1, 0)

# Calculate ROC curve values for optimal threshold
roc_optimal <- roc(test_MOST_DF[, outcomeName], pred_prob_optimal)



# Create a data frame with sensitivity and specificity values for the optimal roc
roc_data_optimal <- data.frame(Sensitivity = roc_optimal$sensitivities,
                               Specificity = 1 - roc_optimal$specificities)



# Define a tolerance level for matching, if needed
tolerance <- 1e-5

# Find the index of the closest match for both Specificity and Sensitivity
closest_specificity_index <- which.min(abs(roc_data_optimal$Specificity - selected_fpr))
closest_sensitivity_index <- which.min(abs(roc_data_optimal$Sensitivity - selected_tpr))

# Check if the closest matches are the same point (i.e., indices are the same)
# This step assumes that for a valid point, both Specificity and Sensitivity should point to the same index after finding the closest match
if (closest_specificity_index == closest_sensitivity_index) {
  # Plot the closest matching point
  points(roc_data_optimal$Specificity[closest_specificity_index],
         roc_data_optimal$Sensitivity[closest_sensitivity_index],
         pch = 20, col = "red")
} else {
  # If the closest points do not correspond to the same index, find the best compromise
  # This could be either taking the average of indices, selecting one based on another criterion, or handling the mismatch differently
  # For simplicity, here we select the point based on the smallest combined distance to both selected_fpr and selected_tpr
  combined_distance <- abs(roc_data_optimal$Specificity - selected_fpr) + abs(roc_data_optimal$Sensitivity - selected_tpr)
  best_index <- which.min(combined_distance)
  if (min(combined_distance) < tolerance) { # Ensure the combined best match is within a reasonable tolerance
    points(roc_data_optimal$Specificity[best_index], roc_data_optimal$Sensitivity[best_index], pch = 20, col = "red")
  } else {
    print("No suitable match found within the tolerance level.")
  }
}



#Add point for the selected TPR and FPR on the ROC curve
text(selected_fpr + 0.10, selected_tpr - 0.10, 
     labels = paste("Threshold =", round(selected_threshold, 3), 
                    "\nTPR =", round(selected_tpr, 3), 
                    "\nFPR =", round(selected_fpr, 3)),
     cex = 1.2, col = "blue", adj = c(0, 0))



cat("roc_data_optimal$Sensitivity: ", roc_data_optimal$Sensitivity, "\n\n", file = file_conn)
cat("roc_data_optimal$Specificity: ", roc_data_optimal$Specificity, "\n\n", file = file_conn)


# Add legend with AUC and 95% CI and F1-score for positive class
# Get the rounded value of F1 score and auc_value
f1_value_str <- sprintf("%.3f", f1_score_pos_xgb_orig)
auc_value <- round(roc_values$auc, 3)
ci <- ci_tuned 

# Construct the legend text to include AUC, its 95% CI, and the F1 score
legend_text <- c(
  paste("AUC: ", auc_value, " (", round(ci[1], 3), " to ", round(ci[3], 3), ")", sep = ""),
  paste("F1 (positive class): ", f1_value_str)
)

# Add the legend to the plot
legend("bottomright", legend = legend_text,
       lty = c(0, 0), col = "black", bty = "n", cex = 1.2)


# Close the PDF device and save the plot
dev.off()

# Close the file connection
close(file_conn)



}    #end of loop for each outcomes 



#################
#
#   END 
#
################
  




