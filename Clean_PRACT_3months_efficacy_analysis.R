# packages

{
  # Load libraries for data analysis and visualization
  
  library(gtsummary)  # Used for creating summary tables in HTML or LaTeX formats, useful for reporting.
  
  library(gt)  # Utilized for creating rich, customizable tables for reporting and publication.
  
  library(redcapAPI)  # Facilitates data extraction from REDCap databases via API.
  
  library(lubridate)  # Provides functions to work with date-times and timespans, simplifying date-time data manipulation.
  
  library(tidyverse)  # A collection of packages designed for data science (includes dplyr, ggplot2, tidyr, readr, purrr, and tibble).
  
  library(Hmisc)  # Offers many functions for data analysis, high-level graphics, utility operations, and biostatistics.
  
  library(arsenal)  # Useful for data cleaning, exploration, and reporting.
  
  # library(nlme) # Used for linear and nonlinear mixed effects models (commented out, possibly for future use).
  
  library(rms)  # Provides tools for regression modeling strategies.
  
  library(glmmTMB)  # Used for fitting generalized linear mixed models, including mixed Poisson and zero-inflated models.
  
  library(lme4)  # Provides functions to fit linear and generalized linear mixed-effects models.
  
  library(stringr)  # Facilitates string manipulation and operations with a consistent and simple interface.
  
  library(qwraps2)  # Useful for quick summary statistics, tables, and plots in a 'tidy' format.
  
  library(broom)  # Helps convert statistical analysis objects into tidy data frames, making them easier to work with in the tidyverse.
  
  library(ggh4x)  # Extends ggplot2 for enhanced plotting capabilities.
  
  library(ggsignif)  # Offers options to add significance markers to 'ggplot2' plots.
  
  library(boot)  # Provides extensive facilities for bootstrapping and related resampling methods.
  
}


options(scipen = 999)
# setting function preferences for dplyr
conflicted::conflict_prefer("summarize", "dplyr")
conflicted::conflict_prefer("summarise", "dplyr")
conflicted::conflict_prefer("recode", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

# read data
# Set directory
setwd("/Users/jps89/Documents/GEMINI/PRACT/3months/")

# Read functions separated in a different file for organizational purposes
source("./Clean_PRACT_3months_efficacy_functions.R")

# load("/Users/jps89/Library/CloudStorage/Box-Box/PRACT 3 Months efficacy paper/data_subset_clean.Rdata")
load("/Users/jps89/Library/CloudStorage/Box-Box/Project backup files/PRACT/PRACT 3 Months efficacy paper/data_subset_clean.Rdata")

# Vectors with subsets of variables to be used afterwards
drinc <- c("drinc1", "drinc2", "drinc3", "drinc4", "drinc6", "drinc7", "drinc8", 
           "drinc9", "drinc10", "drinc11", "drinc12", "drinc13", "drinc14", "drinc16",
           "drinc17", "drinc18", "drinc19", "drinc20","drinc21", "drinc22", "drinc23", "drinc24",
           "drinc26", "drinc27", "drinc28", "drinc29", "drinc30","drinc31", "drinc32",
           "drinc33", "drinc34", "drinc36", "drinc37", "drinc38", "drinc39",
           "drinc40","drinc41", "drinc42", "drinc43", "drinc44", "drinc46", "drinc47",
           "drinc48", "drinc49", "drinc50")

drink_days <- c("drink_q_1d", "drink_q_2d","drink_q_3d","drink_q_4d","drink_q_5d",
                "drink_q_6d","drink_q_7d","drink_q_8d","drink_q_9d","drink_q_10d",
                "drink_q_11d","drink_q_12d","drink_q_13d","drink_q_14d","drink_q_15d",
                "drink_q_16d","drink_q_17d","drink_q_18d","drink_q_19d","drink_q_20d",
                "drink_q_21d","drink_q_22d","drink_q_23d","drink_q_24d","drink_q_25d",
                "drink_q_26d","drink_q_27d","drink_q_28d")

mental_health <- c("phq1", "phq2", "phq3", "phq4", "phq5", "phq6", "phq7", "phq8", "phq9")
## Tables ------------------------------------------------------------------

### Table 1 -----------------------------------------------------------------
# Demographics table
labels_1 <- list(female ~ "Gender",
                 age ~ "Age",
                 edu_years ~ "Years of education",
                 employ ~ "Employment",
                 income_house ~ "Monthly total household income (Tz Shilling)",
                 income_self ~ "Monthly personal income (Tz Shilling)",
                 tribe_recoded ~ "Tribe R")

# Create a summary table for participants in the intervention arm (base period)
tbl_1arm <- data_subset %>%
  filter(period=="base") %>% # Filter the data for the base period
  select(age, female, tribe_recoded, edu_years, employ, income_house, income_self, treat) %>% # Select specific columns
  tbl_summary(by=treat, # Group by 'arm' variable
              label= labels_1, # Use custom labels for the variables
              type=all_continuous() ~ "continuous2", # Set the type for continuous variables
              statistic = list(all_continuous() ~ c("{mean} ({sd})", # Display mean and standard deviation for continuous variables
                                                    "{median} ({p25}, {p75})"), # Display median, 25th and 75th percentiles for continuous variables
                               all_categorical() ~ "{n} / {N} ({p}%)"), # Display counts and percentages for categorical variables
              missing_text = "Missing", # Set the text to display for missing data
              sort = all_categorical() ~ "frequency", # Sort categorical variables by frequency
              digits = list(all_continuous() ~ 1)
              ) %>% 
  bold_labels() %>%  # Make the labels bold
  add_p() %>% 
  bold_p() 

# Create a summary table for all participants (base period)
tbl_1 <- data_subset %>%
  filter(period=="base") %>%
  select(age, female, tribe_recoded, edu_years, employ, income_house, income_self) %>%
  tbl_summary(label = labels_1,
              type=all_continuous() ~ "continuous2",
              statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                    "{median} ({p25}, {p75})"),
                               all_categorical() ~ "{n} / {N} ({p}%)"),
              missing_text = "Missing",
              sort = all_categorical() ~ "frequency",
              digits = list(all_continuous() ~ 1)
              ) %>%
  bold_labels()

# Merge the two summary tables
tbl_1 <- tbl_merge(tbls=list(tbl_1, tbl_1arm), tab_spanner = c("Overall", ""))

tbl_1

# Save the merged summary table as a Microsoft Word document
tbl_1 %>%
  as_flex_table() %>% # Convert the gtsummary table to a flextable
  flextable::save_as_docx(., path="./tables/table1_demographics.docx") # Save the table as a .docx file

### Table x- Enrollment criteria -----------------------------------------------------------------

label_enroll <- list(drinkb4inj ~ "Self-reported alcohol use prior injury",
                     bacpositive ~ "BAC positive",
                     auditover8 ~ "AUDIT > 8")

# Create a summary table for participants in the intervention arm (base period)
tbl_enroll_arm <- data_subset %>%
  filter(period == "base") %>% 
  select(bacpositive, auditover8, drinkb4inj, treat) %>%
  tbl_summary(by = treat, 
              label = label_enroll,
              type=all_continuous() ~ "continuous2",
              statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                    "{median} ({p25}, {p75})"),
                               all_categorical() ~ "{n} / {N} ({p}%)"),
              missing_text = "Missing",
              sort = all_categorical() ~ "frequency",
              digits = list(all_continuous() ~ 1)
              ) %>% 
  bold_labels()

# Create a summary table for all participants (base period)
tbl_enroll <- data_subset %>%
  filter(period == "base") %>% 
  select(bacpositive, auditover8, drinkb4inj) %>%
  tbl_summary(label = label_enroll,
              type=all_continuous() ~ "continuous2",
              statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                    "{median} ({p25}, {p75})"),
                               all_categorical() ~ "{n} / {N} ({p}%)"),
              missing_text = "Missing",
              sort = all_categorical() ~ "frequency",
              digits = list(all_continuous() ~ 1,
                            all_categorical() ~ 1)
              ) %>% 
  bold_labels()

# Merge the two summary tables
tbl_enroll <- tbl_merge(tbls=list(tbl_enroll, tbl_enroll_arm), tab_spanner = c("**Overall**", ""))
tbl_enroll

# Save the merged summary table as a Microsoft Word document
tbl_enroll %>% 
  as_flex_table() %>% 
  flextable::save_as_docx(., path="./tables/table_enrollment_criteria.docx")


### Table 2 - outcomes base and 3 months  -------------------------------------------------
label2 <- list(drinking.days ~ "Drinking days",
               drinking.amount ~ "Number of drinks",
               binge.days ~ "Binge drinking days", phq_sum ~ "PHQ-9 score", 
               audit_sum ~ "AUDIT score", drinc_sum ~ "DrInC")

# Create summary tables for drinking outcomes, psychological outcomes, 
# and other variables at baseline and 3 months, for both the intervention arm and all participants

# Baseline - Intervention Arm
tbl_2_arm <- data_subset %>%
  filter(period == "base") %>% 
  select(binge.days, drinking.days, drinking.amount,
         phq_sum, audit_sum, drinc_sum, treat) %>% 
  tbl_summary(by=treat,
              type=all_continuous() ~ "continuous2",
              statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                    "{median} ({p25}, {p75})"),
                               all_categorical() ~ "{n} / {N} ({p}%)"),
              missing_text = "Missing",
              label=label2,
              digits = list(all_continuous() ~ 1)
              ) %>%
  bold_labels()

tbl_2_arm %>% 
  add_p() %>% 
  bold_p()

# Baseline - All Participants
tbl_2 <- data_subset %>%
  filter(period == "base") %>% 
  select(binge.days, drinking.days, drinking.amount,
         phq_sum, audit_sum, drinc_sum) %>% 
  tbl_summary(type=all_continuous() ~ "continuous2",
              statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                    "{median} ({p25}, {p75})"),
                               all_categorical() ~ "{n} / {N} ({p}%)"),
              missing_text = "Missing",
              label=label2,
              digits = list(all_continuous() ~ 1)
              ) %>%
  bold_labels()

# 3 Months - Intervention Arm
tbl_2_3mo_arm <- data_subset %>%
  filter(period == "3mo") %>% 
  select(binge.days, drinking.days, drinking.amount,
         phq_sum, audit_sum, drinc_sum, treat) %>% 
  tbl_summary(by=treat,
              type=all_continuous() ~ "continuous2",
              statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                    "{median} ({p25}, {p75})"),
                               all_categorical() ~ "{n} / {N} ({p}%)"),
              missing_text = "Missing",
              label=label2,
              digits = list(all_continuous() ~ 1),
              ) %>%
  bold_labels()

# 3 Months - All Participants
tbl_2_3mo <- data_subset %>%
  filter(period == "3mo") %>% 
  select(binge.days, drinking.days, drinking.amount,
         phq_sum, audit_sum, drinc_sum) %>% 
  tbl_summary(type=all_continuous() ~ "continuous2",
              statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                    "{median} ({p25}, {p75})"),
                               all_categorical() ~ "{n} / {N} ({p}%)"),
              missing_text = "Missing",
              label=label2,
              digits = list(all_continuous() ~ 1)
              ) %>%
  bold_labels()


tbl_2_3mo <- tbl_merge(tbls=list(tbl_2_3mo, tbl_2_3mo_arm))
tbl_2 <- tbl_merge(tbls=list(tbl_2, tbl_2_arm), tab_spanner = c("", ""))
tbl_2 <- tbl_merge(tbls=list(tbl_2, tbl_2_3mo), tab_spanner = c("Baseline", "3 Months"))

tbl_2 %>% as_flex_table() %>% flextable::save_as_docx(., path="./tables/table2_outcomes.docx")



# Setting up Ids that should be removed for each outcome analysis
remove_ids_binge <- data_subset %>%
  filter(is.na(binge.days)) %>%
  pull(practid)
remove_ids_drinc <- data_subset %>%
  filter(is.na(drinc_sum)) %>%
  pull(practid)
remove_ids_audit <- data_subset %>%
  filter(is.na(audit_sum)) %>%
  pull(practid)
remove_ids_phq <- data_subset %>%
  filter(is.na(phq_sum)) %>%
  pull(practid)
## Statistical analysis ------------------------------------------------------------------

### Binge Drinking Days - ZINBM -------------------------------------------------------------------
# The outcomes Binge Drinking Days, Drinking Days and Drinking Amount all have the 
# same pattern of missing data so the object "data_drinking" will be used for modelling of them
data_drinking <-  data_subset %>% 
  filter(!practid %in% remove_ids_binge) %>% 
  # forcing variables to be integers
  mutate(binge.days = as.integer(binge.days),
         drinking.days = as.integer(drinking.days),
         drinking.amount = as.integer(drinking.amount))

X <- model.matrix(~period * treat, data = data_drinking)
Xalt <- X[, c("period3mo", "period3mo:treatIntervention")]


zinbm_binge.days <- glmmTMB(binge.days ~  Xalt + (1|practid), 
                            zi = ~ treat, 
                            family = nbinom2, 
                            data = data_drinking)

stats_binge.days <- summary(zinbm_binge.days)
stats_binge.days <- as.data.frame(stats_binge.days$coefficients$cond)
stats_binge.days <- tibble::rownames_to_column(stats_binge.days, "contrast") %>% 
  select(-`z value`) %>% 
  mutate(var = "binge.days")

# Calculate confidence intervals
ci_binge.days <- tibble::rownames_to_column(as.data.frame(confint(zinbm_binge.days)), "contrast") %>% 
  mutate_if(is.numeric, exp) %>% 
  mutate(contrast = dplyr::recode(contrast, "cond.Xaltperiod3mo" = "Xaltperiod3mo",
                                  "cond.Xaltperiod3mo:treatIntervention" = "Xaltperiod3mo:treatIntervention"))
# Merge the statistics with confidence intervals
stats_binge.days <- stats_binge.days %>% 
  left_join(ci_binge.days %>% select(-Estimate), by=join_by(contrast==contrast))


#### Bootstrapping --------------------------------------------

##### Differences in 3 Months - Intervention -----------------------------------

pred_df <- data_drinking %>% 
  select(practid, period, treat)

pred_df <- cbind(pred_df, 
                 pred = predict(zinbm_binge.days, data_drinking, type="response"))

# Bootstrapping the difference for Intervention group _______
boot_data_intervention <- pred_df %>%
  filter(treat == "Intervention")

# Bootstrapping to Estimate Mean Difference in Predictions for Intervention Group
# This part of the code applies a bootstrapping technique to estimate the variability
# of the mean difference in predictions between two specific time periods ('3mo' and 'base')
# for the 'Intervention' group.

# 'boot_result_intervention' is the object where the results will be stored.
# The 'boot' function is used for performing the bootstrapping.

# Parameters:
# 1. 'data': Specifies the dataset to be used for bootstrapping, which is the data filtered
#    for the 'Intervention' group in this case.
# 2. 'statistic': Refers to the function used to calculate the statistic of interest on each bootstrap sample.
#    Here, 'mean_diff_time_func' is used, which calculates the mean difference in predictions between
#    the '3mo' and 'base' periods.
# 3. 'R': The number of bootstrap replications, set to 1000. This determines how many times the 
#    bootstrapping process is repeated to build a distribution of the statistic of interest.

# The 'mean_diff_time_func' function works as follows:
# - It takes a resampled subset of data in each iteration.
# - Groups this data by the 'period' (either '3mo' or 'base').
# - Calculates the mean of the 'pred' (predictions) for each group.
# - Returns the difference of these means (mean at '3mo' - mean at 'base').
# This process is repeated 1000 times (as specified by R = 1000) to create a distribution
# of the mean difference, allowing us to understand and quantify the variability of this estimate
# within the Intervention group.
boot_result_intervention <- boot(data = boot_data_intervention,
                                 statistic = mean_diff_time_func,
                                 R = 1000)

# 95% Confidence Interval for the Intervention group
conf_int_intervention <- boot.ci(boot_result_intervention, conf = 0.95, type = "perc")

##### Differences in 3 Months - Usual care -----------------------------------
boot_data_usual <- pred_df %>%
  filter(treat == "Usual care")

# Bootstrapping to Estimate Mean Difference in Predictions for the Usual Care Group
# This part of the code applies a bootstrapping technique to estimate the variability
# of the mean difference in predictions between two specific time periods ('3mo' and 'base')
# for the 'Usual Care' group.

# 'boot_result_usual' is the object where the results will be stored.
# The 'boot' function is used for performing the bootstrapping.

# Parameters:
# 1. 'data': Specifies the dataset to be used for bootstrapping, which is the data filtered
#    for the 'Usual Care' group in this case.
# 2. 'statistic': Refers to the function used to calculate the statistic of interest on each bootstrap sample.
#    Here, 'mean_diff_time_func' is used, which calculates the mean difference in predictions between
#    the '3mo' and 'base' periods.
# 3. 'R': The number of bootstrap replications, set to 1000. This determines how many times the 
#    bootstrapping process is repeated to build a distribution of the statistic of interest.

# The 'mean_diff_time_func' function works as follows:
# - It takes a resampled subset of data in each iteration.
# - Groups this data by the 'period' (either '3mo' or 'base').
# - Calculates the mean of the 'pred' (predictions) for each group.
# - Returns the difference of these means (mean at '3mo' - mean at 'base').
# This process is repeated 1000 times (as specified by R = 1000) to create a distribution
# of the mean difference, allowing us to understand and quantify the variability of this estimate
# within the Usual Care group.


boot_result_usual <- boot(data = boot_data_usual,
                          statistic = mean_diff_time_func,
                          R = 1000)

# 95% Confidence Interval for the Usual care group
conf_int_usual <- boot.ci(boot_result_usual, conf = 0.95, type = "perc")

plot_bootstrap_results_combined(boot_result_intervention, conf_int_intervention, 
                                boot_result_usual, conf_int_usual,
                                pred_df)


##### Difference in Differences -----------------------------------------------

# Run the bootstrap
set.seed(12345)
# Bootstrapping for Difference-in-Differences (DiD) Estimation
# This section of the code uses bootstrapping to estimate the Difference-in-Differences (DiD),
# which is a useful method in causal inference to evaluate the effect of a treatment or intervention.

# 'boot_result_did' will store the bootstrapping results.
# The 'boot' function is used to perform the bootstrapping process.

# Parameters:
# 1. 'data': This is the complete dataset (pred_df) which includes both the treatment and control groups.
# 2. 'statistic': The function to calculate the statistic of interest on each bootstrap sample.
#    'bootstrap_did' is the function used here.
# 3. 'R': Specifies the number of bootstrap replications, set here to 1000.

# The 'bootstrap_did' function operates as follows:
# - It samples the data (using provided indices for each bootstrap iteration).
# - Groups the sampled data by 'treat' (treatment group) and 'period' (time period).
# - Calculates the mean of the predictions ('pred') for each group.
# - Checks if all necessary combinations of treatment groups ('Intervention' and 'Usual care')
#   and time periods ('3mo' and 'base') are present in the sample. If any combination is missing, it returns NA.
# - Computes the mean differences for both the intervention and control ('Usual care') groups separately
#   (comparing '3mo' to 'base').
# - Calculates the DiD estimate by subtracting the mean difference of the control group from that of the intervention group.
# - This DiD estimate reflects the treatment effect, adjusted for changes over time in the control group.

# By repeating this process 1000 times, the bootstrapping method allows us to understand the variability
# of the DiD estimate and assess its reliability. This is crucial for causal inference studies where understanding
# the treatment effect's consistency and robustness against different samples is important.

boot_result_did <- boot(data = pred_df, statistic = bootstrap_did, R = 1000)

# Calculate 95% CI
conf_int_did <- boot.ci(boot_result_did, conf = 0.95, type = "perc")
print(conf_int_did)


plot_did_distribution(pred_df, boot_result_did, conf_int_did,
                      x=-2, y=0.6,
                      subtitle = "ZINBM - Binge Drinking Days")


final_results <- get_values_boot(boot_result_intervention, conf_int_intervention, 
                                 boot_result_usual, conf_int_usual, 
                                 boot_result_did, conf_int_did,
                                 pred_df, variable = "Binge Drinking Days") 


### Drinking days - NBM -----------------------------------------------------------

drinking.days_nbm <- glmmTMB(drinking.days ~  Xalt + (1|practid), 
                             family = nbinom2, 
                             data = data_drinking)

stats_drinking.days <- summary(drinking.days_nbm)
stats_drinking.days <- as.data.frame(stats_drinking.days$coefficients$cond)
stats_drinking.days <- tibble::rownames_to_column(stats_drinking.days, "contrast") %>% 
  select(-`z value`) %>% 
  mutate(var = "drinking.days")

# Calculate confidence intervals
ci <- tibble::rownames_to_column(as.data.frame(confint(drinking.days_nbm)), "contrast") %>% 
  mutate_if(is.numeric, exp) %>% 
  mutate(contrast = dplyr::recode(contrast, "cond.Xaltperiod3mo" = "Xaltperiod3mo",
                                  "cond.Xaltperiod3mo:treatIntervention" = "Xaltperiod3mo:treatIntervention"))
# Merge the statistics with confidence intervals
stats_drinking.days <- stats_drinking.days %>% 
  left_join(ci %>% select(-Estimate), by=join_by(contrast==contrast))
stats_drinking.days

#### Bootstrapping --------------------------------------------

##### Differences in 3 Months - Intervention -----------------------------------

pred_df <- data_drinking %>% 
  select(practid, period, treat)

pred_df <- cbind(pred_df, 
                 pred = predict(drinking.days_nbm, data_drinking, type="response"))

means_nbm <- pred_df %>% 
  group_by(period, treat) %>% 
  summarise(mean=mean(pred))

# Bootstrapping the difference for Intervention group _______
boot_data_intervention <- pred_df %>%
  filter(treat == "Intervention")

boot_result_intervention <- boot(data = boot_data_intervention,
                                 statistic = mean_diff_time_func,
                                 R = 1000)

# 95% Confidence Interval for the Intervention group
conf_int_intervention <- boot.ci(boot_result_intervention, conf = 0.95, type = "perc")

##### Differences in 3 Months - Usual care -----------------------------------
boot_data_usual <- pred_df %>%
  filter(treat == "Usual care")

boot_result_usual <- boot(data = boot_data_usual,
                          statistic = mean_diff_time_func,
                          R = 1000)

# 95% Confidence Interval for the Usual care group
conf_int_usual <- boot.ci(boot_result_usual, conf = 0.95, type = "perc")

plot_bootstrap_results_combined(boot_result_intervention, conf_int_intervention, 
                                boot_result_usual, conf_int_usual,
                                pred_df)


##### Difference in Differences -----------------------------------------------

# Run the bootstrap
set.seed(12345)
boot_result_did <- boot(data = pred_df, statistic = bootstrap_did, R = 1000)

# Calculate 95% CI
conf_int_did <- boot.ci(boot_result_did, conf = 0.95, type = "perc")
print(conf_int_did)


plot_did_distribution(pred_df, boot_result_did, conf_int_did,
                      x=-2, y=0.6,
                      subtitle = "NBM - Drinking Days")


final_results <- rbind(final_results, 
                       get_values_boot(boot_result_intervention, conf_int_intervention, 
                                       boot_result_usual, conf_int_usual, 
                                       boot_result_did, conf_int_did,
                                       pred_df, variable = "Drinking Days")) 


### Drinking amount - NBM ---------------------------------------------------------------------
drinking.amount_nbm <- glmmTMB(drinking.amount ~  Xalt + (1|practid), 
                               family = nbinom2, 
                               data = data_drinking)

stats_drinking.amount <- summary(drinking.amount_nbm)
stats_drinking.amount <- as.data.frame(stats_drinking.amount$coefficients$cond)
stats_drinking.amount <- tibble::rownames_to_column(stats_drinking.amount, "contrast") %>% 
  select(-`z value`) %>% 
  mutate(var = "binge.days")

# Calculate confidence intervals
ci <- tibble::rownames_to_column(as.data.frame(confint(drinking.amount_nbm)), "contrast") %>% 
  mutate_if(is.numeric, exp) %>% 
  mutate(contrast = dplyr::recode(contrast, "cond.Xaltperiod3mo" = "Xaltperiod3mo",
                                  "cond.Xaltperiod3mo:treatIntervention" = "Xaltperiod3mo:treatIntervention"))
# Merge the statistics with confidence intervals
stats_drinking.amount <- stats_drinking.amount %>% 
  left_join(ci %>% select(-Estimate), by=join_by(contrast==contrast))
stats_drinking.amount

#### Bootstrapping --------------------------------------------

##### Differences in 3 Months - Intervention -----------------------------------

pred_df <- data_drinking %>% 
  select(practid, period, treat)

pred_df <- cbind(pred_df, 
                 pred = predict(drinking.amount_nbm, data_drinking, type="response"))

means_nbm <- pred_df %>% 
  group_by(period, treat) %>% 
  summarise(mean=mean(pred))

# Bootstrapping the difference for Intervention group _______
boot_data_intervention <- pred_df %>%
  filter(treat == "Intervention")

boot_result_intervention <- boot(data = boot_data_intervention,
                                 statistic = mean_diff_time_func,
                                 R = 1000)

# 95% Confidence Interval for the Intervention group
conf_int_intervention <- boot.ci(boot_result_intervention, conf = 0.95, type = "perc")

##### Differences in 3 Months - Usual care -----------------------------------
boot_data_usual <- pred_df %>%
  filter(treat == "Usual care")

boot_result_usual <- boot(data = boot_data_usual,
                          statistic = mean_diff_time_func,
                          R = 1000)

# 95% Confidence Interval for the Usual care group
conf_int_usual <- boot.ci(boot_result_usual, conf = 0.95, type = "perc")

plot_bootstrap_results_combined(boot_result_intervention, conf_int_intervention, 
                                boot_result_usual, conf_int_usual,
                                pred_df)


##### Difference in Differences -----------------------------------------------

# Run the bootstrap
set.seed(12345)
boot_result_did <- boot(data = pred_df, statistic = bootstrap_did, R = 1000)

# Calculate 95% CI
conf_int_did <- boot.ci(boot_result_did, conf = 0.95, type = "perc")
print(conf_int_did)


plot_did_distribution(pred_df, boot_result_did, conf_int_did,
                      x=-10, y=0.05,
                      subtitle = "NBM - Drinking Amount")


final_results <- rbind(final_results, 
                       get_values_boot(boot_result_intervention, conf_int_intervention, 
                                       boot_result_usual, conf_int_usual, 
                                       boot_result_did, conf_int_did,
                                       pred_df, variable = "Drinking Amount")) 
### DrinC - LMER -------------------------------------------------------------------
data_drinc <- data_subset %>% 
  filter(!practid %in% remove_ids_drinc)

X <- model.matrix(~period * treat, data = data_drinc)
Xalt <- X[, c("period3mo", "period3mo:treatIntervention")]

drinc_lmer <- lmerTest::lmer(drinc_sum ~ Xalt + (1 | practid), 
                             data = data_drinc)

stats_drinc <- summary(drinc_lmer)
stats_drinc <- as.data.frame(stats_drinc$coefficients)
stats_drinc <- tibble::rownames_to_column(stats_drinc, "contrast") %>% 
  select(-`t value`) %>% 
  mutate(var = "drinc")

# Calculate confidence intervals
ci_drinc <- tibble::rownames_to_column(as.data.frame(confint(drinc_lmer)), "contrast") %>% 
  mutate(contrast = dplyr::recode(contrast, "cond.Xaltperiod3mo" = "Xaltperiod3mo",
                                  "cond.Xaltperiod3mo:treatIntervention" = "Xaltperiod3mo:treatIntervention"))
# Merge the statistics with confidence intervals
stats_drinc <- stats_drinc %>% 
  left_join(ci_drinc, by=join_by(contrast==contrast))
#### Bootstrapping --------------------------------------------

pred_df <- data_drinc %>% 
  select(practid, period, treat)

pred_df <- cbind(pred_df, pred = predict(drinc_lmer, newdata=data_drinc,
                                         allow.new.levels=T))
##### Differences in 3 Months - Intervention -----------------------------------

# Bootstrapping the difference for Intervention group _______
boot_data_intervention <- pred_df %>%
  filter(treat == "Intervention")

boot_result_intervention <- boot(data = boot_data_intervention,
                                 statistic = mean_diff_time_func,
                                 R = 1000)

# 95% Confidence Interval for the Intervention group
conf_int_intervention <- boot.ci(boot_result_intervention, conf = 0.95, type = "perc")

##### Differences in 3 Months - Usual care -----------------------------------
boot_data_usual <- pred_df %>%
  filter(treat == "Usual care")

boot_result_usual <- boot(data = boot_data_usual,
                          statistic = mean_diff_time_func,
                          R = 1000)

# 95% Confidence Interval for the Usual care group
conf_int_usual <- boot.ci(boot_result_usual, conf = 0.95, type = "perc")

plot_bootstrap_results_combined(boot_result_intervention, conf_int_intervention, 
                                boot_result_usual, conf_int_usual,
                                pred_df, x= -13, y=0.5, 
                                subtitle="DrinC")


##### Difference in Differences -----------------------------------------------

# Run the bootstrap
set.seed(12345)
boot_result_did <- boot(data = pred_df, statistic = bootstrap_did, R = 1000)

# Calculate 95% CI
conf_int_did <- boot.ci(boot_result_did, conf = 0.95, type = "perc")

print(conf_int_did)

plot_did_distribution(pred_df, boot_result_did, conf_int_did,
                      x=-2, y=0.6,
                      subtitle = "LMER - DrinC")


final_results <- rbind(final_results, 
                       get_values_boot(boot_result_intervention, conf_int_intervention, 
                                       boot_result_usual, conf_int_usual, 
                                       boot_result_did, conf_int_did,
                                       pred_df, variable = "DRINC")) 
### AUDIT - LMER -------------------------------------------------------------------
data_audit <- data_subset %>% 
  filter(!practid %in% remove_ids_audit)

X <- model.matrix(~period * treat, data = data_audit)
Xalt <- X[, c("period3mo", "period3mo:treatIntervention")]

audit_lmer <- lmerTest::lmer(audit_sum ~ Xalt + (1 | practid), 
                             data = data_audit)

stats_audit <- summary(audit_lmer)
stats_audit <- as.data.frame(stats_audit$coefficients)
stats_audit <- tibble::rownames_to_column(stats_audit, "contrast") %>% 
  select(-`t value`) %>% 
  mutate(var = "drinc")

# Calculate confidence intervals
ci_audit <- tibble::rownames_to_column(as.data.frame(confint(audit_lmer)), "contrast") %>% 
  mutate(contrast = dplyr::recode(contrast, "cond.Xaltperiod3mo" = "Xaltperiod3mo",
                                  "cond.Xaltperiod3mo:treatIntervention" = "Xaltperiod3mo:treatIntervention"))
# Merge the statistics with confidence intervals
stats_audit <- stats_audit %>% 
  left_join(ci_audit, by=join_by(contrast==contrast))
#### Bootstrapping --------------------------------------------
pred_df <- data_audit %>% 
  select(practid, period, treat)


pred_df <- cbind(pred_df, pred = predict(audit_lmer, newdata=data_audit,
                                         allow.new.levels=T))

##### Differences in 3 Months - Intervention -----------------------------------


# Bootstrapping the difference for Intervention group _______
boot_data_intervention <- pred_df %>%
  filter(treat == "Intervention")

boot_result_intervention <- boot(data = boot_data_intervention,
                                 statistic = mean_diff_time_func,
                                 R = 1000)

# 95% Confidence Interval for the Intervention group
conf_int_intervention <- boot.ci(boot_result_intervention, conf = 0.95, type = "perc")

##### Differences in 3 Months - Usual care -----------------------------------
boot_data_usual <- pred_df %>%
  filter(treat == "Usual care")

boot_result_usual <- boot(data = boot_data_usual,
                          statistic = mean_diff_time_func,
                          R = 1000)

# 95% Confidence Interval for the Usual care group
conf_int_usual <- boot.ci(boot_result_usual, conf = 0.95, type = "perc")

plot_bootstrap_results_combined(boot_result_intervention, conf_int_intervention, 
                                boot_result_usual, conf_int_usual,
                                pred_df, x=-10, y=0.5, bins = 30,
                                subtitle = "AUDIT")


##### Difference in Differences -----------------------------------------------

# Run the bootstrap
set.seed(12345)
boot_result_did <- boot(data = pred_df, statistic = bootstrap_did, R = 1000)

# Calculate 95% CI
conf_int_did <- boot.ci(boot_result_did, conf = 0.95, type = "perc")

print(conf_int_did)


plot_did_distribution(pred_df, boot_result_did, conf_int_did,
                      x=-2, y=0.6,
                      subtitle = "LMER - AUDIT")


final_results <- rbind(final_results, 
                       get_values_boot(boot_result_intervention, conf_int_intervention, 
                                       boot_result_usual, conf_int_usual, 
                                       boot_result_did, conf_int_did,
                                       pred_df, variable = "AUDIT")) 
### PHQ9 - LMER -------------------------------------------------------------------
data_phq <- data_subset %>% 
  filter(!practid %in% remove_ids_phq)

X <- model.matrix(~period * treat, data = data_phq)
Xalt <- X[, c("period3mo", "period3mo:treatIntervention")]

phq_lmer <- lmerTest::lmer(phq_sum ~ Xalt + (1 | practid), 
                             data = data_phq)

stats_phq <- summary(phq_lmer)
stats_phq <- as.data.frame(stats_phq$coefficients)
stats_phq <- tibble::rownames_to_column(stats_phq, "contrast") %>% 
  select(-`t value`) %>% 
  mutate(var = "drinc")

# Calculate confidence intervals
ci_phq <- tibble::rownames_to_column(as.data.frame(confint(phq_lmer)), "contrast") %>% 
  mutate(contrast = dplyr::recode(contrast, "cond.Xaltperiod3mo" = "Xaltperiod3mo",
                                  "cond.Xaltperiod3mo:treatIntervention" = "Xaltperiod3mo:treatIntervention"))
# Merge the statistics with confidence intervals
stats_phq <- stats_phq %>% 
  left_join(ci_phq, by=join_by(contrast==contrast))

#### Bootstrapping --------------------------------------------
pred_df <- data_phq %>% 
  select(practid, period, treat)

pred_df <- cbind(pred_df, pred = predict(phq_lmer, newdata=data_phq,
                                         allow.new.levels=T))
##### Differences in 3 Months - Intervention -----------------------------------


# Bootstrapping the difference for Intervention group _______
boot_data_intervention <- pred_df %>%
  filter(treat == "Intervention")

boot_result_intervention <- boot(data = boot_data_intervention,
                                 statistic = mean_diff_time_func,
                                 R = 1000)

# 95% Confidence Interval for the Intervention group
conf_int_intervention <- boot.ci(boot_result_intervention, conf = 0.95, type = "perc")

##### Differences in 3 Months - Usual care -----------------------------------
boot_data_usual <- pred_df %>%
  filter(treat == "Usual care")

boot_result_usual <- boot(data = boot_data_usual,
                          statistic = mean_diff_time_func,
                          R = 1000)

# 95% Confidence Interval for the Usual care group
conf_int_usual <- boot.ci(boot_result_usual, conf = 0.95, type = "perc")

plot_bootstrap_results_combined(boot_result_intervention, conf_int_intervention, 
                                boot_result_usual, conf_int_usual,
                                pred_df, x=0, y=1,
                                subtitle = "PHQ9")


##### Difference in Differences -----------------------------------------------

# Run the bootstrap
set.seed(12345)
boot_result_did <- boot(data = pred_df, statistic = bootstrap_did, R = 1000)

# Calculate 95% CI
conf_int_did <- boot.ci(boot_result_did, conf = 0.95, type = "perc")

print(conf_int_did)


plot_did_distribution(pred_df, boot_result_did, conf_int_did,
                      x=-2, y=0.6,
                      subtitle = "LMER - PHQ9")


final_results <- rbind(final_results, 
                       get_values_boot(boot_result_intervention, conf_int_intervention, 
                                       boot_result_usual, conf_int_usual, 
                                       boot_result_did, conf_int_did,
                                       pred_df, variable = "PHQ9")) 

### Adding the Pvalues ------------------------------------------------------



pvals <- c(0,0, stats_binge.days[stats_binge.days$contrast == "Xaltperiod3mo:treatIntervention",'Pr(>|z|)'],
           0,0, stats_drinking.days[stats_drinking.days$contrast == "Xaltperiod3mo:treatIntervention",'Pr(>|z|)'],
           0,0, stats_drinking.amount[stats_drinking.amount$contrast == "Xaltperiod3mo:treatIntervention",'Pr(>|z|)'],
           0,0, stats_drinc[stats_drinc$contrast == "Xaltperiod3mo:treatIntervention",'Pr(>|t|)'],
           0,0, stats_audit[stats_audit$contrast == "Xaltperiod3mo:treatIntervention",'Pr(>|t|)'],
           0,0, stats_phq[stats_phq$contrast == "Xaltperiod3mo:treatIntervention",'Pr(>|t|)'])

# adding the column with the p values from the models
final_results <- final_results%>% 
  mutate(pval=pvals,
         combination = paste(variable, group))

# Plots -------------------------------------------------------------------
# adding annotation to the dataframe
final_results <- final_results %>% 
  mutate(annotation = ifelse(group != "DiD", paste0(diff, " (", ci_low, " ; ", ci_high, ")"),
                             paste0(diff, " (", ci_low, " ; ", ci_high, ")", " p=", round(pval, 4))))

plot_1 <- final_results %>% 
  filter(variable == "Binge Drinking Days", group != "DiD") %>% 
  ggplot(aes(x=group, y=diff)) + 
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), position="dodge", width=0.2) +
  theme_Publication() + 
  labs(x="", y="Change from baseline to 3months",
       subtitle = "Binge drinking days") +
  geom_signif(y_position = c(-5), xmin = c(0.5), 
              xmax = c(2.5), 
              annotation = final_results %>% 
                filter(variable == "Binge Drinking Days", group == "DiD") %>% 
                pull(annotation),
              tip_length = 0) +
  geom_text(aes(label=annotation, y= -3), size = 4) + 
  theme(plot.caption = element_text(hjust = 0))
plot_1
ggsave("./figures/primary_outcome_results.svg", width=10, height=12 )
## Secondary

final_results %>% 
  filter(variable != "Binge Drinking Days", group != "DiD") %>% 
  ggplot(aes(x=group, y=diff)) + 
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), position="dodge", width=0.2) +
  theme_Publication() + 
  labs(x="", y="Change from baseline to 3months",
       subtitle = "Binge drinking days") +
  facet_wrap(~variable, scales="free") +
  geom_text(aes(label=annotation, y= -3), size = 4) + 
  theme(plot.caption = element_text(hjust = 0))
ggsave("./figures/secondary_outcome_results.svg", width=10, height=12 )

final_results %>% 
  write.csv("./tables/pred_means.csv")
