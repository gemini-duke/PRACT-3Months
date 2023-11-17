
# Picture and plot functions ----------------------------------------------

theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}


# Recode functions --------------------------------------------------------

# This function recodes categorical variables in the dataset into their corresponding numeric values. 
# The recoding is applied to AUDIT (Alcohol Use Disorders Identification Test) variables, 
# DrinC variables (drinc1 to drinc50), and PHQ (Patient Health Questionnaire) variables.

recode_function <- function(x){
  
  x %>% 
    # Recode the 'arm' variable: If 'arm' is not equal to 3, label it "Intervention", else "Usual care"
    mutate(arm = ifelse(arm != 3, "Intervention", "Usual care"),
    # Recode AUDIT variables (1-8) based on the frequency of alcohol consumption and related behaviors
    audit1 =as.numeric(as.character(recode_factor(audit1,
                                                  "Hakuna (Never)" = 0,
                                                  "Kila mwezi au chini ya mwezi (Monthly or less)" = 1,
                                                  "Mara 2 hadi 4 /mwezi (2 to 4 times a month )" = 2,
                                                  "Mara 2 hadi 3 /wiki (2 to 3 times a week)" = 3,
                                                  "4 au zaidi / wiki (4 or more times a week)" = 4))),
    # Continue recoding AUDIT variables (2-8) in a similar manner
    audit2 =as.numeric(as.character(recode_factor(audit2,
                                                  "1 or 2" = 0,"3 or 4" = 1,"5 or 6" = 2,"7 to 9" = 3,
                                                  "10 or more" = 4))),
    audit3 =as.numeric(as.character(recode_factor(audit3,
                                                  "Haijawahi kutokea (Never)" = 0,
                                                  "Chini ya kila mwezi (Less than monthly)" = 1,
                                                  "Kila Mwezi (Monthly)" = 2, "Kwa wiki (Weekly)" = 3,
                                                  "Kila siku au karibu kila siku (Daily/ almost daily)" = 4))),
    audit4 =as.numeric(as.character(recode_factor(audit4,
                                                  "Haijawahi kutokea (Never)" = 0,
                                                  "Chini ya kila mwezi (Less than monthly)" = 1,
                                                  "Kila Mwezi (Monthly)" = 2,
                                                  "Kwa wiki (Weekly)" = 3,
                                                  "Kila siku au karibu kila siku (Daily/ almost daily)" = 4))),
    audit5 =as.numeric(as.character(recode_factor(audit5,
                                                  "Haijawahi kutokea (Never)" = 0,
                                                  "Chini ya kila mwezi (Less than monthly)" = 1,
                                                  "Kila Mwezi (Monthly)" = 2,"Kwa wiki (Weekly)" = 3,
                                                  "Kila siku au karibu kila siku (Daily/ almost daily)" = 4))),
    audit6 =as.numeric(as.character(recode_factor(audit6,
                                                  "Haijawahi kutokea (Never)" = 0,
                                                  "Chini ya kila mwezi (Less than monthly)" = 1,
                                                  "Kila Mwezi (Monthly)" = 2,
                                                  "Kwa wiki (Weekly)" = 3,
                                                  "Kila siku au karibu kila siku (Daily/ almost daily)" = 4))),
    audit7 =as.numeric(as.character(recode_factor(audit7,
                                                  "Haijawahi kutokea (Never)" = 0,
                                                  "Chini ya kila mwezi (Less than monthly)" = 1,
                                                  "Kila Mwezi (Monthly)" = 2,"Kwa wiki (Weekly)" = 3,
                                                  "Kila siku au karibu kila siku (Daily/ almost daily)" = 4))),
    audit8 =as.numeric(as.character(recode_factor(audit8,
                                                  "Haijawahi kutokea (Never)" = 0,
                                                  "Chini ya kila mwezi (Less than monthly)" = 1,
                                                  "Kila Mwezi (Monthly)" = 2,"Kwa wiki (Weekly)" = 3,
                                                  "Kila siku au karibu kila siku (Daily/ almost daily)" = 4))),
    audit2=if_else(audit1==0 & is.na(audit2), 0, audit2),
    audit3=if_else(audit1==0 & is.na(audit3), 0, audit3),
    audit4=if_else(audit1==0 & is.na(audit4), 0, audit4),
    audit5=if_else(audit1==0 & is.na(audit5), 0, audit5),
    audit6=if_else(audit1==0 & is.na(audit6), 0, audit6),
    audit7=if_else(audit1==0 & is.na(audit7), 0, audit7),
    audit8=if_else(audit1==0 & is.na(audit8), 0, audit8),
    audit9 =as.numeric(as.character(recode_factor(audit9,
                                                  "Hapana (No)" = 0,
                                                  "Ndiyo, lakini si kwa mwaka uliopita (Yes, but not in the last year)" = 2,
                                                  "Ndiyo kwa mwaka uliopita (Yes, during the last year)" = 4))),
    audit9c =as.numeric(as.character(recode_factor(audit9c,
                                                   "Hapana (No)" = 0,
                                                   "Ndiyo, lakini si kwa mwaka uliopita (Yes, but not in the last year)" = 2,
                                                   "Ndiyo kwa mwaka uliopita (Yes, during the last year)" = 4))),
    audit9_final=as.numeric(ifelse(is.na(audit9c), audit9, audit9c)),
    audit10 =as.numeric(as.character(recode_factor(audit10,
                                                   "Hapana (No)" = 0,
                                                   "Ndiyo, lakini si kwa mwaka uliopita (Yes, but not in the last year)" = 2,
                                                   "Ndiyo kwa mwaka uliopita (Yes, during the last year)" = 4)))) %>% 
    # Recode Drinking variables (drinc1 to drinc50) based on the frequency of drinking
    mutate_at(vars(paste0("drinc", 1:50)), 
              ~as.numeric(as.character(recode_factor(.x,
                                                     "Haijawahi kutokea (Never)" = 0,
                                                     "Mara 1 au mara chache (Once or a few times)" = 1,
                                                     "Mara 1 au 2 kwa wiki (Once or twice a week)" = 2,
                                                     "Kila siku au karibu kila siku (Daily or almost daily)" = 3,
                                                     "Haijawahi kutokea" = 0,
                                                     "Mara 1 au mara chache" = 1,
                                                     "Mara 1 au 2 kwa wiki" = 2,
                                                     "Kila siku au karibu kila siku" = 3)))) %>% 
    # Recode PHQ variables (phq1 to phq9) based on the frequency of depression symptoms
    mutate_at(vars(paste0("phq", 1:9)), 
              ~as.numeric(as.character(recode_factor(.x,
                                                     "0: Hapana kabisa, Not at all" = 0,
                                                     "1:Siku kadhaa,Some days" = 1,
                                                     "2: Zaidi ya nusu ya siku zote, More than half the days" = 2,
                                                     "3: Karibu kila siku, Nearly every day" = 3,
                                                     "0: Hapana kabisa" = 0,
                                                     "1:Siku kadhaa" = 1,
                                                     "2: Zaidi ya nusu ya siku zote" = 2,
                                                     "3: Karibu kila siku" = 3))))
}

#' Summarize Scale Scores for Various Psychological and Drinking Measures
#'
#' This function takes a dataset and calculates summary scores for various scales, including AUDIT (Alcohol Use Disorders Identification Test), 
#' DrinC (Drinker Inventory of Consequences), PHQ-9 (Patient Health Questionnaire), and other measures related to drinking and binge drinking behavior.
#' It handles missing values by providing the option to calculate scores with or without removing NA values.
#'
#' @param x A data frame containing the variables for AUDIT, DrinC, PHQ9, and drinking/binge drinking measures.
#'          Expected variables include:
#'          - AUDIT (audit1, audit2, ..., audit10)
#'          - DrinC (divided into various subcategories like physical, interpersonal, social, intrapersonal, impulse control)
#'          - PHQ9 (variables under 'mental_health')
#'          - Drinking amount and binge drinking variables (categorized for males and females)
#' @return A data frame with the original data supplemented with the following columns:
#'         - `audit_sum`: Total score for AUDIT.
#'         - `auditover8`: Binary indicator for AUDIT score over 8.
#'         - `drinc_sum` and sub-scores for each DrinC category.
#'         - `phq_sum`: Total score for PHQ9.
#'         - `drinking.amount`: Total drinking amount.
#'         - `binge.days`, `binge.days_m`, `binge.days_f`, `binge.days_cat`: Measures of binge drinking days, categorized by gender.
#'         - `drinking.days`, `drinking.days_cat`: Number of drinking days and its categorical representation.
#'
#' @examples
#' # Assuming 'data' is a data frame with required variables
#' sum_scales(data)
#'
#' @importFrom dplyr mutate select ifelse rowSums
#' @importFrom magrittr %>%
sum_scales <- function(x){
  x %>% mutate(
    # Calculate the total score for AUDIT variables
    audit_sum=rowSums(select(.,audit1,audit2,audit3,audit4,audit5,audit6,audit7,
                             audit8,audit9_final,audit10), na.rm=FALSE),
    # Create binary variables indicating whether the AUDIT score is over 8
    auditover8 = ifelse(audit_sum > 8, TRUE, FALSE),
    # Continue with the same approach for DrinC, PHQ9, and drinking/binge drinking variables
    # DrinC
    drinc_sum=rowSums(select(.,all_of(c(drinc_physical_vars,drinc_interpersonal_vars,
                                        drinc_social_vars, drinc_intra_vars,
                                        drinc_impulse_vars))), na.rm=FALSE),
    drinc_physical=rowSums(select(.,all_of(drinc_physical_vars)), na.rm=FALSE),
    drinc_interpersonal=rowSums(select(.,all_of(drinc_interpersonal_vars)), na.rm=FALSE),
    drinc_social=rowSums(select(.,all_of(drinc_social_vars)), na.rm=FALSE),
    drinc_intra=rowSums(select(.,all_of(drinc_intra_vars)), na.rm=FALSE),
    drinc_control=rowSums(select(.,all_of(drinc_control_vars)), na.rm=FALSE),
    drinc_impulse=rowSums(select(.,all_of(drinc_impulse_vars)), na.rm=FALSE),
    # PHQ9
    phq_sum=rowSums(select(.,all_of(mental_health)), na.rm = FALSE),
    # Amount of drinks
    drinking.amount = rowSums(select(., all_of(drink_days))),
    # Binge days males and females
    binge.days_m = rowSums(select(., all_of(drink_days)) > 5),
    binge.days_f =rowSums(select(., all_of(drink_days)) > 4),
    binge.days = ifelse(female == 1, binge.days_f, binge.days_m),
    binge.days_cat = ifelse(binge.days >= 1, "Yes (1 or more)", "No (0)"), 
    # Drinking days
    drinking.days = rowSums(select(., all_of(drink_days)) > 0),
    drinking.days_cat = ifelse(drinking.days >= 1, "Yes (1 or more)", "No (0)"))
}


# Bootstrap functions -----------------------------------------------------


#' Calculate Mean Difference Across Time Periods
#'
#' This function calculates the difference in means of a specified variable across two time periods. 
#' It is particularly useful in assessing changes over time in a specific metric.
#'
#' @param data A data frame that includes the variable of interest (`pred`) and a time period identifier (`period`).
#' @param indices A vector of indices indicating which rows of the data frame to consider in the analysis.
#' @return A numeric value representing the difference in means of the `pred` variable between two time periods 
#'         (specifically, "3mo" and "base"). The function calculates the mean of `pred` for each period and then 
#'         subtracts the mean of the base period from the mean of the "3mo" period.
#'
#' @examples
#' # Assuming 'dataset' is a data frame with 'period' and 'pred' columns, and 'indices' is a vector of indices
#' mean_diff_time_func(dataset, indices)
#'
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr %>%
mean_diff_time_func <- function(data, indices) {
  sample_data <- data[indices, ]
  means <- sample_data %>%
    group_by(period) %>%
    summarise(mean = mean(pred))
  return(means$mean[which(means$period == "3mo")] - means$mean[which(means$period == "base")])
}

#' Plot Combined Bootstrap Results for Intervention and Usual Care Groups
#'
#' This function generates a plot that visualizes the bootstrap results for two groups, typically an intervention group 
#' and a usual care group. It displays histograms and density plots for the distribution of bootstrapped mean differences, 
#' along with the associated confidence intervals. The function also computes and displays the actual mean differences 
#' between two time periods for both groups.
#'
#' @param boot_result_intervention A bootstrap result object for the intervention group.
#' @param conf_int_intervention Confidence interval object for the intervention group's bootstrap result.
#' @param boot_result_usual_care A bootstrap result object for the usual care group.
#' @param conf_int_usual_care Confidence interval object for the usual care group's bootstrap result.
#' @param pred_df A data frame containing the variables required for the analysis (`treat`, `period`, `pred`).
#' @param x X-coordinate for placing the mean difference labels on the plot.
#' @param y Y-coordinate for placing the mean difference labels on the plot.
#' @param bins The number of bins to be used in the histogram.
#' @param subtitle An optional subtitle for the plot.
#' @return A ggplot object representing the bootstrap results. The plot includes histograms and density plots for 
#'         each group, lines indicating confidence intervals, and text labels showing the mean differences and 
#'         their confidence intervals.
#'
#' @examples
#' # Assuming boot_result_intervention, conf_int_intervention, boot_result_usual_care, conf_int_usual_care, and pred_df are predefined
#' plot_bootstrap_results_combined(boot_result_intervention, conf_int_intervention, 
#'                                 boot_result_usual_care, conf_int_usual_care, 
#'                                 pred_df, x=-7, y=0.5, bins = 30,
#'                                 subtitle = "My Bootstrap Results")
#'
#' @importFrom ggplot2 ggplot geom_histogram geom_density geom_vline geom_text facet_grid ggtitle labs scale_color_manual theme_Publication
#' @importFrom dplyr group_by summarise filter pull rbind
#' @importFrom magrittr %>%
plot_bootstrap_results_combined <- function(boot_result_intervention, conf_int_intervention, 
                                            boot_result_usual_care, conf_int_usual_care, 
                                            pred_df, x=-7, y=0.5, bins = 30,
                                            subtitle = "") {
  
  diff_usual <- pred_df %>% 
    group_by(treat, period) %>% 
    summarise(mean = mean(pred)) %>% 
    filter(treat == "Usual care", period == "3mo") %>% 
    pull(mean) - 
    pred_df %>% 
    group_by(treat, period) %>% 
    summarise(mean = mean(pred)) %>% 
    filter(treat == "Usual care", period == "base") %>% 
    pull(mean)
  
  diff_int <- pred_df %>% 
    group_by(treat, period) %>% 
    summarise(mean = mean(pred)) %>% 
    filter(treat == "Intervention", period == "3mo") %>% 
    pull(mean) - 
    pred_df %>% 
    group_by(treat, period) %>% 
    summarise(mean = mean(pred)) %>% 
    filter(treat == "Intervention", period == "base") %>% 
    pull(mean)
  
  # Extract bootstrapped statistics and confidence intervals
  bootstrapped_statistics_intervention <- boot_result_intervention$t
  bootstrapped_statistics_usual_care <- boot_result_usual_care$t
  conf_lower_intervention <- conf_int_intervention$percent[4]
  conf_upper_intervention <- conf_int_intervention$percent[5]
  conf_lower_usual_care <- conf_int_usual_care$percent[4]
  conf_upper_usual_care <- conf_int_usual_care$percent[5]
  
  # Create a combined data frame for plotting
  plot_data_intervention <- data.frame(Stat = bootstrapped_statistics_intervention, Group = "Intervention")
  plot_data_usual_care <- data.frame(Stat = bootstrapped_statistics_usual_care, Group = "Usual Care")
  plot_data <- rbind(plot_data_intervention, plot_data_usual_care)
  
  text_df <- data.frame(Group = c('Intervention','Usual Care'), 
                        lbl = c(paste0(round(diff_int, 4), " (", round(conf_lower_intervention, 4) , ", ", round(conf_upper_intervention, 4), ")"),
                                paste0(round(diff_usual, 4), " (", round(conf_lower_usual_care, 4) , ", ", round(conf_upper_usual_care, 4), ")")))
  
  
  # Generate the plot
  p <- ggplot(plot_data, aes(x = Stat)) +
    geom_histogram(aes(y = ..density..), bins = bins, fill = "skyblue", alpha = 0.7) +
    geom_density(color = "blue") +
    geom_vline(data = data.frame(Stat = c(conf_lower_intervention, conf_upper_intervention, conf_lower_usual_care, conf_upper_usual_care),
                                 Group = rep(c("Intervention", "Usual Care"), each = 2)), 
               aes(xintercept = Stat, color = Group), linetype = "dashed") +
    geom_text(data = text_df,  aes(x = x, y = y, label = lbl), hjust = 0, lineheight=3, size=6) + 
    scale_color_manual(values = c("Intervention" = "red", "Usual Care" = "purple")) +
    facet_grid(rows = vars(Group), scales = "free_y") +
    ggtitle("Bootstrap Distribution of Mean Difference: 3mo - Baseline") +
    labs(subtitle = subtitle) +
    xlab("Difference in Means") +
    ylab("Density") +
    theme_Publication()
  
  return(p)
}

## Difference in differences -----------------------------------------------


#' Bootstrap for Difference-in-Differences Analysis
#'
#' This function performs a bootstrap analysis for Difference-in-Differences (DiD). It calculates the DiD estimator 
#' by taking a bootstrap sample of the data, computing means for different groups (treatment vs. control, across different time periods), 
#' and then computing the difference in these means. It is useful for assessing the effect of an intervention over time.
#'
#' @param data A data frame containing the variables required for DiD analysis. It should include columns for 
#'             treatment group (`treat`), time period (`period`), and the predicted values (`pred`).
#' @param indices A vector of indices to draw a bootstrap sample from the `data`.
#' @return The DiD estimate from the bootstrap sample. If any required combination of treatment group and time period 
#'         is missing in the sample, the function returns `NA`. The returned value is a numeric difference 
#'         representing the estimated treatment effect adjusted for baseline differences between groups.
#'
#' @examples
#' # Assuming 'dataset' is a data frame with 'treat', 'period', and 'pred' columns and 'indices' is a vector of indices
#' bootstrap_did(dataset, indices)
#'
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr %>%
bootstrap_did <- function(data, indices) {
  sample_data <- data[indices, ]
  sample_data <- sample_data %>%
    group_by(treat, period) %>%
    summarise(mean_value = mean(pred), .groups = 'drop')
  
  # Check if any necessary combination of treatment group and time period is missing
  if (!any(sample_data$treat == "Intervention" & sample_data$period == "3mo") ||
      !any(sample_data$treat == "Intervention" & sample_data$period == "base") ||
      !any(sample_data$treat == "Usual care" & sample_data$period == "3mo") ||
      !any(sample_data$treat == "Usual care" & sample_data$period == "base")) {
    return(NA)  # Return NA if any combination is missing
  }
  
  # Calculate differences in mean values for the intervention group
  int_diff <- sample_data$mean_value[sample_data$treat == "Intervention" & sample_data$period == "3mo"] -
    sample_data$mean_value[sample_data$treat == "Intervention" & sample_data$period == "base"]
  
  # Calculate differences in mean values for the control group
  ctrl_diff <- sample_data$mean_value[sample_data$treat == "Usual care" & sample_data$period == "3mo"] -
    sample_data$mean_value[sample_data$treat == "Usual care" & sample_data$period == "base"]
  
  # Compute the difference-in-differences estimate
  did <- int_diff - ctrl_diff
  return(did)
}




#' Plot Bootstrap Distribution of Difference-in-Differences Estimates
#'
#' This function visualizes the bootstrap results of a Difference-in-Differences (DiD) analysis. 
#' It calculates the DiD estimate using mean values from provided data and then creates a histogram and density plot 
#' showing the distribution of bootstrap DiD estimates. Confidence intervals are also included on the plot.
#'
#' @param pred_df A data frame containing the variables for DiD analysis, including treatment group (`treat`), 
#'        time period (`period`), and predicted values (`pred`).
#' @param boot_result_did A bootstrap result object for the DiD analysis.
#' @param conf_int_did A confidence interval object for the DiD bootstrap result.
#' @param x X-coordinate for placing the DiD estimate label on the plot.
#' @param y Y-coordinate for placing the DiD estimate label on the plot.
#' @param subtitle An optional subtitle for the plot.
#' @return A ggplot object representing the bootstrap distribution of DiD estimates. The plot includes a histogram 
#'         and density plot, vertical lines indicating the confidence intervals, and a text label showing the DiD estimate 
#'         with its confidence interval.
#'
#' @examples
#' # Assuming pred_df, boot_result_did, and conf_int_did are predefined
#' plot_did_distribution(pred_df, boot_result_did, conf_int_did, x = 0, y = 0.02, subtitle = "My DiD Analysis")
#'
#' @importFrom ggplot2 ggplot geom_histogram geom_density geom_vline geom_text labs theme_Publication
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr %>%
plot_did_distribution <- function(pred_df, boot_result_did, conf_int_did,
                                  x = 0, y = 0.02,
                                  subtitle = "") {
  
  # Calculate means for each combination of 'treat' and 'period'
  mean_values <- pred_df %>%
    group_by(treat, period) %>%
    summarise(mean_value = mean(pred), .groups = 'drop')
  
  # Compute DiD
  int_diff <- mean_values$mean_value[mean_values$treat == "Intervention" & mean_values$period == "3mo"] -
    mean_values$mean_value[mean_values$treat == "Intervention" & mean_values$period == "base"]
  
  ctrl_diff <- mean_values$mean_value[mean_values$treat == "Usual care" & mean_values$period == "3mo"] -
    mean_values$mean_value[mean_values$treat == "Usual care" & mean_values$period == "base"]
  
  did_estimate <- int_diff - ctrl_diff
  
  # Prepare text annotations
  estimate_str <- paste0(
    round(did_estimate, 1), " (",
    round(conf_int_did$percent[4], 1), "; ",  # Lower CI
    round(conf_int_did$percent[5], 1), ")"    # Upper CI
  )
  
  text_df <- data.frame(
    x = x,
    y = y,  # Adjust y value as needed for positioning
    lbl = estimate_str
  )
  
  # Plot bootstrap distribution
  plot_data_did <- data.frame(DiD = boot_result_did$t)
  plot_obj <- ggplot(plot_data_did, aes(x = DiD)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", alpha = 0.7) +
    geom_density(color = "blue") +
    geom_vline(aes(xintercept = conf_int_did$percent[4]), color = "red", linetype = "dashed") +
    geom_vline(aes(xintercept = conf_int_did$percent[5]), color = "red", linetype = "dashed") +
    geom_text(data = text_df,  aes(x = x, y = y, label = lbl), hjust = 0, lineheight=3, size=6) +
    labs(title="Bootstrap Distribution of DiD Estimates",
         subtitle=subtitle) +
    xlab("Difference-in-Differences") +
    ylab("Density") +
    theme_Publication()
  
  return(plot_obj)
}

#' Extract and Compute Statistics from Bootstrap Results and Dataset
#'
#' This function calculates and extracts various statistics related to intervention, usual care, and 
#' Difference-in-Differences (DiD) analyses from bootstrap results and a provided dataset. It computes 
#' mean differences for intervention and usual care groups, the DiD, and extracts relevant confidence intervals.
#'
#' @param boot_result_intervention Bootstrap result object for the intervention group.
#' @param conf_int_intervention Confidence interval object for the intervention group's bootstrap result.
#' @param boot_result_usual_care Bootstrap result object for the usual care group.
#' @param conf_int_usual_care Confidence interval object for the usual care group's bootstrap result.
#' @param boot_results_did Bootstrap result object for the DiD analysis.
#' @param conf_int_did Confidence interval object for the DiD bootstrap result.
#' @param pred_df A data frame containing the variables for analysis (`treat`, `period`, `pred`).
#' @param variable An optional string specifying the name of the variable of interest. Defaults to "variable".
#' @return A data frame with the following columns:
#'         - `group`: Indicates the group (Intervention, Usual Care, DiD).
#'         - `diff`: The mean difference or DiD estimate.
#'         - `ci_low`: Lower bound of the confidence interval.
#'         - `ci_high`: Upper bound of the confidence interval.
#'         - `variable`: The name of the variable of interest.
#'
#' @examples
#' # Assuming all necessary bootstrap results and pred_df are predefined
#' get_values_boot(boot_result_intervention, conf_int_intervention, 
#'                 boot_result_usual_care, conf_int_usual_care, 
#'                 boot_results_did, conf_int_did,
#'                 pred_df, variable = "outcome")
#'
#' @importFrom dplyr group_by summarise filter pull
#' @importFrom magrittr %>%
get_values_boot <- function(boot_result_intervention, conf_int_intervention, 
                            boot_result_usual_care, conf_int_usual_care, 
                            boot_results_did, conf_int_did,
                            pred_df, variable = "variable") {
  
  diff_usual <- pred_df %>% 
    group_by(treat, period) %>% 
    summarise(mean = mean(pred)) %>% 
    filter(treat == "Usual care", period == "3mo") %>% 
    pull(mean) - 
    pred_df %>% 
    group_by(treat, period) %>% 
    summarise(mean = mean(pred)) %>% 
    filter(treat == "Usual care", period == "base") %>% 
    pull(mean)
  
  diff_int <- pred_df %>% 
    group_by(treat, period) %>% 
    summarise(mean = mean(pred)) %>% 
    filter(treat == "Intervention", period == "3mo") %>% 
    pull(mean) - 
    pred_df %>% 
    group_by(treat, period) %>% 
    summarise(mean = mean(pred)) %>% 
    filter(treat == "Intervention", period == "base") %>% 
    pull(mean)
  
  diff_diff <- diff_int - diff_usual
  
  # Extract bootstrapped statistics and confidence intervals
  bootstrapped_statistics_intervention <- boot_result_intervention$t
  bootstrapped_statistics_usual_care <- boot_result_usual_care$t
  conf_lower_intervention <- conf_int_intervention$percent[4]
  conf_upper_intervention <- conf_int_intervention$percent[5]
  conf_lower_usual_care <- conf_int_usual_care$percent[4]
  conf_upper_usual_care <- conf_int_usual_care$percent[5]
  
  conf_lower_did <- conf_int_did$percent[4]
  conf_upper_did <- conf_int_did$percent[5]
  
  obs_did <- bootstrap_did(pred_df, 1:nrow(pred_df))
  
  text_df <- data.frame(group = c('Intervention','Usual Care', "DiD"), 
                        diff = c(round(diff_int, 1), round(diff_usual, 1), round(diff_diff, 1)),
                        ci_low = c(round(conf_lower_intervention, 1), round(conf_lower_usual_care, 1), round(conf_lower_did, 1)),
                        ci_high = c(round(conf_upper_intervention, 1), round(conf_upper_usual_care, 1), round(conf_upper_did, 1)), 
                        variable = rep(variable, 3))
  return(text_df)
  
}