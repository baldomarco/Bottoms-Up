library(survival)
library(survminer)
library(dplyr)
library(ggplot2)
library(readxl)

      
Recovery_wide_df <- read.csv("C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output - Copy/recovery_analysis_by_taxon_climate_wide_xCompare.csv")
Recovery_wide_df


# Prepare survival data
bryo_surv <- Recovery_wide_df %>%
  select(plotID, forest_cat, BRYOPHYTES_CLIM_1961_1990) %>%
  mutate(
    recovered = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 0, 1),
    time = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 266, BRYOPHYTES_CLIM_1961_1990)
  )

cox_bryo <- coxph(Surv(time, recovered) ~ forest_cat, data = bryo_surv)  # old version not including stand age effect
summary(cox_bryo)


fit_bryo <- survfit(Surv(time, recovered) ~ forest_cat, data = bryo_surv)

ggsurvplot(fit_bryo, data = bryo_surv,
           pval = TRUE,
           conf.int = TRUE,
           risk.table = TRUE,
           xlab = "Simulation Year",
           ylab = "Proportion not yet recovered",
           title = "BRYOPHYTES Recovery (1961–1990 Climate)",
           legend.title = "Forest Type",
           ggtheme = theme_minimal())


# invert the axis 

# Get survival data from the fit
bryo_df <- ggsurvplot(fit_bryo, data = bryo_surv, plot = FALSE)

# Invert the y values (i.e., 1 - survival)
bryo_df$plot$data$surv <- 1 - bryo_df$plot$data$surv
bryo_df$plot$data$upper <- 1 - bryo_df$plot$data$lower
bryo_df$plot$data$lower <- 1 - bryo_df$plot$data$upper

# Rebuild the plot with flipped survival
ggplot(bryo_df$plot$data, aes(x = time, y = surv, color = strata)) +
  geom_step() +
  labs(title = "BRYOPHYTES Recovery (1961–1990 Climate)",
       x = "Simulation Year",
       y = "Proportion recovered",
       color = "Forest Type") +
  theme_minimal()





# smooth on the first
#To apply smoothing to your Kaplan–Meier curves (instead of step-wise plots), you need to extract the survival data and replot it using ggplot2::geom_smooth().

#Here's how you can modify your code to get smoothed recovery curves (i.e., proportion recovered) by forest type:

library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# Prepare survival data
bryo_surv <- Recovery_wide_df %>%
  select(plotID, forest_cat, BRYOPHYTES_CLIM_1961_1990) %>%
  mutate(
    recovered = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 0, 1),
    time = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 266, BRYOPHYTES_CLIM_1961_1990)
  )

# Cox model
cox_bryo <- coxph(Surv(time, recovered) ~ forest_cat, data = bryo_surv)

# Survival fit
fit_bryo <- survfit(Surv(time, recovered) ~ forest_cat, data = bryo_surv)

# Extract survival data for plotting
surv_data <- surv_summary(fit_bryo, data = bryo_surv)

# Invert survival to show recovery
surv_data <- surv_data %>%
  mutate(
    recovered = 1 - surv,
    strata = gsub("forest_cat=", "", strata)  # Clean up legend labels
  )

# Smoothed ggplot version
ggplot(surv_data, aes(x = time, y = recovered, color = strata)) +
  geom_smooth(se = TRUE, method = "loess", span = 0.3, size = 1.2) +
  labs(
    title = "Smoothed BRYOPHYTES Recovery (1961–1990 Climate)",
    x = "Simulation Year",
    y = "Proportion recovered",
    color = "Forest Type"
  ) +
  theme_minimal()












# Using the Stand Age Effect

# Prepare survival data
bryo_surv <- Recovery_wide_df %>%
  select(plotID, forest_cat, StandAge, BRYOPHYTES_CLIM_1961_1990) %>%
  mutate(
    recovered = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 0, 1),
    time = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 266, BRYOPHYTES_CLIM_1961_1990)
  )

library(splines)
cox_bryo <- coxph(Surv(time, recovered) ~ forest_cat + ns(StandAge, df = 3), data = bryo_surv)


fit_bryo <- coxph(Surv(time, recovered) ~ forest_cat + StandAge, data = bryo_surv)

ggadjustedcurves(fit_bryo, variable = "forest_cat", data = bryo_surv,
                 legend.title = "Forest Type",
                 xlab = "Year",
                 ylab = "Adjusted probability not yet recovered",
                 title = "BRYOPHYTES Recovery (adjusted for StandAge)",
                 ggtheme = theme_minimal())

fit_bryo <- survfit(Surv(time, recovered) ~ forest_cat + ns(StandAge), data = bryo_surv)

ggsurvplot(fit_bryo, data = bryo_surv,
           pval = TRUE,
           conf.int = TRUE,
           risk.table = TRUE,
           xlab = "Simulation Year",
           ylab = "Proportion not yet recovered",
           title = "BRYOPHYTES Recovery (1961–1990 Climate)",
           legend.title = "Forest Type",
           ggtheme = theme_minimal())







# Second option 

bryo_surv <- bryo_surv %>%
  mutate(AgeClass = ifelse(StandAge >= median(StandAge, na.rm = TRUE), "Old", "Young"))

ggsurvplot(survfit(Surv(time, recovered) ~ AgeClass, data = bryo_surv),
           data = bryo_surv,
           pval = TRUE,
           conf.int = TRUE,
           risk.table = TRUE,
           xlab = "Year",
           ylab = "Proportion not yet recovered",
           title = "Recovery by StandAge Class",
           ggtheme = theme_minimal())


# Thrid a more complex model More complex model (e.g. interaction: forest_cat * StandAge)?


cox_bryo_interaction <- coxph(Surv(time, recovered) ~ forest_cat * StandAge, data = bryo_surv)
summary(cox_bryo_interaction)



ggadjustedcurves(cox_bryo_interaction, variable = "forest_cat", data = bryo_surv,
                 legend.title = "Forest Type",
                 xlab = "Year",
                 ylab = "Adjusted probability not yet recovered",
                 title = "BRYOPHYTES Recovery by Forest Type (adjusted for StandAge interaction)",
                 ggtheme = theme_minimal())

#--------------------------------------------

library(survival)
library(survminer)
library(dplyr)

# Fit Cox model with interaction between forest type and stand age
cox_bryo_inter <- coxph(Surv(time, recovered) ~ forest_cat * StandAge, data = bryo_surv)

# Plot adjusted survival curves for forest types, accounting for StandAge interaction
ggadjustedcurves(cox_bryo_inter,
                 variable = "forest_cat",       # Factor for grouping
                 data = bryo_surv,
                 method = "average",            # Averages over observed StandAge
                 xlab = "Year",
                 ylab = "Proportion not yet recovered",
                 title = "Adjusted BRYOPHYTES Recovery by Forest Type\n(Interaction with StandAge)",
                 ggtheme = theme_minimal())

###########################################################################################















###########################################################################################
TEST PER FARE DAVVERO LA SUR ANALISI MA CON L EFFETTO DELL ETA INIZIALE DELLO STAND
###########################################################################################


Cox proportional hazards model!!!
  p-value= Test	Meaning
Likelihood ratio test	Compares full model vs. null (no predictors)
Wald test	Tests individual coefficients (like t-test for regression slopes)
Score (logrank) test	Non-parametric test comparing survival curves by group (like log-rank)


Thanks for the clarification and the image!
  
  You want to keep the Cox model adjusted for the interaction between forest type and stand age, but plotted with:
  
  Kaplan–Meier-style ribbons (confidence intervals),

p-value displayed,

and styling like your earlier ggsurvplot() outputs.

Unfortunately, ggadjustedcurves() doesn’t support confidence ribbons or p-values in the same way. But we can extract predicted survival curves from the interaction model and plot them manually using ggplot2 with ribbons and p-values.








################################################################################
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# Prepare survival data (no changes needed here as it defines the event)
bryo_surv <- Recovery_wide_df %>%
  select(plotID, forest_cat, StandAge, BRYOPHYTES_CLIM_1961_1990) %>%
  mutate(
    recovered = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 0, 1), # 1 if recovered, 0 if censored
    time = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 266, BRYOPHYTES_CLIM_1961_1990)
  )

# Fit Cox model with interaction
cox_bryo_inter <- coxph(Surv(time, recovered) ~ forest_cat * StandAge, data = bryo_surv)
summary(cox_bryo_inter)


# Extract adjusted survival curves by forest type at the average StandAge
standage_avg <- mean(bryo_surv$StandAge, na.rm = TRUE)

# Create two new dataframes for prediction at average StandAge
newdata <- data.frame(
  forest_cat = c("Native Broadleaves", "Non-Native Coniferous"),
  StandAge = standage_avg
)

# Get survival predictions
fit_adj <- survfit(cox_bryo_inter, newdata = newdata)

# Convert to dataframe for ggplot and calculate 'recovered_proportion'
fit_df <- surv_summary(fit_adj, data = newdata) %>%
  mutate(
    forest_cat = factor(strata,
                        levels = seq_along(newdata$forest_cat),
                        labels = newdata$forest_cat),
    # Calculate the proportion recovered
    recovered_proportion = 1 - surv,
    # Also adjust the confidence intervals
    recovered_lower = 1 - upper, # Lower bound for 'not recovered' becomes upper for 'recovered'
    recovered_upper = 1 - lower  # Upper bound for 'not recovered' becomes lower for 'recovered'
  )

# P-value from model
pval <- summary(cox_bryo_inter)$wald["pvalue"]
pval_label <- paste0("p = ", format.pval(pval, digits = 3, eps = 0.001))

# Set colors
management_colors <- c(
  "Native Broadleaves" = "chocolate",
  "Non-Native Coniferous" = "darkgreen"
)

# Plot with ggplot, using the new 'recovered_proportion' and adjusted CIs
ggplot(fit_df, aes(x = time, y = recovered_proportion, color = forest_cat, fill = forest_cat)) +
  geom_step(size = 1) +
  geom_ribbon(aes(ymin = recovered_lower, ymax = recovered_upper), alpha = 0.25, color = NA) +
  scale_color_manual(values = management_colors) +
  scale_fill_manual(values = management_colors) +
  labs(
    title = "BRYOPHYTES Recovery (1961–1990 Climate)",
    subtitle = "Adjusted for StandAge (mean)",
    x = "Simulation Year",
    y = "Proportion Recovered", # Changed y-axis label
    color = "Forest Type",
    fill = "Forest Type"
  ) +
  annotate("text", x = 10, y = 0.95, # Adjusted y-position for p-value if plotting 'recovered'
           label = pval_label, size = 5, hjust = 0) + # Added hjust for left alignment
  theme_minimal(base_size = 14) +
  scale_y_continuous(limits = c(0, 1)) # Ensure y-axis goes from 0 to 1 for proportion


################################################################################

library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# Prepare survival data (no changes needed here as it defines the event)
bryo_surv <- Recovery_wide_df %>%
  select(plotID, forest_cat, StandAge, BRYOPHYTES_CLIM_1961_1990) %>%
  mutate(
    recovered = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 0, 1), # 1 if recovered, 0 if censored
    time = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 266, BRYOPHYTES_CLIM_1961_1990)
  )

# Fit Cox model with interaction
cox_bryo_inter <- coxph(Surv(time, recovered) ~ forest_cat * StandAge, data = bryo_surv)
summary(cox_bryo_inter)

# Extract adjusted survival curves by forest type at the average StandAge
standage_avg <- mean(bryo_surv$StandAge, na.rm = TRUE)

# Create two new dataframes for prediction at average StandAge
newdata <- data.frame(
  forest_cat = c("Native Broadleaves", "Non-Native Coniferous"),
  StandAge = standage_avg
)

# Get survival predictions
fit_adj <- survfit(cox_bryo_inter, newdata = newdata)

# Convert to dataframe for ggplot and calculate 'recovered_proportion'
fit_df <- surv_summary(fit_adj, data = newdata) %>%
  mutate(
    forest_cat = factor(strata,
                        levels = seq_along(newdata$forest_cat),
                        labels = newdata$forest_cat),
    # Calculate the proportion recovered
    recovered_proportion = 1 - surv,
    # Also adjust the confidence intervals
    recovered_lower = 1 - upper, # Lower bound for 'not recovered' becomes upper for 'recovered'
    recovered_upper = 1 - lower  # Upper bound for 'not recovered' becomes lower for 'recovered'
  )

# P-value from model for forest_cat term (representing difference between groups)
pval_overall_model <- summary(cox_bryo_inter)$wald["pvalue"]
pval_label <- paste0("p = ", format.pval(pval_overall_model, digits = 3, eps = 0.001))

# Set new colors
management_colors <- c(
  "Native Broadleaves" = "chocolate",
  "Non-Native Coniferous" = "darkgreen"
)

# Plot with ggplot, using the new 'recovered_proportion' and adjusted CIs
ggplot(fit_df, aes(x = time, y = recovered_proportion, color = forest_cat, fill = forest_cat)) +
  geom_step(size = 1) +
  geom_ribbon(aes(ymin = recovered_lower, ymax = recovered_upper), alpha = 0.25, color = NA) +
  scale_color_manual(values = management_colors) +
  scale_fill_manual(values = management_colors) +
  labs(
    title = "BRYOPHYTES Recovery by Forest Type (Stratified by Cox Model)",
    subtitle = "Each curve uses StandAge effect fitted within group",
    x = "Simulation Year",
    y = "Proportion Recovered",
    color = "Forest Type",
    fill = "Forest Type"
  ) +
  # Place p-value in the upper left part of the plot
  annotate("text", x = 10, y = 0.95, # Adjusted y-position for p-value if plotting 'recovered'
           label = pval_label, size = 5, hjust = 0) + # Added hjust for left alignment
  theme_minimal(base_size = 14) +
  scale_y_continuous(limits = c(0, 1)) # Ensure y-axis goes from 0 to 1 for proportion









################################################################################

# Prepare survival data (no changes needed here as it defines the event)
moth_surv <- Recovery_wide_df %>%
  select(plotID, forest_cat, StandAge, MOTHS_CLIM_1961_1990) %>%
  mutate(
    recovered = ifelse(is.na(MOTHS_CLIM_1961_1990), 0, 1), # 1 if recovered, 0 if censored
    time = ifelse(is.na(MOTHS_CLIM_1961_1990), 266, MOTHS_CLIM_1961_1990)
  )

# Fit Cox model with interaction
cox_bryo_inter <- coxph(Surv(time, recovered) ~ forest_cat * StandAge, data = moth_surv)
summary(cox_bryo_inter)

# Extract adjusted survival curves by forest type at the average StandAge
standage_avg <- mean(moth_surv$StandAge, na.rm = TRUE)

# Create two new dataframes for prediction at average StandAge
newdata <- data.frame(
  forest_cat = c("Native Broadleaves", "Non-Native Coniferous"),
  StandAge = standage_avg
)

# Get survival predictions
fit_adj <- survfit(cox_bryo_inter, newdata = newdata)

# Convert to dataframe for ggplot and calculate 'recovered_proportion'
fit_df <- surv_summary(fit_adj, data = newdata) %>%
  mutate(
    forest_cat = factor(strata,
                        levels = seq_along(newdata$forest_cat),
                        labels = newdata$forest_cat),
    # Calculate the proportion recovered
    recovered_proportion = 1 - surv,
    # Also adjust the confidence intervals
    recovered_lower = 1 - upper, # Lower bound for 'not recovered' becomes upper for 'recovered'
    recovered_upper = 1 - lower  # Upper bound for 'not recovered' becomes lower for 'recovered'
  )

# P-value from model for forest_cat term (representing difference between groups)
pval_overall_model <- summary(cox_bryo_inter)$wald["pvalue"]
pval_label <- paste0("p = ", format.pval(pval_overall_model, digits = 3, eps = 0.001))

# Set new colors
management_colors <- c(
  "Native Broadleaves" = "chocolate",
  "Non-Native Coniferous" = "darkgreen"
)

# Plot with ggplot, using the new 'recovered_proportion' and adjusted CIs
ggplot(fit_df, aes(x = time, y = recovered_proportion, color = forest_cat, fill = forest_cat)) +
  geom_step(size = 1) +
  geom_ribbon(aes(ymin = recovered_lower, ymax = recovered_upper), alpha = 0.25, color = NA) +
  scale_color_manual(values = management_colors) +
  scale_fill_manual(values = management_colors) +
  labs(
    title = "MOTHS Recovery by Forest Type (Stratified by Cox Model)",
    subtitle = "Each curve uses StandAge effect fitted within group",
    x = "Simulation Year",
    y = "Proportion Recovered",
    color = "Forest Type",
    fill = "Forest Type"
  ) +
  # Place p-value in the upper left part of the plot
  annotate("text", x = 10, y = 0.95, # Adjusted y-position for p-value if plotting 'recovered'
           label = pval_label, size = 5, hjust = 0) + # Added hjust for left alignment
  theme_minimal(base_size = 14) +
  scale_y_continuous(limits = c(0, 1)) # Ensure y-axis goes from 0 to 1 for proportion









################################################################################
































library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# 1. Prepare survival data
bryo_surv <- Recovery_wide_df %>%
  select(plotID, forest_cat, StandAge, BRYOPHYTES_CLIM_1961_1990) %>%
  mutate(
    recovered = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 0, 1),
    time = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 266, BRYOPHYTES_CLIM_1961_1990)
  )

# 2. Fit separate Cox models per forest type
cox_nb <- coxph(Surv(time, recovered) ~ StandAge, data = filter(bryo_surv, forest_cat == "Native Broadleaves"))
summary(cox_nb)

cox_nnc <- coxph(Surv(time, recovered) ~ StandAge, data = filter(bryo_surv, forest_cat == "Non-Native Coniferous"))
summary(cox_nnc)

# 3. Predict survival for each group at mean StandAge
mean_nb <- mean(filter(bryo_surv, forest_cat == "Native Broadleaves")$StandAge, na.rm = TRUE)
mean_nnc <- mean(filter(bryo_surv, forest_cat == "Non-Native Coniferous")$StandAge, na.rm = TRUE)

fit_nb <- survfit(cox_nb, newdata = data.frame(StandAge = mean_nb))
fit_nnc <- survfit(cox_nnc, newdata = data.frame(StandAge = mean_nnc))

# 4. Extract predicted survival curves and tag them
df_nb <- surv_summary(fit_nb, data = data.frame(StandAge = mean_nb)) %>%
  mutate(forest_cat = "Native Broadleaves")

df_nnc <- surv_summary(fit_nnc, data = data.frame(StandAge = mean_nnc)) %>%
  mutate(forest_cat = "Non-Native Coniferous")

# 5. Combine data for plotting
fit_df <- bind_rows(df_nb, df_nnc)

# 6. Plotting setup
management_colors <- c(
  "Native Broadleaves" = "chocolate",
  "Non-Native Coniferous" = "darkgreen"
)

# 7. Plot
ggplot(fit_df, aes(x = time, y = surv, color = forest_cat, fill = forest_cat)) +
  geom_step(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  scale_color_manual(values = management_colors) +
  scale_fill_manual(values = management_colors) +
  labs(
    title = "BRYOPHYTES Recovery by Forest Type (Stratified by Cox Model)",
    subtitle = "Each curve uses StandAge effect fitted within group",
    x = "Simulation Year",
    y = "Proportion not yet recovered",
    color = "Forest Type",
    fill = "Forest Type"
  ) +
  theme_minimal(base_size = 14)


# 7. Plot (proportion recovered + p-value annotation)
ggplot(fit_df, aes(x = time, y = 1 - surv, color = forest_cat, fill = forest_cat)) +
  geom_step(size = 1.2) +
  geom_ribbon(aes(ymin = 1 - upper, ymax = 1 - lower), alpha = 0.3, color = NA) +
  scale_color_manual(values = management_colors) +
  scale_fill_manual(values = management_colors) +
  labs(
    title = "BRYOPHYTES Recovery by Forest Type (Stratified by Cox Model)",
    subtitle = "Each curve uses StandAge effect fitted within group",
    x = "Simulation Year",
    y = "Proportion recovered",
    color = "Forest Type",
    fill = "Forest Type"
  ) +
  annotate("text", x = 10, y = 0.98, label = pval_label, hjust = 0, size = 5) +
  theme_minimal(base_size = 14)
































########## alternative
# Prepare survival data
bryo_surv <- Recovery_wide_df %>%
  select(plotID, forest_cat, StandAge, BRYOPHYTES_CLIM_1961_1990) %>%
  mutate(
    recovered = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 0, 1),
    time = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 266, BRYOPHYTES_CLIM_1961_1990)
  )

# Fit Cox model with interaction
cox_bryo_inter <- coxph(Surv(time, recovered) ~ forest_cat * StandAge, data = bryo_surv)


# Calculate mean StandAge
mean_age <- mean(bryo_surv$StandAge, na.rm = TRUE)

# New data for prediction at average StandAge
new_data <- data.frame(
  forest_cat = c("Native Broadleaves", "Non-Native Coniferous"),
  StandAge = mean_age
)

# Predict survival curve from Cox model
fit_pred <- survfit(cox_bryo_inter, newdata = new_data)


# Custom colors (brightened versions)
management_colors <- c(
  "Native Broadleaves" = "#FF7F50",        # Coral
  "Non-Native Coniferous" = "#20B2AA"      # LightSeaGreen
)

# Define colors manually just for forest type
management_colors <- c(
  "Native Broadleaves" = "chocolate",       
  "Non-Native Coniferous" = "darkgreen"
)

# Plot survival with ribbons (1 - survival = recovered)
ggsurvplot(fit_pred,
           data = new_data,
           fun = "event",  # Shows proportion recovered instead of not recovered
           conf.int = TRUE,
           pval = TRUE,
           pval.coord = c(10, 0.95),  # adjust as needed
           risk.table = FALSE,
           xlab = "Simulation Year",
           ylab = "Proportion recovered",
           title = "BRYOPHYTES Recovery (Adjusted for Stand Age)",
           legend.title = "Forest Type",
           legend.labs = c("Native Broadleaves", "Non-Native Coniferous"),
           palette = management_colors,
           ggtheme = theme_minimal())


summary(cox_bryo_inter)

# annotate("text", x = 20, y = 0.98, label = "Interaction p = 0.02", size = 4)

# Extract p-value of interaction term
p_interaction <- summary(cox_bryo_inter)$coefficients["forest_catNon-Native Coniferous:StandAge", "Pr(>|z|)"]
p_text <- paste0("Wald test p-value = 9e-04", format.pval(p_interaction, digits = 3, eps = 0.001))

# Store plot
plot_bryo <- ggsurvplot(fit_pred,
                        data = new_data,
                        fun = "event",
                        conf.int = TRUE,
                        pval = TRUE,
                        pval.coord = c(10, 0.95),
                        risk.table = FALSE,
                        xlab = "Simulation Year",
                        ylab = "Proportion recovered",
                        title = "BRYOPHYTES Recovery (Adjusted for Stand Age)",
                        legend.title = "Forest Type",
                        legend.labs = c("Native Broadleaves", "Non-Native Coniferous"),
                        palette = management_colors,
                        ggtheme = theme_minimal())

# Add annotation with interaction p-value
plot_bryo$plot + 
  annotate("text", x = 20, y = 0.98, label = p_text, size = 4)







What you really want is stratified prediction:
  Show the survival curve per forest type, adjusting for the distribution of StandAge within that group, not using the global mean.

This is best handled with ggadjustedcurves(), but you were right that:
  
  It uses all StandAges unless we group it explicitly.

It doesn’t plot group-wise averages unless you handle it correctly.




library(survival)
library(survminer)
library(dplyr)
library(tidyr)
library(purrr)

# Prepare survival data
bryo_surv <- Recovery_wide_df %>%
  select(plotID, forest_cat, StandAge, BRYOPHYTES_CLIM_1961_1990) %>%
  mutate(
    recovered = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 0, 1),
    time = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 266, BRYOPHYTES_CLIM_1961_1990)
  )


# 1. Model
cox_bryo_inter <- coxph(Surv(time, recovered) ~ forest_cat * StandAge, data = bryo_surv)
summary(cox_bryo_inter)

# 2. Calculate mean StandAge **within each group**
group_ages <- bryo_surv %>%
  group_by(forest_cat) %>%
  summarise(StandAge = mean(StandAge, na.rm = TRUE)) %>%
  ungroup()

# 3. Create prediction dataset
new_data_grouped <- group_ages

# 4. Predict survival curves per group
fit_grouped <- survfit(cox_bryo_inter, newdata = new_data_grouped)

# 5. Plot
ggsurvplot(fit_grouped,
           data = new_data_grouped,
           fun = "event",  # Show proportion recovered
           conf.int = TRUE,
           pval = TRUE,
           pval.coord = c(10, 0.95),
           risk.table = TRUE,
           xlab = "Simulation Year",
           ylab = "Proportion recovered",
           title = "BRYOPHYTES Recovery by Forest Type (Adjusted by Group Mean StandAge)",
           legend.title = "Forest Type",
           legend.labs = c("Native Broadleaves", "Non-Native Coniferous"),
           palette = management_colors,
           ggtheme = theme_minimal())





###############################################################################

# Create age class factor with 5 quantile bins

bryo_surv <- Recovery_wide_df %>%
  select(plotID, forest_cat, StandAge, BRYOPHYTES_CLIM_1961_1990) %>%
  mutate(
    recovered = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 0, 1),
    time = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 266, BRYOPHYTES_CLIM_1961_1990),
    StandAgeClass = ntile(StandAge, 5),
    StandAgeClass = factor(StandAgeClass, labels = c("Q1", "Q2", "Q3", "Q4", "Q5")),
    AgeForestGroup = interaction(forest_cat, StandAgeClass, sep = " - ")
  )

# Plot survival by Age × ForestType (as strata)
fit_bryo_group <- survfit(Surv(time, recovered) ~ AgeForestGroup, data = bryo_surv)



{
fit_bryo_group
Call: survfit(formula = Surv(time, recovered) ~ AgeForestGroup, data = bryo_surv)

n events median 0.95LCL 0.95UCL
AgeForestGroup=Native Broadleaves - Q1     4      3    232     204      NA
AgeForestGroup=Non-Native Coniferous - Q1 12      2     NA      NA      NA
AgeForestGroup=Native Broadleaves - Q2     5      4    216     171      NA
AgeForestGroup=Non-Native Coniferous - Q2 11      3     NA      NA      NA
AgeForestGroup=Native Broadleaves - Q3     2      1    125     125      NA
AgeForestGroup=Non-Native Coniferous - Q3 14      5     NA     224      NA
AgeForestGroup=Native Broadleaves - Q4     5      4    175      79      NA
AgeForestGroup=Non-Native Coniferous - Q4 11      3     NA      NA      NA
AgeForestGroup=Native Broadleaves - Q5    12     10    196     109      NA
AgeForestGroup=Non-Native Coniferous - Q5  4      1     NA     165      NA
}

# Define colors manually just for forest type
management_colors <- c(
  "Native Broadleaves" = "chocolate",       
  "Non-Native Coniferous" = "darkgreen"
)

# Extract forest type for each level to assign color
forest_types <- sapply(strsplit(levels(bryo_surv$AgeForestGroup), " - "), `[`, 1)
group_colors <- management_colors[forest_types]
names(group_colors) <- levels(bryo_surv$AgeForestGroup)

# Plot -IT IS A MESS BETTER THE UPPER ONE
ggsurvplot(fit_bryo_group,
           data = bryo_surv,
           fun = "event",
           conf.int = TRUE,
           pval = TRUE,
           pval.coord = c(10, 0.95),
           risk.table = TRUE,
           risk.table.title = "Number of forest plots not yet recovered",
           xlab = "Simulation Year",
           ylab = "Proportion recovered",
           title = "BRYOPHYTES Recovery by Forest Type and Stand Age Class",
           legend.title = "Forest Type × Age Class",
           palette = group_colors,
           ggtheme = theme_minimal())


################################################################################
# SAME BUT WITH 3 AGE CLASSES
################################################################################

library(survival)
library(survminer)
library(dplyr)

# 1. Prepare survival data
bryo_surv <- Recovery_wide_df %>%
  select(plotID, forest_cat, StandAge, BRYOPHYTES_CLIM_1961_1990) %>%
  mutate(
    recovered = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 0, 1),
    time = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 266, BRYOPHYTES_CLIM_1961_1990)
  )

# 2. Fit Cox model with interaction
cox_bryo_inter <- coxph(Surv(time, recovered) ~ forest_cat * StandAge, data = bryo_surv)

# 3. Compute mean StandAge within each forest type
mean_ages <- bryo_surv %>%
  group_by(forest_cat) %>%
  summarise(StandAge = mean(StandAge, na.rm = TRUE)) %>%
  ungroup()

# 4. Predict survival curves using those group-specific mean ages
fit_pred <- survfit(cox_bryo_inter, newdata = mean_ages)

# 5. Custom colors
management_colors <- c(
  "Native Broadleaves" = "chocolate",
  "Non-Native Coniferous" = "darkgreen"
)

# 6. Plot
ggsurvplot(fit_pred,
           data = mean_ages,
           fun = "event",  # proportion recovered
           conf.int = TRUE,
           pval = TRUE,
           pval.coord = c(10, 0.95),
           risk.table = TRUE,
           risk.table.title = "Number of plots not recovered",
           xlab = "Simulation Year",
           ylab = "Proportion recovered",
           title = "BRYOPHYTES Recovery by Forest Type (Adjusted for StandAge)",
           legend.title = "Forest Type",
           legend.labs = c("Native Broadleaves", "Non-Native Coniferous"),
           palette = management_colors,
           ggtheme = theme_minimal())



############################################################################ 

library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

#───────────────────────────────────────────────────────────────────────────────
#######  PREPARE THE DATA WITH AGE CLASSES INSTEAD OF CONTINUOUS STANDAGE ######

bryo_surv <- Recovery_wide_df %>%
  select(plotID, forest_cat, StandAge, BRYOPHYTES_CLIM_1961_1990) %>%
  mutate(
    recovered = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 0, 1),
    time = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 266, BRYOPHYTES_CLIM_1961_1990),
    
    # Divide into 5 quantile-based age classes (you can also use fixed breaks)
    age_class = cut(StandAge,
                    breaks = quantile(StandAge, probs = seq(0, 1, length.out = 4), na.rm = TRUE),
                    include.lowest = TRUE,
                    labels = c("Young", "Medium", "Old"))
  )

# Check distribution (optional)
table(bryo_surv$age_class)


#───────────────────────────────────────────────────────────────────────────────
#######     FULL SURVIVAL CURVE: Forest Type + Age Class Grouping       #########

fit_bryo_ageclass <- survfit(Surv(time, recovered) ~ forest_cat + age_class, data = bryo_surv)
{
  > fit_surv
  Call: survfit(formula = Surv(time, recovered) ~ forest_cat + age_class, 
                data = bryo_surv)
  
  n events median 0.95LCL 0.95UCL
  forest_cat=Native Broadleaves, age_class=Young      9      7    216     204      NA
  forest_cat=Native Broadleaves, age_class=Medium     3      2    217     125      NA
  forest_cat=Native Broadleaves, age_class=Old       16     13    193      79      NA
  forest_cat=Non-Native Coniferous, age_class=Young  18      4     NA      NA      NA
  forest_cat=Non-Native Coniferous, age_class=Medium 23      8     NA     224      NA
  forest_cat=Non-Native Coniferous, age_class=Old    11      2     NA      NA      NA
}
summary(fit_bryo_ageclass)

ggsurvplot(fit_bryo_ageclass, 
           data = bryo_surv,
           facet.by = "age_class",  # Separate facet for each age class
           pval = TRUE,
           conf.int = TRUE,
           risk.table = TRUE,
           xlab = "Simulation Year",
           ylab = "Proportion not yet recovered",
           title = "BRYOPHYTES Recovery by Forest Type and Stand Age Class",
           legend.title = "Forest Type",
           legend.labs = c("Native Broadleaves", "Non-Native Coniferous"),
           palette = c("chocolate", "darkgreen"),
           ggtheme = theme_minimal())


#############         Try to make a different visualization


