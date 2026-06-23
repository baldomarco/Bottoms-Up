                                        # SURVIVAL TEST V2 #


library(dplyr)
library(survival)
library(survminer)

Recovery_wide_df <- read.csv("C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output - Copy/recovery_analysis_by_taxon_climate_wide_xCompare.csv")
Recovery_wide_df

# 1. Prepare survival data
bryo_surv <- Recovery_wide_df %>%
  select(plotID, forest_cat, StandAge, BRYOPHYTES_CLIM_1961_1990) %>%
  mutate(
    recovered = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 0, 1),
    time = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 266, BRYOPHYTES_CLIM_1961_1990)
  )

# Create 3 Age Classes based on tertiles (33%, 66%)
bryo_surv <- bryo_surv %>%
  mutate(AgeClass = case_when(
    StandAge < quantile(StandAge, 1/3, na.rm = TRUE) ~ "Young",
    StandAge < quantile(StandAge, 2/3, na.rm = TRUE) ~ "Middle",
    TRUE ~ "Old"
  ))

bryo_surv <- bryo_surv %>%
  mutate(
    AgeClass = factor(AgeClass, levels = c("Young", "Middle", "Old"))
  )

ggplot(bryo_surv, aes(x = StandAge, fill = forest_cat)) +
  geom_histogram(binwidth = 5, position = "dodge", color = "black") +
  facet_wrap(~AgeClass) +
  labs(title = "Stand Age Distribution by Age Class and Forest Type",
       x = "Stand Age", y = "Count") +
  scale_fill_manual(values = c("#d95f02", "#1b9e77")) +  # Customize colors
  theme_minimal()

ggplot(bryo_surv, aes(x = AgeClass, y = StandAge, fill = forest_cat)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  labs(title = "Boxplot of Stand Age by Age Class and Forest Type",
       x = "Age Class", y = "Stand Age") +
  scale_fill_manual(values = c("#d95f02", "#1b9e77")) +
  theme_minimal()

# Plot survival curves by AgeClass
ggsurvplot(
  survfit(Surv(time, recovered) ~ AgeClass, data = bryo_surv),
  data = bryo_surv,
  pval = TRUE, # This fits a Kaplan-Meier survival model comparing all three age classes and tests them using the log-rank test.
  conf.int = TRUE,
  risk.table = TRUE,
  xlab = "Year",
  ylab = "Proportion not yet recovered",
  title = "Recovery by StandAge Class",
  ggtheme = theme_minimal()
)

#------------------------
# ONLY YOUNG VS OLD
#-----------------------

# Subset only "Young" and "Old"
bryo_YO <- bryo_surv %>%
  filter(AgeClass %in% c("Young", "Old"))

ggsurvplot(
  survfit(Surv(time, recovered) ~ AgeClass, data = bryo_YO),
  data = bryo_YO,
  pval = TRUE,                      # Log-rank test between Young and Old
  conf.int = TRUE,
  risk.table = TRUE,
  xlab = "Year",
  ylab = "Proportion not yet recovered",
  title = "Recovery by StandAge Class (Young vs Old)",
  ggtheme = theme_minimal()
)


survdiff(Surv(time, recovered) ~ AgeClass, data = bryo_YO)

#------------------------
# AGE CLASSES BASED ON THEORETICAL INFORMATION
#-----------------------

bryo_surv <- bryo_surv %>%
  mutate(
    AgeClass = case_when(
      StandAge <= 50 ~ "Young",
      StandAge > 50 & StandAge <= 100 ~ "Middle",
      StandAge > 100 ~ "Old"
    ),
    AgeClass = factor(AgeClass, levels = c("Young", "Middle", "Old"))
  )

survdiff(Surv(time, recovered) ~ AgeClass, data = bryo_surv)

bryo_surv <- bryo_surv %>%
  mutate(
    AgeClass = factor(AgeClass, levels = c("Young", "Middle", "Old"))
  )

ggplot(bryo_surv, aes(x = StandAge, fill = forest_cat)) +
  geom_histogram(binwidth = 5, position = "dodge", color = "black") +
  facet_wrap(~AgeClass) +
  labs(title = "Stand Age Distribution by Age Class and Forest Type",
       x = "Stand Age", y = "Count") +
  scale_fill_manual(values = c("#d95f02", "#1b9e77")) +  # Customize colors
  theme_minimal()

ggplot(bryo_surv, aes(x = AgeClass, y = StandAge, fill = forest_cat)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  labs(title = "Boxplot of Stand Age by Age Class and Forest Type",
       x = "Age Class", y = "Stand Age") +
  scale_fill_manual(values = c("#d95f02", "#1b9e77")) +
  theme_minimal()

# Plot survival curves by AgeClass
ggsurvplot(
  survfit(Surv(time, recovered) ~ AgeClass, data = bryo_surv),
  data = bryo_surv,
  fun = "event",  # This plots proportion recovered
  pval = TRUE,    # This fits a Kaplan-Meier survival model comparing all three age classes and tests them using the log-rank test.
  conf.int = TRUE,
  risk.table = TRUE,
  xlab = "Year",
  ylab = "Proportion not yet recovered",
  title = "Recovery by StandAge Class",
  ggtheme = theme_minimal()
)



##################                 Load required packages
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

#───────────────────────────────────────────────────────────────────────────────
# 1. PREPARE THE DATA
#───────────────────────────────────────────────────────────────────────────────

bryo_surv <- Recovery_wide_df %>%
  select(plotID, forest_cat, StandAge, BRYOPHYTES_CLIM_1961_1990) %>%
  mutate(
    recovered = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 0, 1),
    time = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 266, BRYOPHYTES_CLIM_1961_1990),
    age_class = cut(StandAge,
                    breaks = quantile(StandAge, probs = seq(0, 1, length.out = 4), na.rm = TRUE),
                    include.lowest = TRUE,
                    labels = c("Young", "Medium", "Old"))
  )

# Check distribution (optional)
table(bryo_surv$age_class)

# 1. Fit the Cox model with spline on StandAge
cox_spline_fit <- coxph(Surv(time, recovered) ~ forest_cat + pspline(StandAge), data = bryo_surv)
summary(cox_spline_fit)

# 2. Create representative data for predictions (1 curve per forest_cat)
new_data <- bryo_surv %>%
  group_by(forest_cat, age_class) %>%
  summarise(StandAge = mean(StandAge, na.rm = TRUE)) %>%
  ungroup()

# 3. Fit survival curves from the Cox model for each group
fit_spline_surv <- survfit(cox_spline_fit, newdata = new_data)

# 4. Plot
ggsurvplot(fit_spline_surv,
           data = new_data,
           conf.int = TRUE,
           risk.table = FALSE,
           xlab = "Simulation Year",
           ylab = "Proportion Not Yet Recovered",
           title = "Smoothed Recovery Curves (Spline on Stand Age)",
           legend.title = "Forest Type",
           legend.labs = new_data$forest_cat,
           palette = c("chocolate", "darkgreen"),
           ggtheme = theme_minimal())





###################################################
###################################################
###################################################

library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

#─────────────────────────────────────────────────────────────
# 1. Define Age Classes Separately for Each Forest Type
#─────────────────────────────────────────────────────────────

# Apply separate quantile cuts for each forest type
bryo_surv <- Recovery_wide_df %>%
  select(plotID, forest_cat, StandAge, BRYOPHYTES_CLIM_1961_1990) %>%
  mutate(
    recovered = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 0, 1),
    time = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 266, BRYOPHYTES_CLIM_1961_1990)
  ) %>%
  group_by(forest_cat) %>%
  mutate(
    age_class = cut(StandAge,
                    breaks = quantile(StandAge, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                    include.lowest = TRUE,
                    labels = c("Young", "Middle", "Old"))
  ) %>%
  ungroup()

# Check if age classes were assigned correctly
table(bryo_surv$forest_cat, bryo_surv$age_class)



#─────────────────────────────────────────────────────────────
# 2. Plot: Broadleaves
#─────────────────────────────────────────────────────────────

broadleaves_data <- bryo_surv %>% filter(forest_cat == "Native Broadleaves")

fit_broad <- survfit(Surv(time, recovered) ~ age_class, data = broadleaves_data)

survdiff(Surv(time, recovered) ~ age_class, data = broadleaves_data) # important

ggsurvplot(
  fit_broad,
  data = broadleaves_data,
  pval = TRUE,                       # log-rank test between Young and Old
  conf.int = TRUE,
  xlab = "Time",
  ylab = "Survival Probability",
  title = "Recovery Curves by Age Class (Native Broadleaves)",
  legend.title = "Age Class",
  legend.labs = c("Young", "Middle", "Old"),
  palette = c("#FDB813", "#F7821B", "#D7263D"),
  ggtheme = theme_minimal(),
  risk.table = TRUE
)





#─────────────────────────────────────────────────────────────
# 3. Plot: Conifers
#─────────────────────────────────────────────────────────────

conifers_data <- bryo_surv %>% filter(forest_cat == "Non-Native Coniferous")

fit_conifers <- survfit(Surv(time, recovered) ~ age_class, data = conifers_data)
summary(fit_conifers)

ggsurvplot(
  fit_conifers,
  data = conifers_data,
  pval = TRUE,
  conf.int = TRUE,
  xlab = "Time",
  ylab = "Survival Probability",
  title = "Recovery Curves by Age Class (Non-Native Coniferous)",
  legend.title = "Age Class",
  legend.labs = c("Young", "Middle", "Old"),
  palette = c("#FDB813", "#F7821B", "#D7263D"),
  ggtheme = theme_minimal(),
  risk.table = TRUE
)





################################################################
################################################################
################################################################

library(survival)
library(survminer)
library(dplyr)
library(ggplot2)
library(patchwork)

#─────────────────────────────────────────────────────────────
# 1. Prepare data & assign age classes by forest type
#─────────────────────────────────────────────────────────────

bryo_surv <- Recovery_wide_df %>%
  select(plotID, forest_cat, StandAge, BRYOPHYTES_CLIM_1961_1990) %>%
  mutate(
    recovered = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 0, 1),
    time = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 266, BRYOPHYTES_CLIM_1961_1990)
  ) %>%
  group_by(forest_cat) %>%
  mutate(
    age_class = cut(StandAge,
                    breaks = quantile(StandAge, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                    include.lowest = TRUE,
                    labels = c("Young", "Middle", "Old"))
  ) %>%
  ungroup()

#─────────────────────────────────────────────────────────────
# 2. Kaplan-Meier + extract survival for plotting
#─────────────────────────────────────────────────────────────

fit_km <- survfit(Surv(time, recovered) ~ forest_cat + age_class, data = bryo_surv)

summary(fit_km)

surv_df <- surv_summary(fit_km, data = bryo_surv) %>%
  mutate(
    recovered = 1 - surv
  )


#─────────────────────────────────────────────────────────────
# 3. Split the data properly for two graphs
#─────────────────────────────────────────────────────────────

broad_data <- surv_df %>% filter(forest_cat == "Native Broadleaves")
conif_data  <- surv_df %>% filter(forest_cat == "Non-Native Coniferous")

#─────────────────────────────────────────────────────────────
# 4. Plot each forest type with smoothed age class curves
#─────────────────────────────────────────────────────────────

# Add coord_cartesian(ylim = c(0, 1)) to both plots
broad_plot <- ggplot(broad_data, aes(x = time, y = recovered, color = age_class, fill = age_class)) +
  geom_step(size = 0.5, alpha = 0.4) +
  geom_smooth(se = FALSE, method = "loess", span = 0.4, size = 1.2) +
  scale_color_manual(values = c("Young" = "#FDB813", "Middle" = "#F7821B", "Old" = "#D7263D")) +
  scale_fill_manual(values = c("Young" = "#FDB813", "Middle" = "#F7821B", "Old" = "#D7263D")) +
  labs(
    title = "Native Broadleaves",
    x = "Simulation Year",
    y = "Proportion Recovered",
    color = "Age Class",
    fill = "Age Class"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal(base_size = 13)


conif_plot <- ggplot(conif_data, aes(x = time, y = recovered, color = age_class, fill = age_class)) +
  geom_step(size = 0.5, alpha = 0.4) +
  geom_smooth(se = FALSE, method = "loess", span = 0.4, size = 1.2) +
  scale_color_manual(values = c("Young" = "#FDB813", "Middle" = "#F7821B", "Old" = "#D7263D")) +
  scale_fill_manual(values = c("Young" = "#FDB813", "Middle" = "#F7821B", "Old" = "#D7263D")) +
  labs(
    title = "Non-Native Coniferous",
    x = "Simulation Year",
    y = "Proportion Recovered",
    color = "Age Class",
    fill = "Age Class"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal(base_size = 13)

#─────────────────────────────────────────────────────────────
# Combine into panel
combined_plot <- broad_plot + conif_plot + plot_layout(ncol = 2)
print(combined_plot)





##############################
FINAL version
##############################


library(survival)
library(survminer)
library(dplyr)
library(ggplot2)
library(patchwork)
library(broom)     # for tidy()
library(gridExtra) # for table display (if needed)


taxa <- c("BRYOPHYTES", "LICHENS", "MACROFUNGI", "BEETLES", "MOTHS")
climates <- c("CLIM_1961_1990", "CLIM_1991_2020")

all_pval_tables <- data.frame(
  Taxon = character(),
  Climate = character(),
  Forest_Type = character(),
  P_Value = numeric()
)

for (taxon in taxa) {
  plots <- list()
  
  for (climate in climates) {
    colname <- paste0(taxon, "_", climate)
    
    df <- Recovery_wide_df %>%
      select(plotID, forest_cat, StandAge, !!sym(colname)) %>%
      mutate(
        recovered = ifelse(is.na(!!sym(colname)), 0, 1),
        time = ifelse(is.na(!!sym(colname)), 266, !!sym(colname))
      ) %>%
      group_by(forest_cat) %>%
      mutate(
        age_class = cut(
          StandAge,
          breaks = quantile(StandAge, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
          include.lowest = TRUE,
          labels = c("Young", "Middle", "Old")
        )
      ) %>%
      ungroup()
    
    # Fit survival model
    fit_km <- survfit(Surv(time, recovered) ~ forest_cat + age_class, data = df)
    surv_df <- surv_summary(fit_km, data = df) %>%
      mutate(recovered = 1 - surv)
    
    # Split forest types
    broad_data <- surv_df %>% filter(forest_cat == "Native Broadleaves")
    conif_data <- surv_df %>% filter(forest_cat == "Non-Native Coniferous")
    
    # Define consistent aesthetics
    colors <- c("Young" = "#FDB813", "Middle" = "#F7821B", "Old" = "#D7263D")
    
    # Plot for Native Broadleaves
    broad_plot <- ggplot(broad_data, aes(x = time, y = recovered, color = age_class, fill = age_class)) +
      geom_step(size = 0.5, alpha = 0.4) +
      geom_smooth(se = FALSE, method = "loess", span = 0.4, size = 1.2) +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      labs(
        title = paste0("Native Broadleaves (", gsub("CLIM_", "", climate), ")"),
        x = "Simulation Year",
        y = "Proportion Recovered",
        color = "Age Class",
        fill = "Age Class"
      ) +
      coord_cartesian(ylim = c(0, 1)) +
      theme_minimal(base_size = 12)
    
    # Plot for Non-Native Coniferous
    conif_plot <- ggplot(conif_data, aes(x = time, y = recovered, color = age_class, fill = age_class)) +
      geom_step(size = 0.5, alpha = 0.4) +
      geom_smooth(se = FALSE, method = "loess", span = 0.4, size = 1.2) +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      labs(
        title = paste0("Non-Native Coniferous (", gsub("CLIM_", "", climate), ")"),
        x = "Simulation Year",
        y = "Proportion Recovered",
        color = "Age Class",
        fill = "Age Class"
      ) +
      coord_cartesian(ylim = c(0, 1)) +
      theme_minimal(base_size = 12)
    
    # Store in list
    plots[[climate]] <- list(broad_plot, conif_plot)
  }
  
  # Combine into 2x2 layout: rows = climate, cols = forest type
  combined_plot <- (plots[["CLIM_1961_1990"]][[1]] | plots[["CLIM_1961_1990"]][[2]]) /
    (plots[["CLIM_1991_2020"]][[1]] | plots[["CLIM_1991_2020"]][[2]])
  
  # Add title
  final_plot <- combined_plot + plot_annotation(title = paste(taxon, "- Recovery by Climate, Forest Type, and Age Class"))
  
  print(final_plot)
  
  # Store p-values
  pval_table <- data.frame(
    Climate = character(),
    Forest_Type = character(),
    P_Value = numeric()
  )
  
  for (forest in unique(df$forest_cat)) {
    subset_df <- df %>% filter(forest_cat == forest)
    
    surv_diff <- survdiff(Surv(time, recovered) ~ age_class, data = subset_df)
    p_val <- 1 - pchisq(surv_diff$chisq, length(surv_diff$n) - 1)
    
    pval_table <- rbind(pval_table, data.frame(
      Climate = gsub("CLIM_", "", climate),
      Forest_Type = forest,
      P_Value = signif(p_val, 4)
    ))
  }
  
  # Add taxon name to pval_table
  pval_table$Taxon <- taxon
  
  # Append to the master table
  all_pval_tables <- rbind(all_pval_tables, pval_table)
  
  
  cat("\n──────────────────────────────────────────────────────\n")
  cat(paste("Log-rank Test P-Values for", taxon, "\n"))
  print(pval_table)
  
  
  grid.arrange(final_plot, tableGrob(pval_table), ncol = 1, heights = c(4, 1))
  
  # Export all p-values to CSV
  write.csv(all_pval_tables, "logrank_pvalues_by_taxon.csv", row.names = FALSE)
  
}


# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/"
pdf(paste0(dataroot, "FINAL VERSION Survival Analysis StandAge classes + p values.pdf"), height=9, width=16)
