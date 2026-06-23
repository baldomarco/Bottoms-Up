                                           # SURVIVAL TEST V1 #

library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

Recovery_wide_df <- read.csv("C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output - Copy/recovery_analysis_by_taxon_climate_wide_xCompare.csv")
Recovery_wide_df

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






