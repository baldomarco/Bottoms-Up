             # # RECOVERY STAGES TEST V10 #


# --- Add climate period to both datasets and rename for readability ---
rec1 <- recovery_long_1961_1990 %>%
  mutate(climate_period = "Climate Series 1961-1990")

rec2 <- recovery_long_1991_2020 %>%
  mutate(climate_period = "Climate Series 1991-2020")

# --- Combine datasets ---
recovery_all <- bind_rows(rec1, rec2)

# --- Filter relevant taxa_thresholds and assign clean taxon names directly ---
recovery_filtered <- recovery_all %>%
  filter(
    (str_detect(taxa_threshold, "Median") & !str_detect(taxa_threshold, "MOTHS")) |
      (str_detect(taxa_threshold, "Q3") & str_detect(taxa_threshold, "MOTHS"))
  ) %>%
  mutate(
    taxon = case_when(
      str_detect(taxa_threshold, "BEETLES") ~ "BEETLES",
      str_detect(taxa_threshold, "BRYOPHYTES") ~ "BRYOPHYTES",
      str_detect(taxa_threshold, "LICHENS") ~ "LICHENS",
      str_detect(taxa_threshold, "MACROFUNGI") ~ "MACROFUNGI",
      str_detect(taxa_threshold, "MOTHS") ~ "MOTHS",
      TRUE ~ taxa_threshold
    )
  )

# --- Create recovery category ---
recovery_processed <- recovery_filtered %>%
  mutate(
    recovery_status = case_when(
      YoR == 0 ~ "Converged before windthrow",
      YoR < 267 ~ "Converged before windthrow",
      YoR >= 267 & YoR < 350 ~ "Converged after windthrow",
      YoR >= 350 ~ "Not Converged",
      is.na(YoR) ~ "Not Converged"
    )
  )

# --- Summarise counts and convert to proportions ---
summary_long <- recovery_processed %>%
  group_by(taxon, forest_cat, climate_period, recovery_status) %>%
  summarise(n_plots = n_distinct(plotID), .groups = "drop") %>%
  mutate(
    forest_cat = case_when(
      str_detect(forest_cat, regex("native broadleaves", ignore_case = TRUE)) ~ "Broadleaves-dominated",
      str_detect(forest_cat, regex("non-native coniferous", ignore_case = TRUE)) ~ "Coniferous-dominated",
      TRUE ~ forest_cat
    )
  ) %>%
  group_by(taxon, forest_cat, climate_period) %>%                # group for proportion calc
  mutate(prop_plots = n_plots / sum(n_plots, na.rm = TRUE)) %>%  # proportion within group
  ungroup()


# --- Define the order of recovery_status for consistent stacking ---
recovery_status_levels <- c("Not Converged", "Converged after windthrow", "Converged before windthrow")
summary_long$recovery_status <- factor(summary_long$recovery_status, levels = recovery_status_levels)

# --- Define the color palette for the recovery stages ---
palette_recovery <- c(
  "Converged before windthrow" = "#556B2F",
  "Converged after windthrow" = "#C0FF3E",
  "Not Converged" = "#D7263D"
)

ggplot(summary_long, aes(x = taxon, y = prop_plots, fill = recovery_status)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(forest_cat ~ climate_period, scales = "free_y") +
  scale_fill_manual(
    values = palette_recovery,
    name = "Restoration Stage"
  ) +
  labs(
    x = "Taxon",
    y = "Proportion of Plots"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # optional % labels
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "gray95", color = NA),
    strip.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    legend.position = "right"
  )
