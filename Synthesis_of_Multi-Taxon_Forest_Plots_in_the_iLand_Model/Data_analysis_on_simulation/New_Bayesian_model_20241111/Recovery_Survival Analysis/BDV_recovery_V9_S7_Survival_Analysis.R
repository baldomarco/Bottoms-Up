################          MARCO BALDO - 2025.07.04        ######################
################         YEAR OF RECOVERY TAXA BDV        ######################
################      GRAPHIC ANALYSIS SURVIVAL ANALYSIS  ######################
################          ARTICLE BALDO ET AL. 2025       ######################
################################################################################

library(survival)
library(survminer)
library(dplyr)
library(ggplot2)
library(readxl)
library(ggpubr)  



Recovery_wide_df <- read.csv("C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/recovery_analysis_by_taxon_climate_wide_xCompare.csv")
Recovery_wide_df

#--------------------
# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
dataroot <- "C:/iLand/2023/20230901_Bottoms_Up/20230914_plot_experiment/_project/output/"
#pdf(paste0(dataroot, "FINAL VERSION Survival Analysis 2025 07 16.pdf"), height=9, width=16)

#───────────────────────────────────────────────────────────────────────────────
#######  PREPARE THE DATA FOR THE SURVIVAL ANALYSIS USED AS RECOVERY  ##########

bryo_surv <- Recovery_wide_df %>%
  select(plotID, forest_cat, BRYOPHYTES_CLIM_1961_1990) %>%
  mutate(
    recovered = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 0, 1),
    time = ifelse(is.na(BRYOPHYTES_CLIM_1961_1990), 266, BRYOPHYTES_CLIM_1961_1990)
  )

cox_bryo <- coxph(Surv(time, recovered) ~ forest_cat, data = bryo_surv)  # old version not including stand age effect
summary(cox_bryo)
survdiff(Surv(time, recovered) ~ forest_cat, data = bryo_surv)


fit_bryo <- survfit(Surv(time, recovered) ~ forest_cat, data = bryo_surv)


#───────────────────────────────────────────────────────────────────────────────
#######           MAKE THE GRAPHS                             ##########

management_colors <- c(
  "Native Broadleaves" = "chocolate",       
  "Non-Native Coniferous" = "darkgreen"     
)

ggsurvplot(fit_bryo,
           data = bryo_surv,
           pval = TRUE,
           conf.int = TRUE,
           risk.table = TRUE,
           risk.table.title = "Number of forest plots not recovered",
           xlab = "Simulation Year",
           ylab = "Proportion not yet recovered",
           title = "BRYOPHYTES Recovery - Climate 1961–1990 (Kaplan–Meier survival curve)",
           legend.title = "Forest Type",
           legend.labs = c("Native Broadleaves", "Non-Native Coniferous"),
           palette = management_colors,
           ggtheme = theme_minimal())


#───────────────────────────────────────────────────────────────────────────────
#######         JUST INVERTED AXIS IN PROPORTION OF RECOVERED            ##########


# Refit survival object
fit_bryo <- survfit(Surv(time, recovered) ~ forest_cat, data = bryo_surv)

# Plot with p-value positioned higher and updated colors
ggsurvplot(fit_bryo,
           data = bryo_surv,
           fun = "event",  # proportion recovered
           pval = TRUE,
           pval.coord = c(10, 0.98),  # x = year, y = vertical position (adjust if needed)
           conf.int = TRUE,
           risk.table = TRUE,
           risk.table.title = "Number of forest plots not recovered",
           xlab = "Simulation Year",
           ylab = "Proportion recovered",
           title = "BRYOPHYTES Recovery  - Climate 1961–1990 (Kaplan–Meier survival curve)",
           legend.title = "Forest Type",
           legend.labs = c("Native Broadleaves", "Non-Native Coniferous"),
           palette = management_colors,
           ggtheme = theme_minimal())


#───────────────────────────────────────────────────────────────────────────────
####     MAKE THE SAME 2 ABOVE GRAPHS BUT FOR ALL TAXA AND CLIMATES          ####
#───────────────────────────────────────────────────────────────────────────────

# Taxa and climate combinations
taxa_climate <- list(
  c("BRYOPHYTES", "CLIM_1961_1990"),
  c("BRYOPHYTES", "CLIM_1991_2020"),
  c("LICHENS", "CLIM_1961_1990"),
  c("LICHENS", "CLIM_1991_2020"),
  c("MACROFUNGI", "CLIM_1961_1990"),
  c("MACROFUNGI", "CLIM_1991_2020"),
  c("BEETLES", "CLIM_1961_1990"),
  c("BEETLES", "CLIM_1991_2020"),
  c("MOTHS", "CLIM_1961_1990"),
  c("MOTHS", "CLIM_1991_2020")
)

# Loop through each combination
for (combo in taxa_climate) {
  taxon <- combo[1]
  climate <- combo[2]
  colname <- paste0(taxon, "_", climate)
  var_name <- tolower(paste0(taxon, "_", climate))
  df_name <- paste0("df_", var_name)
  
  cat("\n#───────────────────────────────────────────────────────────────────────────────")
  cat("\n#######  PREPARE THE DATA FOR", taxon, "-", climate, "##########\n\n")
  
  eval(parse(text = paste0(
    df_name, " <- Recovery_wide_df %>%\n",
    "  select(plotID, forest_cat, ", colname, ") %>%\n",
    "  mutate(\n",
    "    recovered = ifelse(is.na(", colname, "), 0, 1),\n",
    "    time = ifelse(is.na(", colname, "), 266, ", colname, ")\n",
    "  )"
  )))
  
  # Fit Cox model
  model_name <- paste0("cox_", tolower(taxon), "_", tolower(climate)) # Make model_name unique
  fit_name <- paste0("fit_", tolower(taxon), "_", tolower(climate)) # Make fit_name unique
  
  eval(parse(text = paste0(
    model_name, " <- coxph(Surv(time, recovered) ~ forest_cat, data = ", df_name, ")\n",
    "summary(", model_name, ")\n"
  )))
  
  eval(parse(text = paste0(
    fit_name, " <- survfit(Surv(time, recovered) ~ forest_cat, data = ", df_name, ")"
  )))
  
  # Format the climate string for the title
  formatted_climate <- gsub("CLIM_", "", climate) # Remove "CLIM_"
  formatted_climate <- gsub("_", " - ", formatted_climate) # Replace "_" with " - "
  
  # Plot 1: Proportion NOT yet recovered (survival curve)
  cat("\n#───────────────────────────────────────────────────────────────────────────────")
  cat("\n#######           PLOT PROPORTION NOT YET RECOVERED FOR", taxon, "-", climate, "##########\n\n")
  
  eval(parse(text = paste0(
    "print(ggsurvplot(", fit_name, ",\n",
    "  data = ", df_name, ",\n",
    "  pval = TRUE,\n",
    "  conf.int = TRUE,\n",
    "  risk.table = TRUE,\n",
    "  risk.table.title = 'Number of forest plots not recovered',\n",
    "  xlab = 'Simulation Year',\n",
    "  ylab = 'Proportion not yet recovered',\n",
    "  title = '", taxon, " Recovery - Climate ", formatted_climate, " (Kaplan–Meier survival curve)',\n", # Modified here
    "  legend.title = 'Forest Type',\n",
    "  legend.labs = c('Native Broadleaves', 'Non-Native Coniferous'),\n",
    "  palette = management_colors,\n",
    "  ggtheme = theme_minimal()\n",
    "))"
  )))
  
  # Plot 2: Proportion recovered (event curve)
  cat("\n#───────────────────────────────────────────────────────────────────────────────")
  cat("\n#######           PLOT PROPORTION RECOVERED FOR", taxon, "-", climate, "##########\n\n")
  
  eval(parse(text = paste0(
    "print(ggsurvplot(", fit_name, ",\n",
    "  data = ", df_name, ",\n",
    "  fun = 'event',  # proportion recovered\n",
    "  pval = TRUE,\n",
    "  pval.coord = c(10, 0.98),\n",
    "  conf.int = TRUE,\n",
    "  risk.table = TRUE,\n",
    "  risk.table.title = 'Number of forest plots not recovered',\n",
    "  xlab = 'Simulation Year',\n",
    "  ylab = 'Proportion recovered',\n",
    "  title = '", taxon, " Recovery - Climate ", formatted_climate, " (Kaplan–Meier survival curve)',\n", # Modified here
    "  legend.title = 'Forest Type',\n",
    "  legend.labs = c('Native Broadleaves', 'Non-Native Coniferous'),\n",
    "  palette = management_colors,\n",
    "  ggtheme = theme_minimal()\n",
    "))"
  )))
}

# dev.off() 


################################################################################
################################################################################
################################################################################

# Taxa and climate combinations
taxa_climate <- list(
  c("BRYOPHYTES", "CLIM_1961_1990"),
  c("BRYOPHYTES", "CLIM_1991_2020"),
  c("LICHENS", "CLIM_1961_1990"),
  c("LICHENS", "CLIM_1991_2020"),
  c("MACROFUNGI", "CLIM_1961_1990"),
  c("MACROFUNGI", "CLIM_1991_2020"),
  c("BEETLES", "CLIM_1961_1990"),
  c("BEETLES", "CLIM_1991_2020"),
  c("MOTHS", "CLIM_1961_1990"),
  c("MOTHS", "CLIM_1991_2020")
)

# Loop through each combination
for (combo in taxa_climate) {
  taxon <- combo[1]
  climate <- combo[2]
  colname <- paste0(taxon, "_", climate)
  var_name <- tolower(paste0(taxon, "_", climate))
  df_name <- paste0("df_", var_name)
  
  cat("\n#───────────────────────────────────────────────────────────────────────────────")
  cat("\n#######  PREPARE THE DATA FOR", taxon, "-", climate, "##########\n\n")
  
  eval(parse(text = paste0(
    df_name, " <- Recovery_wide_df %>%\n",
    "  select(plotID, forest_cat, ", colname, ") %>%\n",
    "  mutate(\n",
    "    recovered = ifelse(is.na(", colname, "), 0, 1),\n",
    "    time = ifelse(is.na(", colname, "), 266, ", colname, ")\n",
    "  )"
  )))
  
  # Fit Cox model
  model_name <- paste0("cox_", tolower(taxon), "_", tolower(climate)) # Make model_name unique
  fit_name <- paste0("fit_", tolower(taxon), "_", tolower(climate)) # Make fit_name unique
  
  eval(parse(text = paste0(
    model_name, " <- coxph(Surv(time, recovered) ~ forest_cat, data = ", df_name, ")\n",
    "summary(", model_name, ")\n"
  )))
  
  eval(parse(text = paste0(
    fit_name, " <- survfit(Surv(time, recovered) ~ forest_cat, data = ", df_name, ")"
  )))
  
  # Format the climate string for the title
  formatted_climate <- gsub("CLIM_", "", climate) # Remove "CLIM_"
  formatted_climate <- gsub("_", " - ", formatted_climate) # Replace "_" with " - "
  
  
  # Plot 2: Proportion recovered (event curve)
  cat("\n#───────────────────────────────────────────────────────────────────────────────")
  cat("\n#######           PLOT PROPORTION RECOVERED FOR", taxon, "-", climate, "##########\n\n")
  
  eval(parse(text = paste0(
    "print(ggsurvplot(", fit_name, ",\n",
    "  data = ", df_name, ",\n",
    "  fun = 'event',  # proportion recovered\n",
    "  pval = TRUE,\n",
    "  pval.coord = c(10, 0.98),\n",
    "  conf.int = TRUE,\n",
    "  risk.table = TRUE,\n",
    "  risk.table.title = 'Number of forest plots at risk',\n",
    "  xlab = 'Simulation Year',\n",
    "  ylab = 'Proportion recovered',\n",
    "  title = '", taxon, " Recovery - Climate ", formatted_climate, " (Kaplan–Meier survival curve)',\n", # Modified here
    "  legend.title = 'Forest Type',\n",
    "  legend.labs = c('Native Broadleaves', 'Non-Native Coniferous'),\n",
    "  palette = management_colors,\n",
    "  ggtheme = theme_minimal()\n",
    "))"
  )))
}


#############################
  FINAL GRAPH version
#############################

# Unique taxa
taxa <- c("BRYOPHYTES", "LICHENS", "MACROFUNGI", "BEETLES", "MOTHS")

for (taxon in taxa) {
  plots <- list()
  
  for (climate in c("CLIM_1961_1990", "CLIM_1991_2020")) {
    colname <- paste0(taxon, "_", climate)
    var_name <- tolower(paste0(taxon, "_", climate))
    df_name <- paste0("df_", var_name)
    fit_name <- paste0("fit_", var_name)
    
    # Prepare data
    temp_df <- Recovery_wide_df %>%
      select(plotID, forest_cat, !!sym(colname)) %>%
      mutate(
        recovered = ifelse(is.na(!!sym(colname)), 0, 1),
        time = ifelse(is.na(!!sym(colname)), 266, !!sym(colname))
      )
    
    # Fit KM model
    fit <- survfit(Surv(time, recovered) ~ forest_cat, data = temp_df)
    
    # Format climate title
    formatted_climate <- gsub("CLIM_", "", climate)
    
    # Plot and save to list
    plots[[climate]] <- ggsurvplot(
      fit,
      data = temp_df,
      fun = "event",  # Proportion recovered
      pval = TRUE,
      pval.coord = c(10, 0.98),
      conf.int = TRUE,
      risk.table = TRUE,
      risk.table.title = "Number of forest plots at risk",
      xlab = "Simulation Year",
      ylab = "Proportion recovered",
      title = paste0(taxon, " Recovery - Climate ", formatted_climate),
      legend.title = "Forest Type",
      legend.labs = c("Native Broadleaves", "Non-Native Coniferous"),
      palette = management_colors,
      ggtheme = theme_minimal()
    )
  }
  
  # Arrange 1x2 panel
  combined_plot <- ggarrange(
    plots[["CLIM_1961_1990"]]$plot,
    plots[["CLIM_1991_2020"]]$plot,
    ncol = 2,
    nrow = 1,
    common.legend = TRUE,
    legend = "bottom"
  )
  
  # Print the combined plot
  print(combined_plot)
}

################### dev.off()