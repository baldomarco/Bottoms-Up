total_AG_DW_C_sim_2 <- plot_variables_all%>%
  group_by(run)%>%
  filter(year == c(120,240,360))%>%
  summarise(mean(total_AG_DW_C_sim))
