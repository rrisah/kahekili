df_tempsal <- read_excel(str_glue("{path_sensor}/tempsal.xlsx")) |> 
  mutate(DateTime = make_datetime(year(Date), month(Date), day(Date), hour(Time), minute(Time),
              tz = "Pacific/Honolulu"), .keep = "unused") |> 
  mutate(Site = factor(Site, levels = c("Vent", "Control"),
                       labels = c("Seep", "Control")))

df_tempsal_long <- df_tempsal |> 
  pivot_longer(c(Depth, Temperature, Salinity), names_to = "variable") |> 
  mutate(variable = as.factor(variable))
  

plot_tempsal <- ggplot(df_tempsal_long) +
  geom_point(aes(DateTime, value, shape = Site, fill = Instrument), 
             size = 3) +
  scale_shape_manual(values = c(22, 23)) +
  labs(x = NULL, y = NULL) +
  facet_grid(variable ~ ., scales = "free_y", switch = "y") +
  guides(fill = guide_legend(override.aes = list(shape = 21)))

df_tempsal_offset <- df_tempsal |> 
  mutate(DateTime = floor_date(DateTime, "30 minutes") + minutes(15)) |> 
  pivot_wider(names_from = Instrument, values_from = c(Depth, Temperature, Salinity)) |> 
  mutate("Depth offset" = Depth_CastAway - Depth_MicroCAT,
         "Temperature offset" = Temperature_CastAway - Temperature_MicroCAT,
         "Salinity offset" = Salinity_CastAway - Salinity_MicroCAT, .keep = "unused") 



df_tempsal_summary <- df_tempsal_offset |> 
  summarise(across(where(is.numeric), list(
    \(x) str_glue(round(mean(x), 2), " ± ", round(sd(x), 2), 
                  " (", round(min(x), 2), " - ", round(max(x), 2), ")")
  ), .names = word("{.col}", 1)))
  
  