df_sensors_clim <- df_sensors_long |>
  mutate(Hour = hour(DateTime)) |>
  group_by(Site, variable, Hour) |>
  summarise(
    mean = MeanCI(value, conf.level = 0.95, na.rm = TRUE)[1],
    lower = MeanCI(value, conf.level = 0.95, na.rm = TRUE)[2],
    upper = MeanCI(value, conf.level = 0.95, na.rm = TRUE)[3]
  )

df_sensors_clim |> 
  ggplot() +
  geom_ribbon(aes(x=Hour,ymin=lower, ymax=upper, group=Site,fill=Site), alpha=0.3)+
  geom_line(aes(x=Hour, y=mean, group=Site,color=Site),size=1, alpha=0.8) +
  facet_wrap( ~ variable, ncol = 1, scales = "free_y", strip.position = "left",
              labeller = facet_labels) +
  labs(x = "Hour of the Day", y = NULL) +
  scale_color_manual(values = color_seep_control) +
  scale_fill_manual(values = color_seep_control) +
  scale_x_continuous(expand = c(0, 0), breaks= c(0,3, 6, 9, 12, 15, 18, 21, 23)) 

saveplot(last_plot(), "sensors-hourly", 10, 7)   
