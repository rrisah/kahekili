# Sensor variables
variable_sensors = c("Depth", "Temperature", "Salinity", "SAMIpH", "Dissolved Oxygen")
# Plot sensor data
df_sensors_long <- df_sensors |> 
  pivot_longer(c("Depth", "Temperature", "Salinity", "SAMIpH", "Dissolved Oxygen"), names_to ='variable') |> 
  mutate(variable = factor(variable, levels = variable_sensors))
# Facet labels
facet_labels = as_labeller(c(Depth = "Depth~(m)", Temperature = "Temperature~(degree*C)", Salinity = "Salinity", 
                             SAMIpH = "pH", `Dissolved Oxygen` = "DO~(mg~L^-1)"),
                           default = label_parsed)
# Create tide data frame
df_tide <- read_tsv(str_glue("{path_sensor}/Lahaina, Maui Island, Hawaii.txt"),
                    skip = 18) |> 
  mutate(DateTime = make_datetime(year(Date), month(Date), day(Date), hour(Day), minute(Day),
                                  tz = "Pacific/Honolulu"), 
         variable = "Depth", .keep = "unused") |> 
  rename(value = Time,
         Tide = Pred) |> 
  select(DateTime, value, Tide, variable) |> 
  mutate(variable = factor(variable, levels = variable_sensors),
         value_string = case_when(
           value > 0 ~ str_glue("+{value}"),
           .default = as.character(value)
         ),
         label = str_glue("{Tide} ({value_string})")) 
# Shaded day/night
rect_sensor <- data.frame(DateTime = as.POSIXct(c("2025-07-13 07:00:00", "2025-07-13 19:16:00",
                                                  "2025-07-14 05:58:00", "2025-07-14 19:16:00",
                                                  "2025-07-15 05:58:00", "2025-07-15 19:16:00",
                                                  "2025-07-16 05:58:00")),
                          DateTimeEnd = as.POSIXct(c("2025-07-13 19:16:00", "2025-07-14 05:58:00", 
                                                     "2025-07-14 19:16:00", "2025-07-15 05:58:00", 
                                                     "2025-07-15 19:16:00", "2025-07-16 05:58:00", 
                                                     "2025-07-16 11:00:00")),
                          Sun = rep(c("day", "night"), length.out = 7))
# Plot
plot_sensors <- ggplot(df_sensors_long) +
  geom_rect(data = rect_sensor, 
            aes(xmin = DateTime, xmax = DateTimeEnd, 
                ymin = -Inf, ymax = Inf, , fill = Sun), alpha = 0.2) +
  scale_fill_manual(values = c("lightgoldenrod1", "dodgerblue4")) +
  geom_point(aes(DateTime, value, color = Site, shape = Site)) +
  geom_segment(data = df_tide, aes(x = DateTime, y = 1.25, xend = DateTime, yend = value + 1.4),
               linewidth = 0.3,
               linetype = 2) +
  geom_point(data = df_tide, aes(DateTime, value + 1.4), shape = 18, size = 3) +
  geom_label(data = df_tide, aes(DateTime, value + 1.7, label = label), size = 2.5) +
  scale_color_manual(values = color_seep_control) +
  labs(x = NULL, y = NULL) +
  guides(fill = 'none') +
  facet_wrap( ~ variable, ncol = 1, scales = "free_y", strip.position = "left",
              labeller = facet_labels) +
  scale_x_datetime(
    date_breaks = "6 hours",
    date_labels = "%b %d\n%H:%M",
    minor_breaks = "3 hours",
    limits = as.POSIXct(c(
      "2025-07-13 07:00:00",
      "2025-07-16 11:00:01"
    ))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))

saveplot(last_plot(), "sensors", 10, 7)   
plot(plot_sensors)

DOvspH <- df_sensors |> 
  select("SAMIpH", "Dissolved Oxygen") |> 
  drop_na() |> 
  ggplot(aes(`Dissolved Oxygen`, SAMIpH)) +
  geom_point() +
  stat_poly_eq(use_label(c("eq", "R2", "p", "n")), method = "lm") 
  
# plot(DOvspH)

SalvsTide <- df_sensors |> 
  select("Depth", "Salinity", "Site") |>
  # pivot_longer(c("Site", "Salinity"), names_to = 'variable')
  # drop_na() |> 
  ggplot(aes(Depth, Salinity, fill = Site)) +
  geom_point(pch = 21) +
  scale_fill_manual(values = color_seep_control) +
  facet_grid(~ Site) +
  stat_poly_eq(use_label(c("eq", "R2", "p", "n")), method = "lm",
               label.y = 0.02)
