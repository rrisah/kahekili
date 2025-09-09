list_tcm_temp <- list.files(str_glue("{path_sensor}/TCM"), pattern = "(Temperature)+(.csv)", full.names = T)
df_tcm_temp <-  map2(list_tcm_temp, c("S3", "Vent", "Control"),
                \(x, y) read_csv(x) |> 
                  mutate(Site = y)
) |> 
  bind_rows() |> 
  rename(DateTime = "ISO 8601 Time") |> 
  mutate(DateTime = force_tz(DateTime, tzone = "Pacific/Honolulu")) |> 
  filter(
    case_when(Site == "S3" ~ DateTime > "2025-07-13 19:29:01" & DateTime < "2025-07-16 07:05:02",
              Site == "Vent" ~ DateTime > "2025-07-13 10:29:00" & DateTime < "2025-07-16 07:15:02",
              Site == "Control" ~ DateTime > "2025-07-13 08:59:00" & DateTime < "2025-07-16 08:30:02"
              
    )) |> 
  # mutate(DateTime = floor_date(DateTime, "1 minutes") + seconds(1))  |> 
  mutate(DateTime = round_date(DateTime, "2 minutes") + seconds(1)) |> 
  group_by(Site, DateTime) |> 
  summarise(Temp_TCM = mean(`Temperature (C)`)) |> 
  filter(minute(DateTime) %in% c(0, 14, 30, 44)) |> 
  mutate(DateTime = case_when(
    minute(DateTime) %in% c(14, 44) ~ DateTime + minutes(1),
    .default = DateTime
  )) |> 
  filter(Site %in% c('Control', 'Vent'))

df_temp <- df_sensors |> 
  left_join(df_tcm_temp, by = c("DateTime", "Site")) |> 
  select(Site, DateTime, contains("Temp")) |> 
  rename(Temp_MicroCAT = Temperature)

df_offset <- df_temp |> 
  mutate(across(4:6, ~. - Temp_MicroCAT, .names = "{.col}_offset"), .keep = "unused")

df_offset_summary <- df_offset |> 
  group_by(Site) |>
  summarise(across(where(is.numeric), 
    \(x) str_glue(round(mean(x), 2), " ± ", round(sd(x), 2), 
                  " (", round(min(x), 2), " - ", round(max(x), 2), ")"))) |> 
  mutate("SAMI offset" = Temp_SAMI_offset,
        "miniDOT offset" = Temp_miniDOT_offset,
        "TCM offset" = Temp_TCM_offset,
        .keep = "unused")

df_temp_long <- df_temp |> 
  gather("Instrument", "value", -c(Site, DateTime))

df_offset_long <- df_offset |>
  # select(-starts_with("Temp")) |> 
  gather("Instrument", "value", -c(Site, DateTime))

plot_temp <- ggplot(df_temp_long) +
  geom_line(aes(DateTime, value, linetype = Instrument, color = Instrument), linewidth = 1) +
  # geom_point(aes(DateTime, value, color = Instrument)) +
  scale_color_brewer(palette = "Set1") +
  labs(x = NULL, y = "Temperature") +
  guides(fill = 'none') +
  facet_wrap(Site ~ ., ncol = 1, strip.position = "left") +
  scale_x_datetime(
    date_breaks = "6 hours",
    date_labels = "%b %d\n%H:%M",
    minor_breaks = "3 hours",
    limits = as.POSIXct(c(
      "2025-07-13 07:00:00",
      "2025-07-16 11:00:01"
    ))) +
  theme(axis.text.x = element_text(angle = 90)) 
  
plot_temp

plot_offset <- ggplot(df_offset_long) +
  geom_line(aes(DateTime, value, linetype = Instrument, color = Instrument), linewidth = 1) +
  # geom_point(aes(DateTime, value, color = Instrument)) +
  scale_color_brewer(palette = "Set1") +
  labs(x = NULL, y = "Temperature offset (sensor temp - microcat temp)") +
  guides(fill = 'none') +
  facet_grid(Site ~ ., scales = "free", switch = "y") +
  scale_x_datetime(
    date_breaks = "6 hours",
    date_labels = "%b %d\n%H:%M",
    minor_breaks = "3 hours",
    limits = as.POSIXct(c(
      "2025-07-13 07:00:00",
      "2025-07-16 11:00:01"
    ))) +
  theme(axis.text.x = element_text(angle = 90)) 
plot_offset

density_offset <- ggplot(df_offset_long, aes(x = value, group = Instrument, fill = Instrument)) +
  geom_density(adjust=1.5, alpha=.4) +
  scale_color_brewer(palette = "Set1") +
  facet_grid(Site ~ ., scales = "free", switch = "y") 

density_offset

