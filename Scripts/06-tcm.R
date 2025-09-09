
list_tcm <- list.files(str_glue("{path_sensor}/TCM"), pattern = "(Current)+(.csv)", full.names = T)
df_tcm <-  map2(list_tcm, c("S3", "Vent", "Control"),
                \(x, y) read_csv(x) |> 
                  mutate(Site = y)
) |> 
  bind_rows() |> 
  rename(DateTime = "ISO 8601 Time") |> 
  mutate(DateTime = force_tz(DateTime, tzone = "Pacific/Honolulu")) |> 
  filter(
    case_when(Site == "S3" ~ DateTime > "2025-07-13 19:30:01" & DateTime < "2025-07-16 07:05:02",
              Site == "Vent" ~ DateTime > "2025-07-13 10:30:00" & DateTime < "2025-07-16 07:15:02",
              Site == "Control" ~ DateTime > "2025-07-13 09:00:00" & DateTime < "2025-07-16 08:30:02"
    )) 

rose_tcm <- ggwindrose(
  speed = df_tcm$`Speed (cm/s)`,
  direction = df_tcm$`Heading (degrees)`,
  speed_cuts = seq(0, 30, 10),
  legend_title = "Current speed (cm/s)",
  calm_wind = 0,
  n_col = 3,
  stack_reverse = T,
  facet = df_tcm$Site) +
  theme(legend.position = "bottom")
rose_tcm

df_tcm_binned <- df_tcm |> 
  mutate(DateTime = floor_date(DateTime, "15 minutes")) |> 
  group_by(DateTime, Site) |> 
  summarise(across(where(is.numeric), mean))

df_tcm_binned$`Heading (degrees)`[df_tcm_binned$`Heading (degrees)` == 0] <- 360
df_tcm_binned$u <- (1 * df_tcm_binned$`Speed (cm/s)`) * sin((df_tcm_binned$`Heading (degrees)` * pi / 180.0))
df_tcm_binned$v <- (1 * df_tcm_binned$`Speed (cm/s)`) * cos((df_tcm_binned$`Heading (degrees)` * pi / 180.0))

wind_scale <- 1
y_axis <- seq(-5, 5, 5)

ggplot(df_tcm_binned, aes(x=DateTime, y=y_axis)) + 
         geom_segment(aes(x = DateTime, xend = DateTime + `Velocity-E (cm/s)`*3000, 
                          y = 0, yend = `Velocity-N (cm/s)`*wind_scale), 
                      arrow = arrow(length = unit(0.15, 'cm')), linewidth = 0.5, alpha = 0.7) +
  geom_point(aes(x = DateTime, y = 0), alpha = 0.5, size = 1) +
  scale_x_datetime(
    date_breaks = "3 hours",
    date_labels = "%b %d\n%H:%M",
    minor_breaks = "3 hours") +
  scale_y_continuous(breaks = y_axis, labels = as.character(abs(y_axis)/wind_scale)) +
  facet_grid(Site ~ ., switch = "y")
  # coord_equal(ylim = c(min(y_axis/wind_scale), max(y_axis/wind_scale)))
