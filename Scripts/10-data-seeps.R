df_seeps = df_metadata |> 
  filter(Site %in% c("Seep", "Groundwater"),
         Tide %in% c("Low tide, Daytime",
                     "High tide, Daytime"),
         !`Collected for` == "Calibration")

df_gw = df_seeps |> 
  filter(Site == "Groundwater") |> 
  mutate(grp = 1+ (row_number()-1) %/% 2) %>% 
  group_by(grp) %>% 
  summarise(across(everything(), mean, na.rm = TRUE)) %>% 
  select(-grp) |> 
  mutate(Site = factor("Groundwater"),
         Tide = rep(c("Low tide, Daytime",
                      "High tide, Daytime"), 2),
         `Collected for` = "Groundwater",
         across(c(Tide, CTD, `Collected for`, `Nitrite (μM)`), as.character))

df_seeps2 = df_seeps |> 
  filter(!Site == "Groundwater") |>
  bind_rows(df_gw)

         
df_seeps_long = df_seeps2 |> 
  pivot_longer(12:22 & where(is.numeric))

ggplot(df_seeps_long, aes(DateTime, value)) +
  geom_point(aes(shape = Tide, fill = Site), size = 3) +
  geom_jitter(alpha = 0.3) +
  scale_shape_manual(values = c(22, 23)) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~ name, scales = "free") +
  guides(fill = guide_legend(override.aes = list(shape = 21)))

