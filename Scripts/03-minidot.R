# read miniDOTs
df_minidot_control <- read_csv(str_glue("{path_sensor}/MiniDOT/7450-801393_ControlminiDot/Cat.txt"),
                               skip = 8) |> 
  mutate(Site = "Control") 
df_minidot_vent <- read_csv(str_glue("{path_sensor}/MiniDOT/7450-866331_VentminiDot/Cat.txt"),
                            skip = 8) |> 
  mutate(Site = "Vent")

colnames_minidot <- read_csv(str_glue("{path_sensor}/MiniDOT/7450-801393_ControlminiDot/Cat.txt"),
                             skip = 7, n_max = 1) |> 
  colnames() |> 
  append("Site")

df_minidot <- bind_rows(df_minidot_control, df_minidot_vent) |> 
  rename_with(~colnames_minidot) |> 
  rename(DateTime = `Hawaii Standard Time`) |> 
  mutate(DateTime = force_tz(DateTime, tzone = "Pacific/Honolulu")) |> 
  mutate(DateTime = case_when(Site == "Control" ~ DateTime - minutes(1) + seconds(1),
                              Site == "Vent" ~ DateTime + minutes(5) + seconds(1))) |> 
  rename("Temp_miniDOT" = Temperature) |> 
  select(DateTime, "Dissolved Oxygen", "Dissolved Oxygen Saturation", Temp_miniDOT, "Site") |> 
  filter(
    case_when(Site == "Control" ~ DateTime > "2025-07-13 09:00:00" & DateTime < "2025-07-16 08:30:02",
              Site == "Vent" ~ DateTime > "2025-07-13 10:30:00" & DateTime < "2025-07-16 07:15:02"
  ))

df_sensors <- df_microcat_sami |> 
  left_join(df_minidot, by = c("DateTime", "Site"))
