# Read sami data
df_sami <- read_tsv(str_glue("{path_sensor}/SAMI/SAMI_P0326_160725_Control_QC_Results/SAMI_P0326_160725_Control_out.txt"),
                    skip = 8)  |> 
  mutate(DateTime = mdy_hms(DateTime)) |> 
  mutate(DateTime = with_tz(DateTime, tzone = "Pacific/Honolulu")) |> 
  select(DateTime, SAMIpH, Salinity)

df_samitemp <- read_tsv(str_glue("{path_sensor}/SAMI/SAMI Data Export SAMI_P0326_160725_Control.txt")) |> 
  mutate(DateTime = ISOdatetime(2025, 1, 1, 0, 0, 0, tz = "UTC") + 86400*(`Year Day` - 1)) |> 
  mutate(DateTime = with_tz(DateTime, tzone = "Pacific/Honolulu")) |> 
  mutate(DateTime = round_date(DateTime, "minute") + seconds(1)) |> 
  rename("Temp_SAMI" = `Temperature C`) |> 
  select(DateTime, Temp_SAMI) |> 
  mutate(Site = "Control")

df_microcat_sami <- df_microcat |> 
  left_join(df_sami, by = c("DateTime", "Salinity")) |> 
  left_join(df_samitemp, by = c("DateTime", "Site"))

df_samiCTD_old <- df_samiCTD |> 
  mutate(Temperature = df_samitemp$Temp_SAMI)

write_tsv(df_samiCTD_old, str_glue("{path_sensor}/SAMI/samiCTD_old.tsv"), col_names = F)

df_sami_samitemp <- read_tsv(str_glue("{path_sensor}/SAMI/SAMI_P0326_160725_Control_QC_Results_SAMITemp/SAMI_P0326_160725_Control_out.txt"),
                             skip = 8)  |> 
  mutate(DateTime = mdy_hms(DateTime)) |> 
  mutate(DateTime = with_tz(DateTime, tzone = "Pacific/Honolulu")) |> 
  select(DateTime, SAMIpH, Salinity)

df_samipH_compare <- df_sami |> 
  left_join(df_sami_samitemp, by = c("DateTime", "Salinity")) |> 
  rename("SAMIpH_MicroCATTemp" = SAMIpH.x,
         "SAMIpH_SAMITemp" = SAMIpH.y) |> 
  mutate(pHoffset = SAMIpH_SAMITemp - SAMIpH_MicroCATTemp) |> 
  summarise(pHoffset_mean = mean(pHoffset), pHoffset_sd = sd(pHoffset))
    \(x) str_glue(round(mean(x), 2), " ± ", round(sd(x), 2), 
                  " (", round(min(x), 2), " - ", round(max(x), 2), ")"))
  )
  
  


