# read microcats
path_microcat <-  str_glue("{path_sensor}/MicroCAT/microcat.xlsx")
tab_names <- excel_sheets(path_microcat)

list_microcat <- map(tab_names,\(x) read_excel(path_microcat, x, col_names = F) |> 
                       mutate(Site = x))

df_microcat <- bind_rows(list_microcat) |> 
  rename_with(~c("Conductivity", "Density", "Pressure", "Depth", "Salinity", "Temperature", 
                           "ElapsedSeconds", "JulianDays", "Flag", "Site")) |> 
  mutate(DateTime = ISOdatetime(2025, 1, 1, 0, 0, 0, tz = "UTC") + 86400*(JulianDays - 1)) |> 
  mutate(DateTime = with_tz(DateTime, tzone = "Pacific/Honolulu")) |>
  mutate(DateTime = round_date(DateTime)) |>
  filter(DateTime > ymd_hms("2025-07-13 00:00:00", tz = "Pacific/Honolulu"),
         Depth > 0) |> 
  select(1:6, DateTime, Site)

df_samiCTD <- df_microcat |> 
  mutate(DateTime = with_tz(DateTime, tzone = "UTC")) |>
  filter(Site == "Control") |> 
  mutate(Date = date(DateTime),
         Time = as_hms(DateTime)) |> 
  mutate(Date = format(Date, "%m/%d/%y")) |> 
  select(Date, Time, Salinity, Temperature) 

write_tsv(df_samiCTD, str_glue("{path_sensor}/SAMI/samiCTD.tsv"), col_names = F)


      