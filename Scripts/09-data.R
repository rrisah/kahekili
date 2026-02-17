df_metadata <- read_csv(str_glue("{path_bottle}/Notes - Metadata.csv"),
                        col_types = cols(Date = col_date("%m/%d/%Y"))) |> 
  mutate(across(c(`NOx (μM)`, `Silicate (μM)`), as.numeric),
         Site = factor(Site, levels = c("Vent", "Vent_gw", "S1", "S2", "S3", "S4", "Control"),
                       labels = c("Seep", "Groundwater", "S1", "S2", "S3", "S4", "Control")),
         DateTime = mdy_hms(paste(Date, Time)),
         Tide = str_glue("{`Tide phase`} tide, {`Day phase`}"),
         .keep = "unused") |> 
  relocate(DateTime, Tide)
  # filter(!Site %in% c("S1", "S4"))

df_spatial <- df_metadata |> 
  filter(!`Collected for` == "Calibration") |> 
  filter(!Site == "Groundwater")

variables <- df_spatial |> 
  select(11:22 & where(is.numeric)) |> 
  colnames()

variable_names <- word(variables, 1)

variable_labels <- c("Temperature (\U000B0 C)", "Salinity", 
                     "Nitrate + nitrite (μM)", "Phosphate (\U003BCM)", "Silicate (\U003BCM)", "Ammonium (\U003BCM)",
                     expression(paste("TA (μmol kg"^"-1", ")")),
                     expression(paste("DIC (μmol kg"^"-1", ")")),
                     expression("pH"~italic("in situ")), expression(paste(italic(p),"CO"[2]," (μatm)")),
                     expression("\U003A9"["Aragonite"]) 
                    )

boxplots <- imap(select(df_spatial, any_of(variables)), ~ {
  ggplot(df_spatial, aes(Site, .x)) +
    geom_boxplot() +
      # outlier.shape = NA) +
    geom_jitter(alpha = 0.3) +
    scale_y_continuous(
      trans='log10') 
})

boxplots2 <- map2(boxplots, 
                  variables, ~ {
                    df_mean <- df_spatial |> 
                      pivot_longer(.y, names_to = "variable") |> 
                      group_by(Site, variable) |> 
                      summarise(mean = mean(value, na.rm = T))
                    
                    .x +
                      geom_point(data = df_mean |> filter(variable == .y), 
                                 aes(Site, mean), color = "orangered3", size = 3) 
                  })

boxplots3 <- map2(boxplots2, 
                  variable_labels, ~ {
                    .x + labs(y = .y, x = NULL)
                  })

map2(boxplots3, variable_names, ~saveplot(.x, .y, 5, 5))

df_summary_bottle <- df_spatial |>
  select(Site, "Temperature (Celsius)", "Salinity (Practical Salinity Scale)", "NOx (μM)" , "Nitrite (μM)", 
         "Phosphate (μM)", "Silicate (μM)", "Ammonia (μM)", "TA (μmol kg-1)", "DIC (μmol kg-1)", "pH in situ", 
         "pCO2 in situ", "Aragonite") |>  
  group_by(Site) |>
  summarise(across(where(is.numeric), list(
    \(x) str_glue(round(mean(x, na.rm = T), 2), " ± ", round(sd(x, na.rm = T), 2), 
                  " (", round(min(x, na.rm = T), 2), " - ", round(max(x, na.rm = T), 2), ")")
  ), .names = "{.col}"))

write_csv(df_summary_bottle, str_glue("{path_outputs}/summary_bottle.csv"))
