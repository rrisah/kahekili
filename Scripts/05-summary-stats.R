# Summary

df_summary <- df_sensors |>
  select(Temperature, Salinity, SAMIpH, `Dissolved Oxygen`, Site) |> 
  group_by(Site) |>
  summarise(across(where(is.numeric), list(
    \(x) str_glue(round(mean(x), 2), " ± ", round(sd(x), 2), 
                            " (", round(min(x), 2), " - ", round(max(x), 2), ")")
  ), .names = "{.col}"))

# print(df_summary)

write_csv(df_summary, str_glue("{path_outputs}/summary.csv"))
