plot_tides <- map2(variables, select(df_spatial, any_of(variables)), ~ {
  df_mean <- df_spatial |> 
    pivot_longer(.x, names_to = "variable") |> 
    group_by(Site, Tide, variable) |> 
    summarise(mean = mean(value, na.rm = T),
              min = min(value, na.rm = T),
              max = max(value, na.rm = T))
  
  ggplot(df_mean, aes(Site, mean, shape = Tide, fill = Tide)) +
    geom_pointrange(aes(ymin = min, ymax = max)) +
    scale_shape_manual(values = 21:23) +
    # geom_jitter(alpha = 0.3) +
    scale_y_continuous(
      trans='log10')
})

plot_tides <- map2(plot_tides, 
                  variable_labels, ~ {
                    .x + labs(y = .y, x = NULL)
                  })
p()
