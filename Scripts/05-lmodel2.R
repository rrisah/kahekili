data(mod2ex1)
Ex1.res <- lmodel2(Predicted_by_model ~ Survival, data=mod2ex1, nperm=99)
Ex1.res
plot(Ex1.res)

lmodel_DOpH <- df_sensors |> 
  select("SAMIpH", "Dissolved Oxygen") |> 
  drop_na() |> 
  nest() |> 
  mutate(model_regression = map(data, ~(lmodel2(SAMIpH ~`Dissolved Oxygen`, data = ., 
                                                "relative", "relative", 99))),
         model_coeff = map(model_regression, ~tidy(.x) %>% 
                             filter(method == "RMA") %>% 
                             select(term, estimate) %>% 
                             pivot_wider(names_from = term, values_from = estimate)),
         model_info = map(model_regression, glance), .keep = "unused") |> 
  select(model_coeff, model_info) %>% 
  unnest(cols = everything())

  # do(tidy(lmodel2(SAMIpH ~`Dissolved Oxygen`, data = ., "relative", "relative", 99)))
