# Fit ANOVA
anova_model <- aov(`NOx (μM)` ~ Site, data = df_spatial)

# Define polynomial contrasts (linear, quadratic, etc.)
contrasts(df_spatial$Site) <- contr.poly(5)  

# Refit with contrasts
anova_model_poly <- aov(y ~ group, data = df)
summary(anova_model_poly, split = list(group = list("Linear" = 1,
                                                    "Quadratic" = 2,
                                                    "Cubic" = 3,
                                                    "Quartic" = 4)))