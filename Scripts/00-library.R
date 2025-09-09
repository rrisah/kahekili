# library(readxl)
# library(sf)
# library(rstatix)
library(broom)
library(lmodel2)
library(hrbrthemes)
library(climaemet)
library(hms)
library(ggpmisc)
library(tidyverse)
library(readxl)
# Set ggplot theme
theme_set(theme_ipsum_rc() +
            theme(strip.text.y = element_text(hjust = 0.5),
                  axis.title.x = element_text(hjust = 0.5, size = 16),
                  axis.title.y = element_text(hjust = 0.5, size = 16),
                  strip.placement = "outside",
                  legend.position = "bottom",
                  # panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()
                  )
)
# Set path sensor
path_main <- dirname(getwd())
path_sensor <- str_glue("{path_main}/2025 July/Sensor Data")
# Set path for plots
path_plots <- str_glue("{path_main}/2025 July/Plots")
path_outputs <- str_glue("{path_main}/2025 July/Outputs")
path_bottle <- str_glue("{path_main}/2025 July/Bottle data")
# Function for saving plot
saveplot = \(x, name, h, w) {
  ggsave(str_glue("{path_plots}/plot-{format(Sys.time(), '%Y%m%d-%H%M%S')}-{name}.png"),
         plot = x,
         # device = cairo_pdf,
         height = h,
         width = w,
         units = "in",
         dpi = 300
  )
}

p = \() {
  plot(last_plot())
}
  