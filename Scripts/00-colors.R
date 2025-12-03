library(RColorBrewer)
library(scales)

cols <- colorRampPalette(brewer.pal(11, "Spectral"), alpha=TRUE)(25)
# dry and wet
color_seep_control <- c(cols[2], cols[22])
# four habitats
cols_habitat <- c(cols[20], cols[24], cols[3], cols[11])
# three reef stations
cols_reef <- c(cols[4], cols[8], cols[20])

# get the greys (stolen from https://github.com/zonination/perceptions/blob/master/percept.R)
palette <- brewer.pal("Greys", n=9)
color.background = palette[2]
color.grid.major = palette[5]
# show_col(palette)

cols_season_initial <- c("#E85252", "#206DBA")
# show_col(cols_season)
# scale_color_manual(values = c("#811111", "#15497D"))
