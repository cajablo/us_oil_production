# libraries ----
if (!require("pacman")) install.packages("pacman")
library(pacman)   # pacman::p_load() installs package if not present vs just library()
p_load(tidyverse) # ggplot2,tibble,tidyr,readr,purrr,dplyr,stringr,forcats
p_load(glue, here, fs)                              # text, paths
p_load(rgdal, ncdf4, raster, leaflet, mapview, rmapshaper)      # spatial
p_load(sf, rmapshaper, lwgeom)                      # vector
p_load(fasterize) # raster to vector; p_load_gh("ecohealthalliance/fasterize")
p_load(knitr, rmarkdown, htmltools, DT, htmltools)  # reporting
p_load(RColorBrewer, viridis)                                # graphics
p_load(gganimate)
p_load(gifski)
p_load(geom_map())
p_load(tools)
#p_update()
select <- dplyr::select # vs raster::select
mutate <- dplyr::mutate

#write function to capitalize first letter of each word in a string (from toupper() help file)
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}