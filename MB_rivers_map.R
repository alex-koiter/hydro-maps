
# /*======================*/
#' ### Hydrological features map of Brandon MB and surounding area
#' May 31, 2018
#' Dr. Alex Koiter
# /*======================*/

#devtools::install_github("tidyverse/ggplot2") # ggplot2 version 2.2.1.9 required to use geom_sf()

library(tidyverse)
library(sf)
library(ggsn)
library(maps)
library(grid) 
library(png)

# Maps created using CanVec data (Canada Centre for Mapping and Earth Observation (NRCan, ESS)) which can be found at http://maps.canada.ca/czs/index-en.html

lakes <- st_read("~/Dropbox/Map poster/FME_5544441C_1527277928696_1684/ESRISHAPE_1/canvec_180525_12507/waterbody_2.shp", quiet = TRUE) %>%
  st_transform("+proj=longlat +datum=WGS84 +no_defs") %>% 
  filter(perm_en == "Permanent", definit_en == "Lake") %>%
  select(feature_id, geometry)

water_linear <- st_read("~/Dropbox/Map poster/FME_5544441C_1527277928696_1684/ESRISHAPE_1/canvec_180525_12507/water_linear_flow_1.shp") %>%
  st_transform("+proj=longlat +datum=WGS84 +no_defs") %>% 
  select(feature_id, geometry)

cities <- canada.cities %>%
  filter(country.etc == "MB", between(lat, 49.26413, 50.54668), between(long, -100.6961, -99.15989)) %>%
  mutate(name = gsub(' MB$', '', name), 
         lat = replace(lat, name == "Shilo", 49.81), # Correct coordinates for Shilo MB
         long = replace(long, name == "Shilo", -99.64)) # Correct coordinates for Shilo MB

qrcode <- rasterGrob(readPNG("~/Dropbox/Map poster/qr code.png"), interpolate = TRUE)

g <- ggplot(water_linear) +
  geom_sf(fill = "#699FF0", colour = "#699FF0") +
  geom_sf(data = lakes, fill = "#699FF0", colour = "#699FF0") +
  geom_point(data = cities, aes(x = long, y = lat), colour="black", size = 5) +
  geom_text(data = cities, aes(x = long - 0.01, y = lat -0.015, label = name), 
            hjust=0, size = 9) +
  coord_sf(expand = F) +
  scalebar(data = water_linear, dist = 20, dd2km = T, model = 'WGS84', 
           location = "bottomleft", anchor = c(x=-100.6, y= 49.3), 
           st.bottom = F, st.size = 8, st.dist = 0.01, height = 0.01) +
  north(data = water_linear, location = "topright", scale = 0.08, symbol = 1) +
  theme_bw(base_size = 25)+
  theme(axis.title = element_blank(), 
        plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) +
  ggtitle("Hydrological features of Brandon MB and surounding area") +
  theme(plot.margin = unit(c(10, 10, 40, 10), "mm")) +
  annotation_custom(qrcode, xmin = -100.6, xmax = -100.7, ymin = 49.175, ymax = 49.25) +
  annotation_custom(textGrob(label = "Hydrological features map created with CanVec data
                             \nusing R. Data links and R scripts available on GitHub 
                             \nscan QR code).",just = "left", gp = gpar(fontsize = 18)),
                    xmin = -100.55, xmax = -100.65, ymin = 49.175, ymax = 49.25)

gt <- ggplot_gtable(ggplot_build(g))
gt$layout$clip[gt$layout$name == "panel"] <- "off"

ggsave(filename = "Brandon MB hydro map.pdf", plot = gt, width = 58, 
       height = 75, units = "cm")
