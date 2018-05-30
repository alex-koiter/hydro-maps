
# /*======================*/
#' ### Hydrological features map
# /*======================*/
library(tidyverse)
library(sf)
library(ggsn)

# Maps created using CanVec data which can be found at http://maps.canada.ca/czs/index-en.html

lakes <- st_read("/home/alex/Dropbox/Map poster/FME_5544441C_1527277928696_1684/ESRISHAPE_1/canvec_180525_12507/waterbody_2.shp", quiet = TRUE) %>%
  st_transform("+proj=longlat +datum=WGS84 +no_defs") %>% 
  filter(perm_en == "Permanent", definit_en == "Lake") %>%
  select(feature_id, geometry)

water_linear <- st_read("/home/alex/Dropbox/Map poster/FME_5544441C_1527277928696_1684/ESRISHAPE_1/canvec_180525_12507/water_linear_flow_1.shp") %>%
  st_transform("+proj=longlat +datum=WGS84 +no_defs")  %>% 
  select(feature_id, geometry)


library(maps)
cities <- canada.cities %>%
  filter(country.etc == "MB", between(lat, 49.26413, 50.54668), between(long, -100.6961, -99.15989)) %>%
  mutate(name = gsub(' MB$', '', name), 
         lat = replace(lat, name == "Shilo", 49.81), # Crrect coordinates for Shilo MB
         long = replace(long, name == "Shilo", -99.64))

g <- ggplot(water_linear) +
  geom_sf(fill="#699FF0", colour="#699FF0") +
  geom_sf(data = lakes, fill= "#699FF0", colour="#699FF0") +
  geom_point(data = cities, aes(x= long, y=lat), colour="black", size = 5) +
  geom_text(data = cities, aes(x = long - 0.01, y = lat -0.015, label = name), hjust=0, size = 9) +
  coord_sf(expand = F) +
  scalebar(temp4, dist = 20, dd2km = T, model = 'WGS84', location = "bottomleft", anchor = c(x=-100.6, y= 49.3), st.bottom = F, st.size = 8, st.dist = 0.01, height = 0.01) +
  north(data = temp, location = "topright", scale = 0.08, symbol = 1) +
  theme_bw(base_size = 25)+
  theme(axis.title=element_blank(), plot.title = element_text(size = 40, face = "bold", hjust = 0.5)) +
  ggtitle("Hydrological features of Brandon MB and surounding area")

ggsave(file="test_5_map.pdf", plot = g, width = 60, height = 75, units = "cm", dpi=500)