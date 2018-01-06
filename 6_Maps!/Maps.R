library(tidyverse)
#library(ggmap)


us_data <- map_data("state")

world_data <- map_data("world")

usamap <- ggplot(us_data, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

# Use cartesian coordinates
usamap
# With mercator projection, a cylindrical projection that makes all lattitude lines parallel,
#and all longitude lines parallel (lat and long perpendicular).  What effect does this have?
usamap + coord_map()

#https://www.rdocumentation.org/packages/mapproj/versions/1.2-4/topics/mapproject
usamap + coord_map("conic", lat0=30)




worldmap <- ggplot(world_data, aes(x = long, y = lat, group = group)) +
  geom_path() +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45)


worldmap + coord_map("ortho")



worldmap + coord_map("ortho", orientation = c(41, -74, 0))


worldmap + coord_map("ortho", orientation = c(41, -74, 45))




museums <- read_csv("museums.csv")


museums_by_state <- museums %>% 
  group_by(State) %>%
  summarize(Count = n(),
            log_mean_income = log(mean(Income, na.rm = T)), mean_income=mean(Income, na.rm=T))

us_data_with_museums <- us_data %>%
  left_join(museums_by_state, by = c("region" = "State"))

ggplot(us_data_with_museums) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Count), 
               color = "black") + 
  scale_fill_continuous(low = "green", high = "purple") +
  theme_void() +
  #coord_map()+
  coord_map("polyconic") + 
  labs(title = "Number of Museums Per State")



ggplot(us_data_with_museums) +
  geom_polygon(aes(x = long, y = lat, group = group, 
                   fill = log_mean_income), color = "black") + 
  scale_fill_continuous(low = "green", high = "purple") +
  theme_void() +
  coord_map("polyconic") + 
  labs(title = "Log Mean Income of Museums Per State")


ggplot(us_data_with_museums) +
  geom_polygon(aes(x = long, y = lat, group = group, 
                   fill = mean_income), color = "black") + 
  scale_fill_continuous(low = "green", high = "purple") +
  theme_void() +
  coord_map("polyconic") + 
  labs(title = "Mean Income of Museums Per State")

library(ggmap)
map <- get_map(location = 'United States', zoom = 4)

#  Visualize the basic map
ggmap(map)# +
  geom_point(aes(x = Longitude, y = Latitude, color = Type), 
             data = museums, alpha = .5) + 
  labs(title = "Museum Locations by Type") + 
  theme_void()

  UVA_sat <- get_map("university of virginia", zoom = 15, maptype = "satellite")
  ggmap(UVA_sat)

  UVA_road <- get_map("university of virginia", zoom = 15, maptype = "roadmap")
  ggmap(UVA_road)
  
  rodu <- read_csv('rodu.csv')
  UVA_road <- get_map("university of virginia", zoom = 15, maptype = "roadmap")
  ggmap(UVA_road) +
    geom_point(aes(x=Long, y=Lat, color=type), data=rodu, size=4)+
    labs(title="Where to find Rodu")+
    theme_void()
  
  ny <- map_data("county", region='new york')
  
  ggplot(ny) +
    geom_polygon(aes(x = long, y = lat, group = group), fill="white",
                 color = "black")
  
  country_counties <- map_data("county")
  ggplot(country_counties) +
    geom_polygon(aes(x = long, y = lat, group = group), fill="white",
                 color = "black")
  