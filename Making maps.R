library(tidyverse)
library(scales)
library(lubridate)
library(countrycode)

water <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv')

water <- water %>% 
  mutate(report_date = mdy(report_date)) %>% 
  rename(lat = lat_deg,
         lon = lon_deg,
         country= country_name) %>% 
  separate(water_tech, c("water_tech", "brand"), sep = " - ",
           fill = "right") %>% 
  mutate(install_year = ifelse(install_year>2021, NA_real_,install_year)) %>% 
  filter(!country %in% c("Peru"), 
         !is.na(country) ) %>% 
  filter(!country %in% c("Timor-Leste", "Dominican Republic") ) %>% 
  filter(between(lat, -30,20),
         between(lon, -40, 60))



water %>% 
  count(status_id)

water %>% 
  count(water_tech, sort = TRUE)

water %>% 
  count(water_source, sort = TRUE)

water %>% 
  count(water_source, water_tech, sort = TRUE)

water %>% 
  filter(install_year>2021) %>% 
  

water %>% 
  filter(install_year> 1980) %>% 
  count(install_year) %>% 
  ggplot(aes(install_year,n))+ 
  geom_col()

water %>% 
  count(country_name, sort = TRUE) %>% 
  

water %>% 
  count(installer, sort = TRUE) %>% 
  

# Maps --------------------------------------------------------------------

water %>% 
  sample_n(1000) %>% 
  filter(!country %in% c("Timor-Leste") ) %>% 
  ggplot(aes(lon,lat, color = country))+
  geom_point()

water %>% 
  filter(!country %in% c("Timor-Leste", "Dominican Republic") ) %>% 
  group_by(country) %>% 
  summarise(lat = mean(lat),
            lon = mean(lon)) %>% 
  ggplot(aes(lon,lat))+
  geom_point()+
  geom_text(aes(label= country), vjust = 1, hjust =1)

countries <- unique(water$country)

african_map_data <- map_data("world") %>% 
  as_tibble() %>% 
  mutate(contitent = countrycode(region, "country.name", "continent")) %>% 
  filter(contitent== "Africa")

water %>% 
  sample_n(20000) %>% 
  ggplot(aes(lon,lat))+
  geom_polygon(aes(long, lat, group = group),
               color = "gray",
               fill = "white",
               data = african_map_data,
               size = .5)+
  geom_point(size = .1, alpha =.25)


