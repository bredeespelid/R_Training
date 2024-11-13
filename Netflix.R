library(tidyverse)
library(tidyr)
library(ggplot2)
library(lubridate)

netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')

netflix_titles %>% 
  ggplot(aes(release_year, fill = type))+
  geom_histogram(binwidth = 5)+
  #CTRL+ALT
  facet_wrap(~type, ncol = 1, scales =  "free_y")

netflix_titles %>% 
  count(year =release_year, type )%>% 
  group_by(type) %>% 
  mutate(percent = n / sum(n)) %>% 
  ggplot(
    aes(year, percent, color = type)
  )+
  
  geom_line()

netflix_titles %>% 
  count(rating, sort = TRUE)

netflix_titles <-netflix_titles %>% 
  separate(duration, c("duration", "units"), sep= " ", convert = TRUE) %>% 
  mutate(date_added= mdy(date_added),
         year_added= year(date_added))

netflix_titles %>% 
  filter(type == "Movie") %>% 
  mutate(decade = 10*(release_year%%10)) %>% 
  ggplot(aes(decade, duration, group = decade))+
  geom_boxplot()


netflix_titles %>% 
  separate_rows(listed_in, sep = ", ") %>% 
  group_by(type, genre= listed_in) %>% 
  summarize_titles() %>% 
  filter(type == "Movie") %>% 
  filter(genre != "Movies") %>% 
  mutate(genre = fct_reorder(genre, median_duration)) %>%
  ggplot(aes(median_duration, genre))+
  geom_col()


summarize_titles <- function(tbl){
  tbl %>% 
    summarize(n = n(),
              median_duration= median(duration),
              median_year = median(release_year)) %>% 
    arrange(desc(n))
}

netflix_titles %>% 
  filter(!is.na(date_added)) %>% 
  arrange(date_added) %>% 
  select(type,title,date_added)

netflix_titles  %>% 
  filter(!is.na(date_added)) %>% 
  count(year_added,type) %>%
  ggplot(aes(year_added, n, fill=type))+
  geom_area()

netflix_titles  %>% 
  mutate(year_added= pmax(year_added,2015)) %>% 
  group_by(type) %>% 
  mutate(rating = fct_lump(rating, 4)) %>% 
  ungroup() %>% 
  filter(!is.na(date_added), !is.na(rating)) %>% 
  count(type, year_added, rating) %>%
  group_by(type, year_added) %>% 
  mutate(percent = n/sum(n)) %>% 
  ggplot(aes(year_added, percent, fill=rating))+
  geom_area()+
  facet_wrap(~type)

