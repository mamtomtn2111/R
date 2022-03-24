library(robotstxt)
library(rvest)
library(dplyr)

library(stringr)
library(lubridate)
library(purrr)

#Check the security
paths_allowed(paths = c("https://www.imdb.com/search/title/?groups=top_250&sort=user_"))

#Read the site
mydata <- read_html("https://www.imdb.com/search/title/?groups=top_250&sort=user_")

#Get the name of the film
mydata %>% 
  html_nodes(".lister-item-content h3 a") %>% 
  html_text() -> name_movie

#Get the year of film
mydata %>% 
  html_nodes(".lister-item-content .lister-item-year") %>% 
  html_text() %>% 
  str_sub(start = 2, end = 5) %>% 
  as.Date(format = "%Y") %>% 
  year() -> movie_year

#Get the length of film
mydata %>% 
  html_nodes(".lister-item-content p .runtime") %>%
  html_text() %>%
  str_sub(start = 1, end = 3) %>%
  as.integer() -> film_length

#Get the film genre
mydata %>% 
  html_nodes(".lister-item-content p .genre") %>% 
  html_text() %>%
  str_sub(start = 2) %>% 
  trimws("r") -> movie_genre

mydata %>% 
  html_nodes(".lister-item-content p .genre") %>% 
  html_text() %>%
  str_trim() -> movie_genre

#Get the rating point
mydata %>% 
  html_nodes(".ratings-bar .inline-block strong") %>% 
  html_text() %>%
  as.numeric() -> movie_rate_star


top_50 <- tibble(Title = name_movie, Release = movie_year, 'Duration (min)' = film_length,
                Genre = movie_genre, Rate = movie_rate_star)
views(top_50)
