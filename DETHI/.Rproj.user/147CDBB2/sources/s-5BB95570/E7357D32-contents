library(gapminder)
library(dplyr)
data("gapminder")
# Show the names of countries using unique ()
unique (gapminder$country)
# Show the names of continents
unique (gapminder$continent)

#filer data for Vietnam and from 2000 and go on
gapminder %>% 
  filter(country == "Vietnam" & year > 2000)

gapminder %>% 
  filter(country == "Ghana" | country == "Vietnam")

gapminder %>%
  select (pop, continent) %>%
  head (n = 10)
# Arrange country by pop
gapminder %>%
  select(country, gdpPercap, pop) %>%
  arrange (desc(pop)) %%
  head(n = 7)
# Define the country with the highest gdp by year
gapminder %>%
  select(country, year, gdpPercap) %>%
  arrange(desc(year), gdpPercap) %>% 
  head()

# Define 
gapminder %>%
  mutate(total_gdp = gdpPercap* pop) %>%
  head()

gapminder %>%
  mutate(log_gdp = log10(gdpPercap), log_pop = log10 (pop)) %>% 
  arrange (desc (log_gdp)) %>%
  head ()

#Find the countries with gdp per day is smaller than 1 USD and then arrange
gapminder %>%
  select (country, gdpPercap, pop, lifeExp, continent, year) %>%
  mutate(gdp_per_day = gdpPercap/365) %>%
  filter(gdp_per_day <=1) %>%
  arrange(gdp_per_day) %>% 
  head ()

#Using statistical summary
gapminder %>% 
  group_by(year) %>% 
  summarise(mean_gdp = mean(gdpPercap))


#Handle null value
times <- round (11680/3725,2)
# Count the missing values
myvector <- c(NA, 3, 5, 4, 8, NA, 3, 88, NA, NA, NA)
is.na(myvector)
table (is.na(myvector))

count_missing <- function(x){
  is.na(x) -> missing_values
  sum (missing_values) -> total_missing
  return(total_missing)
}
count_missing(myvector)


install.packages("nycflights13")
library(nycflights13)
data(flights)
vector_missing_values <- c()
# Count the quantity of columns in flights
k <- ncol(flights)
# Using for loops to pull the data
for (i in 1:k){
  flights %>%
    pull(i) -> alternative_values
  count_missing(alter)
  

  



