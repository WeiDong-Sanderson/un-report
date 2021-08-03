#UM 2021-august-umswc-workshop
#Wei Dong

library(tidyverse)
gapminder_data <- read_csv('data/gapminder_data.csv')
gapminder_data
?summarise()
gapminder_data %>%
  summarise(average=mean(lifeExp))

summarise(gapminder_data, average = mean(lifeExp))

#look for the most recent year
?max()
gapminder_data %>%
  summarise(recent_year=max(year))

#select only 2007 data
?filter()
gapminder_data %>%
  filter(year ==2007) %>%
  summarise(average=mean(lifeExp))
#gapminder_data <- read_csv("  

#
gapminder_data %>%
  filter(year ==1952) %>%
  summarise(average_gdp =mean(gdpPercap))

# what is life exp for each year
gapminder_data %>%
  group_by(year) %>%
  summarise(min = min(lifeExp), max =max(lifeExp))

#add a column for total GDP - pop * per capita GDP
head(gapminder_data)
totalGDP <- gapminder_data %>%
  mutate(TotalGDP = pop*gdpPercap)

# filter = choose rows; select = choose columns
gapminder_data %>%
  select(pop, year)

#how to drop columns
gapminder_data %>%
  select(-continent)

# what is tidy data?
gapminder_data_wide <- gapminder_data %>%
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp)
head(gapminder_data_wide)
head(gapminder_data)


# gapminder was tidy, so how do we clean untidy data?
#co2_emissions < - read_csv('co2-un-data.csv', skip=2, col_names = c('region', 'country', 'year', 'series',
#                                                       'value', 'footnotes', 'source')) %>%
#  select(country, year, series, value) %>%
#  mutate(series = recode(series, 
#                         'Emissions (thousand metric tons of carbon dioxide)'
#                         = 'total',
#                         'Emissions per capita (metric tons of carbon dioxide)' ='per_capita'))%>%
#  pivot_wider(names_from = series, values_from=value) %>%
#  filter(year == 2005) %>%
#  select(-year)

co2_data <- read_csv("co2-un-data.csv", skip=2)

co2_emissions <- read_csv("co2-un-data.csv", skip=2,
                          col_names = c("region", "country", "year", 
                                        "series", "value", "footnotes", #update column names
                                        "source")) %>%
  select(country, year, series, value) %>% # select relevant columns
  mutate(series=
           recode(series, 
                  "Emissions (thousand metric tons of carbon dioxide)"
                  = "total",
                  "Emissions per capita (metric tons of carbon dioxide)" =
                    "per_capita")) %>% #simplify emission names
  pivot_wider(names_from=series, values_from=value) %>% # separate types of 
  filter(year == 2005) %>% # subset data from 2005
  select(-year)

View(co2_emissions)


#filter gapminder to get 2007 data for americans
gapminder_data_2007 <- read_csv('data/gapminder_data.csv') %>%
  filter(year == 2007 & continent =='Americas') %>%
  select(-year, -continent)

## Merge together our two datasets
# Only merge common countries
?inner_join()
inner_join(gapminder_data_2007, co2_emissions, by = "country")

#find what is missing from join
anti_join(gapminder_data_2007, co2_emissions, by = "country")
View(co2_emissions)

#fix co2 table to match better
co2_emissions
?mutate
co2_emissions_fixed <- co2_emissions %>%
  mutate(country=recode(country, 
                        'Bolivia (Plurin. State of)' = 
                          'Bolivia', 
                        'United States of America' =
                          'United States', 
                        'Venezuela (Boliv. Rep. of)' =
                          'Venezuela'))
View(co2_emissions_fixed)
# How would we recode Puerto Rico?
gapminder_co2 <- inner_join(gapminder_data_2007, co2_emissions_fixed, by ='country')
anti_join(gapminder_data_2007, co2_emissions_fixed, by ='country')

View(gapminder_co2)

# what is relationship between GDP and amount of CO2
ggplot(gapminder_co2, aes(x=gdpPercap, y =per_capita))+
  geom_point()+
  geom_smooth(method='lm') +
  labs(x='GDP (per capita)',
       y='CO2 emitted (per captita)', 
       title = "There is a strong association between a nation 's GDP and CO2 production")
ggsave('./GDPvsCO2.jpg', width = 6, height =4)


#What are the CO2 emissions for US/Canada/Mexico vs all other countries?"
?if_else
head(gapminder_co2)
gapminder_co2 %>%
  mutate(region = if_else(country == 'Canada' | country =='United States' | country == 'Mexico', 'north', 'south')) %>%
  group_by(region) %>%
  summarise(sumCO2 = sum(total), sumpop = sum(pop))

?write_csv
write_csv(gapminder_co2, './data/GDP_vs_co2_data.csv')
