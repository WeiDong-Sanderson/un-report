library(tidyverse)
gapminder_1997= read_csv(file = 'gapminder_1997.csv')
library(ggplot2)
#Plotting in R
ggplot(data = gapminder_1997) + 
  aes(x=gdpPercap) +
  labs(x='GDP Per Capita') +
  aes(y = lifeExp) +
  labs(y ='Life Expectancy')+
  geom_point()+
  labs(title = 'Do people in wealthy countries live longer?') +
  aes(color = continent) +
  scale_color_brewer(palette = 'Set2')+
  aes(size=pop/1000000) +
  labs(size='Population(in millions')

ggplot(data = gapminder_1997) + 
  aes(x=gdpPercap , y = lifeExp, color = continent, size = pop/1000000) +
  labs(x='GDP Per Capita', y ='Life Expectancy', 
       title = 'Do people in wealthy countries live longer?',
       size='Population(in millions') +
  geom_point()+
  scale_color_brewer(palette = 'Set2')

# larger gapminder dataset
gapminder_data <- read_csv(file='gapminder_data.csv')
gapminder_data
ggplot(data = gapminder_data) + aes(x = continent , y= lifeExp) +
  geom_boxplot()

ggplot(data = gapminder_1997) + aes(x = continent , y= lifeExp) +
  geom_violin() +
  geom_jitter(width =0.3, height =0, aes(size = pop))


