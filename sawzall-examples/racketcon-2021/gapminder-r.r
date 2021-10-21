
library(gapminder)
library(tidyverse)



ggplot(data = gapminder,
       mapping = aes(x = gdpPercap,
                     y = lifeExp)) +
  labs(x = "GDP per capita (USD)",
       y = "Life expectancy (years)") +
  scale_x_log10() +
  geom_point(alpha = 0.4,
             aes(color = continent)) +

  geom_smooth()
