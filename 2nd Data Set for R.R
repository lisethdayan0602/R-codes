library(tidyverse)
data("diamonds")
force(diamonds)
summary(diamonds)

ggplot(diamonds, aes(x = carat)) +  ## When I did this chart I was just trying to look up at any chart. 
  geom_histogram()


ggplot(diamonds, aes(x = carat, fill = cut)) + # run this code to check how the carat was related to the cut, I mean which of the two were more influence.
  geom_histogram()

ggplot(data = diamonds) +
  geom_col(mapping = aes(x = carat, y = price, color = clarity)) # I wanted to check the price in conparason with clarity and carat


ggplot(data = diamonds) +
  geom_boxplot(mapping = aes(x = price, y = table, color = clarity)) # I wanted to see the price in conparason with the table and clarity


ggplot(data = diamonds) +
  geom_col(mapping = aes(x = clarity, y = depth, color = color))
# I wanted to check the deth related with the clarity and color 


ggplot(diamonds, aes(x = cut, fill = price)) + 
  geom_bar()
# I wanted to see the price related with the cut  

