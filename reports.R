library(dplyr)
library(plotly)
naCuisine <- recipes %>% 
  select (sodium, cuisine) %>% 
  group_by(cuisine) %>% 
  summarise(sodium = mean(as.double(sodium))) %>%
  order_by(mean(as.double(sodium)))

x <- naCuisine$cuisine
y <- naCuisine$sodium
data <- data.frame(x, y)

p <- plot_ly(naCuisine, x = x, y = y, type = 'bar', color = I("blue")) %>%
  layout(title = "Sodium (Na), by cuisine",
         xaxis = list(title = "cuisine"),
         yaxis = list(title = "Na"))
p

sugarCuisine <- recipes %>% 
  select (cuisine, sugar) %>% 
  group_by(cuisine) %>% 
  summarise(sugar = mean(as.double(sugar))) %>%
  filter(!is.na(sugar))

x <- sugarCuisine$cuisine
y <- sugarCuisine$sugar
data <- data.frame(x, y)

p <- plot_ly(sugarCuisine, x = x, y = y, type = 'bar', color = I("red")) %>%
  layout(title = "Sugar by cuisine",
         xaxis = list(title = "cuisine"),
         yaxis = list(title = "sugar"))
p



mydata$ingredientLines