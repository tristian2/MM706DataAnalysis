naCuisine <- recipes %>% select (sodium, cuisine) %>% group_by(cuisine) %>% summarise(sodium = mean(as.double(sodium)))
x <- naCuisine$cuisine
y <- naCuisine$sodium
data <- data.frame(x, y)

p <- plot_ly(naCuisine, x = x, y = y, type = 'bar', color = I("black")) %>%
  layout(title = "Features",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
p