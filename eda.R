rm(list=ls())
library(tidyverse)
library(lubridate)
library(purrr)

data <- read_csv('data/statsports.csv')

glimpse(data)

vars <- c(
  "Accelerations Z3 to Z6", 
  "Deceleration Z3 to Z6",
  "Distance Per Min",
  "Distance Total",
  #"Dynamic Stress Load",
  #"Sprints",
  "Player Display Name",
  "Type")

data <- data %>% 
  select(vars) %>%
  filter(Type == 'Training') %>% 
  select(-c("Type"))

data_by_athlete <- data %>% 
  group_by(`Player Display Name`) %>% 
  summarise_all(mean)  %>%
  mutate_if(is.numeric, scale)




glimpse(data_by_athlete)



model <- kmeans(data_by_athlete %>% select(-c(`Player Display Name`)), centers = 3)

library(tidyr)

centers_df <- as.data.frame(model$centers) %>% rownames_to_column("cluster")
gather(centers_df, k = metric, value = value, -cluster) %>%
  ggplot(aes(metric, cluster, fill = value)) + 
  geom_tile() +
  scale_fill_gradient(high = "green", low = "red")


data <- data_by_athlete %>% select(-c(`Player Display Name`))
pcs <- prcomp(data)
summary(pcs)


attributes(pcs)
pcs$x

model <- kmeans(data, centers = 3)
dd2 <- cbind(data_by_athlete, pcs$x, model$cluster)



ggplot(dd2, aes(PC1, PC2, label = `Player Display Name`, color = factor(model$cluster))) + 
  geom_point() + 
  geom_text()


scaled_data_for_clustering <- data_by_athlete %>% 
  select(-c(`Player Display Name`)) %>%
  mutate_all(scale)

tot_withinss <- map_dbl(1:10, function(k) {
  print(k)
  model <- kmeans(scaled_data_for_clustering, centers = k)
  model$tot.withinss
})

data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
) %>% ggplot(aes(k, tot_withinss)) + geom_line() + scale_x_continuous(breaks = 1:10)






