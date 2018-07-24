library(dplyr)
library(ggplot2)
library(PBSmapping)
library(stringr)
library(purrr)
data <- read.table("R_test/territory polygons.txt", header = T)

data(d1) 
d1 <- d1 %>% filter(type == "inter")


data <- data %>% 
  mutate(clan = as.factor(str_sub(clan, 1, 1))) %>%
  group_by(clan) %>%
  mutate_if(is.factor, as.numeric) %>%
  ungroup()

## find the centroid for each clan
test <- data %>% 
  select(clan, latitude, longitude) %>%
  mutate(clan = as.numeric(clan)) %>%
  rename("X" = latitude, "Y" = longitude, "PID" = clan) 

test <- test %>% group_by(PID) %>% 
  mutate(POS = 1:n()) %>% ungroup()
test <- as.data.frame(test)

test <- test %>% split(.$PID)


out <- purrr::map_df(test, ~ PBSmapping::calcCentroid(.))

out$clan <- factor(out$PID, labels  = c("A", "E", "F", "L", "M", "N", "S", "T"))
data2 <- left_join(data, out, by = "clan")
data2$dist <- geosphere::distGeo(cbind(data2$latitude, data2$longitude), cbind(data2$X, data2$Y))/1000

data2 <- data2 %>% group_by(clan) %>% mutate(max_dist = max(dist)) %>% ungroup()
out_max <- data2 %>% group_by(clan) %>% filter(dist == max_dist)
max(data2$max_dist)*2
mean(out_max$max_dist)*2

# visualize the clan 
ggplot(data2, aes(latitude, longitude, col = clan)) +
  geom_polygon(fill =NA) +
  geom_text(aes(y =longitude +0.005, label = point)) +
  geom_point(aes(x = X, y = Y)) +
  geom_segment(data = out_max, aes(x = latitude, y = longitude, xend = X, yend = Y)) +
  coord_fixed()

