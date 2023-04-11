


library(tidyverse)
library(lubridate)
library(here)
library(geosphere) # to calculate distances and bearings
library(caret) # for cross-validation


options(scipen = 999)

df1 <- read.csv(here("data/node_data/MatchedDetections20221003.csv")) %>% 
  mutate(Tag = as.character(Tag))
df2 <- read.csv(here("data/node_data/MatchedDetections20221018.csv")) %>% 
  mutate(Tag = as.character(Tag))

df <- bind_rows(df1, df2) %>% 
  mutate(tsNode = as.POSIXct(tsNode),
         date = as.Date(tsNode)) %>% 
  filter(date != "2022-09-21")

summary(df)

ggplot(df) +
  geom_density(aes(Signal))



# First need matrices of tag locations and tower locations.  


tag.loc <- df %>% 
  select(longitude, latitude) %>% 
  data.matrix()

node.loc <- df %>% 
  select(LonNode, LatNode) %>% 
  data.matrix()


# Calculate distances:  

ant.tag.dist <- distHaversine(node.loc, tag.loc) %>% 
  data.frame() %>% 
  rename(ant.tag.distance = 1) 


# Calculate bearings:  

df_bearings <- bearing(p1 = node.loc, p2 = tag.loc) %>% 
  data.frame() %>% 
  rename(tower.tag.bearing = 1) %>%
  # change scale from -180 to 180 to 0 to 360
  mutate(tower.tag.bearing = ifelse(tower.tag.bearing < 0, (180 + tower.tag.bearing) + 180, tower.tag.bearing))


# Combine distances and bearings.  

df_bearings_dist <- df_bearings %>% 
  cbind(df) %>% 
  cbind(ant.tag.dist) 


ggplot(df_bearings_dist) +
  geom_point(aes(x = ant.tag.distance, y = Signal)) +
  facet_wrap(date~NodeID) +
  labs(x = "Tag-node distance (m)",
       y = "Signal strength")
ggsave(here("figures/node_test_1_2.png"))


# try to predict distance with a simple linear model ----

df_bearings_dist_good <- df_bearings_dist %>% 
  filter((NodeID == "365F7E" & date == "2022-09-22") | (NodeID == "3680B7" & date == "2022-10-18"))


distance_tester <- function(zdf) {

zsize = floor(nrow(zdf) * 0.8)

df_test <- sample_n(zdf, size = zsize)
distance_predict_newdat <- anti_join(zdf, df_test) %>% 
  select(Signal, tsNode, date, NodeID, ant.tag.distance)

test_lm <- lm(ant.tag.distance ~ Signal + I(Signal^2), data = df_test)

distance_predict <- predict(test_lm, distance_predict_newdat) %>% 
  data.frame() %>% 
  rename("predicted.distance" = 1) %>% 
  bind_cols(distance_predict_newdat)
} 


distance_predict <- distance_tester(df_bearings_dist)

distance_predict %>%
  ggplot() +
  geom_point(aes(x = ant.tag.distance, y = predicted.distance)) +
  facet_wrap(date~NodeID) +
  geom_abline(intercept = 0) +
  lims(x = c(0, 225),
       y = c(0, 225)) +
  labs(y = "Predicted distance",
       x = "True distance")


# cross validation ----
# K-fold cross-validation

# setting seed to generate a
# reproducible random sampling
set.seed(125)

# defining training control
# as cross-validation and
# value of K equal to 10
train_control <- trainControl(method = "cv",
							number = 10)

# training the model by assigning sales column
# as target variable and rest other column
# as independent variable
model <- train(ant.tag.distance ~ Signal + I(Signal^2), data = df_bearings_dist,
			method = "lm",
			trControl = train_control)

# printing model performance metrics
# along with other details
print(model)


