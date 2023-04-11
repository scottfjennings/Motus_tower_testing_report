




library(tidyverse)
library(lubridate)
library(here)
library(geosphere) # to calculate distances and bearings


# df1 <- read.csv(here("data/node_data/MatchedDetections20221003.csv")) %>% mutate(Tag = as.character(Tag)) %>% select(NodeID, tsNode, latitude, longitude, Tag, Signal, LatNode, LonNode)
  

# df2 <- read.csv(here("data/node_data/MatchedDetections20221018.csv")) %>% mutate(Tag = as.character(Tag)) %>% select(NodeID, tsNode, latitude, longitude, Tag, Signal, LatNode, LonNode)

df3 <- read.csv(here("data/node_data/MatchedDetections20221025.csv")) %>% 
  mutate(Tag = as.character(Tag),
         travel.mode = "boat") %>% 
  select(NodeID, tsNode = gps.time.key, latitude, longitude, Tag, Signal, LatNode, LonNode, travel.mode)

df4 <- read.csv(here("data/node_data/2022_12_06MatchedDetectionsBoat.csv")) %>% 
  select(NodeID, tsNode, latitude = TagLatitude, longitude = TagLongitude, Tag = id, Signal = rssi, LatNode = Latitude, LonNode = Longitude) %>% 
  mutate(Tag = as.character(Tag),
         travel.mode = "boat")

df5 <- read.csv(here("data/node_data/2022_12_06MatchedDetectionsFoot.csv")) %>% 
  select(NodeID, tsNode, latitude = TagLatitude, longitude = TagLongitude, Tag = id, Signal = rssi, LatNode = Latitude, LonNode = Longitude) %>% 
  mutate(Tag = as.character(Tag),
         travel.mode = "foot")



df <- bind_rows(df3, df4, df5) %>% 
  bind_rows() %>% 
  mutate(tsNode = as.POSIXct(tsNode, tz = "UTC"),
         date = as.Date(tsNode))


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
  rename(distance = 1) 


# Calculate bearings:  

df_bearings <- bearing(p1 = node.loc, p2 = tag.loc) %>% 
  data.frame() %>% 
  rename(bearing = 1) %>%
  # change scale from -180 to 180 to 0 to 360
  mutate(bearing = ifelse(bearing < 0, (180 + bearing) + 180, bearing))


# Combine distances and bearings.  

df_bearings_dist <- df_bearings %>% 
  cbind(df) %>% 
  cbind(ant.tag.dist) 

saveRDS(df_bearings_dist, here("data/node_data/combined_test_data"))
