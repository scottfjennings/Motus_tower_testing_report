library(tidyverse)
library(lubridate)
library(data.table)
library(plotKML)
library(here)

#Read in GPS path - CHANGE NAME
GPSTrack <- readGPX("./Tracks/Node_test_10-25-22.gpx")$tracks %>% 
  as.data.frame() %>% 
  rename("longitude" = 1, "latitude" = 2, "ele" = 3, "GPStime" = 4) %>%
  select(-ele) %>% 
  mutate(GPStime = gsub("T|Z", " ", GPStime),
         GPStime = ymd_hms(GPStime),
         gps.time.key = GPStime)
  


#CHANGE THE TIMES
DeplBegin <- as.POSIXct("10/25/2022 15:36", format="%m/%d/%Y %H:%M")
DeplEnd <- as.POSIXct("10/25/2022 17:13", format="%m/%d/%Y %H:%M")

#Correct Positions
Node365F7E <- as.data.frame(read.csv("./10_25_Nodes/365F7E/beep_0.csv")) %>%
  mutate(NodeID = "365F7E",
         LatNode = as.numeric("38.168975"),
         LonNode = as.numeric("-122.901905"))

Node3650EC <- as.data.frame(read.csv("./10_25_Nodes/3650EC/beep_0.csv")) %>%
  mutate(NodeID = "3650EC",
         LatNode = as.numeric("38.168975"),
         LonNode = as.numeric("-122.901905"))

Node364588 <- as.data.frame(read.csv("./10_25_Nodes/364588/beep_0.csv")) %>%
  mutate(NodeID = "364588",
         LatNode = as.numeric("38.165593"),
         LonNode = as.numeric("-122.900881"))

Node365099 <- as.data.frame(read.csv("./10_25_Nodes/365099/beep_0.csv")) %>%
  mutate(NodeID = "365099",
         LatNode = as.numeric("38.165593"),
         LonNode = as.numeric("-122.900881"))

Node364DC3 <- as.data.frame(read.csv("./10_25_Nodes/364DC3/beep_0.csv")) %>%
  mutate(NodeID = "364DC3",
         LatNode = as.numeric("38.164698"),
         LonNode = as.numeric("-122.900389"))

#Merge node dataframes, remove extra characters, append locations

DetectionsAllNodesFilt <- rbind(Node365F7E, Node3650EC, Node364588, Node365099, Node364DC3) %>% 
  right_join(read.csv(here("data/tagID_fixer.csv"))) %>% # instead of long nested ifelse, just do a right join with a csv that contains a column for the bad tag IDs and a column for the good ones
  mutate(time = gsub("T|Z", " ", time)) %>% # instead of the 2 chartr() calls
  filter(time >= DeplBegin) %>% 
  mutate(time = as.POSIXct(time, tz = "UTC"),
         node.time.key = time) %>% 
  rename("Signal" = rssi,
         "node.time" = time)


#Join GPS points to node file
NodeDT <- data.table(DetectionsAllNodesFilt)
GPSDT <- data.table(GPSTrack)


setkey(NodeDT, node.time.key)
setkey(GPSDT, gps.time.key)

DetectionsCombined <- GPSDT[NodeDT, roll = "nearest" ]
DetectionsCombinedFiltered <- DetectionsCombined %>%
  filter(abs(GPStime-node.time) <= 1) #changed this to 1 second (from 0.5) because I really wasn't paddling all that fast!



write.csv(DetectionsCombinedFiltered, file = here("data/node_data/MatchedDetections20221025.csv"))


DetectionsCombinedFiltered %>% 
  data.frame() %>% 
ggplot() +
  geom_point(aes(x=longitude, y=latitude, color=Signal, shape=Tag)) +
#geom_point(mapping=aes(x=LonNode, y=LatNode, size=5)) +
  facet_wrap(~ NodeID)
