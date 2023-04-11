library(tidyverse)
library(xml2)
#library(DBI)
#library(RSQLite)
library(lubridate)
library(data.table)
#library(plotKML)

#Read in GPS path - CHANGE NAME
GPS <- readGPX("./Tracks/Node_test_10-25-22.gpx")
GPSTrack <- as.data.frame(GPS$tracks)
colnames(GPSTrack)<-gsub(".10.25.22.","",colnames(GPSTrack))
colnames(GPSTrack)<-gsub("Node.test","",colnames(GPSTrack))
GPSTrack$time <- chartr("T", " ", GPSTrack$time)
GPSTrack$time <- chartr("Z", " ", GPSTrack$time)

GPSTrack <- GPSTrack%>%
  mutate(GPStime = (ymd_hms(GPSTrack$time)),
         GPStimekey = (ymd_hms(GPSTrack$time)),
         latitude = lat,
         longitude = lon,
         elevation = ele) %>%
  select(GPStimekey, GPStime, latitude, longitude, elevation)

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
         LonNode = as.numeric("--122.901905"))

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
DetectionsAllNodes <- rbind(Node365F7E, Node3650EC, Node364588, Node365099, Node364DC3)
DetectionsAllNodes$time <- chartr("T", " ", DetectionsAllNodes$time)
DetectionsAllNodes$time <- chartr("Z", " ", DetectionsAllNodes$time)

unique(DetectionsAllNodes$id)

DetectionsAllNodesFiltStrict <- DetectionsAllNodes %>%
  filter(id %in% c("66341907", "4B2A1E4C", "524B1EE6"),
         time >= DeplBegin)

DetectionsAllNodesFilt <- DetectionsAllNodes %>%
  filter(id %in% c("6634190794", "66341907", "66349907","66341987","66B41907",
                   "4B2A1E4CB7", "4B2A1E4C", "4BAA1E4C", "4BAA1ECC", "4B2A1ECC","4B2A9ECC","CBAA1E4C","CB2A1E4C",
                   "524B1E660E", "524B1EE6","52CB1E66", "D24B1E66", "524B9E66"),
         time >= DeplBegin) %>%
  mutate(tsNode = as.POSIXct(time),
         tsNodeRef= tsNode,
         Signal = rssi,
         Tag = if_else(id == "6634190794", "66341907",
              if_else(id == "66341907", "66341907",
              if_else(id == "66349907", "66341907",
              if_else(id == "66341987", "66341907",
              if_else(id == "66B41907", "66341907",
              if_else(id == "4B2A1E4CB7", "4B2A1E4C",
              if_else(id == "4B2A1E4C", "4B2A1E4C",
              if_else(id == "4BAA1E4C", "4B2A1E4C",
              if_else(id == "4BAA1ECC", "4B2A1E4C",
              if_else(id == "4B2A1ECC", "4B2A1E4C",
              if_else(id == "4B2A9ECC", "4B2A1E4C",
              if_else(id == "CBAA1E4C", "4B2A1E4C",
              if_else(id == "CB2A1E4C", "4B2A1E4C",
              if_else(id == "52CB1E66", "524B1E66",
              if_else(id == "524B9E66", "524B1E66",
              if_else(id == "D24B1E66", "524B1E66",
              if_else(id == "524B1E660E", "524B1E66", "524B1E66")))))))))))))))))) %>%
  select(tsNode, NodeID, Signal, Tag, LatNode, LonNode, tsNodeRef)


#Join GPS points to node file
NodeDT <- data.table(DetectionsAllNodesFilt)
GPSDT <- data.table(GPSTrack)

setkey(NodeDT, tsNodeRef)
setkey(GPSDT, GPStimekey)

DetectionsCombined <- GPSDT[NodeDT, roll = "nearest" ]
DetectionsCombinedFiltered <- DetectionsCombined %>%
  mutate(tsdiff = (GPStimekey-tsNode),
         GPStime = GPStimekey) %>%
  filter(tsdiff >= -.5, tsdiff <= .5) %>%
  select(GPStimekey, tsNode, tsdiff, latitude, longitude, elevation, NodeID, Signal, Tag, LatNode, LonNode)

setkey(GPSDT, GPStimekey)
setkey(DetectionsCombinedFiltered, GPStimekey)
AttemptedStupidFix <- DetectionsCombinedFiltered[GPSDT, roll = "nearest" ]



write.csv(DetectionsCombinedFiltered, file = "./MatchedDetections10_25.csv")

#to dataframe for ggplotery
Testing <- as.data.frame(DetectionsCombinedFiltered)
library(ggplot2)

ggplot(Testing) +
  geom_point(mapping=aes(x=i.longitude, y=i.latitude, color=Signal, shape=Tag)) +
#geom_point(mapping=aes(x=LonNode, y=LatNode, size=5)) +
  facet_wrap(~ NodeID)
