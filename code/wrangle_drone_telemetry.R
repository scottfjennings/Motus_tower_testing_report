


library(tidyverse)
library(here)
library(lubridate)



# see email from AS on 9/17/21 for details about which tag was on which drone 
     

tele_files <- list.files(here("data/drone_telemetry"))


drones_flights_tags <- list.files(here("data/drone_telemetry")) %>% 
  data.frame() %>% 
  rename(file = 1) %>% 
  separate(file, into = c("drone", "date", "time", "am.pm", "ext"), remove = FALSE, sep = " ") %>% 
  mutate(date = gsub("_", "/", date),
         time = gsub("_", ":", time),
         time = paste(time, tolower(am.pm)),
         ts = paste(date, time),
         ts = as.POSIXct(ts, format = "%m/%d/%Y %I:%M:%S %p"),
         date = as.Date(date, format = "%m/%d/%Y")) %>% 
  arrange(drone, ts) %>% 
  group_by(drone, date) %>% 
  mutate(flight.num = row_number()) %>% 
  ungroup() %>% 
  mutate(drone = case_when(grepl("White", drone) ~ "white",
                           grepl("Green", drone) ~ "green"),
         CTT = case_when(date == "2021-09-14" & drone == "green" ~ 1,
                         date == "2021-09-14" & drone == "white" & flight.num == 8 ~ 1,
                         date == "2021-09-15" & drone == "white" ~ 1),
         Lotek = case_when(date == "2021-09-14" & drone == "white" ~ 1,
                           date == "2021-09-15" & drone == "green" & flight.num < 5 ~ 1,
                           date == "2021-09-15" & drone == "white" & flight.num == 5 ~ 1)) %>% 
  arrange(date, drone, ts) %>% 
  select(file, date, drone, flight.num, CTT, Lotek) %>% 
  pivot_longer(cols = c("CTT", "Lotek"), names_to = "mfg") %>% 
  filter(!is.na(value)) %>% 
  select(-value) %>% 
  mutate(flight.height = ifelse(date == "2021-09-14" & ((drone == "green" & flight.num >=4) | (drone == "white" & flight.num >= 5)), "low", "high"))
  



get_tele_file <- function(zfile) {
zzz <- read.csv(here(paste("data/drone_telemetry/", zfile, sep = ""))) %>% 
  mutate(file = zfile)
}


drone_tele <- map_df(tele_files, get_tele_file)




drone_tele_tags <- drone_tele %>% 
  select(time, home_longitude, rc_latitude, longitude, altitude_raw, pitch, roll, heading, home_latitude, rc_longitude, rc_altitude_agl, vertical_speed, latitude, course, takeoff_longitude, takeoff_latitude, ground_speed, gimbal.heading, gimbal.roll, gimbal.pitch, takeoff_altitude, file) %>% 
  separate(time, c("date", "time"), sep = "T") %>% 
  mutate(date.time = paste(date, time),
         date.time = as.POSIXct(date.time, tz = Sys.timezone()),
         date.time.utc = with_tz(date.time, tz = "UTC"),
         date = as.Date(date, format = "%Y-%m-%d")) %>%
  full_join(drones_flights_tags) %>% 
  select(date, drone, mfg, latitude, longitude, everything())  %>% 
  relocate(contains("takeoff"), .after = last_col())%>% 
  relocate(contains("gimbal"), .after = last_col()) %>% 
  relocate(contains("rc"), .after = last_col()) %>% 
  relocate(contains("home"), .after = last_col())

write.csv(drone_tele_tags, here("data/combined_drone_telemetry.csv"))
saveRDS(drone_tele_tags, here("data/combined_drone_telemetry"))
