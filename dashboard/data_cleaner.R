library(tidyverse)
library(lubridate)
library(dplyr)



# change this to the directory to csv data sets. This assumes working directory is already set to dashboard file
dir <- file.path(getwd(), "data")

#read YSI in
d.ysi <- read.csv(file.path(dir,"YSI.csv"))[,-2]

#transform YSI data to relevant type
d.ysi <- transform(d.ysi,
               date = as.Date(date, "%m/%d/%Y"),
               turbidity = as.numeric(turbidity),
               site = as.character(site))


#find mean of all surface SS stations
d.ysi.2 <- d.ysi %>% 
  filter(depth == 0.5, !site  %in% c("in1","in2","doc","shc","sho")) %>% 
  select(-depth, -Latitude, -Longitude, -Barometer..mmHg., -sigmat, -sigma, -orp, -ODO_sat, -tds) %>% 
  group_by(site) %>% 
  summarise_all(list(mean), na.rm = TRUE) %>% 
  ungroup() %>% 
  select(-date)
colnames(d.ysi.2)[1] <- "Station"
#make station names uppercase, this is purely for aesthetic
d.ysi.2$Station <- toupper(d.ysi.2$Station)


#read it in
d.photo <- read.csv(file.path(dir,"photometer.csv"))[,c(-3,-10)]

#transform data to relevant type
d.photo <- transform(d.photo,
               Date = as.Date(Date, "%m/%d/%Y"),
               Sample.ID = as.character(Sample.ID),
               Sulphate = as.numeric(Sulphate))
colnames(d.photo)[1:2] <- c("date","Station")

#find mean of all surface SS stations
d.photo.2 <- d.photo %>% 
  select(-Phosphate.LR) %>% #remove low range phosphate
  group_by(date, Station) %>% 
  summarise_all(list(mean), na.rm = TRUE) %>% 
  ungroup() %>% 
  select(-date) %>% 
  group_by(Station) %>% 
  summarise_all(list(mean), na.rm = TRUE) %>% 
  ungroup() %>% 
  rename("Phosphate" = "Phosphate.HR")
#make station names uppercase, this is purely for aesthetic
d.photo.2$Station <- toupper(d.photo.2$Station)


#Create a dataframe with the Lat & Long for each site 
stat <- data.frame(Station = c("SS1", "SS2", "SS3", "SS4", "SS5", "SS6", "SS7", "SS8", "SS9", "IN1", "IN2"), 
                   Latitude = c(33.4697739, 33.4766202, 33.4834556, 33.4902637, 33.4964727, 33.4971163, 33.4909537,33.4659915, 33.4622206, 33.4407619, 33.44251),
                   Longitude = c(-116.0254881, -116.032482, -116.0394782, -116.0464692,-116.0459624,  -116.0535395, -116.0541673, -116.0351766, -116.0450425, -116.0439693, -116.04197))

#merge locations with data
d.all <- stat %>% 
  left_join(., d.ysi.2, by = "Station") %>% 
  left_join(., d.photo.2, by = "Station") 


save(d.ysi, d.photo, d.all, file = "data/data.Rdata")


