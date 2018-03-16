#devtools::install_github('bhaskarvk/leaflet.extras')
library(htmlwidgets) #pour sauvegarder le leaflet
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
#download.file("https://data.cityofchicago.org/api/views/d62x-nvdr/rows.csv?accessType=DOWNLOAD",
              destfile = "crime2017.csv")
rawdata <-read_csv("crime2017.csv") %>%
  rename(CaseNumber = `Case Number`,
         PrimaryType = `Primary Type`,
         LocationDescription = `Location Description`,
         CommunityArea = `Community Area`,
         XCoordinate = `X Coordinate`,
         YCoordinate = `Y Coordinate`,
         FBICode = `FBI Code`,
         UpdatedOn = `Updated On`) 

missing_coords <- rawdata %>% filter(is.na(Latitude)| is.na(Longitude))

crime2017 <- rawdata %>% filter(!is.na(Latitude), !is.na(Longitude)) %>%
  mutate(Date = as.Date(str_sub(Date,start=1,10), "%m/%d/%Y"),
         UpdatedOn = as.Date(str_sub(UpdatedOn,start=1,10), "%m/%d/%Y"))%>%
  st_as_sf(.,coords = c("Longitude","Latitude")) %>%
  st_set_crs(4326) %>% 
  st_transform(., "+proj=longlat +datum=WGS84") 


topzz <- crime2017  %>% as.data.frame() %>% count(PrimaryType) %>% arrange(-n)

mymap <- leaflet(crime2017 )%>% 
  addProviderTiles(providers$Thunderforest.Transport) %>%
  addHeatmap(data= crime2017, blur = 20, max = 100, radius = 9, group ="total") %>%
  addHeatmap(data= crime2017 %>% filter(PrimaryType =="THEFT"), blur = 20, max = 100, radius = 9, group ="theft") %>%  
  addHeatmap(data= crime2017 %>% filter(PrimaryType =="BATTERY"), blur = 20, max = 100, radius = 9, group ="battery") %>%
  addHeatmap(data= crime2017 %>% filter(PrimaryType =="NARCOTICS"), blur = 20, max = 100, radius = 9, group ="narcotics") %>%
  addHeatmap(data= crime2017 %>% filter(PrimaryType =="HOMICIDE"), blur = 20, max = 100, radius = 9, group ="homicide") %>%
  addHeatmap(data= crime2017 %>% filter(PrimaryType =="PROSTITUTION"), blur = 20, max = 100, radius = 9, group ="prostitution") %>%
  addLayersControl(
  baseGroups = c("total",  "theft", "battery", "narcotics", "homicide", "prostitution"),
  options = layersControlOptions(collapsed = FALSE))   


saveWidget(mymap, file = "chicago_crime_2017.html", selfcontained = T)
