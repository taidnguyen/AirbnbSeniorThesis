#https://rpubs.com/jhofman/nycmaps

library(tigris)
library(dplyr)
library(leaflet)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)

#import listing
nyc1  <- read.csv("~/Desktop/SeniorThesis/Data/AirbnbListing/NYC/nyc2015-01-01.csv")
nyc42 <- read.csv("~/Desktop/SeniorThesis/Data/AirbnbListing/NYC/nyc2018-12-06.csv")

#5 boroughs: Bronx, Brooklyn, Manhattan, Queens, Staten Island
#Manhattan - New York County.
#Bronx - Bronx County.
#Brooklyn - Kings County.
#Queens - Queens County.
#Staten Island - Richmond County.
#find code for the counties
lookup_code("New York", "New York") #36, 061
lookup_code("New York", "Bronx") #36, 005
lookup_code("New York", "Kings") #36, 047
lookup_code("New York", "Queens") #36, 081
lookup_code("New York", "Richmond") #36, 085

#In January 1st 2015
nyc_tracts <- tracts(state = '36', county = c('061','047','081','005','085'))
summary(nyc_tracts)
plot(nyc_tracts)
points(nyc1$longitude,nyc1$latitude, col="red",cex=0.05)

#In December 16th, 2018
nyc_tracts <- tracts(state = '36', county = c('061','047','081','005','085'))
summary(nyc_tracts)
plot(nyc_tracts)
points(nyc42$longitude,nyc42$latitude, col="red",cex=0.05)

#########################################################################################################
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)
summary(nyc_neighborhoods) ##neighborthood shapefiles available as GEOJSON file
nyc_neighborhoods_df <- tidy(nyc_neighborhoods)

#In January 1st, 2015
ggplot() + 
  geom_polygon(data=nyc_neighborhoods_df, aes(x=long, y=lat, group=group), colour="black", fill=NA) +
  geom_point(data=nyc1, aes(x=longitude,y=latitude), color="red", size=0.1, alpha=0.4) +
  theme(panel.background = element_blank()) ##blank background 

#In December 16th, 2018
ggplot() + 
  geom_polygon(data=nyc_neighborhoods_df, aes(x=long, y=lat, group=group), colour="black", fill=NA) +
  geom_point(data=nyc42, aes(x=longitude,y=latitude), color="red", size=0.1, alpha=0.4) +
  theme(panel.background = element_blank()) ##blank background




