
#Libraries
library(tidyverse)
library(ggmap)
library(maps)
library(mapdata)
library(rvest)
library(stringr)
library(tilegramsR)
library(sf)
library(leaflet)
library(leaflet.extras)
library(colormap)

#Reading in My Data Set
congress <- readxl::read_excel("Congress Data for R.xlsx")
state <- map_data("state")
#Allows for Zoom Options
getLeafletOptions <- function(minZoom, maxZoom, ...) {
  leafletOptions(
    crs = leafletCRS("L.CRS.Simple"),
    minZoom = minZoom, maxZoom = maxZoom,
    dragging = TRUE, zoomControl = TRUE,
    tap = TRUE,
    attributionControl = TRUE , ...)
}


#1966 Congressional Representatives
demo1 <- congress %>%
  filter(`ELECTED 67` == 0) %>%
  group_by(STATE2) %>%
  count(PARTYN) %>%
  mutate(perc = (n/sum(n)))%>%
  spread(key= PARTYN, value = perc)%>%
  select(STATE2, ds, dn, rr)

#Get Rid of the NAs
demo1[is.na(demo1)] <- 0 

#Merge Southern Democrats and Northern Democrats
demo1 <- demo1 %>%
  mutate(dd = ds + dn) %>%
  select(STATE2, dd, rr)


#My Leaf Color Function (Change Bin Size to Show More Moderates)
pal1 <- colorBin(c("red", "blue"),0:1, bins = 2, 0:1, reverse = TRUE, pretty = FALSE)


#Remove DC from the Polygon Set
sf_NPR1to1 <- sf_NPR1to1%>%
  filter(state != "DC")
sf_NPR1to1.centers <- sf_NPR1to1.centers %>%
  filter(state != "DC")


leaflet(
  sf_NPR1to1, #the dataset for the Map Outline and State Position
  options= getLeafletOptions(-1.5, -.5)) %>% #Zoom Options  
  addPolygons( #Creating the Polygons and the Features of the Polygons
    weight=1.5,color="black", group = 'states', #separation between states
    fillOpacity = .8, opacity = 1, fillColor = pal1(demo1$dd), #aesthetics of polygons
    highlightOptions = highlightOptions(weight = 4)) %>% #not100%sure
  addLabelOnlyMarkers( #adding the labels
    data=sf_NPR1to1.centers, #need the labels to be in the center of the polygons
    label = ~as.character(state), #what is being put in the center of the polygon
    labelOptions = labelOptions( #label options, need to review
      noHide = 'T', textOnly = T,
      offset=c(-4,-10), textsize = '11px')) %>%
  setMapWidgetStyle() #sets the maps CSS key/value properties

##1967 Congress
demo2 <- congress %>%
  filter(`ELECTED 67` == 0) %>%
  group_by(STATE2) %>%
  count(PARTYN) %>%
  mutate(perc = (n/sum(n)))%>%
  spread(key= PARTYN, value = perc)%>%
  select(STATE2, ds, dn, rr)

#Get Rid of the NAs
demo2[is.na(demo2)] <- 0 

#Merge Southern Democrats and Northern Democrats
demo2 <- demo2 %>%
  mutate(dd = ds + dn) %>%
  select(STATE2, dd, rr)


#My Leaf Color Function (Change Bin Size to Show More Moderates)
pal <- colorBin(c("red", "purple", "blue"),0:1, bins = 3, 0:1, reverse = TRUE, pretty = FALSE)
#Bin ChangeD

#Remove DC from the Polygon Set
sf_NPR1to1 <- sf_NPR1to1%>%
  filter(state != "DC")
sf_NPR1to1.centers <- sf_NPR1to1.centers %>%
  filter(state != "DC")


leaflet(
  sf_NPR1to1, #the dataset for the Map Outline and State Position
  options= getLeafletOptions(-1.5, -.5)) %>% #Zoom Options  
  addPolygons( #Creating the Polygons and the Features of the Polygons
    weight=1.5,color="black", group = 'states', #separation between states
    fillOpacity = .8, opacity = 1, fillColor = pal(demo2$dd), #aesthetics of polygons
    highlightOptions = highlightOptions(weight = 4)) %>% #not100%sure
  addLabelOnlyMarkers( #adding the labels
    data=sf_NPR1to1.centers, #need the labels to be in the center of the polygons
    label = ~as.character(state), #what is being put in the center of the polygon
    labelOptions = labelOptions( #label options, need to review
      noHide = 'T', textOnly = T,
      offset=c(-4,-10), textsize = '11px')) %>%
  setMapWidgetStyle() #sets the maps CSS key/value properties

#LOL

st <- state %>%
  group_by(region) %>%
  mutate(ymin = min(lat)) %>%
  mutate(ymax = max(lat)) %>%
  mutate(xmin = min(long)) %>%
  mutate(xmax = max(long)) %>%
  select(region, ymin, ymax, xmin, xmax) %>%
  unique()

ggplot(data = st) +
  geom_rect(aes(ymin = ymin, ymax = ymax, xmin= xmin, xmax= xmax, fill = region), color = "white")+
  coord_fixed(1.3)+
  guides(fill = FALSE)