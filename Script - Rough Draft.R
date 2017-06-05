
#Libraries
library(tidyverse)
library(rvest)
library(tilegramsR)
library(sp)
library(sf)
library(leaflet)
library(leaflet.extras)
library(colormap)
library(USAboundaries)
library(USAboundariesData)
library(shiny)
library(rgeos)
library(flexdashboard)
library(rsconnect)
USAboundaries::us_congressional()
?us_congressional
congress <- readxl::read_excel("Congress Data for R.xlsx")
state <- us_congressional(resolution = "high")
sf_NPR1to1 <- sf_NPR1to1
sf_NPR1to1.centers <- sf_NPR1to1.centers

ui <- fluidPage(titlePanel("89th and 90th Congress Demographics by Nation and State"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput(
                      "CongressInput",
                      "Select a Congress",
                      choices = c("89th Congress (1965 - 1967)", "90th Congress (1967 - 1969)"),
                      selected = "89th Congress (1965 - 1967)",
                      multiple = FALSE),
                    uiOutput("stateInput"),
                    selectInput(
                      "partyInput",
                      "Polarization",
                      choices = c("Strict Polarization", "Loose Polarization"),
                      selected = "Strict Polarization",
                      multiple = FALSE
                    )),
                  mainPanel(leafletOutput("Country"),
                            p(),
                            leafletOutput("State"),
                            p()
                  )))


server <- function(input, output) {
  
  output$stateInput <- renderUI({
    selectizeInput(
      "stateInput",
      "State",
      choices = congress$region,
      multiple = FALSE
    )
  })
  
  getLeafletOptions <- function(minZoom, maxZoom, ...) {
    leafletOptions(
      crs = leafletCRS("L.CRS.Simple",
                       proj4def = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
                       resolutions = 1.5^(25:15)),
      minZoom = minZoom, maxZoom = maxZoom,
      dragging = TRUE, zoomControl = TRUE,
      tap = TRUE,
      attributionControl = TRUE , ...)
  }
  
  
  output$Country <- renderLeaflet({
    
    FID <- sf_NPR1to1
    st_geometry(FID) <- NULL
    
    FID <- FID %>%
      select(FID, state)
    
    demo <- if(input$CongressInput == "89th Congress (1965 - 1967)"){
      congress %>%
        filter(`ELECTED 67` == 0) %>%
        group_by(STATE2) %>%
        count(PARTYN) %>%
        mutate(perc = round((n/sum(n))*100))%>%
        select(STATE2, PARTYN, n, perc)
      
    } else {
      congress %>%
        filter(`ELECTED 67` == 1 | `RE-ELECTED 67` == 1) %>%
        group_by(STATE2) %>%
        count(PARTYN) %>%
        mutate(perc = round((n/sum(n))*100))%>%
        select(STATE2, PARTYN, n, perc)
    }
    dem <- demo %>%
      spread(key = PARTYN, value = perc, fill = 0) %>%
      filter(dd > 0 | rr == 100) %>%
      select(STATE2, dd) 
    
    dem1 <- demo %>%
      spread(key = PARTYN, value = perc, fill = 0) %>%
      filter(dd == 100 | rr > 0) %>%
      select(STATE2, rr)
    
    repr <- demo %>%
      spread(key = PARTYN, value = n, fill = 0) %>%
      filter(rr == max(rr))%>%
      select(STATE2, rr)
    
    repd <- demo %>%
      spread(key = PARTYN, value = n, fill = 0) %>%
      filter(dd == max(dd))%>%
      select(STATE2, dd)
    
    cong <- congress %>%
      select(STATE2, region, `URBAN STATE`:`PUBLIC ADMIN STATE`) %>%
      unique()
    
    tlabs <- congress %>%
      filter(!is.na(`89thDISTRICT`)) %>%
      group_by(STATE2) %>%
      mutate(Total = max(`89thDISTRICT`)) %>%
      select(Total, STATE2) %>%
      unique()
    
    sf_NPR1to1 <- left_join(sf_NPR1to1, dem, by = c("state" = "STATE2"))
    sf_NPR1to1 <- left_join(sf_NPR1to1, dem1, by = c("state" = "STATE2"))
    sf_NPR1to1 <- left_join(sf_NPR1to1, tlabs, by = c("state" = "STATE2"))
    sf_NPR1to1 <- left_join(sf_NPR1to1, cong, by = c("state" = "STATE2"))
    sf_NPR1to1 <- left_join(sf_NPR1to1, repr, by = c("state" = "STATE2"))
    sf_NPR1to1 <- left_join(sf_NPR1to1, repd, by = c("state" = "STATE2"))
    
    sf_NPR1to1 <- sf_NPR1to1%>%
      filter(state != "DC")
    sf_NPR1to1.centers <- sf_NPR1to1.centers %>%
      filter(state != "DC")
    
    dem <- right_join(dem, FID, by = c("STATE2" = "state"))
    dem1 <- right_join(dem1, FID, by = c("STATE2" = "state"))
    repd <- right_join(repd, FID, by = c("STATE2" = "state"))
    repr <- right_join(repr, FID, by = c("STATE2" = "state"))
    tlabs <- right_join(tlabs, FID, by = c("STATE2" = "state"))
    cong <- right_join(cong, FID, by = c("STATE2" = "state"))

    pal <- if(input$partyInput == "Strict Polarization"){
      colorBin(c("#3182bd", "#de2d26"),domain= c(0,100),
               bins = 2, pretty = FALSE, reverse = TRUE)
    } else {
      colorBin(c("#3182bd", "#de2d26"),domain= c(0,100),
               bins = 5, pretty = FALSE, reverse = TRUE)
    }
    labels<- sprintf("<p> <strong> %s </strong> <br/> Total Representatives: %0.2f <br/> Democrats: %0.2f (%0.2f%%)<br/> Republicans: %0.2f (%0.2f%%)<br/>Percent of State Union: %0.2f%% <br/> Percent of State Urban: %0.2f%% <br/> Percent of State Farmland: %0.2f%% <br/> Percent of State Manufacturing: %0.2f%% <br/> Percent of State Agriculture: %0.2f%% <br/> Percent of State Home Owner: %0.2f%% <br/> Percent of State Black: %0.2f%% <br/> Median State Income: $%0.2f<p>",
                     cong$region, tlabs$Total, repd$dd, dem$dd, repr$rr, dem1$rr,cong$`UNION STATE`, cong$`URBAN STATE`, cong$`FARM STATE`, cong$`MANU STATE`, cong$`AGRIC STATE`, cong$`OWNER STATE`, cong$`BLACK STATE`, cong$`STATE MED INCO`) %>%
      lapply(htmltools::HTML)
    
    leaflet(
      sf_NPR1to1, #the dataset for the Map Outline and State Position
      options = getLeafletOptions(-2,2.5)) %>%#Zoom Options  
      addPolygons( #Creating the Polygons and the Features of the Polygons
        weight=1.5,color="black", group = 'states', #separation between states
        fillOpacity = .8, opacity = 1, fillColor = pal(dem$dd), #aesthetics of polygons
        highlightOptions = highlightOptions(weight = 4),
        label = labels)%>% #not100%sure
      addLabelOnlyMarkers( #adding the labels
        data=sf_NPR1to1.centers, #need the labels to be in the center of the polygons
        label = ~as.character(state), #what is being put in the center of the polygon
        labelOptions = labelOptions( #label options, need to review
          noHide = 'T', textOnly = T,
          offset=c(-4,-10), textsize = '11px')) %>%
      addLegend(position = "bottomleft",pal = pal, values = dem$dd, 
                title = "Proportion of Democrats")%>%
      setMapWidgetStyle() #sets the maps CSS key/value properties
    
  })

  output$State <- renderLeaflet({
    if(!is.null(input$stateInput)){
      state <- us_congressional(resolution = "high", state = input$stateInput)
      congress <- filter(congress, region == input$stateInput)
    }
    
    state1 <- state %>%
      as.data.frame() %>%
      select(state_name, geoid, cd114fp)
    
    state = st_as_sf(state)
    
    state1 <- transform(state1, cd114fp = as.numeric(cd114fp),
                        geoid = as.numeric(geoid))
    
    congress <- if(input$CongressInput == "89th Congress (1965 - 1967)"){
      congress %>%
        filter(`ELECTED 67` == 0) %>%
        mutate(District = `89thDISTRICT`) 
      
    } else {
      congress %>%
        filter(`ELECTED 67` == 1 | `RE-ELECTED 67` == 1) %>%
        mutate(District = `90th DISTRICT`)
    }
    
    pal <- colorBin(c("#3182bd", "#de2d26"), domain = 0:1,
                    bins = 2, reverse = TRUE, na.color = "#808080")
    
    congress <- left_join(congress, state1, by = c("region" = "state_name"))
    
    congress <- congress %>%
      select(NAME, District,PARTYDEM, PARTYNAME, `TERM START`, `TERM END`, `UNION (District)`:`MED INCOME (District)`, cd114fp) %>%
      filter(District == cd114fp) %>%
      unique()
    
    labels<- sprintf("<p> <strong> District: %s <br/> Party: %s <br/> Representative: %s </strong> <br/> Career Start: %0.0f <br/> Career End: %0.0f <br/>Percent of District Union: %0.2f%% <br/> Percent of District Urban: %0.2f%% <br/> Percent of District Farmland: %0.2f%% <br/> Percent of District Manufacturing: %0.2f%% <br/> Percent of District Agriculture: %0.2f%% <br/> Percent of District Home Owner: %0.2f%% <br/> Percent of District Black: %0.2f%% <br/> Median District Income: $%0.2f<p>",
                     congress$District, congress$NAME, congress$PARTYNAME, congress$`TERM START`, congress$`TERM END`,congress$`UNION (District)`, congress$`URBAN (District)`, congress$`FARM (District)`, congress$`MANU (District)`, congress$`AGRI (District)`, congress$`OWNER (District)`, congress$`BLACK (District)`, congress$`MED INCOME (District)`) %>%
      lapply(htmltools::HTML)
    
    leaflet(
      state, #the dataset for the Map Outline and State Position
      options = getLeafletOptions(3.5,7.5)) %>%#Zoom Options  
      addPolygons( #Creating the Polygons and the Features of the Polygons
        weight=1.5,color="black", group = 'geoid', #separation between states
        fillOpacity = .8, opacity = 1, fillColor = pal(congress$PARTYDEM), #aesthetics of polygons
        highlightOptions = highlightOptions(weight = 4),
        label = labels)
    
    #not100%sure
    
  })
}

shinyApp(ui = ui, server = server)
runApp()
deployApp(appDir = "~/MAPSS UChicago/Spring Quarter/Data Visualization/viz-brianp1/pennington_final_proj.Rmd", appName = "Congressional_Viz")




#Other stuff Code to Write hexagons in a circle depending up the state' distirct size. Not sure what is wrong


func <- function(x){
  df <- congress %>%
    filter(region == "x") %>%
    filter(!is.na(`89thDISTRICT`)) %>%
    mutate(total = max(``))
  mutate(area = 1/max(`89thDISTRICT`)) %>%
    mutate(center = (`89thDISTRICT`/max(`89thDISTRICT`))*2*pi) %>%
    mutate(side = 3^(1/4)*sqrt(2*(area/9))) %>%
    mutate(inner = (sqrt(3)/2)*side) %>%
    mutate(ymax = center + side) %>%
    mutate(ymin = center - side) %>%
    mutate(x1 = center) %>%
    mutate(x2 = center - inner) %>%
    mutate(x3 = center - inner) %>%
    mutate(x4 = center) %>%
    mutate(x5 = center + inner) %>%
    mutate(x6 = center + inner) %>%
    mutate(y1 = center + side) %>%
    mutate(y2 = (center + side) - sqrt((side*side)-(inner * inner)))%>%
    mutate(y3 = (center - side) + sqrt((side*side)-(inner * inner))) %>%
    mutate(y4 = center + side) %>%
    mutate(y5 = (center - side) + sqrt((side*side)-(inner * inner)))%>%
    mutate(y6 = (center + side) - sqrt((side*side)-(inner * inner))) %>% 
    select(NAME, `89thDISTRICT`, x1:y6)
  
  
  y <- congress %>%
    filter(region == 'x') %>%
    filter(!is.na(`89thDISTRICT`)) %>%
    select(`89thDISTRICT`) %>%
    as.vector()
  
  func1 <- function(z){
    coord <- df %>%
      filter(`89thDISTRICT` == z) %>%
      gather(x1:y6, key= "coord", value = "value") %>%
      select(NAME, value) %>%
      group_by(NAME) %>%
      nest()
  }
  
  statedf <- map_df(y, func1)
  
  return(statedf)
}  
#end of other 


