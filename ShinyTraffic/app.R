library(rdrop2)
library(data.table)
library(ggplot2)
library(dplyr)
library(shiny)
library(leaflet)



traffic <- PI[grepl("TRAFFICKING",offincident),]
traffic[,NumIncidentsPerYear := .N,by = "servyr"]

traffic[,rowid := 1:.N]
traffic[,Date := as.Date(substr(date1,1,10))]
traffic[,LatLongStart := regexpr("(",geocoded_column,fixed = TRUE)[1] + 1,by = rowid]
traffic[,LatLongEnd := regexpr(")",geocoded_column,fixed = TRUE)[1] - 1,by = rowid]
traffic[,LatLong := substr(geocoded_column,start = LatLongStart,stop = LatLongEnd)]
traffic[,LatLongComma := regexpr(",",LatLong,fixed = TRUE)[1],by = rowid]
traffic[,Latitude := substr(LatLong,start = 1,stop = LatLongComma - 1)]
traffic[,Longitude := substr(LatLong,start = LatLongComma + 1,stop = 1000)]
traffic[,Longitude := as.numeric(Longitude)]
traffic[,Latitude := as.numeric(Latitude)]

traffic[is.na(Latitude) & incident_address == "14040 N STEMMONS SERV",Latitude := 32.93769989950343]
traffic[is.na(Longitude) & incident_address == "14040 N STEMMONS SERV",Longitude := -96.90205446873641]

traffic[is.na(Latitude) & incident_address == "7815 L B J FWY",Latitude := 32.925475281010286]
traffic[is.na(Longitude) & incident_address == "7815 L B J FWY",Longitude := -96.77161085979215]

traffic[,Year := as.factor(servyr)]

keepCols <- c("incidentnum", "servyr", "Year", "watch", "signal", "offincident", "premise", "incident_address", "apt", "ra", "beat", "division", "sector", "district",
"involvement", "victimtype", "comprace", "compethnicity", "compsex", "compage", "followup1", "followup2", "status", "ucr_disp", "victiminjurydesc", "victimcond", "mo",
"Latitude","Longitude","NumIncidentsPerYear")
traffic <- traffic[,..keepCols]

ui <- fluidPage(
    titlePanel("Exploring Dallas Tracking Since 2014"),
    tabsetPanel(
        tabPanel("Map",
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput('MarkerSize', 'Marker Size', min = 1, max = 10,value = 5),
                         sliderInput('MarkerOpacity', 'Marker Opacity', min = 0, max = 1,value = 1),
                         width = 4
                        ),
                     mainPanel(
                         leafletOutput("LeafletMap")
                        )
                     ),
                 ),
        tabPanel("Plot",plotOutput("YearlyPlot")),
        tabPanel("Data",tableOutput("Table"))
    )
)


server <- function(input, output) {

    pal <- colorFactor(
        #colorNumeric(
        palette = 'Spectral', #alpha = TRUE,
        #levels = unique(Murder$Year)
        domain = traffic$Year
    )
    
    content <- paste(sep = "<br/>",
                     "<b><a href='http://www.samurainoodle.com'>Samurai Noodle</a></b>",
                     "606 5th Ave. S",
                     "Seattle, WA 98138"
    )
    
    output$LeafletMap <- renderLeaflet({
    
        leaflet(data = traffic) %>% 
            addProviderTiles(providers$Stamen.TonerLite,options = providerTileOptions(noWrap = TRUE)) %>% 
                addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight = 2, radius = input$MarkerSize,
                                 fillOpacity = input$MarkerOpacity,fillColor = ~pal(Year),color = "gray",stroke = 1,
                                 popup = !traffic$mo) %>%
                addLegend(position = "bottomleft",
                          pal = pal, values = ~traffic$Year,
                          title = "Legend",
                          opacity = 1)
    })
    
    output$YearlyPlot <- renderPlot({

        ggplot(data = traffic,aes(x = servyr,y = NumIncidentsPerYear)) + geom_smooth() + geom_point() + 
            ggtitle("Reported Tracking Incidents Per Year")
        
    })
    
    output$Table <- renderTable({
        
        traffic
        
    })
    
}

shinyApp(ui = ui, server = server)
    