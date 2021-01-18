library(rdrop2)
library(data.table)
library(ggplot2)
library(dplyr)
library(shiny)
library(leaflet)

token <- readRDS("droptoken.rds")

drop_download(path = "Shiny/Traffic.RDS",local_path =  paste0(tempdir(), "/TrafficData.RDS"),overwrite = TRUE, dtoken = token)
traffic <- readRDS(paste0(tempdir(), "/TrafficData.RDS"))


# PI <- readRDS("C:/Users/sconroy/Documents/DallasPoliceData/PoliceIncidents1-9-21.RDS")
# setDT(PI)
# traffic <- PI[grepl("TRAFFICKING",offincident),]
# traffic[,NumIncidentsPerYear := .N,by = "servyr"]
# 
# traffic[,rowid := 1:.N]
# traffic[,Date := as.Date(substr(date1,1,10))]
# traffic[,LatLongStart := regexpr("(",geocoded_column,fixed = TRUE)[1] + 1,by = rowid]
# traffic[,LatLongEnd := regexpr(")",geocoded_column,fixed = TRUE)[1] - 1,by = rowid]
# traffic[,LatLong := substr(geocoded_column,start = LatLongStart,stop = LatLongEnd)]
# traffic[,LatLongComma := regexpr(",",LatLong,fixed = TRUE)[1],by = rowid]
# traffic[,Latitude := substr(LatLong,start = 1,stop = LatLongComma - 1)]
# traffic[,Longitude := substr(LatLong,start = LatLongComma + 1,stop = 1000)]
# traffic[,Longitude := as.numeric(Longitude)]
# traffic[,Latitude := as.numeric(Latitude)]
# 
# traffic[is.na(Latitude) & incident_address == "14040 N STEMMONS SERV",Latitude := 32.93769989950343]
# traffic[is.na(Longitude) & incident_address == "14040 N STEMMONS SERV",Longitude := -96.90205446873641]
# 
# traffic[is.na(Latitude) & incident_address == "7815 L B J FWY",Latitude := 32.925475281010286]
# traffic[is.na(Longitude) & incident_address == "7815 L B J FWY",Longitude := -96.77161085979215]
# 
# traffic[,Year := as.factor(servyr)]
# 
# keepCols <- c("incidentnum", "servyr", "Date", "Year", "watch", "signal", "offincident", "premise", "incident_address", "apt", "ra", "beat", "division", "sector", "district",
# "involvement", "victimtype", "comprace", "compethnicity", "compsex", "compage", "followup1", "followup2", "status", "ucr_disp", "victiminjurydesc", "victimcond", "mo",
# "Latitude","Longitude","NumIncidentsPerYear")
# traffic <- traffic[,..keepCols]

keepCols <- c("incidentnum", "Date", "watch", "signal", "offincident", "premise", "incident_address", "apt", "ra", "beat", "division", "sector", "district",
              "involvement", "victimtype", "comprace", "compethnicity", "compsex", "compage", "followup1", "followup2", "status", "ucr_disp", "victiminjurydesc", "victimcond", "mo")


ui <- fluidPage(
    titlePanel("Exploring Dallas Trafficking Since 2014"),
    tabsetPanel(
        tabPanel("Map",
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput('MarkerSize', 'Marker Size', min = 1, max = 10,value = 5),
                         sliderInput('MarkerOpacity', 'Marker Opacity', min = 0, max = 1,value = 1),
                         width = 4
                        ),
                     mainPanel(
                         leafletOutput("LeafletMap",height = 700)
                        )
                     )
                 ),
        tabPanel("Plot",mainPanel(
                 plotOutput("YearlyPlot"),
                 plotOutput("Histogram")
                 )),
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
    
    output$LeafletMap <- renderLeaflet({
    
        # input <- list()
        # input$MarkerSize <- 5
        # input$MarkerOpacity <- 1
        
        leaflet(data = traffic) %>% 
            addProviderTiles(providers$Stamen.TonerLite,options = providerTileOptions(noWrap = TRUE)) %>% 
                addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight = 2, radius = input$MarkerSize,
                                 fillOpacity = input$MarkerOpacity,fillColor = ~pal(Year),color = "gray",stroke = 1,
                                 popup = ~paste0("Incident Date: ",traffic$Date,
                                                 ",<BR> Incident Address: ",incident_address,
                                                 ",<BR> Victim Sex: ",traffic$compsex,
                                                ",<BR> Victim Age: ",traffic$compage,",<BR> MO: ",traffic$mo)) %>%
                addLegend(position = "bottomleft",
                          pal = pal, values = ~traffic$Year,
                          title = "Legend",
                          opacity = 1)
    })
    
    output$YearlyPlot <- renderPlot({

        ggplot(data = traffic,aes(x = servyr,y = NumIncidentsPerYear)) + geom_smooth() + geom_point() + 
            ggtitle("Reported Trafficking Incidents Per Year")
        
    })
    
    output$Histogram <- renderPlot({
        
        ggplot(data = traffic,aes(x = servyr)) + geom_histogram(bins = 6,colour="grey20", lwd=0.2,fill = "orange") + 
            stat_bin(bins = 6,geom="text", colour="black", size=5.5,aes(label=..count..),
                     position=position_stack(vjust=1.055)) +
            ggtitle("Reported Trafficking Incidents Per Year")
        
    })
    
    output$Table <- renderTable({
        
        printTable <- traffic[,..keepCols]
        printTable[,Date := as.character(Date)]
    })
    
}

shinyApp(ui = ui, server = server)
    