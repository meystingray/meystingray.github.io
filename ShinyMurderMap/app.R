library(rdrop2)
library(data.table)
library(ggplot2)
library(dplyr)
library(shiny)
library(leaflet)
library(RSocrata)
library(shinyWidgets)
library(leaflet.extras)

#setwd("C:/Users/sconroy/Documents/meystingray.github.io/ShinyMurderMap")
token <- readRDS("droptoken.rds")

# Download Murder Data
drop_download(path = "Shiny/Murder.RDS",local_path =  paste0(tempdir(), "/MurderData.RDS"),overwrite = TRUE, dtoken = token)
Murder <- readRDS(paste0(tempdir(), "/MurderData.RDS"))

# Download Police Stations
drop_download(path = "Shiny/PoliceStations.RDS",local_path =  paste0(tempdir(), "/PoliceStations.RDS"),overwrite = TRUE, dtoken = token)
PoliceStations <- readRDS(paste0(tempdir(), "/PoliceStations.RDS"))


columnChoices <- c("Year","Watch", "Beat", "Officer_Incident", "Comp_Race", "Comp_Ethnicity", 
       "Comp_Sex", "Status","Victim_Condition", "NumPerYear")


MurderRefresh <- function(Murder) {
    
    lastRefreshDate <- max(Murder$Date,na.rm = TRUE)
    #print(lastRefreshDate)
    refreshString <- paste0("https://www.dallasopendata.com/Public-Safety/Police-Incidents/qv6i-rri7?$where=date1>'",
                            lastRefreshDate,"'")
    
    PI <- read.socrata(refreshString)
    setDT(PI)
    NewMurder <- PI[grepl("Murder",offincident) | grepl("HOMICIDE",offincident) | grepl("Murder",ucr_offense) | 
                        grepl("HOMICIDE",nibrs_crime_category) | grepl("Murder",nibrs_crime),]
    
    if (nrow(NewMurder) > 0) {
        NewMurder[,rowid := 1:.N]
        NewMurder[,Date := as.Date(substr(date1,1,10))]
        NewMurder[,MonthDate := as.Date(paste0(format(Date,"%Y-%m"),"-01"))]
        NewMurder[,WeekNum := strftime(Date, format = "%V")]
        NewMurder <- merge(NewMurder,NewMurder[,head(.SD, 1L),.SDcols = "Date",by = c("servyr","WeekNum")],by = c("servyr","WeekNum"))
        setnames(NewMurder,old = c("Date.x","Date.y"),new = c("Date","WeekDate"))
        
        NewMurder[,LatLongStart := regexpr("(",geocoded_column,fixed = TRUE)[1] + 1,by = rowid]
        NewMurder[,LatLongEnd := regexpr(")",geocoded_column,fixed = TRUE)[1] - 1,by = rowid]
        NewMurder[,LatLong := substr(geocoded_column,start = LatLongStart,stop = LatLongEnd)]
        NewMurder[,LatLongComma := regexpr(",",LatLong,fixed = TRUE)[1],by = rowid]
        NewMurder[,Latitude := substr(LatLong,start = 1,stop = LatLongComma - 1)]
        NewMurder[,Longitude := substr(LatLong,start = LatLongComma + 1,stop = 1000)]
        NewMurder[,Longitude := as.numeric(Longitude)]
        NewMurder[,Latitude := as.numeric(Latitude)]
        NewMurder[,Year := as.factor(servyr)]
        NewMurder$servyr <- NULL
        NewMurder <- NewMurder[!is.na(Latitude) & !is.na(Longitude) & !is.na(Date) & !is.na(Year),]
        
        NewMurder[,c("LatLongStart","LatLongEnd","LatLong","LatLongComma") := NULL]
        
        setnames(NewMurder,old = c("beat","watch","offincident","comprace","compsex","compage","compethnicity","status","victimcond"),
                 c("Beat","Watch","Officer_Incident","Comp_Race","Comp_Sex","Comp_Age","Comp_Ethnicity","Status","Victim_Condition"))
        Murder <- rbindlist(list(Murder,NewMurder),use.names = TRUE,fill = TRUE)
        setorder(Murder,Date)
        Murder[,NumPerYear := 1:.N,by = Year]
        
    }
    #print(max(Murder$Date,na.rm = TRUE))
    Murder
}

ui <- fluidPage(
    fluidRow(
        column(12,titlePanel("Exploring Dallas Murders"))
    ),
    fluidRow(
        column(12,uiOutput("BottomHeader"))
    ),
    p(),
    fluidRow(
        column(4,
            selectInput("FILTER_COLUMN", "Filter By:", choices = columnChoices,selected = "Year"),
            uiOutput("FILTER_VALUE"),
            selectInput("COLOR_COLUMN", "Color By:", choices = columnChoices,selected = "Year"),
            sliderInput('MarkerSize', 'Marker Size', min = 1, max = 10,value = 5),
            sliderInput('NumClusters', '# Clusters \n (reduce Marker Size to view)', min = 0, max = 30,value = 5),
            splitLayout(cellWidths = c("40%", "60%"), #style = "border: 1px solid silver;",
                strong("Show Heat Map"),
                strong("Show Police Stations")
            ),
            splitLayout(cellWidths = c("50%", "50%"),
                switchInput(inputId = "ShowHeatMap",label = NULL,value = FALSE,size = "small"),
                switchInput(inputId = "ShowPoliceStations",label = NULL,value = FALSE,size = "small")
            )
        ),
        column(6,leafletOutput("LeafletMap")),
        column(2,tableOutput("SummaryTable"))
    ),
    fluidRow(
        column(4,
               sliderInput('HIST_BINS', 'Histogram # Bins', min = 10, max = 50,value = 12),
               textOutput('RefreshDate'),
               p(),
               actionButton("RefreshData", "Refresh Data"),
               p(),
               strong(em("'Exploring Dallas Murders'"),p(),"an R-Shiny app by ",
                                a("Sean Conroy",href ='https://www.seantconroy.com/'))
               ),
        column(8,plotOutput("hist"))
        ),
    #fluidRow(
    #    column(12,strong("Exploring Dallas Murders.  An R-Shiny app by ",
    #               a("Sean Conroy",href ='https://www.seantconroy.com/')))
    #)
    
)

#shinyApp(ui = ui, server = server)

# Define server logic required to draw a histogram
url <- a("'Police Incidents'",
         href="https://www.dallasopendata.com/Public-Safety/Police-Incidents/qv6i-rri7")


LegendPal <- colorFactor(
        #colorNumeric(
        palette = 'Spectral', #alpha = TRUE,
        #levels = unique(Murder$Year)
        domain = NULL
)

icons <- awesomeIcons(
    icon = 'building',
    iconColor = 'black',
    library = 'fa'
)

server <- function(input, output) {
    
    MurderData <- reactiveValues()
    MurderData$Murder <- Murder


    pal <- reactive({
        #x <- Murder[,unique(get(input$COLOR_COLUMN))]
        #print(Murder[,unique(get(input$COLOR_COLUMN))])
        colorFactor(
            #colorNumeric(
            palette = 'Spectral', #alpha = TRUE,
            #levels = unique(Murder$Year)
            domain = NULL #Murder$Year
        )
    })
    
    
    output$BottomHeader <- renderUI({
        tagList("Based on Dallas Open Data source:", url,
                ". Note: this is not a complete data set; it appears that a significant # of incidents are missing.")
    })
    
    output$FILTER_VALUE <- renderUI({
        x <- MurderData$Murder %>% select(!!sym(input$FILTER_COLUMN)) %>% arrange(!!sym(input$FILTER_COLUMN))
        selectInput("FILTER_VALUE", label = "Filter Value", choices = c(x,"None"), selected = 'None')
    })
    
    #output$ShowHeatMap <- renderPrint({ input$ShowHeatMap })
    #output$ShowPoliceStations <- renderPrint({ input$ShowPoliceStations })
    
    filtering_string <- reactive ({
    
        if (!is.null(input$FILTER_COLUMN) && !is.null(input$FILTER_VALUE)) {
            paste0("filter(MurderData$Murder, ", input$FILTER_COLUMN, " ", "==", " '", input$FILTER_VALUE, "')")
        } else {
            ""
        }
    })
    
    SummaryTableString <- reactive({
        if (!is.null(input$FILTER_COLUMN) && !is.null(input$FILTER_VALUE) && !(input$FILTER_VALUE == 'None')) {
            paste0("SummaryTable <- MurderData$Murder[",input$FILTER_COLUMN," == '",input$FILTER_VALUE,
                        "',.N,by = '",input$COLOR_COLUMN,"']")
        } else {
            paste0("SummaryTable <- MurderData$Murder[,.N,by = '",input$COLOR_COLUMN,"']")
        }
    })
    
    SummaryTableOrderString <- reactive({
        if (input$COLOR_COLUMN == "Year") {
            paste0("setorder(SummaryTable,-",input$COLOR_COLUMN,")")
        } else {
            paste0("setorder(SummaryTable,-N)")    
        }
        
    })
    
    output$RefreshDate <- renderText({
        paste0("Latest Incident Date: ",max(MurderData$Murder$Date,na.rm = TRUE),"")
        # if (!is.null(input$FILTER_COLUMN) && !is.null(input$FILTER_VALUE)) {
        #     filtering_string()
        # } else {
        #     ""
        # }
    })
    
    observeEvent(input$RefreshData,{
        MurderRefresh(MurderData$Murder)
    })
    

    output$SummaryTable <- renderTable({
        eval(parse(text = SummaryTableString()))
        eval(parse(text = SummaryTableOrderString()))
        #SummaryTable
    })
    
    filtered_table <- reactive({
        
        if (!is.null(input$FILTER_COLUMN) && input$FILTER_VALUE != 'None' && 
            !is.null(input$FILTER_VALUE) && nchar(input$FILTER_COLUMN) > 0 && nchar(input$FILTER_VALUE) > 0) {
            
            eval(parse(text = filtering_string()))
            
        } else {
            MurderData$Murder
        }
    })
    
    output$LeafletMap <- renderLeaflet({
        
        w <- 1
        fOp <- 1
        
        theMap <- leaflet(data = filtered_table()) %>% 
            addProviderTiles(providers$Stamen.TonerLite,options = providerTileOptions(noWrap = TRUE))
        
         if (input$COLOR_COLUMN == "Year") {
         

            theMap <- theMap %>% 
                addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight = w, radius = input$MarkerSize,
                                 fillOpacity = fOp,fillColor = ~pal()(Year)) %>%
                            addLegend(position = "bottomleft",
                                pal = LegendPal, values = ~Year,
                                title = "Legend",
                                opacity = 1)
            
        } else if (input$COLOR_COLUMN == "NumPerYear") {
                
                #print(filtered_table()$Date)
                
                theMap <- theMap %>% 
                    addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight = w, radius = input$MarkerSize,
                                     fillOpacity = fOp,fillColor = ~pal()(NumPerYear)) %>%
                    addLegend(position = "bottomleft",
                              pal = LegendPal, values = ~NumPerYear,
                              title = "Legend",
                              opacity = 1)
                

         } else if (input$COLOR_COLUMN == "Watch") {
            
            theMap <- theMap %>%  
                addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight = w, radius = input$MarkerSize,
                                 fillOpacity = fOp,fillColor = ~pal()(Watch)) %>%
                addLegend(position = "bottomleft",
                          pal = LegendPal, values = ~Watch,
                          title = "Legend",
                          opacity = 1)
            
         } else if (input$COLOR_COLUMN == "Beat") {
             
             theMap <- theMap %>%  
                 addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight = w, radius = input$MarkerSize,
                                  fillOpacity = fOp,fillColor = ~pal()(Beat)) %>%
                 addLegend(position = "bottomleft",
                           pal = LegendPal, values = ~Beat,
                           title = "Legend",
                           opacity = 1)
             
         } else if (input$COLOR_COLUMN == "Officer_Incident") {
             
             theMap <- theMap %>%  
                 addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight = w, radius = input$MarkerSize,
                                  fillOpacity = fOp,fillColor = ~pal()(Officer_Incident)) %>%
                 addLegend(position = "bottomleft",
                           pal = LegendPal, values = ~Officer_Incident,
                           title = "Legend",
                           opacity = 1)
             
         } else if (input$COLOR_COLUMN == "Comp_Race") {
             
             theMap <- theMap %>%  
                 addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight = w, radius = input$MarkerSize,
                                  fillOpacity = fOp,fillColor = ~pal()(Comp_Race)) %>%
                 addLegend(position = "bottomleft",
                           pal = LegendPal, values = ~Comp_Race,
                           title = "Legend",
                           opacity = 1)
             
         } else if (input$COLOR_COLUMN == "Comp_Sex") {
             
             theMap <- theMap %>%  
                 addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight = w, radius = input$MarkerSize,
                                  fillOpacity = fOp,fillColor = ~pal()(Comp_Sex)) %>%
                 addLegend(position = "bottomleft",
                           pal = LegendPal, values = ~Comp_Sex,
                           title = "Legend",
                           opacity = 1)
             
         } else if (input$COLOR_COLUMN == "Comp_Ethnicity") {
             
             theMap <- theMap %>%  
                 addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight = w, radius = input$MarkerSize,
                                  fillOpacity = fOp,fillColor = ~pal()(Comp_Ethnicity)) %>%
                 addLegend(position = "bottomleft",
                           pal = LegendPal, values = ~Comp_Ethnicity,
                           title = "Legend",
                           opacity = 1)
             
         } else if (input$COLOR_COLUMN == "Status") {
             
             theMap <- theMap %>%  
                 addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight = w, radius = input$MarkerSize,
                                  fillOpacity = fOp,fillColor = ~pal()(Status)) %>%
                 addLegend(position = "bottomleft",
                           pal = LegendPal, values = ~Status,
                           title = "Legend",
                           opacity = 1)
             
         } else if (input$COLOR_COLUMN == "Victim_Condition") {
             
             theMap <- theMap %>%  
                 addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight = w, radius = input$MarkerSize,
                                  fillOpacity = fOp,fillColor = ~pal()(Victim_Condition)) %>%
                 addLegend(position = "bottomleft",
                           pal = LegendPal, values = ~Victim_Condition,
                           title = "Legend",
                           opacity = 1)
             
         }
        

        if (input$NumClusters > 0) {
            #KM <- kmeans(Murder[!is.na(Latitude) & !is.na(Longitude),.(Latitude,Longitude)],centers = 12)
            #print(sizeVect)
            
            KM <- kmeans(filtered_table()[!is.na(Latitude) & !is.na(Longitude),.(Latitude,Longitude)],centers = input$NumClusters)
            sizeVect <- round(scale(KM$size,center = 1)*10,digits = 0)
            
            # Plot the clusters
            theMap <- theMap %>%  
                addCircleMarkers(data = as.data.table(KM$centers),
                    lng = ~Longitude, lat = ~Latitude, weight = 1, radius = sizeVect,
                                 fillOpacity = 0.2,fillColor = "yellow")
            
        }
        
        if (input$ShowHeatMap == TRUE) {
            
            theMap <- theMap %>% addHeatmap(lng = ~Longitude, lat = ~Latitude, intensity = NULL,
                    layerId = NULL, group = NULL, minOpacity = 0, max = 0.1,
                        radius = 7, blur = 15, gradient = NULL, cellSize = 5)
        }
        
        if (input$ShowPoliceStations == TRUE) {

            theMap <- theMap %>% addMarkers(data = PoliceStations,lng = ~Longitude, lat = ~Latitude,
                                            popup = ~paste0(station,"<BR>","<BR>",address),
                                            options = popupOptions(closeButton = TRUE),
                                            icon = icons
                                            )
        }
        
        theMap <- theMap %>%
            addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight = 0, radius = input$MarkerSize,
                             fillOpacity = 0,
                             popup = ~paste0("Incident Date: ",Date,
                                             "<BR> Beat: ",Beat,
                                             "<BR> Incident Address: ",incident_address,
                                             "<BR> Victim Sex: ",Comp_Sex,
                                             "<BR> Victim Age: ",Comp_Age,
                                             "<BR> Victim Race: ",Comp_Race,
                                             "<BR> Victim Condition: ",Victim_Condition,
                                             "<BR> Victim Injury: ",victiminjurydesc,
                                             "<BR> Off. Incident: ",Officer_Incident,
                                             "<BR> Signal: ",signal,
                                             "<BR> MO: ",mo))
        
        return(theMap)
        
    })
    
    
    output$hist <- renderPlot({
        
        ggplot(filtered_table(),aes(x = Date)) + geom_histogram(bins = input$HIST_BINS,colour='red',size = 1) +
                stat_bin(bins = input$HIST_BINS,geom="text", colour="black", size=5.5,aes(label=..count..),
                position = position_stack(vjust = 1.15)) +
            theme(text = element_text(size=20),axis.text.x = element_text(size = 15))

    })   
    #}, height = 200)
    
}

# Run the application 
shinyApp(ui = ui, server = server)

