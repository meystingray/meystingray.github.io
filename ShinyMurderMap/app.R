library(rdrop2)
library(data.table)
library(ggplot2)
library(dplyr)
library(shiny)
library(leaflet)
library(RSocrata)


# drop_read_rds <- function(file, dest = tempdir(), dtoken = get_dropbox_token(), ...) {
#      localfile = paste0(dest, "/", basename(file))
#      drop_download(file, local_path = localfile, overwrite = TRUE, dtoken = dtoken)
#      readRDS(localfile, ...)
# }


token <- readRDS("droptoken.rds")

drop_download(path = "Shiny/Murder.RDS",local_path =  paste0(tempdir(), "/MurderData.RDS"),overwrite = TRUE, dtoken = token)
Murder <- readRDS(paste0(tempdir(), "/MurderData.RDS"))

#Murder <- readRDS("C:/Users/sconroy/Documents/DallasPoliceData/Murder.RDS")
#Murder <- drop_read_rds("Shiny/Murder.RDS",dtoken = token)

#setwd("C:/Users/sconroy/Documents/meystingray.github.io")


Murder[,rowid := 1:.N]
Murder[,Date := as.Date(substr(date1,1,10))]
Murder[,MonthDate := as.Date(paste0(format(Date,"%Y-%m"),"-01"))]
Murder[,WeekNum := strftime(Date, format = "%V")]
Murder <- merge(Murder,Murder[,head(.SD, 1L),.SDcols = "Date",by = c("servyr","WeekNum")],by = c("servyr","WeekNum"))
setnames(Murder,old = c("Date.x","Date.y"),new = c("Date","WeekDate"))

Murder[,LatLongStart := regexpr("(",geocoded_column,fixed = TRUE)[1] + 1,by = rowid]
Murder[,LatLongEnd := regexpr(")",geocoded_column,fixed = TRUE)[1] - 1,by = rowid]
Murder[,LatLong := substr(geocoded_column,start = LatLongStart,stop = LatLongEnd)]
Murder[,LatLongComma := regexpr(",",LatLong,fixed = TRUE)[1],by = rowid]
Murder[,Latitude := substr(LatLong,start = 1,stop = LatLongComma - 1)]
Murder[,Longitude := substr(LatLong,start = LatLongComma + 1,stop = 1000)]
Murder[,Longitude := as.numeric(Longitude)]
Murder[,Latitude := as.numeric(Latitude)]
Murder[,Year := as.factor(servyr)]
Murder$servyr <- NULL
Murder <- Murder[!is.na(Latitude) & !is.na(Longitude) & !is.na(Date) & !is.na(Year),]

Murder[,c("LatLongStart","LatLongEnd","LatLong","LatLongComma") := NULL]

setnames(Murder,old = c("watch","offincident","comprace","compsex","compage","compethnicity","status","victimcond"),
         c("Watch","Officer_Incident","Comp_Race","Comp_Sex","Comp_Age","Comp_Ethnicity","Status","Victim_Condition"))

columnChoices <- c("Year","Watch", "Officer_Incident", "Comp_Race", "Comp_Ethnicity", 
       "Comp_Sex", "Status","Victim_Condition")

LegendPal <- colorFactor(
    #colorNumeric(
    palette = 'Spectral', #alpha = TRUE,
    #levels = unique(Murder$Year)
    domain = NULL
)


ui <- fluidPage(
    fluidRow(
        column(12,titlePanel("Exploring Dallas Murders Since 2014"))
    ),
    fluidRow(
        column(4,
            selectInput("FILTER_COLUMN", "Filter By:", choices = columnChoices,selected = "Year"),
            uiOutput("FILTER_VALUE"),
            selectInput("COLOR_COLUMN", "Color By:", choices = columnChoices,selected = "Year"),
            sliderInput('MarkerSize', 'Marker Size', min = 1, max = 10,value = 5),
            sliderInput('FillOpacity', 'Fill Opacity', min = 0, max = 1,value = 1)
        ),
        column(6,leafletOutput("LeafletMap")),
        column(2,tableOutput("SummaryTable"))
    ),
    fluidRow(
        column(4,
               sliderInput('HIST_BINS', 'Histogram # Bins', min = 10, max = 50,value = 12),
               actionButton("RefreshData", "Refresh Data")
               ),
        column(8,plotOutput("hist"))
        )
)

#shinyApp(ui = ui, server = server)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$FILTER_VALUE <- renderUI({
        x <- MurderData$Murder %>% select(!!sym(input$FILTER_COLUMN)) %>% arrange(!!sym(input$FILTER_COLUMN))
        selectInput("FILTER_VALUE", label = "Value", choices = c(x,"None"), selected = 'None')
    })
    

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
    
    output$as_text <- renderText({
        paste0("Latest Incident Date: ",max(MurderData$Murder$Date,na.rm = TRUE),"")
        # if (!is.null(input$FILTER_COLUMN) && !is.null(input$FILTER_VALUE)) {
        #     filtering_string()
        # } else {
        #     ""
        # }
    })
    
    # eventReactive(input$RefreshData,{
    #     print("Refreshing")
    #     MurderRefresh()
    # })
    observeEvent(input$RefreshData,{
        print("Refreshing")
        MurderRefresh()
    })
    
    MurderData <- reactiveValues()
    MurderData$Murder <- Murder
    
    MurderRefresh <- function() {
        
        lastRefreshDate <- max(Murder$Date,na.rm = TRUE)
        
        refreshString <- paste0("https://www.dallasopendata.com/Public-Safety/Police-Incidents/qv6i-rri7?$where=date1>'",
                                lastRefreshDate,"'")
        
        PI <- read.socrata(refreshString)
        setDT(PI)
        NewMurder <- PI[grepl("NewMurder",offincident) | grepl("HOMICIDE",offincident) | grepl("NewMurder",ucr_offense) | 
                         grepl("HOMICIDE",nibrs_crime_category) | grepl("NewMurder",nibrs_crime),]
        
        
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
            
            setnames(NewMurder,old = c("watch","offincident","comprace","compsex","compage","compethnicity","status","victimcond"),
                     c("Watch","Officer_Incident","Comp_Race","Comp_Sex","Comp_Age","Comp_Ethnicity","Status","Victim_Condition"))
            MurderData$Murder <- rbindlist(list(MurderData$Murder,NewMurder),use.names = TRUE,fill = TRUE)
        }
        #Murder
    }
    
    output$SummaryTable <- renderTable({
        eval(parse(text = SummaryTableString()))
    })
    
    filtered_table <- reactive({
        
        if (!is.null(input$FILTER_COLUMN) && input$FILTER_VALUE != 'None' && 
            !is.null(input$FILTER_VALUE) && nchar(input$FILTER_COLUMN) > 0 && nchar(input$FILTER_VALUE) > 0) {
            
            eval(parse(text = filtering_string()))
            
        } else {
            MurderData$Murder
        }
    })
    


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
    

    output$LeafletMap <- renderLeaflet({
        
        w <- input$FillOpacity
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
                
         } else if (input$COLOR_COLUMN == "Watch") {
            
            theMap <- theMap %>%  
                addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight = w, radius = input$MarkerSize,
                                 fillOpacity = fOp,fillColor = ~pal()(Watch)) %>%
                addLegend(position = "bottomleft",
                          pal = LegendPal, values = ~Watch,
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
        
        theMap <- theMap %>%
            addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight = 0, radius = input$MarkerSize,
                             fillOpacity = 0,
                             popup = ~paste0("Incident Date: ",Date,
                                             "<BR> Incident Address: ",incident_address,
                                             "<BR> Victim Sex: ",Comp_Sex,
                                             "<BR> Victim Age: ",Comp_Age,
                                             "<BR> Victim Race: ",Comp_Race,
                                             "<BR> Victim Condition: ",Victim_Condition,
                                             "<BR> MO: ",mo))
        
        
    })
    
    
    output$hist <- renderPlot({

        if (!is.null(input$FILTER_COLUMN) && input$FILTER_VALUE != 'None' &&
            !is.null(input$FILTER_VALUE) && nchar(input$FILTER_COLUMN) > 0 && nchar(input$FILTER_VALUE) > 0) {

            #print(paste0("Murder %>% filter(",input$COLUMN," == '",input$VALUE,
            #             "') %>% pull(MonthDate)"))

            vect <- eval(parse(text = paste0("Murder %>% filter(",input$FILTER_COLUMN," == '",input$FILTER_VALUE,
                                             "') %>% pull(MonthDate)")))

        } else {
            vect <- Murder$MonthDate
        }

        #print(vect)
        #if (is.numeric(vect)) {

            bins <- seq(min(vect), max(vect), length.out = input$HIST_BINS + 1)
            hist <- tryCatch(
                        hist(vect, breaks = bins, col = "#75AADB", xlab = NULL, #border = "white",
                            ylab = "# Murders (filtered)",
                            main = "Histogram of Filtered Murders"),
                        error = function(e) {
                            vect <- Murder[,MonthDate]
                            bins <- seq(min(vect), max(vect), length.out = input$HIST_BINS + 1)
                            hist(vect, breaks = bins, col = "#75AADB", xlab = NULL, #border = "white",
                                 ylab = "# Murders (filtered)",
                                 main = "Histogram of Filtered Murders")
                        })

    }, height = 200)
    
}

# Run the application 
shinyApp(ui = ui, server = server)

# input <- list()
# input$FILTER_COLUMN <- "servyr"
# input$FILTER_VALUE <- "None"
# input$COLOR_COLUMN <- "watch"
# input$MarkerSize <- 2
# filterPiece <- ""
# colors <- eval(parse(text = paste0("Murder %>% ",filterPiece,"pull(",input$COLOR_COLUMN,")")))
# 
# map <- eval(parse(text = paste0("ggmap(DallasZoom11,extent = 'device') + geom_point(data = Murder, aes(x = Longitude, y = Latitude,fill = as.factor(",input$COLOR_COLUMN,")),size = ",input$MarkerSize,",color = 'black',stroke = 1,shape = 21) + labs(fill = '",input$COLOR_COLUMN,"')")))
# print(map)


# output$Map <- renderPlot({
#     
#     conditions <- rep(FALSE,5)
#     conditions[1] <- !is.null(input$FILTER_COLUMN)
#     conditions[2] <- !is.null(input$FILTER_VALUE)
#     conditions[3] <- !is.null(input$COLOR_COLUMN)
#     conditions[4] <- nchar(input$FILTER_VALUE) > 0
#     conditions[5] <- ifelse(!is.null(input$FILTER_VALUE),input$FILTER_VALUE != 'None',FALSE)
#     
#     filterPiece <- ""
#     if (all(conditions)) {
#         filterPiece <- paste0("filter(",input$FILTER_COLUMN," == '",input$FILTER_VALUE,"') %>% ")
#         #print(filterPiece)
#     }
#     
#     if (!is.null(input$COLOR_COLUMN) && input$COLOR_COLUMN != 'None') {
#         
#         colors <- eval(parse(text = paste0("Murder %>% ",filterPiece,"pull(",input$COLOR_COLUMN,")")))
#         
#         if (length(unique(colors)) > 25) {
#             NoColor <- TRUE
#         } else {
#             NoColor <- FALSE
#         }
#         
#     }
#     
#     #cat("\n","No COLOR",NoColor)
#     
#     if (NoColor == FALSE) {
#         mapString <- paste0(
#             "ggmap(DallasZoom11,extent = 'device') + geom_point(data = filtered_table(), aes(x = Longitude, y = Latitude,fill = as.factor(",
#             input$COLOR_COLUMN,")),size = ",input$MarkerSize,",color = 'black',stroke = 1,shape = 21) + labs(fill = '",
#             input$COLOR_COLUMN,"')")
#         
#         #print(mapString)
#         map <- eval(parse(text = mapString))
#         
#     } else {
#         
#         map <- ggmap(DallasZoom11,extent = "device") + 
#             geom_point(data = filtered_table(), aes(x = Longitude, y = Latitude),fill = "gray",
#                        size = input$MarkerSize,color = "black",stroke = 1,shape = 21)
#         
#     }
#     
#     print(map)
#     
# })
