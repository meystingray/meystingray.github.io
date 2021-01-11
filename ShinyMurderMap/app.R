library(rdrop2)
library(data.table)
library(ggplot2)
library(dplyr)
library(shiny)
library(leaflet)


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

colorChoices <- 
    c( "Year","Watch", "Officer Incident", "Comp Race", "Comp Ethnicity", 
       "Comp Sex", "Status","Victim Condition")

ui <- fluidPage(
    titlePanel("Exploring Dallas Murders Since 2014"),
    sidebarLayout(
        sidebarPanel(
            #column(4, selectInput("CONDITION", "Boolean", choices = c("==", "!=", ">", "<"))),
            selectInput("FILTER_COLUMN", "Filter By:", choices = colnames(Murder),selected = "Year"),
            uiOutput("FILTER_VALUE"),
            selectInput("COLOR_COLUMN", "Color By:", choices = colorChoices,selected = "Year"),
            sliderInput('MarkerSize', 'Marker Size', min = 1, max = 5,value = 3),
            sliderInput('HIST_BINS', 'Histogram # Bins', min = 10, max = 50,value = 12)
        ),
        mainPanel(
            verbatimTextOutput("as_text"),
            #tableOutput("the_data"),
            leafletOutput("LeafletMap"),
            p(),
            plotOutput("hist")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$FILTER_VALUE <- renderUI({
        x <- Murder %>% select(!!sym(input$FILTER_COLUMN)) %>% arrange(!!sym(input$FILTER_COLUMN))
        selectInput("FILTER_VALUE", label = "Value", choices = c(x,"None"), selected = 'None')
    })
    
    filtering_string <- reactive ({
        if (!is.null(input$FILTER_COLUMN) && !is.null(input$FILTER_VALUE)) {
            paste0("filter(Murder, ", input$FILTER_COLUMN, " ", "==", " '", input$FILTER_VALUE, "')")
        } else {
            ""
        }
    })
    
    output$as_text <- renderText({
        if (!is.null(input$FILTER_COLUMN) && !is.null(input$FILTER_VALUE)) {
            filtering_string()
        } else {
            ""
        }
    })
    
    # output$the_data <- renderTable({
    #     eval(parse(text = filtering_string()))
    # })
    
    filtered_table <- reactive({
        if (!is.null(input$FILTER_COLUMN) && input$FILTER_VALUE != 'None' && 
            !is.null(input$FILTER_VALUE) && nchar(input$FILTER_COLUMN) > 0 && nchar(input$FILTER_VALUE) > 0) {
            
            eval(parse(text = filtering_string()))
            
        } else {
            Murder
        }
    })
    

    # points <- eventReactive(input$recalc, {
    # 
    #     cbind(filtered_table()$Latitude, filtered_table()$Latitude)
    # 
    # }, ignoreNULL = TRUE)
    
    
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
        
        w <- 1
        fOp <- 1
        
        theMap <- leaflet(data = filtered_table()) %>% 
            addProviderTiles(providers$Stamen.TonerLite,options = providerTileOptions(noWrap = TRUE))
        
         if (input$COLOR_COLUMN == "Year") {
         
            theMap <- theMap %>% 
                addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight = w, radius = input$MarkerSize,
                                 fillOpacity = fOp,fillColor = ~pal()(Year)) %>%
                addLegend(position = "bottomleft",
                           pal = pal(), values = Year,
                           title = "Legend",
                           opacity = .5)
            
         } else if (input$COLOR_COLUMN == "Watch") {
            
            theMap <- theMap %>%  
                addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight = w, radius = input$MarkerSize,
                                 fillOpacity = fOp,fillColor = ~pal()(watch))
              
         } else if (input$COLOR_COLUMN == "Officer Incident") {
             
             theMap <- theMap %>%  
                 addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight = w, radius = input$MarkerSize,
                                  fillOpacity = fOp,fillColor = ~pal()(offincident))
             
         } else if (input$COLOR_COLUMN == "Comp Race") {
             
             theMap <- theMap %>%  
                 addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight = w, radius = input$MarkerSize,
                                  fillOpacity = fOp,fillColor = ~pal()(comprace))
             
         } else if (input$COLOR_COLUMN == "Comp Sex") {
             
             theMap <- theMap %>%  
                 addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight = w, radius = input$MarkerSize,
                                  fillOpacity = fOp,fillColor = ~pal()(compsex))
             
         } else if (input$COLOR_COLUMN == "Comp Ethnicity") {
             
             theMap <- theMap %>%  
                 addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight = w, radius = input$MarkerSize,
                                  fillOpacity = fOp,fillColor = ~pal()(compethnicity))
             
         } else if (input$COLOR_COLUMN == "Status") {
             
             theMap <- theMap %>%  
                 addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight = w, radius = input$MarkerSize,
                                  fillOpacity = fOp,fillColor = ~pal()(status))
             
         } else if (input$COLOR_COLUMN == "Victim Condition") {
             
             theMap <- theMap %>%  
                 addCircleMarkers(lng = ~Longitude, lat = ~Latitude, weight = w, radius = input$MarkerSize,
                                  fillOpacity = fOp,fillColor = ~pal()(victimcond))
             
         }
        
        
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
            hist(vect, breaks = bins, col = "#75AADB", xlab = NULL, #border = "white",
                 ylab = "# Murders (filtered)",
                 main = "Histogram of Filtered Murders")

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
