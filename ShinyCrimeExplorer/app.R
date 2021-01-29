library(rdrop2)
library(data.table)
#library(ggplot2)
library(shiny)
library(leaflet)
library(RSocrata)
#library(shinyWidgets)
#library(leaflet.extras)
library(dygraphs)
library(prophet)
library(leafgl)
library(colourvalues)
library(sf)
library(dplyr)

#setwd("C:/Users/sconroy/Documents/meystingray.github.io/ShinyCrimeExplorer")
token <- readRDS("droptoken.rds")

# Download Murder Data
drop_download(path = "Shiny/CleanPI.RDS",local_path =  paste0(tempdir(), "/CleanPI.RDS"),overwrite = TRUE, dtoken = token)
CleanPI <- readRDS(paste0(tempdir(), "/CleanPI.RDS"))
setDT(CleanPI)

CleanPI[,c("offincident","offensecode","signal","sector") := NULL]
CleanPI <- CleanPI[Year %in% c("2014","2015","2016","2017","2018","2019","2020","2021"),]
CleanPI[,division := toupper(division)]
MapPI <- CleanPI[!is.na(Latitude) & !is.na(Longitude) & (Latitude < 33) & (Latitude > 32.5) & (Longitude < -96) & (Longitude > -97),]
MapPI <- st_as_sf(MapPI,coords = c("Longitude","Latitude"),crs = 4326)


#filterCols <- c("Year", "Type", "offensecode", "signal", "beat", "division", "sector")

ui <- fluidPage(title = "Forecasting Dallas Crime Rates",
    titlePanel("Forecasting Dallas Crime Rates"),
    p(),
    fluidRow(
        column(4,
               selectInput("DIVISION_FILTER", "Select Division:", choices = c('All',unique(CleanPI$division))),
               selectInput("TYPE_FILTER", "Select Crime Type:", choices = c('All',unique(CleanPI$Type))),
               #selectInput("FILTER_COLUMN", "Filter by:", filterCols),
               #uiOutput("FILTER_VALUE")
               
        ),
        column(8,leafletOutput("LeafletMap"))
    ),
    fluidRow(
        column(4,
               sliderInput('ProphetPeriods', 'Forecast # Periods', min = 10, max = 1000,value = 365),

               strong(em("'Forecasting Dallas Crime Rates'"),p(),"an R-Shiny app by ",
                      a("Sean Conroy",href ='https://www.seantconroy.com/'))
        ),
        p(),
        column(8,dygraphOutput("Forecast"))
    ),
    fluidRow(
        column(12,
               strong(em("Forecasting using Facebook's ",a("Prophet package",href ='https://facebook.github.io/prophet/')))
        )
     
    )
    
)

# input <- list()
# input$DIVISION_FILTER <- "CENTRAL"
# input$TYPE_FILTER <- "THEFT"


server <- function(input, output, session) {

    # output$FILTER_VALUE <- renderUI({
    #         x <- CleanPI %>% select(!!sym(input$FILTER_COLUMN)) %>% arrange(!!sym(input$FILTER_COLUMN)) %>% select(!!sym(input$FILTER_COLUMN))
    #         selectInput("FILTER_VALUE", label = "Filter Value", choices = c("None",unique(x)), selected = "None")
    #     })

    FilteredMapPI <- reactive({
        req(input$DIVISION_FILTER)
        req(input$TYPE_FILTER)
        
        divFil <- typeFil <- ""
        
        if (input$DIVISION_FILTER !=  'All') {
            divFil <- paste0("%>% filter(division == '",input$DIVISION_FILTER,"')")
        }
        if (input$TYPE_FILTER != 'All') {
            typeFil <- paste0("%>% filter(Type == '",input$TYPE_FILTER,"')")
        }
        
        if (any(length(c(divFil,typeFil)) > 0)) {
            eval(parse(text =  paste0("MapPI ",divFil,typeFil)))
        } else {
            MapPI
        }
        
    })
    
    output$LeafletMap <- renderLeaflet({
        req(nrow(FilteredMapPI()) > 0)
        
        #cols = colour_values_rgb(FilteredMapPI()$division, include_alpha = FALSE) / 255
        
        theMap <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>% 
            addProviderTiles(providers$Stamen.TonerLite) %>% 
            addGlPoints(data = FilteredMapPI())
            #setView(lng = -96.79, lat = 32.778, zoom = 6)
    })

    FilteredCrimeForProphet <- reactive({
        
        req(input$DIVISION_FILTER)
        req(input$TYPE_FILTER)
        
        divFil <- typeFil <- ""
        
        if (input$DIVISION_FILTER != 'All') {
            divFil <- paste0("(division == '",input$DIVISION_FILTER,"')")
        }
        if (input$TYPE_FILTER != 'All') {
            typeFil <- paste0("(Type == '",input$TYPE_FILTER,"')")
        }
        
        if ((divFil != "") && (typeFil != "")) {
            filtStr <- paste0(divFil," & ",typeFil)
        } else {
            filtStr <- trimws(paste0(divFil,typeFil))
        }
        
        print(filtStr)
        
        if (nchar(filtStr) > 1) {
            eval(parse(text =  paste0("CleanPI[",filtStr,",.(y = .N,ds = Date),by = Date]")))
        } else {
            CleanPI[,.(y = .N,ds = Date),by = Date]
        }
        
        # if (is.null(input$FILTER_VALUE) | length(input$FILTER_VALUE) == 0) {
        #     eval(parse(text = paste0("CleanPI[,.(y = .N,ds = Date),by = Date]")))
        # } else if (input$FILTER_VALUE == 'None') {
        #     eval(parse(text = paste0("CleanPI[,.(y = .N,ds = Date),by = Date]")))
        # } else {
        #     eval(parse(text = paste0("CleanPI[",input$FILTER_COLUMN," == '",input$FILTER_VALUE,"',.(y = .N,ds = Date),by = Date]")))
        # }
    })

    output$Forecast <- renderDygraph({

        m <- prophet(FilteredCrimeForProphet())
        future <- make_future_dataframe(m, periods = input$ProphetPeriods)
        forecast <- predict(m, future)
        dyplot.prophet(m, forecast)
    })
    
}

shinyApp(ui, server)
