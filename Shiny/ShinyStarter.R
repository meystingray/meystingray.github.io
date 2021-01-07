library(data.table)
pkg <- c("data.table","ggplot2","stringr","dplyr","gganimate","RSocrata","ggmap","shiny","rdrop2","DT")
suppressWarnings(suppressPackageStartupMessages(
    invisible(lapply(pkg, function(x) require(x, character.only = T, quietly = T)))
))

library(ggmap)

DallasZoom11 <- readRDS("C:/Users/sconroy/Documents/meystingray.github.io/Shiny/Maps/DallasZoom11.RDS")


Murder <- readRDS("C:/Users/sconroy/Documents/DallasPoliceData/Murder.RDS")

Murder[,rowid := 1:.N]
Murder[,Date := as.Date(substr(date1,1,10))]
Murder[,LatLongStart := regexpr("(",geocoded_column,fixed = TRUE)[1] + 1,by = rowid]
Murder[,LatLongEnd := regexpr(")",geocoded_column,fixed = TRUE)[1] - 1,by = rowid]
Murder[,LatLong := substr(geocoded_column,start = LatLongStart,stop = LatLongEnd)]
Murder[,LatLongComma := regexpr(",",LatLong,fixed = TRUE)[1],by = rowid]
Murder[,Latitude := substr(LatLong,start = 1,stop = LatLongComma - 1)]
Murder[,Longitude := substr(LatLong,start = LatLongComma + 1,stop = 1000)]
Murder[,Longitude := as.numeric(Longitude)]
Murder[,Latitude := as.numeric(Latitude)]
Murder[,Year := as.factor(servyr)]
Murder <- Murder[!is.na(Latitude) & !is.na(Longitude) & !is.na(Date) & !is.na(servyr),]

#ggmap(DallasZoom11,extent = "device") + 
#    geom_point(data = Murder, aes(x = Longitude, y = Latitude),#fill = as.factor(cols)),
#               size = 2,color = "black",stroke = 1,shape = 21)


ui <- fluidPage(

    titlePanel("Dallas Murders Explorer"),
    
    sidebarPanel(
        
        sliderInput('zoom', 'Zoom', min = 1, max = 10,value=5),
        
        sliderInput('MarkerSize', 'Marker Size', min = 1, max = 5,value=2),
        
        selectInput("selectYear", label = "Select Year", choices = sort(unique(Murder$Year))),
                    
        #selectInput('x', 'X', names(dataset)),
        #selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
        
        #selectInput('color', 'Color', c('None', names(dataset)))
        
        selectInput('filterName', 'Filter By', c('None', names(Murder))),
        
        #uiOutput("filterValue")
        
        selectInput('filterValue', 'Filter Value', c('None', unique(Murder[,get(input$filterName)])))
        #uiOutput("filterValue"),
        # conditionalPanel(
        #     condition = "input.filterName != 'None'",
        #     selectInput("filterValue", "Filter Value",
        #                 as.list(unique(dataset[,get(input$filterName)])))
        # ),
        
        #checkboxInput('jitter', 'Jitter'),
        #checkboxInput('smooth', 'Smooth'),
        
        #selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
        #selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
    ),
    
    mainPanel(
        #verbatimTextOutput("filterName")
        plotOutput('Map')
        #tableOutput('dataset')
    )
    
)

server <- function(input, output) {
    
    ## create dummy data
    # set.seed(1)
    # location <- c("locA", "locB", "locC")
    # location <- sample(location, 20, replace=TRUE)
    # service <- c("serviceA", "serviceB")
    # service <- sample(service, 20, replace=TRUE)
    # person <- c("A", "B")
    # person <- sample(person, 20, replace=TRUE)
    # value <- runif(20, 0, 5)
    # 
    # df <- data.frame(location, service, person, value)
    
    # output$filterValue <- renderUI({
    #     selectInput('filterValue', 'Filter Value', c('None', Murder[,get(input$filterName)]))
    # })
    
    observe({
        updateSelectInput(session, "User", choices = as.character(Murder[,input$filterName]))
    })
    
    ## reactive to user input
    data <- reactive({
        print(input$filterValue)
        if (!is.null(input$filterValue)) {
            Murder[Year == input$filterValue,]
        } else {
            Murder
        }
    })
    
    # output$table1 <- renderDataTable({
    #     data()    
    # })
    
    ## plot 1
    output$Map <- renderPlot({
        ## filter data frame for use in first plot
        Temp <- data()
        #Murder <- Murder %>%
        #    filter(Year == selectYear())   ## select() - calls reactive expression
        
        ggmap(DallasZoom11,extent = "device") +
             geom_point(data = Temp, aes(x = Longitude, y = Latitude),fill = "red",
                        size = input$MarkerSize,color = "black",stroke = 1,shape = 21)
             #+ labs(fill = input$color)
    })    
    ## plot 2
    # output$plot2 <- renderPlot({
    #     ## filter data frame for use in second plot 
    #     ## this is the same data as in plot1
    #     df <- df %>%
    #         filter(person==select())  ## select() - calls reactive expression
    #     
    #     ggplot(data=df, aes(x=location, y=value, fill=person)) +
    #         geom_bar(stat="identity") +
    #         facet_wrap(~service)
    # })  
}




# Run the application 
shinyApp(ui = ui, server = server)
