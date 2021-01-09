library(shiny)
library(ggplot2)

dataset <- Murder #diamonds

fluidPage(
    
    titlePanel("Dallas Murders Explorer"),
    
    sidebarPanel(
        
        sliderInput('zoom', 'Zoom', min = 1, max = 10,value=5),
        sliderInput('MarkerSize', 'Marker Size', min = 1, max = 5,value=2),
        
        #selectInput('x', 'X', names(dataset)),
        #selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
        
        selectInput('color', 'Color', c('None', names(dataset))),
        
        selectInput('filterName', 'Filter By', c('None', names(dataset))),
        uiOutput("filterValue"),
        #selectInput('filterValue', 'Filter Value', c('None', unique(dataset[,get(input.filterName)])),
        #uiOutput("filterValue"),
        # conditionalPanel(
        #     condition = "input.filterName != 'None'",
        #     selectInput("filterValue", "Filter Value",
        #                 as.list(unique(dataset[,get(input$filterName)])))
        # ),
        
        #checkboxInput('jitter', 'Jitter'),
        #checkboxInput('smooth', 'Smooth'),
        
        selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
        selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
    ),
    
    mainPanel(
        plotOutput('plot')
        #tableOutput('dataset')
    )

)