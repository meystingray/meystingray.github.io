pkg <- c("data.table","ggplot2","stringr","dplyr","gganimate","RSocrata","ggmap","Shiny","rdrop2")
suppressWarnings(suppressPackageStartupMessages(
    invisible(lapply(pkg, function(x) require(x, character.only = T, quietly = T)))
))

library(ggmap)
token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)

function(input, output) {
    
    #dataset <- Murder #reactive({Murder})
    dataset <- reactive({
        Murder
        # if (input$filterValue != "None" & input$filterName != "None") {
        #     x <- paste0(input$filterName," == ",input$filterValue)
        #     dataset[eval(parse(text = x)),]
        # } else {
        #     dataset
        # }
    })
    
    
    Map <- DallasZoom11 #reactive({DallasZoom11})
    
    #output$filterValue <- renderUI({
    #    selectInput('filterValue', 'Filter Value', c('None', dataset[,get(input.filterName)]))
    #})
    
    # Render selectInput
    output$filterValue <- renderUI({
        vals <- as.vector(unique(unique(dataset[,get(input.filterName)])))
        selectInput("filterValue","Filter Value", choices = vals, multiple = TRUE)    
    })
    
    output$dataset <- renderTable({ dataset() })

    output$plot <- renderPlot({

        if (input$color != 'None') {
            
            #print(input$color)
            cols <- eval(parse(text = paste0("Murder$",input$color)))
            #print(cols)
            
            p <- ggmap(DallasZoom11,extent = "device") + 
                geom_point(data = output$dataset, aes(x = Longitude, y = Latitude,fill = as.factor(cols)),
                           size = input$MarkerSize,color = "black",stroke = 1,shape = 21) +
                labs(fill = input$color)
            
        } else {
            p <- ggmap(DallasZoom11,extent = "device") + 
                geom_point(data = Murder, aes(x = Longitude, y = Latitude),fill = "red",color = "black",
                           shape = 21, size = input$MarkerSize, stroke = 1)
        }    
        

        if (input$facet_row != input$facet_col) {
            facets <- paste(input$facet_row, '~', input$facet_col)
            if (facets != '. ~ .')
                p <- p + facet_grid(facets)
        }
        
        #if (input$jitter)
        #     p <- p + geom_jitter()
        # if (input$smooth)
        #     p <- p + geom_smooth()
        
        print(p)
        
    }, height=700)
    
}