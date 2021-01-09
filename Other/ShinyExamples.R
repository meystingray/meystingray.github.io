

require(shiny)

ui <- fluidPage( 
    sidebarLayout(
        sidebarPanel(
            uiOutput('choose_course')
        ),
        mainPanel(
            tableOutput('courseTable')
        )
    )
)

server <- function(input, output, session) {
    # Build data, would be replaced by the csv loading in your case
    n <- 10
    model.data0 <- reactive ({
        data.frame( "COURSE" = sample(LETTERS[1:3], n, replace=TRUE),
                    "VALUE"  = sample(1:10, n, replace=TRUE),
                    "SUB" = sample(LETTERS[1:3], n, replace=TRUE),
                    "ATOMIC" = sample(LETTERS[1:3], n, replace=TRUE),
                    "PARTICLES" = sample(LETTERS[1:3], n, replace=TRUE)
        )
    })
    
    # Render selectInput 
    output$choose_course <- renderUI({
        course.names <- as.vector( names(model.data0()) )
        selectInput("courses","Choose courses", choices=course.names, multiple=FALSE)    
    })
    
    # Render filterInput
    output$choose_course <- renderUI({
        course.names <- as.vector( names(model.data0()) )
        selectInput("filterValue","Filter Value", choices=course.names, multiple=FALSE)    
    })
    
    # Subset so that only the selected rows are in model.data
    model.data <- reactive({
        subset(model.data0(), COURSE %in% input$courses)
    })
    
    output$courseTable <- renderTable({ model.data() })
}

runApp(shinyApp(ui,server))


##################################

library(shiny)
library(dplyr)
library(ggplot2)

server <- function(input, output) {
    
    ## create dummy data
    set.seed(1)
    location <- c("locA", "locB", "locC")
    location <- sample(location, 20, replace=TRUE)
    service <- c("serviceA", "serviceB")
    service <- sample(service, 20, replace=TRUE)
    person <- c("A", "B")
    person <- sample(person, 20, replace=TRUE)
    value <- runif(20, 0, 5)
    
    df <- data.frame(location, service, person, value)
    
    ## reactive to user input
    select <- reactive({
        input$select
    })
    
    ## plot 1
    output$plot1 <- renderPlot({
        ## filter data frame for use in first plot
        df <- df %>%
            filter(person==select())   ## select() - calls reactive expression
        
        ggplot(data=df, aes(x=location, y=value, fill=person)) +
            geom_bar(stat="identity")
    })    
    ## plot 2
    output$plot2 <- renderPlot({
        ## filter data frame for use in second plot 
        ## this is the same data as in plot1
        df <- df %>%
            filter(person==select())  ## select() - calls reactive expression
        
        ggplot(data=df, aes(x=location, y=value, fill=person)) +
            geom_bar(stat="identity") +
            facet_wrap(~service)
    })  
}

ui <- navbarPage("Test",
                 tabPanel("panel",
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput("select", label="select", choices=c("A", "B"))
                              ),
                              mainPanel(
                                  plotOutput("plot1"),
                                  hr(),
                                  plotOutput("plot2")
                              )
                          )
                 )
)
)


shinyApp(ui,server)

#####################################################

library(shiny)
library(dplyr)
library(ggplot2)

set.seed(1)
location <- c("locA", "locB", "locC")
location <- sample(location, 20, replace=TRUE)
service <- c("serviceA", "serviceB")
service <- sample(service, 20, replace=TRUE)
person <- c("A", "B")
person <- sample(person, 20, replace=TRUE)
value <- runif(20, 0, 5)

df <- data.frame(location, service, person, value)
names(df)

server <- shinyServer(function(input, output) {
    
    ## create dummy data
    
    ## reactive to user input
    select <- reactive({
        input$select
    })
    
    filter_df <- reactive({
        df %>%
            filter(person==select())
    })
    
    ## plot 1
    output$table <- renderTable({
        filter_df()
    })
    # output$plot1 <- renderPlot({
    #     ## filter data frame for use in first plot
    #     df <- df %>%
    #         filter(person==select())   ## select() - calls reactive expression
    #     
    #     ggplot(data = df, aes(x=location, y=value, fill=person)) +
    #         geom_bar(stat="identity")
    # })    
    ## plot 2
    # output$plot2 <- renderPlot({
    #     ## filter data frame for use in second plot 
    #     ## this is the same data as in plot1
    #     df <- df %>%
    #         filter(person==select())  ## select() - calls reactive expression
    #     
    #     ggplot(data = df, aes(x=location, y=value, fill=person)) +
    #         geom_bar(stat="identity")
    #         # + facet_wrap(~service)
    # })  
})

ui <- shinyUI(navbarPage("Test",
                         tabPanel("panel",
                                  sidebarLayout(
                                      sidebarPanel(
                                          selectInput("select", label = "select", choices = as.list(names(df))),
                                          
                                      ),
                                      mainPanel(
                                          tableOutput("table")
                                          #plotOutput("plot1"),
                                          #hr()
                                      )
                                  )
                         )
)
)

shinyApp(ui = ui, server = server)



library(shiny)
library(tidyverse)


ui <- fluidPage(
    
    # Sidebar with an input for column
    # boolean input
    # and value input
    sidebarLayout(
        sidebarPanel(
            fluidRow(column(4, selectInput("COLUMN", "Filter By:", choices = colnames(iris))),
                     column(4, selectInput("CONDITION", "Boolean", choices = c("==", "!=", ">", "<"))),
                     column(4, uiOutput("COL_VALUE")))
        ),
        
        # Show text generated by sidebar
        # use text in tidy pipeline to create subsetted dataframe
        mainPanel(
            verbatimTextOutput("as_text"),
            tableOutput("the_data"),
            plotOutput("plot1")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$COL_VALUE <- renderUI({
        x <- iris %>% select(!!sym(input$COLUMN))
        selectInput("VALUE", "Value", choices = x, selected = x[1])
    })
    
    filtering_string <- reactive ({
        paste0("filter(iris, ", input$COLUMN, " ", input$CONDITION, " ", input$VALUE, ")")
    })
    
    output$as_text <- renderText({
        filtering_string()
    })
    
    
    output$the_data <- renderTable({
        eval(parse(text = filtering_string()))
    })
    
    filtered_table <- reactive({
        eval(parse(text = filtering_string()))
    })
    
    output$plot1 <- renderPlot({
        plot(filtered_table()$Sepal.Width, filtered_table()$Sepal.Length, col = "red", lwd = 10)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)


library(shiny)
library(DT)

shinyApp(
    ui = fluidPage(dataTableOutput('tbl'),
                   plotOutput('plot1')),
    server = function(input, output) {    
        output$tbl = renderDataTable({
            datatable(iris, options = list(lengthChange = FALSE))
        })
        output$plot1 = renderPlot({
            filtered_data <- input$tbl_rows_all
            hist(iris[filtered_data, "Sepal.Length"])
        })
    }
)


###############################################################
library(shiny)
library(DT)

shinyApp(
    ui = fluidPage(dataTableOutput('tbl'),
                   plotOutput('plot1')),
    server = function(input, output) {    
        output$tbl = renderDataTable({
            datatable(iris, filter="top",options = list(lengthChange = FALSE),callback=JS("
                                                                                          //hide column filters for the first two columns
                                                                                          $.each([0, 1], function(i, v) {
                                                                                          $('input.form-control').eq(v).hide()
                                                                                          });"))
        })
        output$plot1 = renderPlot({
            filtered_data <- input$tbl_rows_all
            hist(iris[filtered_data, "Sepal.Length"])
        })
        }
            )


ui <- fluidPage(
    h2("The mtcars data"),
    DT::dataTableOutput("mytable"),
    verbatimTextOutput("test"),
    plotOutput('plot1')
    
)

server <- function(input, output) {
    
    mc <- head(mtcars) # could be reactive in real world case
    
    output$mytable = DT::renderDataTable({
        datatable(mc, filter = 'top')
    })
    
    filtered_table <- reactive({
        req(input$mytable_rows_all)
        mc[input$mytable_rows_all, ]  
    })
    
    output$plot1 <- renderPlot({
        plot(filtered_table()$wt, filtered_table()$mpg, col = "red", lwd = 10)
    })
    
    output$test <- renderPrint({
        filtered_table()
    })
    
}

shinyApp(ui, server)