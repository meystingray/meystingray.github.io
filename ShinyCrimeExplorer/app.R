library(rdrop2)
library(data.table)
library(ggplot2)
library(shiny)
library(leaflet)
library(RSocrata)
library(shinyWidgets)
library(leaflet.extras)
library(dygraphs)
library(prophet)

#setwd("C:/Users/sconroy/Documents/meystingray.github.io/ShinyCrimeExplorer")
token <- readRDS("droptoken.rds")

# Download Murder Data
drop_download(path = "Shiny/CleanPI.RDS",local_path =  paste0(tempdir(), "/CleanPI.RDS"),overwrite = TRUE, dtoken = token)
CleanPI <- readRDS(paste0(tempdir(), "/CleanPI.RDS"))

# df <- CleanPI[,.(ds = min(WeekDate),y = .N),by = WeekDate]
# df
# m <- prophet(df)
# future <- make_future_dataframe(m, periods = 365)
# forecast <- predict(m, future)
# #prophet_plot_components(m, forecast)
# names(CleanPI)


ui <- fluidPage(
    fluidRow(
        column(12,titlePanel("Exploring Dallas Crime Rates"))
    ),
    p(),
    fluidRow(
        column(4,
               selectInput("INTERVAL", "Interval:", choices = c("MonthDate","WeekDate")),
               selectInput("SELECT_TYPE", "Crime Type:", choices = c("All",unique(CleanPI$Type)))
               
        )
    ),
    fluidRow(
        column(4,
               sliderInput('ProphetPeriods', 'Forecast # Periods', min = 10, max = 1000,value = 365),

               strong(em("'Exploring Dallas Crime Rates'"),p(),"an R-Shiny app by ",
                      a("Sean Conroy",href ='https://www.seantconroy.com/'))
        ),
        column(8,dygraphOutput("Dyplot"))
    ),
    fluidRow(
        column(12,dygraphOutput("Forecast"))
    )
    
)

server <- function(input, output, session) {

    # FilterString <- reactive({
    #     
    #     eval(parse(text = paste0("df <- as.xts.data.table(CleanPI[,.(Rate = .N),by = '",input$interval,"')])")))
    #     
    # })
        

    FilteredCrime <- reactive({
        if (input$SELECT_TYPE == "All") {
            eval(parse(text = paste0("as.xts.data.table(CleanPI[,.(Rate = .N),by = '",input$INTERVAL,"'])")))
        } else {
            eval(parse(text = paste0("as.xts.data.table(CleanPI[Type == '",input$SELECT_TYPE,"',.(Rate = .N),by = '",input$INTERVAL,"'])")))
        }
    })
    # predicted <- reactive({
    #     hw <- HoltWinters(ldeaths)
    #     predict(hw, n.ahead = input$months, 
    #             prediction.interval = TRUE,
    #             level = as.numeric(input$interval))
    # })
    
    
    output$Dyplot <- renderDygraph({
        
        dygraph(data = FilteredCrime())

    })
    
    
    FilteredCrimeForProphet <- reactive({
        if (input$SELECT_TYPE == "All") {
            eval(parse(text = paste0("CleanPI[,.(y = .N,ds = Date),by = Date]")))
        } else {
            eval(parse(text = paste0("CleanPI[Type == '",input$SELECT_TYPE,"',.(y = .N,ds = Date),by = Date]")))
        }
    })
    
    output$Forecast <- renderDygraph({
        
        m <- prophet(FilteredCrimeForProphet())
        future <- make_future_dataframe(m, periods = input$ProphetPeriods)
        forecast <- predict(m, future)
        dyplot.prophet(m, forecast)
    })
    
    
    
    
}

shinyApp(ui, server)
