# Load packages ----------------------------------------------------------------

library(shiny)
library(plotly)
library(COVID19)

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  
  selectInput("country", label = "Country", multiple = TRUE, choices = unique(covid19()$administrative_area_level_1), selected = "Singapore"),
  selectInput("type", label = "type", choices = c("confirmed", "tests", "recovered", "deaths")),
  selectInput("level", label = "Granularity", choices = c("Country" = 1, "Region" = 2, "City" = 3), selected = 1),
  dateRangeInput("date", label = "Date", start = "2020-01-01"),
  
  plotlyOutput("covid19plot")
  
)

# Define server ----------------------------------------------------------------

server <- function(input, output) {
  
  output$covid19plot <- renderPlotly({
    if(!is.null(input$country)){
      x <- covid19(country = input$country, level = input$level, start = input$date[1], end = input$date[2])
      color <- paste0("administrative_area_level_", input$level)
      plot_ly(x = x[["date"]], y = x[[input$type]], color = x[[color]])
    }
  })
  
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)