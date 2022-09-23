# Load packages ----------------------------------------------------------------

library(shiny)
library(shinythemes)
library(ggplot2)
library(tools)
library(DT)

# Load data --------------------------------------------------------------------

movies <- read.csv(file = "https://raw.githubusercontent.com/aaron-chen-angus/S3729C-Lesson-Files/6a58d56d3d42231fb011af462db7efc01537e515/movies.csv", header = TRUE, sep = ",")
all_studios <- sort(unique(movies$studio))
min_date <- min(as.numeric(as.character(movies$thtr_rel_date)))
max_date <- max(as.numeric(as.character(movies$thtr_rel_date)))

# Define UI --------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("simplex"),

h1("Movies Database Viewer for S3729C"),
h4(tags$a(href = "https://shiny.rstudio.com/", "Powered by R Shiny")),
                
    sidebarLayout(
    sidebarPanel(
      
      HTML(paste0("Movies released between the following dates will be plotted.")),
      
      br(), br(),
      
      dateRangeInput(
        inputId = "date",
        label = "Select dates:",
        start = "2013-01-01", end = "2014-01-01",
        min = min_date, max = max_date,
        startview = "year"
      ),
      
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = c(
          "IMDB rating" = "imdb_rating",
          "IMDB number of votes" = "imdb_num_votes",
          "Critics Score" = "critics_score",
          "Audience Score" = "audience_score",
          "Runtime" = "runtime"
        ),
        selected = "audience_score"
      ),
      
      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = c(
          "IMDB rating" = "imdb_rating",
          "IMDB number of votes" = "imdb_num_votes",
          "Critics Score" = "critics_score",
          "Audience Score" = "audience_score",
          "Runtime" = "runtime"
        ),
        selected = "critics_score"
      ),
      
      selectInput(
        inputId = "z",
        label = "Color by:",
        choices = c(
          "Title Type" = "title_type",
          "Genre" = "genre",
          "MPAA Rating" = "mpaa_rating",
          "Critics Rating" = "critics_rating",
          "Audience Rating" = "audience_rating"
        ),
        selected = "mpaa_rating"
      ),
      
      sliderInput(
        inputId = "alpha",
        label = "Alpha:",
        min = 0, max = 1,
        value = 0.5
      ),
      
      sliderInput(
        inputId = "size",
        label = "Size:",
        min = 0, max = 5,
        value = 2
      ),
      
      textInput(
        inputId = "plot_title",
        label = "Plot title",
        placeholder = "Enter text to be used as plot title"
      ),
      
      actionButton(
        inputId = "update_plot_title",
        label = "Update plot title"
      ),
      
      br(), br(),
      
      selectInput(
        inputId = "studio",
        label = "Select the Movie Studio:",
        choices = all_studios,
        selected = "20th Century Fox",
        multiple = TRUE
      ),
      
      downloadButton('download',"Download data")
      
    ),
    
    mainPanel(
      plotOutput(outputId = "scatterplot", hover = "plot_hover"),
      dataTableOutput(outputId = "moviestablehover"),
      br(),
      dataTableOutput(outputId = "moviestable")
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  new_plot_title <- eventReactive(
    eventExpr = input$update_plot_title,
    valueExpr = {
      toTitleCase(input$plot_title)
    }
  )
  
  output$scatterplot <- renderPlot({
    req(input$date)
    movies_selected_date <- movies %>%
      filter(thtr_rel_date >= as.POSIXct(input$date[1]) & thtr_rel_date <= as.POSIXct(input$date[2]))
      ggplot(data = movies, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point(alpha = input$alpha, size = input$size) +
      labs(title = new_plot_title())
  })
  
  output$moviestablehover <- renderDataTable({
    nearPoints(movies, input$plot_hover) %>%
      select(title, thtr_rel_year, title_type, genre, runtime, mpaa_rating, studio, director)
    
  })
    
  output$moviestable <- renderDataTable({
    req(input$studio)
    movies_from_selected_studios <- movies %>%
      filter(studio %in% input$studio) %>%
      select(title:studio)
    DT::datatable(
      data = movies_from_selected_studios,
      options = list(pageLength = 10),
      rownames = FALSE)
    
  })

  
  
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
