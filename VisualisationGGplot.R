library(shiny)
library(shinythemes)
library(ggplot2)
library(ggridges)

# Define UI for application that draws a histogram and boxplot
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  # App title
  titlePanel("Data Visualisation with Histograms, Boxplots, Density and Ridgeline Plots Using ggplot2"),
  
  navbarPage(
    title = "Data Visualisation app Developed for S3729C",
    
    # Sidebar with a slider input for alpha
    sidebarLayout(
      sidebarPanel(
        
        # Add file input to upload the csv
        fileInput("file", "Choose CSV File"),
        
        # Histogram tab inputs
        tabsetPanel(type = "tabs",
                    tabPanel("Histograms",
                             textInput("x_axis_hist", "X-Axis", ""),
                             sliderInput("bin_width_hist", "Bin Width",
                                         min = 1, max = 20, value = 1),
                             textInput("fill_hist", "Fill", "orange"),
                             textInput("color_hist", "Color", "blue"),
                             sliderInput("alpha_hist", "Alpha",
                                         min = 0, max = 1, value = 0.5, step = 0.1),
                             textInput("title_hist", "Title", "Graph Title"),
                             textInput("start_hist", "Start", "0"),
                             textInput("end_hist", "End", "100"),
                             actionButton("submit_hist", "Submit")
                    ),
                    
                    # Boxplot tab inputs
                    tabPanel("Boxplots",
                             textInput("x_axis_box", "X-Axis", ""),
                             textInput("y_axis_box", "Y-Axis", ""),
                             textInput("fill_box", "Fill", "green"),
                             textInput("color_box", "Color", "red"),
                             sliderInput("alpha_box", "Alpha",
                                         min = 0, max = 1, value = 0.5, step = 0.1),
                             textInput("title_box", "Title", "Graph Title"),
                             textInput("start_box", "Start", "0"),
                             textInput("end_box", "End", "100"),
                             actionButton("submit_box", "Submit")
                    ),
                    
                    # Density Plot tab inputs
                    tabPanel("Density Plots",
                             textInput("x_axis_density", "X-Axis", ""),
                             textInput("fill_density", "Fill", ""),
                             textInput("color_density", "Color", ""),
                             sliderInput("alpha_density", "Alpha",
                                         min = 0, max = 1, value = 0.5, step = 0.1),
                             textInput("x_label_density", "X Label", "Type your X Label here"),
                             textInput("subtitle_density", "Subtitle", "Type your Subtitle here"),
                             textInput("caption_density", "Caption", "Type your Caption here"),
                             actionButton("submit_density", "Submit")
                    ),
                    
                    # Ridgeline Plot tab inputs
                    tabPanel("Ridgeline Plots",
                             textInput("x_axis_ridge", "X-Axis", ""),
                             textInput("y_axis_ridge", "Y-Axis", ""),
                             textInput("fill_ridge", "Fill", ""),
                             actionButton("submit_ridge", "Submit")
                    )
        )
      ),
      
      # Show a plot of the generated histogram, boxplot, density plot or ridgeline plot
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Home",
                             tableOutput("head")
                    ),
                    tabPanel("Histograms",
                             plotOutput("histogram")
                    ),
                    tabPanel("Boxplots",
                             plotOutput("boxplot")
                    ),
                    tabPanel("Density Plots",
                             plotOutput("densityplot")
                    ),
                    tabPanel("Ridgeline Plots",
                             plotOutput("ridgelineplot")
                    )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram, boxplot, density plot and ridgeline plot
server <- function(input, output) {
  
  # Read the csv data
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, header = TRUE, stringsAsFactors = FALSE)
  })
  
  # Show the first 10 rows of data
  output$head <- renderTable({
    head(data(), 10)
  })
  
  # Create the histogram
  output$histogram <- renderPlot({
    x_axis <- input$x_axis_hist
    bin_width <- input$bin_width_hist
    fill <- input$fill_hist
    color <- input$color_hist
    alpha <- input$alpha_hist
    title <- input$title_hist
    start <- input$start_hist
    end <- input$end_hist
    if(input$submit_hist > 0) {
      ggplot(data = data(), aes_string(x = x_axis)) + 
        geom_histogram(binwidth = bin_width, fill = fill, color = color, alpha = alpha) +
        ggtitle(title) + ylim(start, end)
    }
  })
  
  # Create the boxplot
  output$boxplot <- renderPlot({
    x_axis <- input$x_axis_box
    y_axis <- input$y_axis_box
    fill <- input$fill_box
    color <- input$color_box
    alpha <- input$alpha_box
    title <- input$title_box
    start <- input$start_box
    end <- input$end_box
    if(input$submit_box > 0) {
      ggplot(data = data(), aes_string(x = x_axis, y = y_axis)) + 
        geom_boxplot(color = color, fill = fill, alpha = alpha) +
        ggtitle(title) + ylim(start, end)
    }
  })
  
  # Create the density plot
  output$densityplot <- renderPlot({
    x_axis <- input$x_axis_density
    fill <- input$fill_density
    color <- input$color_density
    alpha <- input$alpha_density
    x_label <- input$x_label_density
    subtitle <- input$subtitle_density
    caption <- input$caption_density
    if(input$submit_density > 0) {
      ggplot(data = data(), aes_string(x = x_axis, color = color, fill = fill)) +
        geom_density(alpha = alpha, size = 1) +
        labs(x = x_label, subtitle = subtitle, caption = caption)
    }
  })
  
  # Create the ridgeline plot
  output$ridgelineplot <- renderPlot({
    x_axis <- input$x_axis_ridge
    y_axis <- input$y_axis_ridge
    fill <- input$fill_ridge
    if(input$submit_ridge > 0) {
      ggplot(data = data(), aes_string(x = x_axis, y = y_axis, fill = fill)) +
        geom_density_ridges() +
        theme_ridges() + 
        theme(legend.position = "none")
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)