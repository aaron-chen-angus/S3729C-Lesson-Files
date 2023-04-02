# Load packages ----------------------------------------------------------------

library(shiny)
library(shinythemes)

# Define UI --------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("cerulean"),
                
                # App title ----
                titlePanel(HTML("Rockport Walk Test VO<span style='vertical-align:sub;'>2</span> max calculator")),
                
                # Navbar Panel with two tabs
                navbarPage(
                  title = "Rockport Walk Test VO2 max calculator",
                  tabPanel("Home",
                           # Sidebar layout with a input and output definitions ----
                           sidebarLayout(
                             
                             # Sidebar panel for inputs ----
                             sidebarPanel(
                               h3(HTML("VO<span style='vertical-align:sub;'>2</span> max Calculator")),
                               hr(),
                               p(HTML("For an estimate of your VO<span style='vertical-align:sub;'>2</span> max, enter your gender, age, weight, and heart rate at the end of the test, the time to complete the walk and then select the 'Calculate' button.")),
                               h5("Inputs"),
                               # Input: Gender ----
                               numericInput(inputId = "gender",
                                            label = "Gender (1 = male, 0 = female)",
                                            value = 0
                               ),
                               # Input: Age ----
                               numericInput(inputId = "age",
                                            label = "Age (years)",
                                            value = 0
                               ),
                               # Input: Heart rate ----
                               numericInput(inputId = "heart_rate",
                                            label = "Heart rate (bpm)",
                                            value = 0
                               ),
                               # Input: Weight ----
                               numericInput(inputId = "weight",
                                            label = "Weight (lbs)",
                                            value = 0
                               ),
                               # Input: Time ----
                               numericInput(inputId = "time",
                                            label = "Time (minutes and 100ths of minutes)",
                                            value = 0
                               ),
                               # Action button ----
                               actionButton(inputId = "calculate",
                                            label = "Calculate"
                               )
                             ),
                             
                             # Main panel for displaying outputs ----
                             mainPanel(
                               h3("Output"),
                               # Output: VO2 max ----
                               textOutput(outputId = "vo2max"),
                               # Output: Hyperlink to VO2 max page ----
                               uiOutput(outputId = "vo2max_page"),
                               # Output: Reference section ----
                               hr(),
                               textOutput(outputId = "vo2max_references")
                             )
                           )
                  ),
                  tabPanel("About",
                           p(HTML("The Rockport Walk Test is a common aerobic fitness test for those of low fitness level. The aim if the test is to walk as fast as possible for 1 mile.<br><br>

To conduct this test, you will require:<br>
<ul><li>400-metre track</li><li>Stopwatch</li><li>Weighing Scales</li><li>Assistant</li></ul><br><br>

<b>How to conduct the test</b><br>
This test requires the athlete to walk one mile (1609 metres) as fast as possible.<br><br>

The assistant:<br>
<ol><li>Weighs and records the athlete's weight</li><li>Gives the command “GO”, starts the stopwatch, and the athlete commences the test</li><li>Records the time taken for the athlete to complete the test and the athlete's heart rate immediately after finishing</li></ol>"))
                  )
                )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  # Reactive expression for VO2 max ----
  vo2max <- reactive({
    # Compute VO2 max ----
    v <- 132.853 - (0.0769 * input$weight) - (0.3877 * input$age) + 
      (6.315 * input$gender) - (3.2649 * input$time) - (0.1565 * input$heart_rate)
    # Return VO2 max ----
    return(v)
  })
  
  # Show VO2 max ----
  output$vo2max <- renderText({
    paste0("VO2max = ", vo2max(), " mls/kg/min")
  })
  
  # Hyperlink to VO2 max page ----
  output$vo2max_page <- renderUI({
    # Return a link to the VO2 max page ----
    a(href = "https://www.brianmac.co.uk/vo2max.htm#vo2",
      "For an analysis of your VO2 max score, please click on this link to view the VO2 max page.")
  })
  
  # Reactive expression for VO2 max formula ----
  vo2max_formula <- reactive({
    # Return the formula for computing VO2 max ----
    paste0("VO2 max = 132.853 - (0.0769 × Weight) - (0.3877 × Age) + (6.315 × Gender) - (3.2649 × Time) - (0.1565 × Heart rate)")
  })
  
  # Show VO2 max formula ----
  output$vo2max_formula <- renderText({
    vo2max_formula()
  })
  
  # Reactive expression for VO2 max references ----
  vo2max_references <- reactive({
    # Return the references for computing VO2 max ----
    paste0("References:
1.	KILNE, G. et al. (1987) Estimation of VO2 max from a one-mile track walk, gender, age and body weight. Med Sci. Sports Exerc., 19, p. 253-259")
  })
  
  # Show VO2 max references ----
  output$vo2max_references <- renderText({
    vo2max_references()
  })
  
}

# Create a Shiny app object ----------------------------------------------------
shinyApp(ui = ui, server = server)