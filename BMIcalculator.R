# Load packages ----------------------------------------------------------------

library(shiny)
library(shinythemes)


# Define UI --------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("cyborg"),
                navbarPage("BMI Calculator:",
                           
                           tabPanel("Home",
                                    # Input values
                                    sidebarPanel(
                                      HTML("<h3>Input parameters</h3>"),
                                      sliderInput("height", 
                                                  label = "Height in cm", 
                                                  value = 185, 
                                                  min = 40, 
                                                  max = 250),
                                      sliderInput("weight", 
                                                  label = "Weight in kg", 
                                                  value = 88, 
                                                  min = 40, 
                                                  max = 120),
                                      
                                      actionButton("submitbutton", 
                                                   "Submit", 
                                                   class = "btn btn-primary")
                                    ),
                                    
                                    mainPanel(
                                      tags$label(h3('Status/Output')), # Status/Output Text Box
                                      verbatimTextOutput('contents'),
                                      tableOutput('tabledata') # Results table
                                    ) # mainPanel()
                                    
                           ), #tabPanel(), Home
                           tabPanel("About", 
                                    titlePanel("About"),
                                    p("Body Mass Index (BMI) is essentially a value obtained from the weight and height of a person [1]."),
                                    br(),
                                    strong("Calculating the BMI"),
                                    p("BMI can be computed by dividing the person's weight (kg) by their squared height (m) as follows:"),
                                    p("BMI = kg/m^2"),
                                    p("where",
                                      span("kg", style = "color:blue"), 
                                      "represents the person's weight and", 
                                      span("m^2", style = "color:blue"), 
                                      "the person's squared height."),
                                    br(),
                                    strong("Take Note"),                                    
                                    p("This BMI Calculator is for adults 20 years and older. Further information on calculating BMI for children and teenagers is available from the CDC [2]."),
                                    br(),
                                    strong("References"),                                    
                                    p("1. Centers for Disease Control. [Body Mass Index (BMI)](https://www.cdc.gov/healthyweight/assessing/bmi/index.html), Accessed September 13, 2022."),
                                    p("2. Centers for Disease Control. [BMI Percentile Calculator for Child and Teen](https://www.cdc.gov/healthyweight/bmi/calculator.html), Accessed September 13, 2022.")
                           ) #tabPanel(), About
                           
                ) # navbarPage()
) # fluidPage()


# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    bmi <- input$weight/( (input$height/100) * (input$height/100) )
    bmi <- data.frame(bmi)
    names(bmi) <- "BMI"
    print(bmi)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation of BMI complete.") 
    } else {
      return("Press submit after you have selected your height and weight using the sliders.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}


# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)