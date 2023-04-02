# Load packages ----------------------------------------------------------------

library(shiny)
library(shinythemes)
library(shinydashboard)        
library(nortest)
library(mvnormtest)
library(MASS)
library(cluster)
library(psych)
library(tseries)
library(TTR)
# library(forecast) for forecasting time series data

# PREAMBLE begins
# READ (FOR REPORTS) https://shiny.rstudio.com/articles/generating-reports.html

# PREAMBLE ends

# Define UI --------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("sandstone"),
  
  # tags$head(tags$style(HTML("
  #       .selectize-input, .selectize-dropdown {
  #         font-size: 75%;
  #       }
  #       "))),
  
  navbarPage(title = "S3729C Data Science Application",
             tabPanel("DataSets", 
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file1", "Choose CSV File", accept=c('text/csv', 'text/comma-separated-values', 'text/plain', '.csv')),
                          radioButtons("indata", "Choice:", choices = c("Full", "Columns")),
                          selectInput("cols", "Choose the variable", choices = "", selected = " ", multiple = TRUE), 
                          downloadButton('downloaddatset', "Download"),
                          hr(),
                          radioButtons("trans1", "Transformation:", choices = c("Not-Required", "log", "inverselog", "exponential", "lognormal", "standardize")),
                          hr(),
                          radioButtons("trans2", "Transformation:", choices = c("Trignometric", "Mathematical")),
                          textInput("trigno", "Write Trig. Function"),
                          textInput("mathtrans", "Math Trans.", placeholder = "Fourier or Laplace"), 
                          hr()
                        ), 
                        
                        mainPanel(tableOutput("tab1"))
                      )
                      
             ), 
             
             navbarMenu("Descriptive Data Analysis",
                        tabPanel("Summary Statistics",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("cols1", "Choose Variable:", choices = "", selected = " ", multiple = TRUE)
                                     
                                   ), 
                                   mainPanel(
                                     fluidRow(
                                       h3("Summary Statistics"),
                                       div(
                                         verbatimTextOutput("summar")
                                       )
                                     )
                                   )
                                 )
                        ), 
                        tabPanel("Frequency Tables",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("cols2", "Choose Variable 1:", choices = "", selected = " ", multiple = TRUE),
                                     selectInput("cols3", "Choose Variable 2:", choices = "", selected = " ", multiple = TRUE)
                                   ), 
                                   mainPanel(
                                     fluidRow(
                                       h3("Frequency Tables"),
                                       div(
                                         verbatimTextOutput("freq_tables")
                                       )
                                     )
                                   )
                                 )
                                 
                        ), 
                        
                        tabPanel("Cross Tabulation",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("cols4", "Choose Variable 1:", choices = "", selected = " ", multiple = TRUE),
                                     selectInput("cols5", "Choose Variable 2:", choices = "", selected = " ", multiple = TRUE),
                                     hr(),
                                     helpText("For details visit:"),
                                     a(href="https://en.wikipedia.org/wiki/Chi-squared_test", "Karl Pearson Chisquare Test"),
                                     hr()
                                   ), 
                                   mainPanel(
                                     fluidRow(
                                       h3("Chisquare Test"),
                                       verbatimTextOutput("chi_t")
                                     )
                                   )
                                 )
                                 
                        ),
                        tabPanel("Plots",
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons("plotoption", "Choose the Option:", choices = c("Histogram", "BarPlot", "Scatter", "Pie" )),
                                     selectInput("cols6", "Choose Variable 1:", choices = "", selected = " ", multiple = TRUE),
                                     textInput("xaxisname", "Write X Axis Name"),
                                     textInput("yaxisname", "Write Y Axis Name"),
                                     textInput("title", "Write Title For the Graph")
                                   ), 
                                   mainPanel(
                                     h3("Plots"),
                                     fluidRow(
                                       plotOutput("plot")
                                     )
                                   )
                                 )
                                 
                        )
             ),
             
             navbarMenu("Predictive Data Analysis",
                        
                        tabPanel("Statistical Tests", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("cols7", "Choose Variable 1:", choices = "", selected = " ", multiple = TRUE),
                                     selectInput("cols8", "Choose Variable 2:", choices = "", selected = " ", multiple = TRUE),
                                     radioButtons("normaltest", "Select Method:", choices = c("A-D-Test", "Shapiro", "KS-Test", "MV-Shapiro")),
                                     hr(),
                                     helpText("For more details visit:"),
                                     a(href="https://en.wikipedia.org/wiki/Anderson%E2%80%93Darling_test", "Anderson-Darling test"), br(),
                                     a(href="https://en.wikipedia.org/wiki/Shapiro%E2%80%93Wilk_test", "Shapiro-Wilk test"), br(),
                                     a(href="https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test", "Kolmogorov-Smirnov test"), br(),
                                     
                                     hr()
                                   ), 
                                   mainPanel(
                                     h3("Statistical Tests"),
                                     fluidRow(
                                       div(
                                         plotOutput("qqp")
                                       ),
                                       div(
                                         verbatimTextOutput("normtest")
                                       )
                                     )
                                   )
                                   
                                 )
                                 
                        ),
                        
                        tabPanel("Correlation", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("cols9", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                     selectInput("cols10", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                     radioButtons("cormethod", "Select Method:", choices = c("KarlPearson", "Spearman", "Kendals")),
                                     hr(),
                                     helpText("For Details Visit:"),
                                     a(href="https://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient", "Karl Pearson Correlation Test"),
                                     hr()
                                   ), 
                                   mainPanel(
                                     h3("Correlation"),
                                     verbatimTextOutput("cor_t")
                                   )
                                   
                                 )
                                 
                        ),
                        tabPanel("Regression & ANOVA", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("cols11", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                     selectInput("cols12", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                     radioButtons("regmethod", "Select Method:", choices = c("Fit", "Summary", "ANOVA")), 
                                     hr(),
                                     helpText("For more information please visit"),
                                     a(href="https://en.wikipedia.org/wiki/Simple_linear_regression", "Simple Linear Regression"),
                                     hr()
                                   ), 
                                   mainPanel(
                                     h3("Regression & ANOVA"),
                                     fluidRow(
                                       div(
                                         verbatimTextOutput("regout")
                                       ),
                                       div(
                                         plotOutput("regplot")
                                       )
                                     )
                                   )
                                   
                                 )
                                 
                        ),
                        
                        tabPanel("MANOVA", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("cols13", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                     selectInput("cols14", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                     radioButtons("manmethod", "Choose Method:", choices = c("Fit", "Summary")),
                                     hr(),
                                     helpText("For more information please visit"),
                                     a(href="https://en.wikipedia.org/wiki/Multivariate_analysis_of_variance", "MANOVA"),
                                     hr(),
                                     helpText("Right now MANOVA supports only two dependent variables"),
                                     hr()
                                   ), 
                                   mainPanel(
                                     h3("MANOVA"),
                                     fluidRow(
                                       div(
                                         verbatimTextOutput("manovaout")
                                       ),
                                       div(
                                         plotOutput("manovaplot")
                                       )
                                     )
                                   )
                                   
                                 )
                                 
                        ), 
                        
                        tabPanel("Forecasting",
                                 # http://r-statistics.co/Time-Series-Analysis-With-R.html
                                 sidebarLayout(
                                   sidebarPanel(
                                     
                                     selectInput("forcvar", "Select Variables:", choices = "", selected = "", multiple = TRUE),
                                     radioButtons("forctasks", "Select Task:", choices = c("Description", "Convert", "Make-Stationary", "Decompose", "De-trend", "De-Seasonalize", "ACF", "PACF", "Predict")),
                                     # textInput("fre", "Frequency:"),
                                     numericInput("forclag", "Lag:", 1),
                                     numericInput("forcdiff", "Diff:", 1),
                                     fileInput("preddata", "Upload New Data for Prediction:", accept=c('text/csv', 'text/comma-separated-values', 'text/plain', '.csv')),
                                     textInput("additvsmult", "Type", placeholder = "write 'additive' or 'mult'"),
                                     radioButtons("forcanal", "Choose Method:", choices = c("Moving-Averages", "Exponential(HW)")),
                                     radioButtons("forcplottype", "Select Plot:", choices = c("No-Plot", "TS", "ACF", "PACF")),
                                     radioButtons("forctests", "Tests:", choices = c("No-Tests", "ADF", "KPSS"))
                                   ), 
                                   mainPanel(
                                     h3("Forecating"),
                                     fluidRow(
                                       
                                       div(
                                         verbatimTextOutput("tsconvert")
                                       ),
                                       div(
                                         verbatimTextOutput("forcoutput")
                                       ),
                                       div(
                                         plotOutput("forcplot")
                                       )
                                     )
                                   )
                                 )
                                 
                        )
                        
             ),         
             
             navbarMenu("Exploratory Data Analysis", 
                        tabPanel("Discriminant Analysis",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput('dafactvar', "Choose the Factor:", choices = "", selected = ""),
                                     selectInput('daNumvar1', "Choose the Vector1:", choices = "", selected = ""),
                                     selectInput('daNumvar2', "Choose the Vector2:", choices = "", selected = ""),
                                     selectInput('daNumtvar3', "Choose the Vector3:", choices = "", selected = ""),
                                     hr(), 
                                     helpText("Supports only 3 vectors (Vector: variable at right hand side of the equation). Implements only LDA at presents. For details visit:"),
                                     a(href="https://en.wikipedia.org/wiki/Linear_discriminant_analysis", "Discriminant Analysis"),
                                     hr()
                                   ),
                                   mainPanel(
                                     h3("Discriminant Analysis"),
                                     fluidRow(
                                       div(
                                         verbatimTextOutput("daoutput")
                                       ),
                                       div(
                                         plotOutput("daplot")
                                       )
                                     )
                                   )
                                 )
                                 
                        ),
                        
                        tabPanel("Reliability Analysis",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("relalpha", "Choose the variables:", choices = "", selected = "", multiple = TRUE),
                                     hr(),
                                     helpText("For more details visit:"),
                                     a(href="https://en.wikipedia.org/wiki/Cronbach%27s_alpha", "Cronbach's alpha"),
                                     hr()
                                   ),
                                   mainPanel(
                                     h3("Reliability Analysis"),
                                     div(
                                       verbatimTextOutput("reloutput")
                                     )
                                   )
                                 )
                                 
                        ),
                        
                        tabPanel("Factor Analysis",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("fadataset", "Choose the Variables:", choices = "", selected = "", multiple = TRUE),
                                     textInput("nf", "Mention No. of Factors", value = "2", width = NULL, placeholder = "Write here number of factors"), 
                                     hr(),
                                     helpText("For details visit:"),
                                     a(href="https://en.wikipedia.org/wiki/Factor_analysis", "Factor Analysis"),
                                     hr()
                                   ),
                                   mainPanel(
                                     h3("Factor Analysis"),
                                     div(
                                       verbatimTextOutput("faoutput")
                                     ), 
                                     div(
                                       plotOutput("faplot")
                                     )
                                   )
                                 )
                                 
                        ),
                        
                        tabPanel("Cluster Analysis",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("cavars", "Choose Variables:", choices = "", selected = "", multiple = TRUE),
                                     textInput("nc", "Number of Clusters:", value = "2", placeholder = "Choose No. Clusters"), 
                                     radioButtons("showcl", "Show Individuals by Clusters:", choices = c("ShowClus", "NoClus")),
                                     hr(),
                                     helpText("For more details visit:"),
                                     a(href="https://en.wikipedia.org/wiki/Cluster_analysis", "Cluster analysis"),
                                     hr()
                                   ),
                                   mainPanel(
                                     h3("Cluster Analysis"),
                                     div(
                                       verbatimTextOutput("caoutput")
                                     ), 
                                     div(
                                       plotOutput("caplot")
                                     )
                                   )
                                 )
                        )
             ), 
             
             # tabPanel("REPORTS", 
             #          sidebarLayout(
             #            sidebarPanel("Reports"), 
             #            mainPanel(tableOutput("pdfview"))
             #          )
             # ),        
             
             tabPanel("About", 
                      sidebarLayout(
                        sidebarPanel(
                          "Information about this App"
                        ), 
                        mainPanel(htmlOutput("text1"))
                      )
             )
  )
) 

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  # for DATASET TAB
  
  data_input <- reactive({
    infile <- input$file1
    req(infile)
    data.frame(read.csv(infile$datapath)) 
  })
  
  observeEvent(input$file1,{
    updateSelectInput(session, inputId = "cols", choices = names(data_input()))
  }
  )
  
  logno <- reactive({
    df <- data_input()
    x <- matrix(NA, length(df[, input$cols]), length(df[, input$cols][[1]]))
    for(i in 1:length(df[, input$cols])){
      for(j in 1:length(df[, input$cols][[1]])){
        x[i, j] <- dlnorm(df[, input$cols][[i]][j]) 
      }
    }
    return(t(x))
  })
  
  standout <- reactive({
    df <- data_input()
    
    x <- matrix(NA, length(df[, input$cols]), length(df[, input$cols][[1]]))
    
    if(!is.list(df[, input$cols])){
      df[, input$cols] <- list(df[, input$cols])
    }
    
    for(i in 1:length(df[, input$cols])){
      
      for(j in 1:length(df[, input$cols][[1]])){
        x[i, j] <- df[, input$cols][[i]][j]-mean(df[, input$cols][[i]])/sd(df[, input$cols][[i]])
      }
    }
    return(t(x))
    
  })
  
  output$tab1 <- renderTable(
    {
      df <- data_input()
      
      if (input$indata == "Full"){
        print(df)
      } else if(input$trans1 == "Not-Required"){
        data <- df[, input$cols]
        print(data)
      } else if(input$trans1 == "log"){
        data <- log(df[input$cols])
        print(data)
      } else if(input$trans1 == "inverselog"){
        data <- 1/log(df[input$cols])
        print(data)
      } else if(input$trans1 == "exponential"){
        data <- exp(df[input$cols])
        print(data)
      } else if(input$trans1 == "lognormal"){
        logno()
      } else if(input$trans1 == "standardize"){
        standout()
      }
      
    }
  )
  
  
  output$downloaddatset <- downloadHandler(
    
    filename <- function(){
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    
    content <- function(file){
      df <- data_input()
      write.csv(df[, input$cols], file, row.names = TRUE)
    }
    
  )
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols1", choices = names(data_input()))
  }
  )
  
  summ <- reactive({
    var1 <- data_input()[,input$cols1]
    
    su <- summary(var1)
    return(su)
  })
  
  output$summar <- renderPrint({
    summ()
  })
  
  # frequency tab
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols2", choices = names(data_input()))
    updateSelectInput(session, inputId = "cols3", choices = names(data_input()))
  }
  )
  
  freq <- reactive({
    var1 <- data_input()[,input$cols2]
    var2 <- data_input()[,input$cols3]
    fre <- table(var1, var2)
    return(fre)
  })
  
  output$freq_tables <- renderPrint({
    freq()
  })
  
  # Cross tabulation
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols4", choices = names(data_input()))
    updateSelectInput(session, inputId = "cols5", choices = names(data_input()))
  }
  )
  
  cross <- reactive({
    var1 <- data_input()[,input$cols4]
    var2 <- data_input()[,input$cols5]
    
    cro <- chisq.test(var1, var2)
    return(cro)
  })
  
  output$chi_t <- renderPrint({
    cross()
    
  })
  
  # Plots 
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols6", choices = names(data_input()))
  }
  )
  
  output$plot <- renderPlot({
    df <- data_input()
    if(input$plotoption == "Histogram"){
      hist(df[, input$cols6], freq = FALSE, xlab = input$xaxisname, ylab = input$yaxisname, main = input$title); lines(density(df[, input$cols6]), col = "red", lwd = 1.5)
    } else if(input$plotoption == "BarPlot"){
      barplot(df[, input$cols6], xlab = input$xaxisname, ylab = input$yaxisname, main = input$title)
    } else if(input$plotoption == "Scatter"){
      scatter.smooth(df[, input$cols6], xlab = input$xaxisname, ylab = input$yaxisname, main = input$title)
    } else {
      pie(table(df[, input$cols6]))
    }
  })
  
  # Statistical Tests
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols7", choices = names(data_input()))
    updateSelectInput(session, inputId = "cols8", choices = names(data_input()))
  }
  )
  
  output$qqp <- renderPlot({
    df <- data_input()
    qqnorm(df[, input$cols7]);qqline(df[, input$cols7])
  })
  
  adt <- reactive({
    df <- data_input()
    var <- df[, input$cols7]
    ad <- ad.test(var)
    return(ad)
  })
  
  sht <- reactive({
    df <- data_input()
    var <- df[, input$cols7]
    sh <- shapiro.test(var)
    return(sh)
  })
  
  kst <- reactive({
    df <- data_input()
    var1 <- df[, input$cols7]
    var2 <- df[, input$cols8]
    ks <- ks.test(var1, var2)
    return(ks)
  })
  
  mvst <- reactive({
    df <- data_input()
    var1 <- df[, input$cols7]
    var2 <- df[, input$cols8]
    return(mshapiro.test(t(as.data.frame(var1, var2))))
  })
  
  output$normtest <- renderPrint({
    
    if(input$normaltest == "A-D-Test"){
      print(adt())
    } else if(input$normaltest == "Shapiro"){
      print(sht())
    } else if(input$normaltest == "KS-Test"){
      print(kst())
    } else if(input$normaltest == "MV-Shapiro"){
      print(mvst())
    }
    
  }
  
  )
  # correlation & regression 
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols9", choices = names(data_input()))
    updateSelectInput(session, inputId = "cols10", choices = names(data_input()))
  }
  )
  
  cortest <- reactive({
    var1 <- data_input()[,input$cols9]
    var2 <- data_input()[,input$cols10]
    if (input$cormethod == "KarlPearson"){
      return(cor.test(var1, var2, method = "pearson"))
    } else if(input$cormethod == "Spearman"){
      return(cor.test(var1, var2, method="spearman"))
    } else {
      return(cor.test(var1, var2, method="kendall"))
    }
  }
  )
  
  output$cor_t <- renderPrint({
    cortest()
  })
  
  # Regression
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols11", choices = names(data_input()))
    updateSelectInput(session, inputId = "cols12", choices = names(data_input()))
  }
  )
  
  reganal <- reactive({
    df <- data_input()
    var1 <- df[, input$cols11]
    var2 <- df[, input$cols12]
    rego <- lm(var1 ~ var2, data = df)
    return(list(fit = rego, fitsum = summary(rego), anov = anova(rego)))
    
  })
  
  output$regout <- renderPrint({
    if (input$regmethod == "Fit"){
      reganal()$fit
    } else if(input$regmethod == "Summary"){
      reganal()$fitsum
    } else if(input$regmethod == "ANOVA"){
      reganal()$anov
    }
  })
  
  output$regplot <- renderPlot({
    df <- data_input()
    var1 <- df[, input$cols11]
    var2 <- df[, input$cols12]
    plot(var1, var2); abline(lm(var1 ~ var2, data = df), col = "red", lwd=2)
  })
  
  # MANOVA
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols13", choices = names(data_input()))
    updateSelectInput(session, inputId = "cols14", choices = names(data_input()))
  }
  )
  
  manovaform <- reactive({
    df <- data_input()
    #formula <- as.formula(paste(cbind(df[, input$cols13]), '~', df[, input$cols14])) 
    manform <- as.formula(paste("cbind(unlist(rbind(df[, input$cols13]))[1:length(df[, input$cols14])], unlist(rbind(df[, input$cols13]))[length(df[, input$cols14])+1:length(df[, input$cols14])*2])", "~", "df[, input$cols14]"))
    return(manform)
  })
  
  manovaanal <- reactive({
    df <- data_input()
    manout <- manova(manovaform(), data = df)
    return(manout)
  })
  
  output$manovaout <- renderPrint({
    if(input$manmethod=="Fit"){
      manovaanal()
    } else if(input$manmethod == "Summary"){
      summary(manovaanal())
    }
    
  })
  
  output$manovaplot <- renderPlot({
    df <- data_input()
    var1 <- df[, input$cols13]
    var2 <- df[, input$cols14]
    plot(data.frame(var1, var2))
  }) 
  
  # Forecasting
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "forcvar", choices = names(data_input()))
  }
  )
  
  # tsconver <- reactive({
  #   df <- data_input()
  #   # out <- ts(df[, input$forcvar], frequency = input$freq, start = c(input$startyr, input$startmonth))
  #   out <- ts(df[, input$forcvar]) # , freq = input$freq, start = c(input$startyr, input$startmonth))
  #   return(out)
  # })
  
  # output$tsconvert <- renderPrint({
  #   tsconver()
  # })
  
  tstasks <- reactive({
    df <- data_input()
    
    if (input$forctasks == "Convert"){
      out <- ts(df[, input$forcvar]) # , freq = input$freq, start = c(input$startyr, input$startmonth))
      return(out)
    } else if(input$forctasks == "Make-Stationary"){
      dif <- diff(df[, input$forcvar], input$forclag, input$forcdiff)
      return(dif)
    } else if (input$forctasks == "Decompose"){
      out <- decompose(ts(df[, input$forcvar], frequency = 4, start = 1), type = input$additvsmult)
      return(out)
    } else if (input$forctasks == "De-trend"){
      dtmodel <- lm(df[, input$forcvar] ~ c(1:length(df[, input$forcvar])))
      out <- resid(dtmodel)
      return(out)
    } else if(input$forctasks == "De-Seasonalize"){
      ddc <- decompose(ts(df[, input$forcvar], frequency = 4, start = 1), type = input$additvsmult)
      out <- df[, input$forcvar]-unlist(ddc["seasonal"])
      return(as.data.frame(out)$out)
    } else if(input$forctests == "ADF"){
      out <- adf.test(ts(df[, input$forcvar], frequency = 4, start = 1))
      return(out)
    } else if(input$forctests == "KPSS"){
      out <- kpss.test(ts(df[, input$forcvar], frequency = 4, start = 1))
      return(out)
    } else if(input$forctasks == "ACF"){
      out <- acf(ts(df[, input$forcvar]))
      return(out)
    } else if(input$forctasks == "PACF"){
      out <- pacf(ts(df[, input$forcvar]))
      return(out)
    } else if(input$forcanal == "Moving-Averages"){
      out <- SMA(ts(df[, input$forcvar]))
      return(out)
    } else if(input$forcanal == "Exponential(HW)"){
      out <- HoltWinters(ts(df[, input$forcvar], frequency = 4, start = 1))
      return(out)
    }
  })
  
  desctext <- reactive({
    cat("Welcome to Forecasting; Following is the very little documentation on methods", "\n", "Convert: Converts given data variable into a time series data.",
        "\n", "Make-Stationary: Makes time series into stationary; requires inputs viz. Lag, Diff.",
        "\n", "Decompose: Decomposes data set into three components viz. trend, seasonal, random; methods - 'additive' or 'mult'.",
        "\n", "Detrend: Eleminates Trend component.",
        "\n", "Deseasonlize: Eleminates Seasonal component.",
        "\n", "ACF: Autocorrelation Function.",
        "\n", "PACF: Partial Autocorrelation Function.",
        "\n", "Predict: Not implemented Yet.",
        "\n", "Moving Averages: Computes moving average default number is 1.",
        "\n", "Exponential(HW): Computes Holt-Winter estimates.")
    
  })
  
  output$forcoutput <- renderPrint({
    
    if (input$forctasks == "Description"){
      desctext()
    } else if(input$forctasks == "Convert"){
      tstasks()
    } else if(input$forctasks == "De-trend"){
      list(head(tstasks()), "Only first 6 records are displayed")
    } else if(input$forctasks == "Make-Stationary"){
      return(tstasks())
    } else if(input$forctasks == "Decompose"){
      tstasks()
    } else if(input$forctasks == "De-Seasonalize"){
      print(list(info = "Only First Few Records are Printed", head(tstasks())))
    } else if(input$forctests == "ADF"){
      tstasks()
    } else if(input$forctests == "KPSS"){
      tstasks()
    } else if(input$forctasks == "ACF"){
      tstasks()$acf
    } else if(input$forctasks == "PACF"){
      tstasks()$acf
    } else if(input$forcanal == "Moving-Averages"){
      tstasks()
    } else if(input$forcanal == "Exponential(HW)"){
      tstasks()
    }
  }
  
  )
  
  output$forcplot <- renderPlot({
    # df <- data_input()
    if (input$forctasks == "De-trend" & input$forcplottype == "TS"){
      plot(tstasks(), type = "b", col = "red")
    } else if (input$forctasks == "Make-Stationary" & input$forcplottype == "TS"){
      plot(tstasks(), type = "b", col = "red", lwd = 1.75)
    } else if (input$forctasks == "Decompose" & input$forcplottype == "TS"){
      plot(tstasks(), type = "b", col = "red", lwd = 1.75)
    } else if(input$forctasks == "De-Seasonalize" & input$forcplottype == "TS"){
      plot(tstasks(), type ="b", col = "red", lwd = 1.75)
    } else if (input$forctasks == "Convert" & input$forcplottype == "TS"){
      plot(tstasks(), type = "b", col = "red", lwd = 1.75)
    } else if(input$forctasks == "ACF" & input$forcplottype == "ACF"){
      plot(tstasks())
    } else if(input$forctasks == "PACF" & input$forcplottype == "PACF"){
      plot(tstasks())
    } else if(input$forcanal == "Moving-Averages" & input$forcplottype == "TS"){
      plot(tstasks())
    } 
  })
  
  # Exploratory 
  # Disctiminant Analysi s
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "dafactvar", choices = names(data_input()))
    updateSelectInput(session, inputId = "daNumvar1", choices = names(data_input()))
    updateSelectInput(session, inputId = "daNumvar2", choices = names(data_input()))
    updateSelectInput(session, inputId = "daNumtvar3", choices = names(data_input()))
  }
  )
  
  daout <- reactive({
    df <- data_input()
    var1 <- df[, input$dafactvar]
    var2 <- df[, input$daNumvar1]
    var3 <- df[, input$daNumvar2]
    var4 <- df[, input$daNumtvar3]
    daformula <- as.formula(paste("var1", "~", "var2", "+", "var3", "+", "var4"))
    fit <- lda(daformula, data = df)
    return(fit)
    
  })
  
  output$daoutput <- renderPrint({
    daout()
  })
  
  output$daplot <- renderPlot({
    plot(daout(), dimen=1, type="both")
  })
  
  # Reliability analysis 
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "relalpha", choices = names(data_input()))
  }
  )
  
  relout <- reactive({
    df <- data_input()
    out <- alpha(df[, input$relalpha])
    return(out)
    
  })
  
  output$reloutput <- renderPrint({
    relout()
  })
  
  # FActor analysis 
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "fadataset", choices = names(data_input()))
  }
  )
  
  faout <- reactive({
    df <- data_input()
    # out <- factanal(matrix(unlist(list(df[, input$fadataset])), dim(df)[1], length(input$fadataset)), input$nf)
    out <- fa(df[, input$fadataset], input$nf)
    return(out)
  })
  
  output$faoutput <- renderPrint({
    faout()
  })
  
  output$faplot <- renderPlot({
    plot(faout())
  })
  
  # Cluster analysis 
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cavars", choices = names(data_input()))
  }
  )
  
  caout <- reactive({
    df <- data_input()
    out <- kmeans(df[,input$cavars], input$nc)
    return(out)
  })
  
  output$caoutput <- renderPrint({
    df <- data_input()
    if(input$showcl == "NoClus"){
      caout()
    } else if(input$showcl == "ShowClus"){
      out <- cbind(1:dim(df)[1], caout()$cluster)
      colnames(out) <- c("individuals", "Cluster")
      print(out)
    }
    
  })
  
  output$caplot <- renderPlot({
    df <- data_input()
    clusplot(df, caout()$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)
  })
  
  
  # About this App 
  
  output$text1 <- renderText({
    str1 <- paste("Created using R Shiny") 
    str2 <- paste("for S3729C Lesson 14") 
    str3 <- paste("visit https://shiny.rstudio.com for more information")
    HTML(paste(str1, str2, str3, sep = '<br/>'))
  })
  
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui, server)