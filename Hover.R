library(shiny)
library(ggplot2)

dat <- data.frame(x = rnorm(100), y = rnorm(100))

ui <- basicPage(
  div(
    style = "position:relative",
    plotOutput("ggplot", hover = hoverOpts("plot_hover")),
    uiOutput("hoverinfo")
  )
)

server <- function(input, output){
  output$ggplot <- renderPlot(
    ggplot(dat, aes(x=x, y=y)) + geom_point()
  )
  
  output$hoverinfo <- renderUI({ 
    hover <- input[["plot_hover"]]
    if(is.null(hover)) return(NULL)
    
    point <- nearPoints(dat, hover, threshold = 5, maxpoints = 1)
    if(nrow(point) == 0) return(NULL)
    
    left_px <- hover$coords_css$x
    top_px  <- hover$coords_css$y
    
    style <- paste0(
      "position:absolute; z-index:100; pointer-events:none; ", 
      "background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 0, 
      "px; top:", top_px + 0, "px;"
    )
    
    # tooltip created as wellPanel
    tooltip <- paste0(
      "<b> x: </b>", point[["x"]], "<br/>",
      "<b> y: </b>", point[["y"]], "<br/>"
    )
    wellPanel(
      style = style, p(HTML(tooltip))
    )
  }) 
  
}

shinyApp(ui, server)