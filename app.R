# Packages #
if (!require(shiny)) install.packages("shiny")
if (!require(shinythemes)) install.packages("shinythemes")
if (!require(bootstrap)) install.packages("bootstrap")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(plyr)) install.packages("plyr")
if (!require(dplyr)) install.packages("dplyr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(viridis)) install.packages("viridis")
if (!require(ggforce)) install.packages("ggforce")

## simple app

ui <- fluidPage(
                titlePanel(
                  tags$div(class = "jumbotron text-center"
                           , style = "margin-bottom:5px; margin-top:5px",
                           tags$h2(class = 'jumbotron-heading', stye = 'margin-bottom:0px;margin-top:0px', 'Circle Art'),
                           p('Change the sliders to generate your bespoke image, then click Plot Me!'),
                  )
                ),
                fluidRow(
                  column(4,
                  sliderInput("ncirc.in",
                              "Number of circles:",
                              min = 1,
                              max = 2000,
                              step = 1,
                              value = 1000),
                  sliderInput("r.in",
                              "circle radius:",
                              min = 0.1,
                              step = 0.1,
                              max = 2,
                              value = 0.5),
                  sliderInput("alpha.in",
                              "circle transparency:",
                              min = 0.1,
                              step = 0.1,
                              max = 1,
                              value = 0.5),
                  actionButton("results","Plot Me!"),
                  downloadButton('downloadPlot','Download Me!')
                ),
                column(8,
                       plotOutput("circPlot")
                ),
                fluidRow(
                  column(12, align = "right",
                         img(src = "imageShiny.png",
                             width = "70px", height = "70px")
                         )
                )
                )
              )

server <- function(input, output, session){
  


  data <- reactiveValues()
  observeEvent(input$results,{
  circs <- input$ncirc.in
  rad <- input$r.in
  
   circ.df <- data.frame(
      x = round(runif(circs, -10, 10), 2)
    , y = round(runif(circs, -10, 10), 2)
    , r = rad
    , circ.n = seq(1, circs, 1))
      
    data$plot <- ggplot(circ.df, aes(x = x, y = y)) +
      geom_point(aes(colour = circ.n), show.legend = FALSE, alpha = 0, size = 0) +
      scale_colour_viridis() +
      geom_circle(data = circ.df, aes(x0 = x, y0 = y, r = r, fill = circ.n), alpha = input$alpha.in, size = 0.1, show.legend = FALSE) +
      scale_fill_viridis() +
      theme_void()}
)
   
  output$circPlot  <- renderPlot({data$plot})

  output$downloadPlot <- downloadHandler(
    filename = function(){paste("MyCircleArt", as.character(Sys.Date()), ".png", sep = "")},
    content = function(file){
   ggsave(file, plot = data$plot)
    }
  )
}
shinyApp(ui, server)