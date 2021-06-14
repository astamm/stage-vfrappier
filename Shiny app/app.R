#Shiny app pour le projet de stage
#Objectif : Cr?er un diagramme interactif permettant d'illustrer le meilleur intervalle de confiance

library(shiny)
library(ggplot2)
library(tidyverse)
library(tibble)


# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Best CI"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of observations to generate ----
      sliderInput("n", "Number of observations:",
                  value = 20,
                  min = 5,
                  max = 100),

      # Input: Selector for choosing parameter to estimate ----
      selectInput(inputId = "parameter",
                  label = "Choose a parameter that you would like to estimate:",
                  choices = c("mean"="moy",
                              "variance"="var")),

      # Input: Slider for the number of bins ----
      sliderInput(inputId = "alpha", label = "Select a value for alpha:",
                  min = 0,
                  max = 10,
                  value = 5,
                  step = 1,
                  post = "%",
                  sep = ",",
                  animate = FALSE
                  ),
#-------------------------------------------------------------------------------
#Calcul du meilleur IC possible avec ces inputs
#-------------------------------------------------------------------------------
      #Input for the comparison criteria wanted
      radioButtons("criteria", "Comparison criteria you would like to use for determining the best CI:",
                 c("Centered and Symetrical" = "cenandsym",
                   "Minimal length" = "minl")),


    br(),

      # Input: Slider for the number of bins ----
      sliderInput(inputId = "lambda",
                  label = "Select a value for lambda:",
                  min = 0,
                  max = 1,
                  value = 0.5,
                  step = 0.01)


),
    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Distribution
      plotOutput(outputId = "distPlot"),

      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),

      # Output: HTML table with requested number of observations ----
      tableOutput("view")

    )

  )
)
# Define server logic required to draw a histogram ----
server <- function(input, output) {


  tab <- reactive({
    tibble(
      x = switch(
        input$parameter,
        moy = seq(-3 * sqrt((input$n-1)/(input$n-3)), 3*sqrt((input$n-1)/(input$n-3)), len = 1000),
        var = seq((input$n-1) - 3*sqrt(2*input$n-1), (input$n-1) + 3*sqrt(2*input$n-1), len = 1000)
      ),
      y = switch(
        input$parameter,
        moy = dt(x, df = input$n-1),
        var = dchisq(x, df = input$n-1)
      )
    )
  })
  
  tabq <- reactive({
    tibble(
      x2 = switch(
        input$parameter,
        moy = qt(seq(1e-4, 1-1e-4, len = 10000),input$n-1),
        var = qchisq(seq(1e-4, 1-1e-4, len = 10000),input$n-1)
      )
    )
  })


  #
  output$distPlot <- renderPlot({
    ggplot(tab(), aes(x = x, y = y)) +
      geom_line() +
      labs(x = "X", y = expression(f[X](x))) 
    
    ggplot(tabq(),aes(x2, y = 0, fill = factor(stat(quantile))), show.legend = FALSE) +
      ggridges::stat_density_ridges(
        geom = "density_ridges_gradient",
        calc_ecdf = TRUE,
        quantiles = c(input$lambda*(input$alpha/100),1-(input$alpha/100)*(1-input$lambda))
      ) +
      scale_fill_manual(
        name = paste("Quantiles d'ordre ",input$lambda*(input$alpha/100)," et ", 1-(input$alpha/100)*(1-input$lambda), sep=""),
        values = c("blue","pink","blue"),
        labels = scales::label_parse()(c("P(X <= q[0.90]) == 0.90", "P(X >= q[0.90]) == 0.10"))
      ) +
      labs(x = "", y = "") +
      # theme(legend.position = none) +
      theme_bw() +
      geom_vline(xintercept = switch(input$parameter,
                                     moy = qt(input$lambda*(input$alpha/100),input$n-1),
                                     var = qchisq(input$lambda*(input$alpha/100),input$n-1)), 
                 color = "black", linetype = "dashed") +
      
      annotate(geom = "label", x = switch(input$parameter,
                                          moy = qt(input$lambda*(input$alpha/100),input$n-1),
                                          var = qchisq(input$lambda*(input$alpha/100),input$n-1)),
                               y = switch(input$parameter,
                                          moy = -0.04,
                                          var =-0.001),
               label = expression(q[alpha])) +
      
      geom_vline(xintercept = switch(input$parameter,
                                     moy = qt(1-(input$alpha/100)*(1-input$lambda),input$n-1),
                                     var = qchisq(1-(input$alpha/100)*(1-input$lambda),input$n-1)), 
                 color = "black", linetype = "dashed") +
      
      annotate(geom = "label", x = switch(input$parameter,
                                          moy = qt(1-(input$alpha/100)*(1-input$lambda),input$n-1),
                                          var = qchisq(1-(input$alpha/100)*(1-input$lambda),input$n-1)),
               y = switch(input$parameter,
                          moy = -0.04,
                          var =-0.001),
               label = expression(q[1-alpha])) +
      theme(legend.position = "none")
  })
}
shinyApp(ui = ui, server = server)




#?tibble
#?sliderInput
# exists("parameter")
#?geom_line
# ?stat_density_ridges

#?theme

# #?runApp
# #runApp("C:\Users\valentin\Documents\Universit?\Stage L3\Repositories\stage-vfrappier\Shiny app")
# #?switch
# #?distribution
# #runExample("01_hello")      # a histogram
#  runExample("02_text")       # tables and data frames
# # runExample("03_reactivity") # a reactive expression
# # runExample("04_mpg")        # global variables
# #runExample("05_sliders")    # slider bars
# runExample("06_tabsets")    # tabbed panels
# #runExample("07_widgets")    # help text and submit buttons
# #runExample("08_html")       # Shiny app built from HTML
# #runExample("09_upload")     # file upload wizard
# #runExample("10_download")   # file download wizard
#runExample("11_timer")      # an automated timer
