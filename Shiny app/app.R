#Shiny app pour le projet de stage
#Objectif : Cr?er un diagramme interactif permettant d'illustrer le meilleur intervalle de 
#confiance pour estimer la moyenne ou la variance d'une v.a X suivant une distribution normale,
#ainsi que les quantiles permettant d'illustrer cet intervalle 

library(shiny)
library(ggplot2)
library(tidyverse)
library(tibble)
library (shinyjs)

# Define UI for the app
ui <- fluidPage(

  # App title ----
  titlePanel("Illustration of the best CI for a chosen parameter, and associated quantiles"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of observations ----
      sliderInput("n", "Number of observations:",
                  value = 20,
                  min = 5,
                  max = 100),

      # Input: Selector for choosing parameter to estimate ----
      selectInput(inputId = "parameter",
                  label = "Choose a parameter that you would like to estimate:",
                  choices = c("mean"="moy",
                              "variance"="var")),

      # Input: Slider for the value of alpha, probability of commiting a type I error  ----
      sliderInput(inputId = "alpha", label = "Select a value for alpha:",
                  min = 0,
                  max = 10,
                  value = 5,
                  step = 1,
                  post = "%",
                  sep = ",",
                  animate = FALSE
                  ),
      
      #shinyjs::useShinyjs(),
      #Input: Buttons for selecting the comparison criteria
      radioButtons("criteria", "Comparison criteria you would like to use for determining the best CI:",
                 c("Centered and Symetrical" = "cenandsym",
                   "Minimal length" = "minl")),
      
      #Input: Buttons for selecting the quantity to center on, if the centered and symetrical
#criteria is selected
      radioButtons("center", "Quantity you would like to center on: ",
                   c("Mode" = "mode",
                     "Mean" = "mean",
                     "Median"="med")),


      # Input: Slider for the value of lambda, a coefficient before alpha in [0,1] ----
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
# Define server logic required to draw a the distribution of the pivotal variable, its quantiles,
# and to determine and display the best CI possible for the estimation of each parameter ----
server <- function(input, output) {

#creating a data frame in order to draw the distribution : variables x = choice between two 
#sequances of values abscisse of the distribution ; y = choice between two probability laws
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
  
#creating a data frame to plot the quantiles of each distribution (student and chisquare) for 
#alpha in ]0,1[ 
  tabq <- reactive({
    tibble(
      x2 = switch(
        input$parameter,
        moy = qt(seq(1e-4, 1-1e-4, len = 10000),input$n-1),
        var = qchisq(seq(1e-4, 1-1e-4, len = 10000),input$n-1)
      )
    )
  })

#Outpout that plots the dsitribution selected, the quantile of order lambda*alpha and 
#1-alpha(1-lambda)
  output$distPlot <- renderPlot({
    ggplot(tab(), aes(x = x, y = y)) +        #draws the selected distribution
      geom_line() +                           #marquers = line
      labs(x = "X", y = expression(f[X](x)))  #graphics labels

#plots the two quantiles and the color the area from the tails of the distribution to the quantile
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
      theme_bw() +
      #vertical black dashed line at the quantile of order lambda*alpha absciss
      geom_vline(xintercept = switch(input$parameter,
                                     moy = qt(input$lambda*(input$alpha/100),input$n-1),
                                     var = qchisq(input$lambda*(input$alpha/100),input$n-1)), 
                 color = "black", linetype = "dashed") +
     
      #changing annotations for the quantile of order lambda*alpha and plot scale options  
      annotate(geom = "label", x = switch(input$parameter,
                                          moy = qt(input$lambda*(input$alpha/100),input$n-1),
                                          var = qchisq(input$lambda*(input$alpha/100),input$n-1)),
                               y = switch(input$parameter,
                                          moy = -0.04,
                                          var =-0.001),
               label = expression(q[lambda%*%alpha])) +
      #vertical black dashed line at the quantile of order 1-alpha(1-lambda) absciss
      geom_vline(xintercept = switch(input$parameter,
                                     moy = qt(1-(input$alpha/100)*(1-input$lambda),input$n-1),
                                     var = qchisq(1-(input$alpha/100)*(1-input$lambda),input$n-1)), 
                 color = "black", linetype = "dashed") +
      #changing annotations for the quantile of order 1-alpha(1-lambda) and plot scale options
      annotate(geom = "label", x = switch(input$parameter,
                                          moy = qt(1-(input$alpha/100)*(1-input$lambda),input$n-1),
                                          var = qchisq(1-(input$alpha/100)*(1-input$lambda),input$n-1)),
               y = switch(input$parameter,
                          moy = -0.04,
                          var =-0.001),
               label = expression(q[1-alpha%*%(1-lambda)])) +
      theme(legend.position = "none") #Unshow legend
  })
}
shinyApp(ui = ui, server = server)