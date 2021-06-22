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
      sliderInput("n", "Sample size:",
                  value = 20,
                  min = 5,
                  max = 100),
      
      # Input: Selector for choosing parameter to estimate ----
      selectInput(inputId = "parameter",
                  label = "Choose the parameter that you would like to estimate:",
                  choices = c("mean"="moy",
                              "variance"="var")),
      
      p("Alpha is the probability of commiting a type I error"),
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
      
      p("Lambda is a coefficient in [0;1] of alpha which allows us to build a infinity of confidence intervalls of level 1-alpha."),
      p("Play with its value to showcase different CI of level 1-minus alpha and their associated quantiles"),
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
      
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("distPlot")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", tableOutput("table"))
      )
      
    )
    
  )
)

# VALENTIN: ici il faut integrer un param suppémentaire pour savoir sur quelle chose doit se faire la centralisation
# + reecrire les fonctions pour la moyenne qui ne sont pas bonnes


#Length of the CI to minimize via uniroot in lambda_opt for mean and variance
ci_minimal_length_mean <- function(w, n, s2 = 1, alpha) {
  qsup <- qt(1 - w * alpha, df = n - 1)
  qinf <- qt((1 - w) * alpha, df = n - 1)
  sqrt(s2/n)*(qsup-qinf)
}

ci_minimal_length_var <- function(w, n, s2 = 1, alpha) {
  w <- 1 - w
  qsup <- qchisq(1 - w * alpha, df = n - 1)
  qinf <- qchisq((1 - w) * alpha, df = n - 1)
  s2 * (n-1) * (1 / qinf - 1 / qsup)
}

#Functions representing the centered aspect of the CI. CI centered => corresponding function = 0
ci_centered_symmetric_mean <- function(w, n, s2 = 2, alpha) {
  qsup <- qt(1 - w * alpha, df = n - 1)
  qinf <- qt((1 - w) * alpha, df = n - 1)
  2*mean(rt(n,df=n-1)) + sqrt(s2/n)*(qsup-qinf)
}

ci_centered_symmetric_varmode <- function(w, n, s2 = 2, alpha) {
  w <- 1 - w
  qsup <- qchisq(1 - w * alpha, df = n - 1)
  qinf <- qchisq((1 - w) * alpha, df = n - 1)
  mode = s2 * (n-1) * (1 / qinf + 1 / qsup - 2/(n-3))
}


ci_centered_symmetric_varmean <- function(w, n, s2 = 2, alpha) {
  w <- 1 - w
  qsup <- qchisq(1 - w * alpha, df = n - 1)
  qinf <- qchisq((1 - w) * alpha, df = n - 1)
  mean = s2 * ((n-1) * (1 / qinf + 1 / qsup) - 2) 
  
}

ci_centered_symmetric_varmed <- function(w, n, s2 = 2, alpha) {
  w <- 1 - w
  qsup <- qchisq(1 - w * alpha, df = n - 1)
  qinf <- qchisq((1 - w) * alpha, df = n - 1)
  med = s2 * ((n-1) * (1 / qinf + 1 / qsup) - 2/(1-2/(n-1))^3)
}

eps0 <- .Machine$double.eps

# Define server logic required to draw the distribution ----
server <- function(input, output, session) {
  
  #Calculation of the lambda corresponding to the best CI for both parameter mean and variance,
  #for an alpha and a sample size selected by the user
  lambda_opt <- reactive({
    switch(input$criteria,
           cenandsym = switch(input$parameter,
                              moy = uniroot(
                                f = ci_centered_symmetric_mean,
                                interval = c(eps0, 1 - eps0),
                                n = input$n,
                                alpha = input$alpha / 100
                              )$root,
                              var = switch(input$center,
                                           mode = uniroot(
                                             f = ci_centered_symmetric_varmode,
                                             interval = c(eps0, 1 - eps0),
                                             n = input$n,
                                             alpha = input$alpha / 100
                                           )$root,
                                           
                                           mean = uniroot(
                                             f = ci_centered_symmetric_varmean,
                                             interval = c(eps0, 1 - eps0),
                                             n = input$n,
                                             alpha = input$alpha / 100
                                           )$root,
                                           
                                           med = uniroot(
                                             f = ci_centered_symmetric_varmed,
                                             interval = c(eps0, 1 - eps0),
                                             n = input$n,
                                             alpha = input$alpha / 100
                                           )$root
                              )
                              
           ),
           minl = switch(input$parameter,
                         moy = optimise(
                           f = ci_minimal_length_mean,
                           interval = c(eps0, 1 - eps0),
                           n = input$n,
                           alpha = input$alpha / 100
                         )$minimum,
                         var = optimise(
                           f = ci_minimal_length_var,
                           interval = c(eps0, 1 - eps0),
                           n = input$n,
                           alpha = input$alpha / 100
                         )$minimum
           )
    )
  })
  #creating a data frame in order to draw the distribution : variables x = choice between two 
  #sequances of values abscisse of the distribution ; y = choice between two probability laws
  tab <- reactive({
    tibble(
      x = switch(
        input$parameter,
        moy = seq(-3 * sqrt((input$n-1)/(input$n-3)),
                  3*sqrt((input$n-1)/(input$n-3)), len = 1000),
        var = seq((input$n-1) - 3*sqrt(2*input$n-1), (input$n-1) +
                    3*sqrt(2*input$n-1), len = 1000)
      ),
      y = switch(
        input$parameter,
        moy = dt(x, df = input$n-1),
        var = dchisq(x, df = input$n-1)
      )
    )
  })
  
  #creating a data frame to plot the quantiles of each distribution (student and chi-square) for 
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
  #Output that plots the distribution selected, the quantile of order lambda*alpha and 
  #1-alpha(1-lambda
  output$distPlot <- renderPlot({
    ggplot(tab(), aes(x = x, y = y)) +
      geom_line() +
      labs(x = "X", y = expression(f[X](x)))
    
    #plots the two quantiles and the color the area from the tails of the distribution to the quantile
    ggplot(tabq(),aes(x2, y = 0, fill = factor(stat(quantile))),
           show.legend = FALSE) +
      ggridges::stat_density_ridges(
        geom = "density_ridges_gradient",
        calc_ecdf = TRUE,
        quantiles =
          c(input$lambda*(input$alpha/100),1-(input$alpha/100)*(1-input$lambda))
      ) +
      scale_fill_manual(
        name = paste("Quantiles d'ordre ",input$lambda*(input$alpha/100),"
et ", 1-(input$alpha/100)*(1-input$lambda), sep=""),
        values = c("blue","pink","blue"),
        labels = scales::label_parse()(c("P(X <= q[0.90]) == 0.90", "P(X
= q[0.90]) == 0.10"))
      ) +
      labs(x = "", y = "") +
      theme_bw() +
      geom_vline(xintercept = switch(input$parameter,
                                     moy =
                                       qt(input$lambda*(input$alpha/100),input$n-1),
                                     var =
                                       qchisq(input$lambda*(input$alpha/100),input$n-1)),
                 color = "black", linetype = "dashed") +
      
      annotate(geom = "label", x = switch(input$parameter,
                                          moy =
                                            qt(input$lambda*(input$alpha/100),input$n-1),
                                          var =
                                            qchisq(input$lambda*(input$alpha/100),input$n-1)),
               y = switch(input$parameter,
                          moy = -0.04,
                          var =-0.001),
               label = expression(q[lambda*alpha])) +
      #vertical black dashed line at the quantile of order 1-alpha(1-lambda) abscissa
      geom_vline(xintercept = switch(input$parameter,
                                     moy =
                                       qt(1-(input$alpha/100)*(1-input$lambda),input$n-1),
                                     var =
                                       qchisq(1-(input$alpha/100)*(1-input$lambda),input$n-1)),
                 color = "black", linetype = "dashed") +
      
      #changing annotations for the quantile of order 1-alpha(1-lambda) and plot scale options
      annotate(geom = "label", x = switch(input$parameter,
                                          moy =
                                            qt(1-(input$alpha/100)*(1-input$lambda),input$n-1),
                                          var =
                                            qchisq(1-(input$alpha/100)*(1-input$lambda),input$n-1)),
               y = switch(input$parameter,
                          moy = -0.04,
                          var =-0.001),
               label = expression(q[1-alpha(1-lambda)])) +
      #plotting vertical lines matching the quantiles used to build the best CI for the selected parameters
      geom_vline(
        xintercept = switch(input$parameter,
                            moy = qt(lambda_opt()*(input$alpha/100),input$n-1),
                            var = qchisq(lambda_opt()*(input$alpha/100),input$n-1)),
        color = "orange",
        linetype = "dashed"
      ) +
      geom_vline(
        xintercept = switch(input$parameter,
                            moy = qt(1-(input$alpha/100)*(1-lambda_opt()),input$n-1),
                            var = qchisq(1-(input$alpha/100)*(1-lambda_opt()),input$n-1)),
        color = "orange",
        linetype = "dashed"
      ) +
      theme(legend.position = "none")
  })
  
  output$table <- renderTable({
    lambda_opt()
  })
}
shinyApp(ui = ui, server = server)