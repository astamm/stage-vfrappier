#Shiny app pour le projet de stage
#Objectif : Cr?er un diagramme interactif permettant d'illustrer le meilleur intervalle de 
#confiance pour estimer la moyenne ou la variance d'une v.a X suivant une distribution normale,
#ainsi que les quantiles permettant d'illustrer cet intervalle 

library(shiny)
library(ggplot2)
library(tidyverse)
library(tibble)
# library (shinyjs)
# library(hablar)


# Define UI for the app
ui <- fluidPage(
  
  # App title ----
  titlePanel("Illustration of the best CI for a chosen parameter, and associated quantiles"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # titlePanel("Sample simulation :"),
      p("Choose the value of the mean, and the value of the standard deviation of the normal distribution"),
      
      numericInput("mu", "Select a value for mu",
                   value = 0,
                   min = -5,
                   max = 5),
      
      numericInput("sigma", "Select a value for sigma",
                   value = 1,
                   min = 1,
                   max= 10),
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
                  step = 0.01),
      
      numericInput("nIC", "Select the number of CI you would like to plot",
                   value = 20,
                   min = 1,
                   max = 50)
      
      
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("distPlot")),
                  tabPanel("Table", tableOutput("table")),
                  tabPanel("Summary", tableOutput("table2")),
                  tabPanel("CItibble", tableOutput("table3")),
                  tabPanel("CI danse", plotOutput("CIPlot"))
                  
      )
      
    )
    
  )
)

#Length of the CI to minimize via uniroot in lambda_opt for mean and variance
ci_minimal_length_mean <- function(w, n, s2 = 1, alpha) {
  qsup <- qt(1 - w * alpha, df = n - 1)
  qinf <- qt((1 - w) * alpha, df = n - 1)
  sqrt(s2/n)*(qsup-qinf)
}

ci_minimal_length_var <- function(w, n, s2 = 1, alpha) {
  qsup <- qchisq(1 - (1-w) * alpha, df = n - 1)    #Production of NAN
  qinf <- qchisq(w * alpha, df = n - 1)        ##if na.omit, CIlength is of size 0
  s2 * (n-1) * (1 / qinf - 1 / qsup)
}

#Functions representing the centered aspect of the CI. CI centered => corresponding function = 0
ci_centered_symmetric_mean <- function(w, n, s2 = 2, alpha) {
  qsup <- qt(1 - (1-w) * alpha, df = n - 1)
  qinf <- qt(w * alpha, df = n - 1)
  -2*sqrt(s2/n)*(qsup + qinf)
}

ci_centered_symmetric_varmode <- function(w, n, s2 = 2, alpha) {
  qsup <- qchisq(1 - (1-w) * alpha, df = n - 1)
  qinf <- qchisq(w * alpha, df = n - 1)
  mode = s2 * (n-1) * (1 / qinf + 1 / qsup - 2/(n-3))
}


ci_centered_symmetric_varmean <- function(w, n, s2 = 2, alpha) {
  w <- 1 - w
  qsup <- qchisq(1 - (1-w) * alpha, df = n - 1)
  qinf <- qchisq(w * alpha, df = n - 1)
  mean = s2 * ((n-1) * (1 / qinf + 1 / qsup) - 2) 
  
}

ci_centered_symmetric_varmed <- function(w, n, s2 = 2, alpha) {
  w <- 1 - w
  qsup <- qchisq(1 - (1-w) * alpha, df = n - 1)
  qinf <- qchisq(w * alpha, df = n - 1)
  med = s2 * ((n-1) * (1 / qinf + 1 / qsup) - 2/(1-2/(n-1))^3)
}

compute_lb <- function(X_n,alpha,lambda, parameter){
  n = length(X_n)
  switch(parameter,
         moy = mean(X_n)-(sd(X_n)/sqrt(n))*qt(1-alpha*(1-lambda),n-1),
         var = (n-1)*(sd(X_n)^2)/qchisq(1-alpha*(1-lambda),n-1)
  )
  
}

compute_ub <- function(X_n,alpha,lambda,parameter){
  n = length(X_n)
  switch(parameter,
         moy = mean(X_n)-(sd(X_n)/sqrt(n))*qt(alpha*lambda,n-1),
         var =(n-1)*(sd(X_n)^2)/qchisq(alpha*lambda,n-1)
  )
  
}

eps0 <- .Machine$double.eps

f1 <- function (X, mu, sigma){
  rnorm(length(X), mu, sigma)
}

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
  
  lambdaopt_mode <-reactive({uniroot(
    f = ci_centered_symmetric_varmode,
    interval = c(eps0, 1 - eps0),
    n = input$n,
    alpha = input$alpha / 100
  )$root})
  
  lambdaopt_mean <-reactive({ uniroot(
    f = ci_centered_symmetric_varmean,
    interval = c(eps0, 1 - eps0),
    n = input$n,
    alpha = input$alpha / 100
  )$root})
  
  lambdaopt_med <-reactive({ uniroot(
    f = ci_centered_symmetric_varmed,
    interval = c(eps0, 1 - eps0),
    n = input$n,
    alpha = input$alpha / 100
  )$root})
  
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
    tibble(
      optimal_lambda = lambda_opt()
    )
  })
  
  output$table2 <-renderTable({
    tibble(
      location = c("mode","mean", "med"),
      lambda  = c(lambdaopt_mode(),lambdaopt_mean(),lambdaopt_med()),
      CIlength = c(ci_minimal_length_var(lambdaopt_mode(), n = input$n, s2 = 1, input$alpha/100),
                   ci_minimal_length_var(lambdaopt_mean(), n = input$n, s2 = 1, input$alpha/100),
                   ci_minimal_length_var(lambdaopt_med(), n = input$n, s2 = 1, input$alpha/100))
    )
  })
  
  # table2$location <-unlist(table2$location)
  # table2$lambda <-unlist( table2$lambda)
  # table2$CIlength <-unlist(table2$CIlength)
  
  tabnech <-reactive ({
    tib <- tibble(
      X = 1:input$nIC,
      X_n = lapply(X, rnorm, mean = input$mu, sd = input$sigma),
      L = sapply(X_n,compute_lb,alpha = input$alpha/100, lambda = input$lambda, parameter = input$parameter),
      U = sapply(X_n,compute_ub,alpha = input$alpha/100, lambda = input$lambda, parameter = input$parameter),
      M = switch(input$parameter,
                 moy = sapply(X_n, mean),
                 var = sapply(X_n, var)
      )
    )
    subset(tib,select=-X_n)
  })
  
  output$table3 <-renderTable({
    tabnech()
  })
  # tabnech$M <- unlist(tabnech$M)
  # tabnech$X <- unlist(tabnech$X)
  # tabnech$U <- unlist(tabnech$U)
  # tabnech$L <- unlist(tabnech$L)
  
  output$CIPlot <-renderPlot({
    
    ggplot(tabnech(), aes(x = M, y = X)) +
      geom_point(size = 4) +
      geom_errorbarh(aes(xmax = U, xmin = L)) 
    # +
    #   geom_vline(xintercept = switch(input$parameter,
    #                                  moy = mean(tabnech$X_n)-(sd(tabnech$X_n)/sqrt(n))*qt(1-input$alpha/100*(1-lambda_opt()),n-1),
    #                                  var = (n-1)*(sd(tabnech$X_n)^2)/qchisq(1-input$alpha/100*(1-lambda_opt()),n-1)
    #                                  ),
    #              color = "orange") +
    #   geom_vline (xintercept = switch(input$parameter,
    #                                   moy = mean(tabnech$X_n)-(sd(tabnech$X_n)/sqrt(n))*qt(input$alpha/100*lambda_opt(),n-1),
    #                                   var =(n-1)*(sd(tabnech$X_n)^2)/qchisq(input$alpha/100*lambda_opt(),n-1)
    #                                   ),
    #               color = "orange")

  })
  
}
shinyApp(ui = ui, server = server)

