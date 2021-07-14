#Shiny app pour le projet de stage
#Objectif : Cr?er un diagramme interactif permettant d'illustrer le meilleur intervalle de
#confiance pour estimer la moyenne ou la variance d'une v.a X suivant une distribution normale,
#ainsi que les quantiles permettant d'illustrer cet intervalle

library(shiny)
library(ggplot2)
library(tidyverse)
library(tibble)
library(dplyr)
library (shinyjs)
# library(hablar)


# Define UI for the app
ui <- fluidPage(

  # App title ----
  titlePanel("Illustration of the best CI for a chosen parameter, and associated quantiles"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      
      shinyjs::useShinyjs(),

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
                   max = 200)


    ),
    # Main panel for displaying outputs ----
    mainPanel(

      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("distPlot")),
                  tabPanel("Table", tableOutput("table")),
                  tabPanel("Summary", br(), textOutput("msg1summary"), br(), tableOutput("table2"), br(), textOutput("msg2summary")),
                  tabPanel("CItibble", tableOutput("table3")),
                  tabPanel("CI danse", plotOutput("CIPlot"), br(), textOutput("msg1CIdance"), br(), tableOutput("percentage"))

      )

    )
  )

)

#Length of the CI to minimize via uniroot in lambda_opt for mean and variance
ci_minimal_length_mean <- function(w, n, alpha, s2 = 1) {
  qsup <- qt(1 - w * alpha, df = n - 1)
  qinf <- qt((1 - w) * alpha, df = n - 1)
  sqrt(s2/n)*(qsup-qinf)
}

ci_minimal_length_var <- function(w, n, alpha, s2 = 1) {
  qsup <- qchisq(1 - (1-w) * alpha, df = n - 1)    #Production of NAN
  qinf <- qchisq(w * alpha, df = n - 1)        ##if na.omit, CIlength is of size 0
  s2 * (n-1) * (1 / qinf - 1 / qsup)
}

#Functions representing the centered aspect of the CI. CI centered => corresponding function = 0
ci_centered_symmetric_mean <- function(w, n, alpha, s2 = 2) {
  qsup <- qt(1 - (1-w) * alpha, df = n - 1)
  qinf <- qt(w * alpha, df = n - 1)
  -2*sqrt(s2/n)*(qsup + qinf)
}

ci_centered_symmetric_varmode <- function(w, n, alpha, s2 = 2) {
  qsup <- qchisq(1 - (1-w) * alpha, df = n - 1)
  qinf <- qchisq(w * alpha, df = n - 1)
  cen <- s2 * (n-1) / (n-3)
  (n-1) * s2 * (1 / qinf + 1 / qsup) - 2 * cen
}


ci_centered_symmetric_varmean <- function(w, n, alpha, s2 = 2) {
  qsup <- qchisq(1 - (1-w) * alpha, df = n - 1)
  qinf <- qchisq(w * alpha, df = n - 1)
  cen <- s2
  (n-1) * s2 * (1 / qinf + 1 / qsup) - 2 * cen
}

ci_centered_symmetric_varmed <- function(w, n, alpha, s2 = 2) {
  qsup <- qchisq(1 - (1-w) * alpha, df = n - 1)
  qinf <- qchisq(w * alpha, df = n - 1)
  cen <- s2 / (1 - 2 / (9*(n-1)))^3
  (n-1) * s2 * (1 / qinf + 1 / qsup) - 2 * cen
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
  
  # observeEvent(input$dist, {
  #   if (input$dist != "negbin")
  #     updateSliderInput(
  #       session = session,
  #       inputId = "variance",
  #       value = if (input$dist == "pois")
  #         input$mean
  #       else if (input$dist == "const")
  #         0
  #     )
  #   shinyjs::toggleState(id = "variance", condition = input$dist == "negbin")
  # })
  
  observeEvent(input$criteria, {
    if (input$criteria != "cenandsym")
      updateRadioButtons(
        session = session, 
        inputId = "center",
        selected = NULL
        )
    shinyjs::toggleState(id = "center", condition = input$criteria == "cenandsym")
  })
  
  
  
  # toggleState (input$center, condition = observe(input$criteria == "cenandsym"))

  lambdaopt_mode_var <-reactive({uniroot(
    f = ci_centered_symmetric_varmode,
    interval = c(eps0, 1 - eps0),
    n = input$n,
    alpha = input$alpha / 100
  )$root})

  lambdaopt_mean_var <-reactive({ uniroot(
    f = ci_centered_symmetric_varmean,
    interval = c(eps0, 1 - eps0),
    n = input$n,
    alpha = input$alpha / 100
  )$root})

  lambdaopt_med_var <-reactive({ uniroot(
    f = ci_centered_symmetric_varmed,
    interval = c(eps0, 1 - eps0),
    n = input$n,
    alpha = input$alpha / 100
  )$root})
  
  lambdaopt_moy <-reactive({ uniroot(
    f = ci_centered_symmetric_mean,
    interval = c(eps0, 1 - eps0),
    n = input$n,
    alpha = input$alpha / 100
  )$root})
  
##
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
      lambda  = switch(input$parameter,
                       moy = c(lambdaopt_moy(),lambdaopt_moy(),lambdaopt_moy()),
                       var = c(lambdaopt_mode_var(),lambdaopt_mean_var(),lambdaopt_med_var())),
      CIlength = switch(input$parameter,
                        moy = c(ci_minimal_length_mean(lambdaopt_moy(), n = input$n, s2 = 1, input$alpha / 100),
                                ci_minimal_length_mean(lambdaopt_moy(), n = input$n, s2 = 1, input$alpha / 100),
                                ci_minimal_length_mean(lambdaopt_moy() , n = input$n, s2 = 1, input$alpha / 100)),
                        var = c(ci_minimal_length_var(lambdaopt_mode_var(), n = input$n, s2 = 1, input$alpha / 100),
                                ci_minimal_length_var(lambdaopt_mean_var(), n = input$n, s2 = 1, input$alpha / 100),
                                ci_minimal_length_var(lambdaopt_med_var() , n = input$n, s2 = 1, input$alpha / 100))
                        )
    )
  })
  
  output$msg1summary <- renderText(
    "Is displayed bellow a table with the length of the optimum CI, along 
    with the lambda corresponding to such CI, for each quantity to center the CI on"
  )
  
  output$msg2summary <- renderText(
    switch(input$parameter,
           moy = "Let's note that for a symetrical distribution, the mode, mean and median are mingeled wich explain the redundancy of the results",
           var = "")
  )

  tabnech <-reactive ({
    tib <- tibble(
      X = 1:input$nIC,
      X_n = replicate(input$nIC, rnorm(n = input$n, mean = input$mu, sd = input$sigma), simplify = FALSE),
      L = sapply(X_n,compute_lb,alpha = input$alpha / 100, lambda = input$lambda, parameter = input$parameter),
      U = sapply(X_n,compute_ub,alpha = input$alpha / 100, lambda = input$lambda, parameter = input$parameter),
      M = switch(input$parameter,
                  moy = sapply(X_n, mean),
                  var = sapply(X_n, var)
                 ),
      missed = switch(input$parameter,
                      moy = input$mu < L | input$mu > U,
                      var = input$sigma < L | input$sigma > U)
      
    )
    subset(tib, select = - X_n)
  })

  output$table3 <-renderTable({
    tabnech()
  })

  output$CIPlot <-renderPlot({
    p<- ggplot(tabnech(), aes(x = M, y = X), color = "blue")
    p + geom_point(size = 4) +
      geom_errorbarh(aes(xmax = U, xmin = L, color = missed)) +
      scale_color_discrete(direction = -1) +
      geom_vline( xintercept = switch(input$parameter,
                                      moy = input$mu,
                                      var = input$sigma), color = "red")
    
  })
  
  output$percentage <- renderTable ({
    tibble (
      percentage = sum(tabnech()$missed) / length(tabnech()$missed)*100,"%"
    )
  })
  
  
  output$msg1CIdance <- renderText(
    "Percentage of CI wich do not contain the parameter to estimate (percentage of missed) :"
  )
}
shinyApp(ui = ui, server = server)

