#créons une application qui nous dessine 2 distribution (student et chideux)
library(shiny)

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Distribution Drawing"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select the random distribution type ----
      radioButtons("dist", "Distribution type:",
                   c("Normal" = "norm",
                     "Student" = "stud",
                     "Chisquare" = "chisq")),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Slider for the number of observations to generate ----
      sliderInput("n","Number of observations:",
                  value = 500,
                  min = 1,
                  max = 1000)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", tableOutput("table"))
      )
      
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  d <- reactive({
    dist <- switch(input$dist,
                   norm = rnorm(input$n),
                   stud = rt(input$n,input$n-1,0),
                   chisq = rchisq(input$n,input$n-1,0))
  })
  
  y<-reactive({
    x<-seq(-4,4,(8/input$n))
  })
  
  densite <- reactive({
    graph <- function(){ switch(input$dist,
                   norm = dnorm(d,0,1),
                   stud = dt(d,input$n-1,0),
                   chisq = dchisq(d,input$n-1,0))
    }
    
  })
  
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    #curve(densite,add=TRUE, col="blue") 
     lines (y,d())
     hist(d(),
        main = paste("r", dist, "(", n, ")", sep = ""),
        col = "light gray", border = "orange")
     
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(d())
    length(y)
    length (d())
  })
  
  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    d()
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)

#?distribution
