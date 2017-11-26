#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Different types of hazard functions"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(

      sidebarPanel(

        selectInput("dist", "Choose the model:",
                    choices = c("Exponential",
                                "Increasing weibull",
                                "Decreasing weibull",
                                "Lognormal"),
                    selected = "disp"),



        sliderInput("lambda",
                    "Choose the lambda:",
                    min = 1,
                    max = 10,
                    value = 1)
        #
        # sliderInput("scale",
        #             "Choose the scale:",
        #             min = 1,
        #             max = 10,
        #             value = 1)


        ),

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$distPlot <- renderPlot({

     if(input$dist == "Exponential"){
       x <- seq(0, 5, length.out = 500)
       den <- dexp(x, input$lambda)
       surv <- 1 - pexp(x, input$lambda)
     }

     if(input$dist == "Increasing weibull"){
       x <- seq(0, 5, 0.01)
       den <- dweibull(x, shape = 13, scale = 4)
       surv <- 1 - pweibull(x, shape = 13, scale = 4)
     }

     if(input$dist == "Decreasing weibull"){
       x <- seq(0, 5, 0.01)
       den <- dweibull(x, shape = 0.06, scale = 4)
       surv <- 1 - pweibull(x, shape = 0.06, scale = 4)
     }

     if(input$dist == "Lognormal"){
       x <- seq(0, 5, 0.01)
       den <- dlnorm(x, meanlog = 0, sdlog = 0)
       surv <- 1 - plnorm(x, meanlog = 0, sdlog = 0)
     }

     plot(x, round(den/surv, 0), type = "l", ylab = "Hazard", xlab = "t")

   })
}

# Run the application
shinyApp(ui = ui, server = server)

