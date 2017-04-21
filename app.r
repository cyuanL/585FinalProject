
#
setwd("U:/Helena/585/Final_project")
library(plotly)
library(shiny)
library(ggplot2)
library(rvest)


ames <- read.csv("Ames_housing.csv", header = T)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("House Sales Search"),
  
  sidebarLayout(
    sidebarPanel(
    
      selectInput("style", label = h3("Select Housing Style"), 
                  choices = unique(ames$Style),
                  selected = "Two story")),
    
      
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("plot"),
      textOutput("text"),
      tableOutput("table")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  ames_subset <- reactive(
    ames %>% filter(Style == input$style))
  
  output$plot <- renderPlotly(
    ames_subset() %>% ggplot(aes(y=Sale.Price/1000, x= Total.Living.Area, 
                                 colour = as.factor(Bedrooms))) + geom_point()
                      +theme_classic() + xlab("Total Living Area") + ylab("\nSale Price(k)\n")
                    +scale_colour_discrete(name  ="Bedroom")                   
                   + theme(legend.position="bottom")
  )
  
}


# Run the application 
shinyApp(ui = ui, server = server)

