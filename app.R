#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(hrbrthemes)
library(scalesextra)
library(googlesheets4)

# Define UI for application that draws a histogram

sal <- read.csv("teachersalaryadj.csv")


district <- sal %>%
  distinct(district)%>%
  pull(district)

ui <- fluidPage(
    # set theme
  theme = shinytheme("lumen"),
    # Application title
    titlePanel("Massachusetts Teacher Salaries 1997-2021"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("district",
                        "District:",
                        choices=district,
                        multiple=T,
                        selectize=T,
                        selected =c("Holyoke")),
            selectInput("model",
                    "Model:",
                    choices = c("None", "Linear", "Quadratic", "Cubic", "Smooth"),
                    selected = c("None"))
  ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("salaryplot")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  d <- reactive(sal %>%
       filter(district == input$district)%>%
       mutate(district=factor(district, levels=input$district))
  )
   
  output$salaryplot <- renderPlot({
      p <- d() %>%
        ggplot(aes(x=year, y=adjust21, color=district))+
        scale_y_continuous(labels=scales::dollar_format())+
        scale_x_continuous(limits=c(1997, 2021), breaks=seq(1997,2021, 3))+
        ggtitle("Average Teacher Salary in 2021 dollars")+
        
        theme_ipsum()+
        theme(
          axis.title.y = element_blank(), 
          axis.title.x = element_blank(), 
          legend.title = element_blank())
        
        if (input$model == "Linear") {
          p + geom_smooth(method = "lm", se = FALSE)
        } else if (input$model == "Quadratic") {
          p + geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) 
        } else if (input$model == "Cubic") {
          p + geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) 
        } else if (input$model == "Smooth") {
          p + geom_smooth(se = FALSE)
        } else {
          p + geom_line()
        }   
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
