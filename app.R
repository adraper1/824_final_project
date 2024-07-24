library(shiny)
library(tidyverse)
library(insuranceData)
library(DT)
library(modelr)
library(corrplot)

dCar <- dataCar %>% 
  as_tibble() %>% 
  filter(numclaims > 0) %>% 
  mutate(severity = claimcst0 / numclaims) %>% 
  select(-X_OBSTAT_, -clm) %>% 
  select(severity, everything())

x_vars <- c("veh_value", "veh_body", "veh_age", "gender", "area", "agecat")
y_vars <- c("severity", "numclaims", "claimcst0")

ui <- fluidPage(
  titlePanel("Insurance Data Visualizations"),
  mainPanel(
    tabsetPanel(
      
      tabPanel("The Data", 
               br(),
               "This shiny app is really cool!",
               br(),
               "It loads the dataset named 'dataCar' from the 'insuranceData' package. 
               You can scroll through all the data in a table, view a summary of each of the fields, 
               see plots of each variable, and finally, build a custom model.",
               br(),
               "The original dataset contains 60k rows. We opted to focus this shiny app only on the severity
               of claims. So we excluded rows in the dataset where there were no claims.",
               
               br(),
               br(),
               dataTableOutput("the_data"),
               
               br(),
               br(),
               verbatimTextOutput("data_summary")),
      
      tabPanel("Response Histogram",
               br(),
               "Insurance data is very skewed. Here are histograms of severity and log-scaled severity.",
               br(),
               plotOutput("Sev"),
               plotOutput("SevScale")),
      
      tabPanel("Variable Plots",
               br(),
               "These are plots of each variable against severity",
               plotOutput("visA"),
               plotOutput("visB"),
               plotOutput("visC"),
               plotOutput("visD")),
      
      tabPanel("Correlation",
               br(),
               "Here is the correlation matrix of all the numeric variables",
               plotOutput("cor")),
      
      tabPanel("User Scatter", 
               br(),
               "On this tab, please select the X and Y variables that you wish to plot...",
               selectInput("x", "x variable", names(dCar), "veh_value"),
               selectInput("y", "y variable", names(dCar), "severity"),
               plotOutput("vis1")),
      
      tabPanel("A Model",
               br(),
               "Let's build a model on the dataset.",
               br(),
               "It will be a generalized linear regression model with Gamma error terms.",
               br(),
               "You choose the fields to include in the model formula.",
               selectInput("x2", "x variable(s)", x_vars, "veh_age", multiple = TRUE),
               selectInput("y2", "y variable", y_vars, "claimcst0", multiple = FALSE),
               verbatimTextOutput("vis2"),
               plotOutput("mp1"),
               plotOutput("mp2"),
               plotOutput("mp3"),
               plotOutput("mp4"))
    )
  )
)

server <- function(input, output, session) {
  
  output$the_data <- renderDT(dCar)
  
  output$data_summary <- renderPrint(summary(dCar))
  
  output$vis1 <- renderPlot(dCar %>% 
                              ggplot(aes(x = get(input$x), y = get(input$y))) +
                              geom_point(color = "#0051ba") +
                              labs(title = "Scatter Plot",
                                   x = input$x,
                                   y = input$y)
  )
  
  model <- reactive({glm(reformulate(input$x2, input$y2), 
                         family = Gamma(link = "log"),
                         data = dCar,
                         offset = log(numclaims)
  )})
  ?glm
  output$vis2 <- renderPrint(summary(model()))
  
  output$mp1 <- renderPlot(plot(model(), 1))
  output$mp2 <- renderPlot(plot(model(), 2))
  output$mp3 <- renderPlot(plot(model(), 3))
  output$mp4 <- renderPlot(plot(model(), 4))
  
  output$Sev <- renderPlot(
    dCar %>% 
      ggplot(aes(x = severity)) +
      geom_histogram(fill = "#0051ba") +
      labs(title = "Severity Histogram")
  )
  
  output$SevScale <- renderPlot(
    dCar %>% 
      ggplot(aes(x = severity)) +
      geom_histogram(fill = "#0051ba") +
      scale_x_log10() +
      labs(title = "Severity Histogram on a Log Scale")
  )
  
  
  output$vis3 <- renderPlot(
    dCar %>% 
      add_predictions(model()) %>% 
      ggplot(aes(x = pred, y = get(input$y2))) +
      geom_point(color = "#0051ba") +
      labs(y = input$y2))
  
  output$visA <- renderPlot(
    dCar %>% 
      ggplot(aes(x = veh_value, y = severity)) +
      geom_point(color = "#0051ba") +
      scale_y_log10() +
      scale_x_log10())
  
  output$visB <- renderPlot(
    dCar %>% 
      ggplot(aes(x = veh_body, y = severity)) +
      geom_boxplot(fill = "#0051ba") +
      scale_y_log10())
  
  output$visC <- renderPlot(
    dCar %>% 
      ggplot(aes(x = factor(veh_age), y = severity)) +
      geom_boxplot(fill = "#0051ba") +
      scale_y_log10() +
      labs(x = "veh_age"))
  
  output$visD <- renderPlot(
    dCar %>% 
      ggplot(aes(x = gender, y = severity)) +
      geom_boxplot(fill = "#0051ba") +
      scale_y_log10())
  
  output$cor <- renderPlot(corrplot(cor(select_if(dCar, is.numeric)),
                                    method = "ellipse"))
  
}

shinyApp(ui, server)