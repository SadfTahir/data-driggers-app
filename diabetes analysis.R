
library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(MASS)
library(lmtest)
library(shinydashboard)


Diabetes <- read_excel("data.xlsx")
data_path <- "data.xlsx"
data <- read_excel(data_path)

datanew <- read_excel(data_path)

lm_model <- lm(Cr ~ age, data = data)
age_CI <- t.test(data$age)$conf.int

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = tags$div(
      style = "white; color: #ffffff; font-weight: bold;",
      "Diabetes Data "
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(" Graphical/Tabular", tabName = "graphical", icon = icon("chart-line")),
      menuItem(" Statistical Measures", tabName = "descriptive", icon = icon("list-alt")),
      menuItem(" Prob. Methods/Distribution", tabName = "probability", icon = icon("signal")),
      menuItem(" Reg. Modeling & Prediction", tabName = "regression", icon = icon("line-chart")),
      menuItem(" Confidence Interval", tabName = "confidence", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "graphical",
        fluidRow(
          column(width = 4, sliderInput("bin_size", "Bin Size", min = 1, max = 50, value = 10)),
        ),
        fluidRow(
          column(width = 4, selectInput("x_axis", "Select X-axis variable", choices = names(Diabetes))),
          column(width = 4, selectInput("y_axis", "Select Y-axis variable", choices = names(Diabetes))),
          column(width = 4, selectInput("model_variable", "Select model variable for probability", choices = names(Diabetes)))
        ),
        fluidRow(
          column(width = 12, actionButton("analyze", "Analyze"))
        ),
        fluidRow(
          column(width = 12, plotOutput("plot"))
        ),
        fluidRow(
          column(width = 12, dataTableOutput("table"))
        )
      ),
      tabItem(
        tabName = "descriptive",
        fluidRow(
          column(
            width = 4,
            valueBoxOutput("n_box")
          ),
          column(
            width = 4,
            valueBoxOutput("mean_box")
          ),
          column(
            width = 4,
            valueBoxOutput("sd_box")
          )
        ),
        fluidRow(
          column(
            width = 4,
            valueBoxOutput("min_box")
          ),
          column(
            width = 4,
            valueBoxOutput("median_box")
          ),
          column(
            width = 4,
            valueBoxOutput("max_box")
          )
        ),
        fluidRow(
          column(
            width = 12,
            box(
              title = "Summary",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              verbatimTextOutput("summary")
            )
          )
        )
      ),
      tabItem(
        tabName = "probability",
        plotOutput("density_plot"),
        verbatimTextOutput("distribution_summary")
      ),
      tabItem(
        tabName = "regression",
        plotOutput("scatterplot"),
        sidebarPanel(
          sliderInput(
            "age",
            "Select age value:",
            min = min(datanew$age),
            max = max(datanew$age),
            value = 50
          )
        )
      ),
      tabItem(
        tabName = "confidence",
        plotOutput("scatterplot2"),
        fluidRow(
          column(
            width = 12,
            box(
              title = "Confidence Interval for Age Mean",
              status = "primary",
              solidHeader= TRUE,
           
              verbatimTextOutput("confidence_interval")
            ),
            box(
              title = "Confidence Interval for Age Mean",
              status = "primary",
              solidHeader= TRUE,
              
              
              verbatimTextOutput("lm_CI"), 
              
            ),
            box(
              title = "Confidence Interval for Age Mean",
              status = "primary",
              solidHeader= TRUE,
              verbatimTextOutput("age_CI"), 
            )
          )
    
        )
      
      )
      
    )
  )
)

  
  
server <- function(input, output) {
  data <- eventReactive(input$analyze, {
    Diabetes
  })
  
  output$plot <- renderPlot({
    ggplot(data(), aes(x = !!sym(input$x_axis))) +
      geom_histogram(binwidth = input$bin_size, aes(y = ..density..), fill = "lightblue", color = "black") +
      geom_density(alpha = .2, fill = "orange") +
      labs(x = input$x_axis, y = "Density") +
      theme_minimal()
  })
  
  
  output$table <- renderDataTable({
    data()
  })
  
  output$summary <- renderPrint({
    summary(data()[, -1])
  })
  
  output$density_plot <- renderPlot({
    ggplot(data(), aes(x = !!sym(input$model_variable))) + geom_density()
  })
  
  output$distribution_summary <- renderPrint({
    summary(data()[, input$model_variable])
  })
  
  output$regression_summary <- renderPrint({
    model <- lm(!!sym(input$model_variable) ~ ., data = data())
    summary(model)
  })
  
  # Scatter plot
  output$scatterplot2 <- renderPlot({
    ggplot(Diabetes, aes(x = age, y = Cr)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      ggtitle("Cr vs Age")
  })
  
  # Confidence intervals for age mean
  
  output$age_CI <- renderPrint({
    paste("Confidence Intervals for Age Mean:",
          paste(round(age_CI, 2), collapse = " - "))
  })
  
  # Confidence intervals for linear regression coefficients
  lm_CI <- confint(lm_model)
  output$lm_CI <- renderPrint({
    paste("Confidence Intervals for Linear Regression Coefficients:",
          paste(round(lm_CI, 2), collapse = " - "))
  })
  
  output$confidence_interval <- renderPrint({
    
    summary(lm_model)
    
  })
  
  # Define the data subset based on the selected age value
  data_subset <- reactive({
    datanew %>% filter(age == input$age)
  })
  
  # Define the linear regression model based on the data subset
  my_lm_model <- reactive({
    lm(Cr ~ age, data = data_subset())
  })
  
  # Define the predicted values based on the selected age value
  predictions <- reactive({
    predict(my_lm_model(), newdata = data.frame(age = input$age))
  })
  
  # Define the scatterplot
  output$scatterplot <- renderPlot({
    ggplot(datanew, aes(x = age, y = Cr)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      geom_vline(xintercept = input$age, linetype = "dashed") +
      geom_segment(aes(x = input$age, xend = input$age, y = 0, yend = predictions()), linetype = "dashed", color = "red") +
      labs(x = "Age (years)", y = "Cr", title = "Cr vs Age") +
      theme_bw()
  })
  
}

shinyApp(ui, server)


