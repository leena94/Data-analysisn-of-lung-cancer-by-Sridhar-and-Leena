# Define UI
ui <- fluidPage(
  titlePanel("Lung Cancer Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plotType", label = "Select Plot Type",
                  choices = c("Bar Plot of Smoking History by Lung Cancer",
                              "Boxplot of Weight by Smoking History",
                              "Boxplot of Weight by Age Group with Smoking History"),
                  selected = "Bar Plot of Smoking History by Lung Cancer"),
      selectInput("fillVar", label = "Select Fill Variable", choices = c("", "Sex"), selected = ""),
      actionButton("updatePlot", "Update Plot")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Reactive data
  df <- reactive({
    # Perform feature engineering
    df <- df  # Replace with your actual dataframe
    df$Age_Group <- cut(df$Age, breaks = age_breaks, labels = age_labels)
    df$Age_Group <- as.factor(df$Age_Group)
    df$Smoking_History <- as.factor(df$Smoking_History)
    df$BMI <- df$Weight / ((df$Age / 100)^2)
    return(df)
  })
  
  # Render plot based on user input
  output$plot <- renderPlot({
    plotType <- input$plotType
    fillVar <- input$fillVar
    df <- df()
    
    if (plotType == "Bar Plot of Smoking History by Lung Cancer") {
      ggplot(df, aes(x = Histopath_Grading, fill = Smoking_History)) +
        geom_bar(position = "fill") +
        labs(x = "Proportion", y = "Smoking History",
             title = "Bar plot of Smoking History by Lung Cancer")
    } else if (plotType == "Boxplot of Weight by Smoking History") {
      ggplot(df, aes(x = Smoking_History, y = Weight, fill = fillVar)) +
        geom_boxplot() +
        labs(x = "Smoking History", y = "Weight",
             title = "Boxplot of Weight by Smoking History")
    } else if (plotType == "Boxplot of Weight by Age Group with Smoking History") {
      ggplot(df, aes(x = Age_Group, y = Weight, color = Smoking_History)) +
        geom_boxplot() +
        labs(x = "Age Group", y = "Weight",
             title = "Boxplot of Weight by Age Group with Smoking History")
    }
  })
  
}

# Run the app
shinyApp(ui, server)




