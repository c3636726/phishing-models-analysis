library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(corrplot)
library(pROC)
library(randomForest)
library(e1071)
library(class)
library(rpart)
library(rpart.plot)
library(ROCR)
install.packages(c("shiny", "DT", "ggplot2", "dplyr", "shinythemes", "corrplot", "pROC", "randomForest", "e1071", "class", "rpart", "rpart.plot", "ROCR"))



# Load and clean the data
data <- read.csv("Phishing_Legitimate.csv")
data <- na.omit(data)
data$CLASS_LABEL <- as.factor(data$CLASS_LABEL)

# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("🔐 Full Phishing Detection Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("model_choice", "Choose a Model:", 
                  choices = c("Random Forest", "Logistic Regression", "KNN", "Naive Bayes", "Decision Tree")),
      selectInput("plot_var", "Choose variable to plot:", 
                  choices = names(data)[sapply(data, is.numeric)]),
      actionButton("run_model", "🚀 Run Model")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", DTOutput("data_table")),
        tabPanel("EDA Plot", plotOutput("eda_plot")),
        tabPanel("Correlation Plot", plotOutput("cor_plot")),
        tabPanel("Confusion Matrix", verbatimTextOutput("conf_matrix")),
        tabPanel("ROC Curve", plotOutput("roc_curve")),
        tabPanel("Model Accuracy", verbatimTextOutput("accuracy")),
        tabPanel("Feature Importance", plotOutput("importance_plot"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  output$data_table <- renderDT({ datatable(data) })
  
  output$eda_plot <- renderPlot({
    ggplot(data, aes_string(x = input$plot_var)) +
      geom_histogram(fill = "steelblue", bins = 30, color = "white") +
      theme_minimal()
  })
  
  output$cor_plot <- renderPlot({
    corrplot(cor(data[sapply(data, is.numeric)]), method = "color")
  })
  
  model_results <- eventReactive(input$run_model, {
    set.seed(123)
    index <- sample(1:nrow(data), 0.8 * nrow(data))
    train <- data[index, ]
    test <- data[-index, ]
    
    train_x <- train[, -which(names(train) == "CLASS_LABEL")]
    test_x <- test[, -which(names(test) == "CLASS_LABEL")]
    train_y <- train$CLASS_LABEL
    test_y <- test$CLASS_LABEL
    
    if (input$model_choice == "Random Forest") {
      model <- randomForest(CLASS_LABEL ~ ., data = train)
      probs <- predict(model, test, type = "prob")[, 2]
      preds <- predict(model, test)
      imp_plot <- varImpPlot(model)
    } else if (input$model_choice == "Logistic Regression") {
      model <- glm(CLASS_LABEL ~ ., data = train, family = "binomial")
      probs <- predict(model, test, type = "response")
      preds <- ifelse(probs > 0.5, 1, 0)
      imp_plot <- NULL
    } else if (input$model_choice == "KNN") {
      preds <- knn(train_x, test_x, cl = train_y, k = 5)
      probs <- as.numeric(as.character(preds))
      imp_plot <- NULL
    } else if (input$model_choice == "Naive Bayes") {
      model <- naiveBayes(CLASS_LABEL ~ ., data = train)
      probs <- predict(model, test, type = "raw")[, 2]
      preds <- predict(model, test)
      imp_plot <- NULL
    } else if (input$model_choice == "Decision Tree") {
      model <- rpart(CLASS_LABEL ~ ., data = train, method = "class")
      probs <- predict(model, test)[, 2]
      preds <- predict(model, test, type = "class")
      imp_plot <- model
    }
    
    cm <- table(Predicted = preds, Actual = test_y)
    acc <- sum(diag(cm)) / sum(cm)
    roc_obj <- roc(test_y, probs)
    
    list(acc = acc, cm = cm, roc = roc_obj, imp_plot = imp_plot, model_name = input$model_choice)
  })
  
  output$conf_matrix <- renderPrint({ req(model_results()); model_results()$cm })
  
  output$accuracy <- renderPrint({
    req(model_results())
    paste(model_results()$model_name, "Accuracy:", round(model_results()$acc * 100, 2), "%")
  })
  
  output$roc_curve <- renderPlot({
    req(model_results())
    plot(model_results()$roc, col = "darkred", lwd = 2, main = paste("ROC Curve -", model_results()$model_name))
  })
  
  output$importance_plot <- renderPlot({
    if (input$model_choice == "Random Forest") {
      varImpPlot(randomForest(CLASS_LABEL ~ ., data = data))
    } else if (input$model_choice == "Decision Tree") {
      rpart.plot(model_results()$imp_plot)
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
