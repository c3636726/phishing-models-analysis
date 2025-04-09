# FINAL SHINY DASHBOARD WITH FULL PIPELINE

# Install and load all required libraries
packages <- c("shiny", "shinydashboard", "dplyr", "tidyverse", "Amelia", "mice", "corrplot", "ggplot2",
              "pROC", "class", "e1071", "rpart", "rpart.plot", "reshape2", "randomForest", "ROCR")
lapply(packages, function(p) if (!require(p, character.only = TRUE)) install.packages(p))
lapply(packages, library, character.only = TRUE)

# Load Data
data <- read.csv("Phishing_Legitimate.csv")

# Missing Value Analysis
missing_values <- colSums(is.na(data))
data_cleaned <- if (mean(missing_values / nrow(data)) < 0.02) na.omit(data) else complete(mice(data, m = 1, method = 'pmm', seed = 500))

# Outlier Removal
outliers <- unique(unlist(lapply(data_cleaned, function(col) {
  if (is.numeric(col)) {
    Q1 <- quantile(col, 0.25); Q3 <- quantile(col, 0.75)
    IQR <- Q3 - Q1
    which(col < Q1 - 1.5 * IQR | col > Q3 + 1.5 * IQR)
  }
})))
data_cleaned <- data_cleaned[-outliers, ]
data_before_removal <- data_cleaned

# Low Variance and Identifier Columns
variances <- apply(data_cleaned[sapply(data_cleaned, is.numeric)], 2, var)
data_cleaned <- data_cleaned[, !(names(data_cleaned) %in% names(variances[variances < 0.01]))]
identifier_columns <- names(data_cleaned)[sapply(data_cleaned, function(x) length(unique(x)) == nrow(data_cleaned))]
data_cleaned <- data_cleaned[, !(names(data_cleaned) %in% identifier_columns)]

# Scaling
scaled_data <- data_cleaned
scaled_data[sapply(scaled_data, is.numeric)] <- scale(scaled_data[sapply(scaled_data, is.numeric)])

# Train/Test Split
set.seed(123)
data_cleaned$CLASS_LABEL <- as.factor(data_cleaned$CLASS_LABEL)
train_index <- sample(1:nrow(data_cleaned), 0.8 * nrow(data_cleaned))
train_data <- data_cleaned[train_index, ]
test_data <- data_cleaned[-train_index, ]

# Logistic Regression
log_model <- glm(CLASS_LABEL ~ ., data = train_data, family = "binomial")
log_pred <- predict(log_model, test_data, type = "response")
log_class <- ifelse(log_pred > 0.5, 1, 0)
log_acc <- mean(log_class == test_data$CLASS_LABEL)
log_roc <- roc(test_data$CLASS_LABEL, log_pred)
# KNN Confusion Matrix Plot
knn_cm <- confusionMatrix(knn_pred, test_data$CLASS_LABEL)
knn_cm_table <- as.data.frame(knn_cm$table)
colnames(knn_cm_table) <- c("Prediction", "Reference", "Freq")



# Naive Bayes
nb_model <- naiveBayes(train_knn, train_data$CLASS_LABEL)
nb_pred <- predict(nb_model, test_knn)
nb_probs <- predict(nb_model, test_knn, type = "raw")
nb_acc <- mean(nb_pred == test_data$CLASS_LABEL)
nb_roc <- roc(as.numeric(test_data$CLASS_LABEL), nb_probs[, 2])

# Decision Tree
dt_model <- rpart(CLASS_LABEL ~ ., data = train_data, method = "class")
dt_pred <- predict(dt_model, test_data, type = "class")
dt_acc <- mean(dt_pred == test_data$CLASS_LABEL)

# Random Forest
rf_model <- randomForest(CLASS_LABEL ~ ., data = train_data, ntree = 100, importance = TRUE)
rf_pred <- predict(rf_model, test_data)
rf_acc <- mean(rf_pred == test_data$CLASS_LABEL)
rf_probs <- predict(rf_model, test_data, type = "prob")
rf_roc <- roc(test_data$CLASS_LABEL, rf_probs[, 2])

# Accuracy summary
data_accuracy <- data.frame(
  Model = c("Logistic Regression", "KNN", "Naive Bayes", "Decision Tree", "Random Forest"),
  Accuracy = c(log_acc, knn_acc, nb_acc, dt_acc, rf_acc)
)

# Shiny UI
ui <- dashboardPage(
  dashboardHeader(title = "Phishing Detection Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "summary"),
      menuItem("Missing", tabName = "missing"),
      menuItem("Outliers", tabName = "outliers"),
      menuItem("Correlation", tabName = "correlation"),
      menuItem("Visuals", tabName = "visuals"),
      menuItem("Models", tabName = "models"),
      menuItem("Feature Importance", tabName = "importance")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("summary", box(verbatimTextOutput("summary"), width = 12)),
      tabItem("missing", box(tableOutput("missing"), width = 12)),
      tabItem("outliers", fluidRow(
        box(plotOutput("before_box"), title = "Before", width = 6),
        box(plotOutput("after_box"), title = "After", width = 6)
      )),
      tabItem("correlation", box(plotOutput("corrplot"), width = 12)),
      tabItem("visuals",
              fluidRow(
                box(plotOutput("url_plot"), width = 6),
                box(plotOutput("class_plot"), width = 6)
              )),
      tabItem("models",
              fluidRow(
                valueBoxOutput("log_acc"), valueBoxOutput("knn_acc"), valueBoxOutput("nb_acc"),
                valueBoxOutput("dt_acc"), valueBoxOutput("rf_acc")
              ),
              box(plotOutput("acc_plot"), width = 12),
              fluidRow(
                box(plotOutput("roc_log")),
                box(plotOutput("roc_nb")),
                box(plotOutput("roc_rf"))
              )),
      tabItem("importance", box(plotOutput("importance_plot"), width = 12))
    )
  )
)

# Server
server <- function(input, output) {
  output$summary <- renderPrint({ summary(data) })
  output$missing <- renderTable({ data.frame(Column = names(missing_values), Missing = missing_values) })
  output$before_box <- renderPlot({ boxplot(data_before_removal, main = "Before", col = "skyblue") })
  output$after_box <- renderPlot({ boxplot(data_cleaned, main = "After", col = "lightgreen") })
  output$corrplot <- renderPlot({ corrplot(cor(data_cleaned[sapply(data_cleaned, is.numeric)]), method = "color") })
  output$url_plot <- renderPlot({ ggplot(data, aes(x = UrlLength)) + geom_histogram(binwidth = 10, fill = "steelblue") })
  output$class_plot <- renderPlot({ ggplot(data, aes(x = factor(CLASS_LABEL))) + geom_bar(fill = "darkorange") })
  output$log_acc <- renderValueBox({ valueBox(paste0(round(log_acc * 100, 2), "%"), "Logistic", icon = icon("check"), color = "aqua") })
  output$knn_acc <- renderValueBox({ valueBox(paste0(round(knn_acc * 100, 2), "%"), "KNN", icon = icon("check"), color = "blue") })
  output$nb_acc <- renderValueBox({ valueBox(paste0(round(nb_acc * 100, 2), "%"), "Naive Bayes", icon = icon("check"), color = "yellow") })
  output$dt_acc <- renderValueBox({ valueBox(paste0(round(dt_acc * 100, 2), "%"), "Decision Tree", icon = icon("check"), color = "orange") })
  output$rf_acc <- renderValueBox({ valueBox(paste0(round(rf_acc * 100, 2), "%"), "Random Forest", icon = icon("check"), color = "green") })
  output$acc_plot <- renderPlot({ ggplot(data_accuracy, aes(x = Model, y = Accuracy, fill = Model)) +
      geom_col() + geom_text(aes(label = paste0(round(Accuracy * 100, 1), "%")), vjust = -0.5) + theme_minimal() })
  output$roc_log <- renderPlot({ plot(log_roc, col = "blue", main = "Logistic ROC") })
  output$roc_nb <- renderPlot({ plot(nb_roc, col = "orange", main = "Naive Bayes ROC") })
  output$roc_rf <- renderPlot({ plot(rf_roc, col = "darkgreen", main = "Random Forest ROC") })
  output$importance_plot <- renderPlot({
    imp_df <- data.frame(Feature = rownames(rf_model$importance), Importance = rf_model$importance[, "MeanDecreaseGini"])
    imp_df <- imp_df[order(-imp_df$Importance), ][1:20, ]
    ggplot(imp_df, aes(x = reorder(Feature, Importance), y = Importance)) + geom_col(fill = "tomato") + coord_flip()
  })
}






# Run the app
shinyApp(ui = ui, server = server)
