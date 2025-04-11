# Load required packages
packages <- c("shiny", "shinydashboard", "dplyr", "tidyverse", "mice", "ggplot2", 
              "pROC", "class", "e1071", "rpart", "rpart.plot", "reshape2", "randomForest", "ROCR", "caret")
lapply(packages, function(p) if (!require(p, character.only = TRUE)) install.packages(p))
lapply(packages, library, character.only = TRUE)

# Load Data
data <- read.csv("Phishing_Legitimate.csv")

# Preprocessing
missing_values <- colSums(is.na(data))
data_cleaned <- if (mean(missing_values / nrow(data)) < 0.02) na.omit(data) else complete(mice(data, m = 1, method = 'pmm', seed = 500))
outliers <- unique(unlist(lapply(data_cleaned, function(col) {
  if (is.numeric(col)) {
    Q1 <- quantile(col, 0.25); Q3 <- quantile(col, 0.75); IQR <- Q3 - Q1
    which(col < Q1 - 1.5 * IQR | col > Q3 + 1.5 * IQR)
  }
})))
data_cleaned <- data_cleaned[-outliers, ]
data_before_removal <- data_cleaned

# Feature selection
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

train_knn <- train_data[, -which(names(train_data) == "CLASS_LABEL")]
test_knn <- test_data[, -which(names(test_data) == "CLASS_LABEL")]

# Models and evaluation
log_model <- glm(CLASS_LABEL ~ ., data = train_data, family = "binomial")
log_pred <- predict(log_model, test_data, type = "response")
log_class <- ifelse(log_pred > 0.5, 1, 0)
log_acc <- mean(log_class == test_data$CLASS_LABEL)
log_roc <- roc(test_data$CLASS_LABEL, log_pred)
conf_log <- confusionMatrix(as.factor(log_class), test_data$CLASS_LABEL)

knn_pred <- knn(train_knn, test_knn, cl = train_data$CLASS_LABEL, k = 5)
knn_acc <- mean(knn_pred == test_data$CLASS_LABEL)
knn_roc <- roc(as.numeric(test_data$CLASS_LABEL), as.numeric(knn_pred))
conf_knn <- confusionMatrix(knn_pred, test_data$CLASS_LABEL)

nb_model <- naiveBayes(train_knn, train_data$CLASS_LABEL)
nb_pred <- predict(nb_model, test_knn)
nb_probs <- predict(nb_model, test_knn, type = "raw")
nb_acc <- mean(nb_pred == test_data$CLASS_LABEL)
nb_roc <- roc(as.numeric(test_data$CLASS_LABEL), nb_probs[, 2])
conf_nb <- confusionMatrix(nb_pred, test_data$CLASS_LABEL)

dt_model <- rpart(CLASS_LABEL ~ ., data = train_data, method = "class")
dt_pred <- predict(dt_model, test_data, type = "class")
dt_prob <- predict(dt_model, test_data)[, 2]
dt_acc <- mean(dt_pred == test_data$CLASS_LABEL)
dt_roc <- roc(as.numeric(test_data$CLASS_LABEL), dt_prob)
conf_dt <- confusionMatrix(dt_pred, test_data$CLASS_LABEL)

rf_model <- randomForest(CLASS_LABEL ~ ., data = train_data, ntree = 100, importance = TRUE)
rf_pred <- predict(rf_model, test_data)
rf_probs <- predict(rf_model, test_data, type = "prob")
rf_acc <- mean(rf_pred == test_data$CLASS_LABEL)
rf_roc <- roc(test_data$CLASS_LABEL, rf_probs[, 2])
conf_rf <- confusionMatrix(rf_pred, test_data$CLASS_LABEL)

data_accuracy <- data.frame(
  Model = c("Logistic Regression", "KNN", "Naive Bayes", "Decision Tree", "Random Forest"),
  Accuracy = c(log_acc, knn_acc, nb_acc, dt_acc, rf_acc)
)

metric_table <- data.frame(
  Model = data_accuracy$Model,
  Accuracy = data_accuracy$Accuracy,
  Sensitivity = c(conf_log$byClass["Sensitivity"], conf_knn$byClass["Sensitivity"],
                  conf_nb$byClass["Sensitivity"], conf_dt$byClass["Sensitivity"], conf_rf$byClass["Sensitivity"]),
  Specificity = c(conf_log$byClass["Specificity"], conf_knn$byClass["Specificity"],
                  conf_nb$byClass["Specificity"], conf_dt$byClass["Specificity"], conf_rf$byClass["Specificity"]),
  Precision = c(conf_log$byClass["Precision"], conf_knn$byClass["Precision"],
                conf_nb$byClass["Precision"], conf_dt$byClass["Precision"], conf_rf$byClass["Precision"])
)

# Helper to render heatmaps
render_heatmap <- function(conf_mat, title, color) {
  mat <- as.data.frame(conf_mat$table)
  ggplot(mat, aes(Prediction, Reference, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), size = 5) +
    scale_fill_gradient(low = "white", high = color) +
    ggtitle(title) + theme_minimal()
}

# UI
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
      menuItem("Confusion Heatmaps", tabName = "heatmaps"),
      menuItem("Feature Importance", tabName = "importance"),
      menuItem("Metrics Table", tabName = "metrics")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("summary", box(verbatimTextOutput("summary"), width = 12)),
      tabItem("missing", box(tableOutput("missing"), width = 12)),
      tabItem("outliers", fluidRow(
        box(plotOutput("before_box"), width = 6),
        box(plotOutput("after_box"), width = 6)
      )),
      tabItem("correlation", box(plotOutput("corrplot"), width = 12)),
      tabItem("visuals", fluidRow(
        box(plotOutput("url_plot"), width = 6),
        box(plotOutput("class_plot"), width = 6)
      )),
      tabItem("models",
              fluidRow(
                valueBox(paste0(round(log_acc * 100, 2), "%"), "Logistic", icon = icon("check"), color = "blue"),
                valueBox(paste0(round(knn_acc * 100, 2), "%"), "KNN", icon = icon("check"), color = "purple"),
                valueBox(paste0(round(nb_acc * 100, 2), "%"), "Naive Bayes", icon = icon("check"), color = "orange"),
                valueBox(paste0(round(dt_acc * 100, 2), "%"), "Decision Tree", icon = icon("check"), color = "red"),
                valueBox(paste0(round(rf_acc * 100, 2), "%"), "Random Forest", icon = icon("check"), color = "green")
              ),
              box(plotOutput("acc_plot"), width = 12),
              fluidRow(
                box(plotOutput("roc_log"), width = 6),
                box(plotOutput("roc_knn"), width = 6),
                box(plotOutput("roc_nb"), width = 6),
                box(plotOutput("roc_dt"), width = 6),
                box(plotOutput("roc_rf"), width = 6)
              )
      ),
      tabItem("heatmaps", fluidRow(
        box(plotOutput("heatmap_log"), width = 6),
        box(plotOutput("heatmap_knn"), width = 6),
        box(plotOutput("heatmap_nb"), width = 6),
        box(plotOutput("heatmap_dt"), width = 6),
        box(plotOutput("heatmap_rf"), width = 6)
      )),
      tabItem("importance", box(plotOutput("importance_plot"), width = 12)),
      tabItem("metrics", box(tableOutput("metric_table"), width = 12))
    )
  )
)

# Server
server <- function(input, output) {
  output$summary <- renderPrint({ summary(data) })
  output$missing <- renderTable({ data.frame(Column = names(missing_values), Missing = missing_values) })
  output$before_box <- renderPlot({ boxplot(data_before_removal, main = "Before", col = "blue") })
  output$after_box <- renderPlot({ boxplot(data_cleaned, main = "After", col = "purple") })
  output$corrplot <- renderPlot({ corrplot(cor(data_cleaned[sapply(data_cleaned, is.numeric)]), method = "color") })
  output$url_plot <- renderPlot({ ggplot(data, aes(x = UrlLength)) + geom_histogram(binwidth = 10, fill = "orange") })
  output$class_plot <- renderPlot({ ggplot(data, aes(x = factor(CLASS_LABEL))) + geom_bar(fill = "red") })
  
  output$acc_plot <- renderPlot({
    ggplot(data_accuracy, aes(x = Model, y = Accuracy, fill = Model)) +
      geom_col() +
      geom_text(aes(label = paste0(round(Accuracy * 100, 1), "%")), vjust = -0.5) +
      scale_fill_manual(values = c(
        "Logistic Regression" = "blue", "KNN" = "purple",
        "Naive Bayes" = "orange", "Decision Tree" = "red", "Random Forest" = "green"
      )) + theme_minimal()
  })
  
  output$roc_log <- renderPlot({ plot(log_roc, col = "blue", main = "Logistic ROC") })
  output$roc_knn <- renderPlot({ plot(knn_roc, col = "purple", main = "KNN ROC") })
  output$roc_nb <- renderPlot({ plot(nb_roc, col = "orange", main = "Naive Bayes ROC") })
  output$roc_dt <- renderPlot({ plot(dt_roc, col = "red", main = "Decision Tree ROC") })
  output$roc_rf <- renderPlot({ plot(rf_roc, col = "green", main = "Random Forest ROC") })
  
  output$importance_plot <- renderPlot({
    imp_df <- data.frame(Feature = rownames(rf_model$importance), Importance = rf_model$importance[, "MeanDecreaseGini"])
    imp_df <- imp_df[order(-imp_df$Importance), ][1:20, ]
    ggplot(imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_col(fill = "tomato") + coord_flip()
  })
  
  output$metric_table <- renderTable({
    metric_table %>% mutate(across(where(is.numeric), ~ round(. * 100, 2)))
  })
  
  output$heatmap_log <- renderPlot({ render_heatmap(conf_log, "Logistic Regression", "#0073C2") })
  output$heatmap_knn <- renderPlot({ render_heatmap(conf_knn, "KNN", "#8E44AD") })
  output$heatmap_nb <- renderPlot({ render_heatmap(conf_nb, "Naive Bayes", "#E67E22") })
  output$heatmap_dt <- renderPlot({ render_heatmap(conf_dt, "Decision Tree", "#C0392B") })
  output$heatmap_rf <- renderPlot({ render_heatmap(conf_rf, "Random Forest", "#27AE60") })
}





Sys.getenv("SHINYAPPS_NAME")
Sys.getenv("SHINYAPPS_TOKEN")
Sys.getenv("SHINYAPPS_SECRET")

rsconnect::setAccountInfo(
  name = 'hamzahc3636726',
  token = '5270137F7B02772D7F9D9BD322D488BA',
  secret = '+Twm95iC1nBhvfy6aixDDnU5UcPwZKeCfJDF96Qt'
)


rsconnect::deployApp()



# Run the app
shinyApp(ui = ui, server = server)
