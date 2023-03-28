# ReGEC Classifier Updated

# Load the required packages
# install.packages("e1071", dep = TRUE) 

library(MASS)
library(caret)
library(e1071)

# Load the Cleveland Heart Disease dataset
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
data <- read.csv(url, header = FALSE, na.strings = "?")

# Checking how many rows is the dataset = 303
nrow(data)

# Assign column names to the dataset
colnames(data) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", 
                    "thalach", "exang", "oldpeak", "slope", "ca", "thal", "target")


colnames(data)

# data$target <- ifelse(data$target > 0, 1, 0) # convert response variable to binary

# data <- data[!(is.na(data$ca) | is.na(data$thal)),]
# nrow(data) #303 - 6 = 297 rows remaining



# Feature Extraction using LDA approach
feature_extraction <- function(train_data, test_data) {
  # Fit an LDA model to the training data
  lda.fit <- lda(train_data, train_data$target)
  
  # Extract the LDA features from the training and testing data
  train_lda <- predict(lda.fit, train_data)
  test_lda <- predict(lda.fit, test_data)
  
  return(c(train_lda=train_lda, test_lda=test_lda))
}


predict_RGEC_classifier <- function(model, projected_test_data) {
  # Make predictions on the projected data
  predictions <- predict(model, newdata = as.data.frame(projected_test_data))
  
  # Return the predicted labels
  return(predictions)
}


train_RGEC_classifier <- function(data) {
  
  # Train/test splitting the dataset to 90/10 
  set.seed(123)
  
  # data$target <- ifelse(data$target > 0, 1, 0)
  data <- data[!(is.na(data$ca) | is.na(data$thal)),]
  
  train_index <- sample(1:nrow(data), 0.9*nrow(data))
  train_data <- data[train_index, 1:13]
  train_target <- data[train_index, 14]
  test_data <- data[-train_index, 1:13]
  test_target <- data[-train_index, 14]
  
  # Scale the training and testing datasets
  train_data <- scale(train_data)
  test_data <- scale(test_data)

  str(train_data)
  colnames(train_data)
  
  # LDA Feature Extraction
  lda_train <- lda(train_data, train_target)
  lda_train_data <- predict(lda_train, train_data)$x
  lda_test_data <- predict(lda_train, test_data)$x
  
  
  
  
  # LDA Feature Extraction
  # lda_train <- lda(train_data, train_data$target)
  # lda_train_features <- predict(lda_train, train_data)$x
  # lda_test_features <- predict(lda_train, test_data)$x
  
  
  # Defining the regularization parameter to help prevent over-fitting.
  regularization_parameter <- 0.1
  
  # Compute the overall mean and variance for train features
  train_mean <- colMeans(lda_train_features)
  train_conv <- cov(lda_train_features)
  
  # Compute the between-class and within-class scatter matrices
  train_within_class <- train_conv +
    diag(regularization_parameter, ncol(lda_train_features))
  train_between_class <- (t(train_mean - lda_train_features)
                          %*% (train_mean - lda_train_features)) / nrow(lda_train_features)
  
  # Compute the generalized eigenvectors and eigenvalues
  eigen <- eigen(solve(train_within_class) %*% train_between_class)
  
  # Sort the eigenvalues in descending order
  eigen_order <- order(eigen$values, decreasing = TRUE)
  eigen$values <- eigen$values[eigen_order]
  eigen$vectors <- eigen$vectors[, eigen_order]
  
  # Obtain the projection matrix - matrix d
  projection_matrix <- as.matrix(eigen$vectors)
  
  # Project the test data on the selected eigenvectors, to transform the sample 
  # onto the new subspace - d * k eigenvector matrix
  projected_train_data <- as.matrix(lda_train_features) %*% projection_matrix
  projected_test_data <- as.matrix(lda_test_features) %*% projection_matrix
  
  # Train SVM model on the projected training data using both kernels 
  # (linear and gaussian)
  str(lda_train_data)
  svm_model <- svm(x = lda_train_data, y = train_target, kernel = "linear")
  
  svm_pred <- predict(svm_model, lda_test_data)
  

  accuracy <- sum(svm_pred == test_target) / length(test_target)
  cat("Accuracy:", round(accuracy, 2))
  
  
  
  # svm_model <- svm(train_data$target ~., data = data.frame(projected_train_data), kernel="linear", cost=1)
  
  # Obtain the predictions 
  predictions <- predict_RGEC_classifier(svm_model, projected_test_data)
  
  confusionMatrix(table(predictions, test_data$target))
  
  return(predictions)
}

predictions = train_RGEC_classifier(data)
