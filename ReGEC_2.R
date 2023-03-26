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

# There are some rows of data that contain Na, based on the observation
# we can drop them from the dataset
data <- data[!(is.na(data$ca) | is.na(data$thal)),]
nrow(data) #303 - 6 = 297 rows remaining


# Convert the target variable to a factor
data$target <- factor(ifelse(data$target == 0, "negative", "positive"))


# Feature Extraction using LDA approach
feature_extraction <- function(target, train_data, test_data) {
  # Fit an LDA model to the training data
  lda.fit <- lda(target ~ ., data= train_data)
  
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
  train_index <- createDataPartition(data$target, p = 0.9, list = FALSE)
  train_data <- data[train_index,]
  test_data <- data[-train_index,]

  # LDA Feature Extraction
  lda <- feature_extraction(target=data$target, train_data, test_data)
  
  lda_train_features = lda$train_lda.x
  lda_test_features = lda$test_lda.x
  
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
  svm_model <- svm(train_data$target ~., data = data.frame(projected_train_data), kernel="linear", cost=1)

  # Obtain the predictions 
  predictions <- predict_RGEC_classifier(svm_model, projected_test_data)
  
  confusionMatrix(table(predictions, test_data$target))
  
  return(predictions)
}

predictions = train_RGEC_classifier(data)

