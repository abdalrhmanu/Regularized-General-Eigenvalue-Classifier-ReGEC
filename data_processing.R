library(MASS)
library(caret)

set.seed(123)

# Importing dataset - Heart Disease Data Set
dataset <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
data <- read.csv(dataset, header=FALSE, stringsAsFactors = TRUE)

str(data)

head(data) # none of the columns are labeled

colnames(data) <- c(
  "age",
  "sex",# 0 = female, 1 = male
  "cp", # chest pain
  # 1 = typical angina,
  # 2 = atypical angina,
  # 3 = non-anginal pain,
  # 4 = asymptomatic
  "trestbps", # resting blood pressure (in mm Hg)
  "chol", # serum cholestoral in mg/dl
  "fbs",  # fasting blood sugar if less than 120 mg/dl, 1 = TRUE, 0 = FALSE
  "restecg", # resting electrocardiographic results
  # 0 = normal
  # 1 = having ST-T wave abnormality
  # 2 = showing probable or definite left ventricular hypertrophy
  "thalach", # maximum heart rate achieved
  "exang",   # exercise induced angina, 1 = yes, 0 = no
  "oldpeak", # ST depression induced by exercise relative to rest
  "slope", # the slope of the peak exercise ST segment
  # 1 = upsloping
  # 2 = flat
  # 3 = downsloping
  "ca", # number of major vessels (0-3) colored by fluoroscopy
  "thal", # this is short of thalium heart scan
  # 3 = normal
  # 6 = fixed defect
  # 7 = reversible defect
  "num" # (the predicted attribute) - diagnosis of heart disease
  # 0 if less than or equal to 50% diameter narrowing
  # 1 if greater than 50% diameter narrowing
)

head(data)

# Understanding the data set, there are some factors as in ca and thal as well 
# as sex is represented in numbers
# also there are some unknown values "?" in both ca and thal that need to be 
# processed
str(data)

# We can convert all "?" to NAs
data[data == "?"] <- NA
anyNA(data)

## Now add factors for variables that are factors and clean up the factors
## that had missing data...
data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex)

data$cp <- as.factor(data$cp) #chest pain
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)

data$ca <- as.integer(data$ca) # since this column had "?"s in it
# R thinks that the levels for the factor are strings, but
# we know they are integers, so first convert the strings to integers...

data$ca <- as.factor(data$ca)  # ...then convert the integers to factor levels

data$thal <- as.integer(data$thal) # "thal" also had "?"s in it.
data$thal <- as.factor(data$thal)

str(data) ## this shows that the correct columns are factors

# Processing the rows with "?" that were converted to Na
nrow(data[is.na(data$ca) | is.na(data$thal),])
data[is.na(data$ca) | is.na(data$thal),]

# There seem to be only 6 rows thus we can drop them from the dataset
data <- data[!(is.na(data$ca) | is.na(data$thal)),]
nrow(data) #303 - 6 = 297 rows remaining


### Now we start splitting the data set for SVM classification
# We split the data to 70% train and 30% testing
train_idx <- sample(1:nrow(data), nrow(data) * 0.9)
train_data <- data[train_idx, -ncol(data)] # Remove the last column (class label)
train_labels <- ifelse(data[train_idx, ncol(data)] > 0, -1, 1)

test_data <- data[-train_idx, -ncol(data)] # Remove the last column (class label)
test_labels <- ifelse(data[-train_idx, ncol(data)] > 0, -1, 1)

# str(test_data)

nrow(train_data)
ncol(train_data)

nrow(test_data)

# Using LDA for feature extraction
# ldaModel <- lda(data$num ~ ., data=train_data)





train_RGEC_classifier <- function(data, features, labels, lambda) {
  
  
}

predict_RGEC_classifier <- function(classifier, data, features) {
  
  
}















# 
# 
# 
# 
# # Training function
# train_RGEC_classifier <- function(data, features, labels, lambda) {
#   # Subset the data to the specified features and labels
#   subset_data <- data[, c(features, labels)]
#   
#   # Compute the mean and variance for each class
#   mean_mat <- by(subset_data[, features], subset_data[, labels], colMeans)
#   cov_mat <- by(subset_data[, features], subset_data[, labels], cov)
#   
#   # Compute the overall mean and covariance
#   overall_mean <- colMeans(subset_data[, features])
#   overall_cov <- cov(subset_data[, features])
#   
#   # Compute the between-class and within-class scatter matrices
#   Sb <- Reduce("+", lapply(names(mean_mat), function(x) {
#     n <- sum(subset_data[, labels] == x)
#     (mean_mat[[x]] - overall_mean) %*% t(mean_mat[[x]] - overall_mean) * n
#   }))
#   
#   Sw <- Reduce("+", lapply(names(cov_mat), function(x) {
#     n <- sum(subset_data[, labels] == x)
#     if (n > 1) {
#       cov_mat[[x]] <- cov_mat[[x]] * (n - 1)
#     }
#     cov_mat[[x]]
#   }))
#   
#   # Add regularization to the within-class scatter matrix
#   Sw_reg <- Sw + lambda * diag(ncol(Sw))
#   
#   # Compute the generalized eigenvectors and eigenvalues
#   eig <- eigen(Sb, Sw_reg, symmetric = TRUE)
#   
#   # Sort the eigenvalues in descending order
#   eig_order <- order(eig$values, decreasing = TRUE)
#   eig$values <- eig$values[eig_order]
#   eig$vectors <- eig$vectors[, eig_order]
#   
#   # Extract the projection matrix
#   projection_matrix <- eig$vectors[, 1:(length(unique(subset_data[, labels])) - 1)]
#   
#   # Return the model
#   return(list(projection_matrix = projection_matrix, 
#               overall_mean = overall_mean, 
#               overall_cov = overall_cov))
# }
# 
# 
# # Predicting function
# predict_RGEC_classifier <- function(classifier, data, features) {
#   # Project the data onto the projection matrix
#   data_proj <- as.matrix(data[, features]) %*% classifier$scaling
#   
#   # Make predictions on the projected data
#   predictions <- predict(classifier, newdata = data_proj)$class
#   
#   # Return the predicted labels
#   return(predictions)
# }
# 
# 
# # Load the iris dataset
# data(iris)
# 
# # Train a regularized generalized eigenvalue classifier on the iris dataset
# classifier <- train_RGEC_classifier(iris, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), "Species", 0.1)
# 
# # Make predictions on the first 10 rows of the iris dataset
# new_data <- iris[1:10, ]
# predictions <- predict_RGEC_classifier(classifier, new_data, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
# 
# # Print the predicted labels
# print(predictions)

