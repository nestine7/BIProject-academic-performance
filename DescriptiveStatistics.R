#Step 1 Load the required packages ----

if (!is.element("renv", installed.packages()[, 1])) {
  install.packages("renv", dependencies = TRUE)
}
require("renv")

renv::init()

renv::restore()

if (!is.element("languageserver", installed.packages()[, 1])) {
  install.packages("languageserver", dependencies = TRUE)
}
require("languageserver")



library(readr)
dataset <- read_csv("data/dataset.csv")
View(dataset)
# Step 2 Perform the respective measures ----
## 1. Measures of frequency ----
dataset_freq <- dataset$`Daytime/evening attendance`
cbind(frequency = table(dataset_freq),
      percentage = prop.table(table(dataset_freq)) * 100)

## 2. Measures of central tendency ----
dataset_mode <- names(table(dataset$`Daytime/evening attendance`))[
  which(table(dataset$`Daytime/evening attendance`) == max(table(dataset$`Daytime/evening attendance`)))
]
print(dataset_mode)

## 3. Measures of distribution/dispersion/spread/scatter/variability ----
summary(dataset)

## 4. Measures of Relationship ----
dataset_cov <- cov(dataset[, 4])
View(dataset_cov)

#Perform ANOVA on the “dataset” dataset ----
dataset_one_way_anova <- aov(GDP ~ `Curricular units 1st sem (grade)` + `Curricular units 2nd sem (grade)`, data = dataset)
summary(dataset_one_way_anova)

### Univariate Plots ----
## Create Histograms for Each Numeric Attribute ----
dataset_GDP <- as.numeric(unlist(dataset[, 4]))
hist(dataset_GDP, main = names(dataset)[4])

## Create Box and Whisker Plots for Each Numeric Attribute ----
par(mfrow = c(1, 3))
for (i in 1:3) {
  boxplot(dataset[, i], main = names(dataset)[i])
}

### Create Bar Plots for Each Categorical Attribute ----
barplot(table(dataset[, 4]), main = names(dataset)[4])

# Execute the following to create a map to identify the missing data in each
# dataset:
if (!is.element("Amelia", installed.packages()[, 1])) {
  install.packages("Amelia", dependencies = TRUE)
}
require("Amelia")

missmap(dataset, col = c("red", "grey"), legend = TRUE)

### Create a Correlation Plot ----
if (!is.element("corrplot", installed.packages()[, 1])) {
  install.packages("corrplot", dependencies = TRUE)
}

require("corrplot")
corrplot(cor(dataset[, 4]), method = "circle")

### Create Multivariate Box and Whisker Plots by Class ----
if (!is.element("caret", installed.packages()[, 1])) {
  install.packages("caret", dependencies = TRUE)
}

### Loaded the required packages
require("caret")
featurePlot(x = dataset[, 1:4], y = dataset[, 5], plot = "box")


## NHANES ----
if (!is.element("NHANES", installed.packages()[, 1])) {
  install.packages("NHANES", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("NHANES")

## dplyr ----
if (!is.element("dplyr", installed.packages()[, 1])) {
  install.packages("dplyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("dplyr")

## naniar ----
# Documentation:
#   https://cran.r-project.org/package=naniar or
#   https://www.rdocumentation.org/packages/naniar/versions/1.0.0
if (!is.element("naniar", installed.packages()[, 1])) {
  install.packages("naniar", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("naniar")

## ggplot2 ----
# We require the "ggplot2" package to create more appealing visualizations
if (!is.element("ggplot2", installed.packages()[, 1])) {
  install.packages("ggplot2", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("ggplot2")

## MICE ----
# We use the MICE package to perform data imputation
if (!is.element("mice", installed.packages()[, 1])) {
  install.packages("mice", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("mice")

## Amelia ----
if (!is.element("Amelia", installed.packages()[, 1])) {
  install.packages("Amelia", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("Amelia")

# STEP 3. Create a subset of the variables/features ----
# Are there missing values in the dataset?
any_na(dataset)

# How many?
n_miss(dataset)

# What is the percentage of missing data in the entire dataset?
prop_miss(dataset)

# How many missing values does each variable have?
dataset %>% is.na() %>% colSums()

# What is the number and percentage of missing values grouped by
# each variable?
miss_var_summary(dataset)

# What is the number and percentage of missing values grouped by
# each observation?
miss_case_summary(dataset)

# Which variables contain the most missing values?
gg_miss_var(dataset)

# Where are missing values located (the shaded regions in the plot)?
vis_miss(dataset) + theme(axis.text.x = element_text(angle = 80))

# Which combinations of variables are missing together?
gg_miss_upset(dataset)



## klaR ----
if (require("klaR")) {
  require("klaR")
} else {
  install.packages("klaR", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## e1071 ----
if (require("e1071")) {
  require("e1071")
} else {
  install.packages("e1071", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}


## readr ----
if (require("readr")) {
  require("readr")
} else {
  install.packages("readr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## LiblineaR ----
if (require("LiblineaR")) {
  require("LiblineaR")
} else {
  install.packages("LiblineaR", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## naivebayes ----
if (require("naivebayes")) {
  require("naivebayes")
} else {
  install.packages("naivebayes", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
  
## caret ----
  if (require("caret")) {
    require("caret")
  } else {
    install.packages("caret", dependencies = TRUE,
                     repos = "https://cloud.r-project.org")
}

# Step 4 (Splitting the dataset): Dataset ----

library(readr)
dataset <- read_csv("data/dataset.csv")
View(dataset)

summary(dataset)

str(dataset)

## 1. Split the dataset ====
# Define a 75:25 train:test data split of the dataset.
# That is, 75% of the original data will be used to train the model and
# 25% of the original data will be used to test the model.
train_index <- createDataPartition(dataset$Target,
                                   p = 0.75,
                                   list = FALSE)
dataset_train <- dataset[train_index, ]
dataset_test <- dataset[-train_index, ]

## 2. Train a Naive Bayes classifier using the training dataset ----
### 2.a. OPTION 1: naiveBayes() function in the e1071 package ----
# The "naiveBayes()" function (case sensitive) in the "e1071" package
# is less sensitive to missing values hence all the features (variables
# /attributes) are considered as independent variables that have an effect on
# the dependent variable (stock).

dataset_model_nb_e1071 <- # nolint
  e1071::naiveBayes(Target ~ `Marital status` + `Application mode` + `Application order` 
                    + `Daytime/evening attendance` + `Previous qualification` + 
                      Nacionality + `Mother's qualification` + `Father's qualification` 
                    + `Mother's occupation` + `Father's occupation` + Displaced +
                      `Educational special needs` + `Tuition fees up to date` + Gender
                    + `Scholarship holder` + `Age at enrollment` + International +
                      `Curricular units 1st sem (credited)` + 
                      `Curricular units 1st sem (enrolled)` + 
                      `Curricular units 1st sem (evaluations)` + 
                      `Curricular units 1st sem (approved)` + 
                      `Curricular units 1st sem (grade)` +
                      `Curricular units 1st sem (without evaluations)` +
                      `Curricular units 2nd sem (credited)` + 
                      `Curricular units 2nd sem (enrolled)` + 
                      `Curricular units 2nd sem (evaluations)` + 
                      `Curricular units 2nd sem (approved)` + 
                      `Curricular units 2nd sem (grade)` +
                      `Curricular units 2nd sem (without evaluations)` + 
                      `Unemployment rate` + `Inflation rate` + GDP,
                    data = dataset_train)

## 3. Test the trained model using the testing dataset ----
### 3.a. Test the trained e1071 Naive Bayes model using the testing dataset ----
predictions_nb_e1071 <-
  predict(dataset_model_nb_e1071,
          dataset_test[, c("Marital status", "Application mode", "Application order",
                           "Daytime/evening attendance", "Previous qualification",  
                           "Nacionality","Mother's qualification", "Father's qualification", 
                           "Mother's occupation", "Father's occupation", "Displaced", 
                           "Educational special needs", "Tuition fees up to date", 
                           "Gender", "Scholarship holder", "Age at enrollment",  
                           "International", "Curricular units 1st sem (credited)",  
                           "Curricular units 1st sem (enrolled)", 
                           "Curricular units 1st sem (evaluations)", 
                           "Curricular units 1st sem (approved)",
                           "Curricular units 1st sem (grade)",
                           "Curricular units 1st sem (without evaluations)",
                           "Curricular units 2nd sem (credited)",  
                           "Curricular units 2nd sem (enrolled)",
                           "Curricular units 2nd sem (evaluations)",
                           "Curricular units 2nd sem (approved)",
                           "Curricular units 2nd sem (grade)",
                           "Curricular units 2nd sem (without evaluations)",
                           "Unemployment rate", "Inflation rate", "GDP")])


## 4. View the Results ----
### 4.a. e1071 Naive Bayes model and test results using a confusion matrix ----
# Please watch the following video first: https://youtu.be/Kdsp6soqA7o
print(predictions_nb_e1071)
caret::confusionMatrix(predictions_nb_e1071,
                       dataset_test[, c("Marital status", "Application mode", "Application order",
                                        "Daytime/evening attendance", "Previous qualification",  
                                        "Nacionality","Mother's qualification", "Father's qualification", 
                                        "Mother's occupation", "Father's occupation", "Displaced", 
                                        "Educational special needs", "Tuition fees up to date", 
                                        "Gender", "Scholarship holder", "Age at enrollment",  
                                        "International", "Curricular units 1st sem (credited)",  
                                        "Curricular units 1st sem (enrolled)", 
                                        "Curricular units 1st sem (evaluations)", 
                                        "Curricular units 1st sem (approved)",
                                        "Curricular units 1st sem (grade)",
                                        "Curricular units 1st sem (without evaluations)",
                                        "Curricular units 2nd sem (credited)",  
                                        "Curricular units 2nd sem (enrolled)",
                                        "Curricular units 2nd sem (evaluations)",
                                        "Curricular units 2nd sem (approved)",
                                        "Curricular units 2nd sem (grade)",
                                        "Curricular units 2nd sem (without evaluations)",
                                        "Unemployment rate", "Inflation rate", "GDP", "Target")]$Target)
plot(table(predictions_nb_e1071,
           dataset_test[, c("Marital status", "Application mode", "Application order",
                            "Daytime/evening attendance", "Previous qualification",  
                            "Nacionality","Mother's qualification", "Father's qualification", 
                            "Mother's occupation", "Father's occupation", "Displaced", 
                            "Educational special needs", "Tuition fees up to date", 
                            "Gender", "Scholarship holder", "Age at enrollment",  
                            "International", "Curricular units 1st sem (credited)",  
                            "Curricular units 1st sem (enrolled)", 
                            "Curricular units 1st sem (evaluations)", 
                            "Curricular units 1st sem (approved)",
                            "Curricular units 1st sem (grade)",
                            "Curricular units 1st sem (without evaluations)",
                            "Curricular units 2nd sem (credited)",  
                            "Curricular units 2nd sem (enrolled)",
                            "Curricular units 2nd sem (evaluations)",
                            "Curricular units 2nd sem (approved)",
                            "Curricular units 2nd sem (grade)",
                            "Curricular units 2nd sem (without evaluations)",
                            "Unemployment rate", "Inflation rate", "GDP",
                            "Target")]$Target))

# Step 5 (Bootstrapping): Performance Data Set =====
dataset <-
    dataset <- readr::read_delim(
      "data/dataset.csv",
      escape_double = FALSE,
      col_types = cols(
        `Marital status` = col_integer(),
        `Application mode` = col_integer(),
        `Application order` = col_integer(),
        `Course` = col_integer(),
        `Daytime/evening attendance` = col_integer(),
        `Previous qualification` = col_integer(),
        `Nacionality` = col_integer(),
        `Mother's qualification` = col_integer(),
        `Father's qualification` = col_integer(),
        `Mother's occupation` = col_integer(),
        `Father's occupation` = col_integer(),
        `Displaced` = col_integer(),
        `Educational special needs` = col_integer(),
        `Debtor` = col_integer(),
        `Tuition fees up to date` = col_integer(),
        `Gender` = col_integer(),
        `Scholarship holder` = col_integer(),
        `Age at enrollment` = col_integer(),
        `International` = col_integer(),
        
        `Curricular units 1st sem (credited)` = col_integer(),
        `Curricular units 1st sem (enrolled)` = col_integer(),
        `Curricular units 1st sem (evaluations)` = col_integer(),
        `Curricular units 1st sem (approved)` = col_integer(),
        `Curricular units 1st sem (grade)` = col_double(),
        `Curricular units 1st sem (without evaluations)` = col_integer(),
        
        `Curricular units 2nd sem (credited)` = col_integer(),
        `Curricular units 2nd sem (enrolled)` = col_integer(),
        `Curricular units 2nd sem (evaluations)` = col_integer(),
        `Curricular units 2nd sem (approved)` = col_integer(),
        `Curricular units 2nd sem (grade)` = col_double(),
        `Curricular units 2nd sem (without evaluations)` = col_integer(),
        
        `Unemployment rate` = col_double(),
        `Inflation rate` = col_double(),
        `GDP` = col_double(),
        `Target` = col_factor(levels = c("Dropout", "Enrolled", "Graduate"))
        
      ),
  
trim_ws = TRUE
)

summary(dataset)
str(dataset)

## 1. Split the dataset ----
dataset_cor <- cor(dataset[, 3:13])
View(dataset_cor)

train_index <-
  createDataPartition(dataset$`Target`,
                      p = 0.75, list = FALSE)
dataset_train <- dataset[train_index, ] # nolint
dataset_test <- dataset[-train_index, ] # nolint


## 2. Classification: LDA with k-fold Cross Validation ----

### 2.a. LDA classifier based on a 5-fold cross validation ----
train_control <- trainControl(method = "cv", number = 5)

dataset_model_lda <-
  caret::train(`Target` ~ ., data = dataset_train,
               trControl = train_control, na.action = na.omit, method = "lda2",
               metric = "Accuracy")

### 2.b. Test the trained LDA model using the testing dataset ----
predictions_lda <- predict(dataset_model_lda,
                           dataset_test[, 1:35])

### 2.c. View the summary of the model and view the confusion matrix ----
print(dataset_model_lda)
caret::confusionMatrix(predictions_lda, dataset_test$Target)

## 3. Classification: Naive Bayes with Repeated k-fold Cross Validation ----
### 3.a. Train an e1071::naive Bayes classifier based on the Target variable ----
dataset_model_nb <-
  e1071::naiveBayes(`Target` ~ ., data = dataset_train)

### 3.b. Test the trained naive Bayes classifier using the testing dataset ----
predictions_nb_e1071 <-
  predict(dataset_model_nb, dataset_test[, 1:14])

### 3.c. View a summary of the naive Bayes model and the confusion matrix ----
print(dataset_model_nb)
caret::confusionMatrix(predictions_nb_e1071, dataset_test$Target)


## 4. Classification: NNaive Bayes with Repeated k-fold Cross Validation ----
# In Leave One Out Cross-Validation (LOOCV), a data instance is left out and a
# model constructed on all other data instances in the training set. This is
# repeated for all data instances.

### 4.a. Train a Naive Bayes classifier based on an LOOCV ----
train_control <- trainControl(method = "LOOCV")

dataset_model_nb_loocv <-
  caret::train(`Target` ~ ., data = dataset_train,
               trControl = train_control, na.action = na.omit,
               method = "naive_bayes", metric = "Accuracy")

### 4.b. Test the trained model using the testing dataset ====
predictions_nb_loocv <-
  predict(dataset_model_nb_loocv, dataset_test[, 1:35])

### 4.c. View the confusion matrix ====
print(dataset_model_nb_loocv)
caret::confusionMatrix(predictions_nb_loocv, dataset_test$Target)

# Step 6: Evaluation Metrics ----


## mlbench ----
if (require("mlbench")) {
  require("mlbench")
} else {
  install.packages("mlbench", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## pROC ----
if (require("pROC")) {
  require("pROC")
} else {
  install.packages("pROC", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

# 1. Accuracy and Cohen's Kappa ----
data(dataset)

dataset_freq <- dataset$Target
cbind(frequency =
        table(dataset_freq),
      percentage = prop.table(table(dataset_freq)) * 100)
## 1.b. Determine the Baseline Accuracy ----
Target_freq <- dataset$Target
cbind(frequency =
        table(dataset_freq),
      percentage = prop.table(table(dataset_freq)) * 100)

## 1.c. Split the dataset ----
# Define a 75:25 train:test data split of the dataset.
# That is, 75% of the original data will be used to train the model and
# 25% of the original data will be used to test the model.
train_index <- createDataPartition(dataset$Target,
                                   p = 0.75,
                                   list = FALSE)
dataset_train <- dataset[train_index, ]
dataset_test <- dataset[-train_index, ]

## 1.d. Train the Model ----
# We apply the 5-fold cross validation resampling method
train_control <- trainControl(method = "cv", number = 5)

set.seed(7)
dataset_model_multinom <-
  train(Target ~ ., data = dataset_train, method = "multinom",
        metric = "Accuracy", trControl = train_control)

## 1.e. Display the Model's Performance ----
print(dataset_model_multinom)
# Step 7.a.: Algorithm Selection for Classification and Regression ----

## stats ----
if (require("stats")) {
  require("stats")
} else {
  install.packages("stats", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## MASS ----
if (require("MASS")) {
  require("MASS")
} else {
  install.packages("MASS", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## glmnet ----
if (require("glmnet")) {
  require("glmnet")
} else {
  install.packages("glmnet", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## kernlab ----
if (require("kernlab")) {
  require("kernlab")
} else {
  install.packages("kernlab", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## rpart ----
if (require("rpart")) {
  require("rpart")
} else {
  install.packages("rpart", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## b. Logistic Regression ----

#### Load and split the dataset ----
library(readr)
dataset <- read_csv("data/dataset.csv")
View(dataset)

# Define a 75:25 train:test data split of the dataset.
train_index <- createDataPartition(dataset$Target,
                                   p = 0.75,
                                   list = FALSE)
dataset_train <- dataset[train_index, ]
dataset_test <- dataset[-train_index, ]



#### Train the model ----
dataset_model_multinom <- multinom(Target ~ ., data = dataset_train,
                          family = binomial(link = "logit"))

#### Display the model's details ----
print(dataset_model_multinom)

#### Make predictions ----
probabilities <- predict(dataset_model_multinom, dataset_test[, 1:35],
                         type = "prob")
print(probabilities)
predictions <- ifelse(probabilities > 0.5, "pos", "neg")
print(predictions)

#### Display the model's evaluation metrics ----
table(predictions, dataset_test$Target)

####c. Logistic Regression with caret ----
### Load and split the dataset ----
library(readr)
dataset <- read_csv("data/dataset.csv")
View(dataset)

# Define a 70:30 train:test data split of the dataset.
train_index <- createDataPartition(dataset$Target,
                                   p = 0.7,
                                   list = FALSE)
dataset_train <- dataset[train_index, ]
dataset_test <- dataset[-train_index, ]

#### Train the model ----
# We apply the 5-fold cross validation resampling method
train_control <- trainControl(method = "cv", number = 5)
# We can use "regLogistic" instead of "glm"
# Notice the data transformation applied when we call the train function
# in caret, i.e., a standardize data transform (centre + scale)
set.seed(7)
Target_caret_model_logistic <-
  train(Target ~ ., data = dataset_train,
        method = "regLogistic", metric = "Accuracy",
        preProcess = c("center", "scale"), trControl = train_control)

#### Display the model's details ----
print(Target_caret_model_logistic)

#### Make predictions ----
predictions <- predict(Target_caret_model_logistic,
                       dataset_test[, 1:34])

#### Display the model's evaluation metrics ----
confusion_matrix <-
  caret::confusionMatrix(predictions,
                         dataset_test[, 1:35]$Target)
print(confusion_matrix)

fourfoldplot(as.table(confusion_matrix), color = c("grey", "lightblue"),
             main = "Confusion Matrix")

# Step 8.: Model Performance Comparison ----
## randomForest ----
if (require("randomForest")) {
  require("randomForest")
} else {
  install.packages("randomForest", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

library(readr)
dataset <- read_csv("Data/dataset.csv")
View(dataset)

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

### a. LDA ----
set.seed(7)
Target_model_lda <- train(Target ~ ., data = dataset,
                            method = "lda", trControl = train_control)

### b. CART ----
set.seed(7)
Target_model_cart <- train(Target ~ ., data = dataset,
                             method = "rpart", trControl = train_control)


### c. SVM ----
set.seed(7)
Target_model_svm <- train(Target ~ ., data = dataset,
                            method = "svmRadial", trControl = train_control)

### d. Random Forest ----
set.seed(7)
Target_model_rf <- train(Target ~ ., data = dataset,
                           method = "rf", trControl = train_control)

## 8.1.a. Call the `resamples` Function ----
# We then create a list of the model results and pass the list as an argument
# to the `resamples` function.

results <- resamples(list(LDA = Target_model_lda, 
                        
                           SVM = Target_model_svm,
                          RF = Target_model_rf))

summary(results)

## b. Box and Whisker Plot ----
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
bwplot(results, scales = scales)

## c. Dot Plots ----
# They show both the mean estimated accuracy as well as the 95% confidence
# interval (e.g. the range in which 95% of observed scores fell).

scales <- list(x = list(relation = "free"), y = list(relation = "free"))
dotplot(results, scales = scales)

## d. Scatter Plot Matrix ----
splom(results)

# Step 9: Hyperparameter Tuning ----
## A. Install and Load the Required Packages ----
## randomForest ----
if (require("randomForest")) {
  require("randomForest")
} else {
  install.packages("randomForest", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## mlbench ----
if (require("mlbench")) {
  require("mlbench")
} else {
  install.packages("mlbench", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## RRF ----
if (require("RRF")) {
  require("RRF")
} else {
  install.packages("RRF", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

# B. Load the Dataset ----
library(readr)
dataset <- read_csv("Data/dataset.csv")
View(dataset)
dataset_independent_variables <- dataset[, 1:34]
dataset_dependent_variables <- dataset[, 35]

# C. Train the Model ----
seed <- 7
metric <- "Accuracy"

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(seed)
mtry <- sqrt(ncol(dataset_independent_variables))
tunegrid <- expand.grid(.mtry = mtry)
dataset_model_default_rf <- train(Target ~ ., data = dataset, method = "rf",
                                metric = metric,
                                # enables us to maintain mtry at a constant
                                tuneGrid = tunegrid,
                                trControl = train_control)
print(dataset_model_default_rf)

# D. Apply a "Random Search" to identify the best parameter value ----
# A random search is good if we are unsure of what the value might be and
# we want to overcome any biases we may have for setting the parameter value
# (like the suggested "mtry" equation above).

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                              search = "random")
set.seed(seed)
mtry <- sqrt(ncol(dataset_independent_variables))

dataset_model_random_search_rf <- train(Target ~ ., data = dataset, method = "rf",
                                      metric = metric,
                                      # enables us to randomly search 12 options
                                      # for the value of mtry
                                      tuneLength = 12,
                                      trControl = train_control)

print(dataset_model_random_search_rf)
plot(dataset_model_random_search_rf)

# E. Apply a "Grid Search" to identify the best parameter value ----
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                              search = "grid")
set.seed(seed)

getModelInfo("RRFglobal")

tunegrid <- expand.grid(.mtry = c(1:10),
                        .coefReg = seq(from = 0.1, to = 1, by = 0.1))

dataset_model_grid_search_rrf_global <- train(Target ~ ., data = dataset, # nolint
                                            method = "RRFglobal",
                                            metric = metric,
                                            tuneGrid = tunegrid,
                                            trControl = train_control)
print(dataset_model_grid_search_rrf_global)
plot(dataset_model_grid_search_rrf_global)

# F. Apply a "Manual Search" to identify the best parameter value ----
# Manual Search
train_control <- trainControl(method = "repeatedcv", number = 10,
                              repeats = 3, search = "random")

tunegrid <- expand.grid(.mtry = c(1:5))

modellist <- list()
for (ntree in c(500, 800, 1000)) {
  set.seed(seed)
  dataset_model_manual_search_rf <- train(Class ~ ., data = dataset,
                                        method = "rf", metric = metric,
                                        tuneGrid = tunegrid,
                                        trControl = train_control,
                                        ntree = ntree)
  key <- toString(ntree)
  modellist[[key]] <- dataset_model_manual_search_rf
}

print(modellist)

results <- resamples(modellist)
summary(results)
dotplot(results)

# STEP 10.Training the Model----
## A. Install and Load the Required Packages ----
## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## mlbench ----
if (require("mlbench")) {
  require("mlbench")
} else {
  install.packages("mlbench", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## plumber ----
if (require("plumber")) {
  require("plumber")
} else {
  install.packages("plumber", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")}
#  B. Load the Dataset ----
library(readr)
dataset <- read_csv("Data/dataset.csv")
View(dataset)

# C. Train the Model ----
# create an 80%/20% data split for training and testing datasets respectively
set.seed(9)
train_index <- createDataPartition(dataset$Target,
                                   p = 0.80, list = FALSE)
Target_training <- dataset[train_index, ]
Target_testing <- dataset[-train_index, ]

set.seed(9)
train_control <- trainControl(method = "cv", number = 10)
Target_model_lda <- train(Target ~ ., data = Target_training,
                            method = "lda", metric = "Accuracy",
                            trControl = train_control)

# We print a summary of what caret has done
print(Target_model_lda)

# We then print the details of the model that has been created
print(Target_model_lda$finalModel)

# D. Test the Model ----
# We can test the model
set.seed(9)
predictions <- predict(Target_model_lda, newdata = Target_testing)
confusionMatrix(predictions, Target_testing$Target)

# E. Save and Load your Model ----
saveRDS(Target_model_lda, "./model/saved_Target_model_lda.rds")
# The saved model can then be loaded later as follows:
loaded_Target_model_lda <- readRDS("./model/saved_Target_model_lda.rds")
print(loaded_Target_model_lda)

predictions_with_loaded_model <-
  predict(loaded_Target_model_lda, newdata = Target_testing)
confusionMatrix(predictions_with_loaded_model, Target_testing$Target)

# STEP 11. Creating Functions in R ----

# Plumber requires functions, an example of the syntax for creating a function
# in R is:

name_of_function <- function(arg) {
  # Do something with the argument called `arg`
}

## plumber ----
if (require("plumber")) {
  require("plumber")
} else {
  install.packages("plumber", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

loaded_Target_model_lda <- readRDS("./model/saved_Target_model_lda.rds")

predict_Target <-
  function(`arg_Marital status, arg_Application mode, arg_Application order, 
           arg_Daytime/evening attendance, arg_Previous qualification,
             arg_Nacionality, arg_Mother's qualification, arg_Father's qualification, 
            arg_Mother's occupation, arg_Father's occupation, arg_Displaced,
             arg_Educational special needs, arg_Tuition fees up to date, arg_Gender,
            arg_Scholarship holder, arg_Age at enrollment, arg_International,
             arg_Curricular units 1st sem (credited),
             arg_Curricular units 1st sem (enrolled) , 
             arg_Curricular units 1st sem (evaluations),
             arg_Curricular units 1st sem (approved), 
             arg_Curricular units 1st sem (grade),
             arg_Curricular units 1st sem (without evaluations),
             arg_Curricular units 2nd sem (credited),
             arg_Curricular units 2nd sem (enrolled),
             arg_Curricular units 2nd sem (evaluations),
             arg_Curricular units 2nd sem (approved),
             arg_Curricular units 2nd sem (grade),
             arg_Curricular units 2nd sem (without evaluations), 
             arg_Unemployment rate, arg_Inflation rate, arg_GDP`)
   {
    # Create a data frame using the arguments
    to_be_predicted <-
    data.frame(
    `Marital status = as.numeric(arg_Marital status)
    Application mode = as.numeric(arg_Application mode), 
    Application order = as.numeric(arg_Application order), 
    Daytime/evening attendance = as.numeric(arg_Daytime/evening attendance), 
    Previous qualification = as.numeric(arg_Previous qualification),
    Nacionality = as.numeric(arg_Nacionality), 
    Mother's qualification = as.numeric(arg_Mother's qualification), 
    Father's qualification = as.numeric(arg_Father's qualification), 
    Mother's occupation = as.numeric(arg_Mother's occupation), 
    Father's occupation = as.numeric(arg_Father's occupation), 
    Displaced = as.numeric(arg_Displaced),
    Educational special needs = as.numeric(arg_Educational special needs), 
    Tuition fees up to date = as.numeric(arg_Tuition fees up to date), 
    Gender = as.numericarg_(Gender),
    Scholarship holder = as.numeric(arg_Scholarship holder), 
    Age at enrollment, = as.numeric(arg_Age at enrollment), 
    International= as.numeric(arg_International),
    Curricular units 1st sem (credited) = as.numeric(arg_Curricular units 1st sem (credited)),
    Curricular units 1st sem (enrolled) = as.numeric(arg_Curricular units 1st sem (enrolled)) , 
    Curricular units 1st sem (evaluations) = as.numeric(arg_Curricular units 1st sem (evaluations)),
    Curricular units 1st sem (approved) = as.numericarg_(Curricular units 1st sem (approved)), 
    Curricular units 1st sem (grade) = as.numericarg_(Curricular units 1st sem (grade)),
    Curricular units 1st sem (without evaluations) = as.numeric(arg_Curricular units 1st sem (without evaluations)),
    Curricular units 2nd sem (credited) = as.numeric(arg_Curricular units 2nd sem (credited)),
    Curricular units 2nd sem (enrolled) = as.numeric(arg_Curricular units 2nd sem (enrolled)),
    Curricular units 2nd sem (evaluations) = as.numeric(arg_Curricular units 2nd sem (evaluations)),
    Curricular units 2nd sem (approved) = as.numeric(arg_Curricular units 2nd sem (approved)),
    Curricular units 2nd sem (grade) = as.numeric(arg_Curricular units 2nd sem (grade)),
    Curricular units 2nd sem (without evaluations) = as.numeric(arg_Curricular units 2nd sem (without evaluations)), 
    Unemployment rate = as.numeric(arg_Unemployment rate), 
    Inflation rate = as.numeric(arg_Inflation rate), 
    GDP = as.numeric(arg_GDP)`)
                 
    # Make a prediction based on the data frame
    predict(loaded_Target_model_lda, to_be_predicted)
  }

# STEP 12. Process a Plumber API ----
# This allows us to process a plumber API
api <- plumber::plumb("DescriptiveStatistics.R")

# STEP 13. Run the API on a specific port ----
# Specify a constant localhost port to use
api$run(host = "127.0.0.1", port = 5022)
