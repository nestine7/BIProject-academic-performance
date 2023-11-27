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

# 1. Measures of frequency ----
dataset_freq <- dataset$`Daytime/evening attendance`
cbind(frequency = table(dataset_freq),
      percentage = prop.table(table(dataset_freq)) * 100)

# 2. Measures of central tendency ----
dataset_mode <- names(table(dataset$`Daytime/evening attendance`))[
  which(table(dataset$`Daytime/evening attendance`) == max(table(dataset$`Daytime/evening attendance`)))
]
print(dataset_mode)

# 3. Measures of distribution/dispersion/spread/scatter/variability ----
summary(dataset)

# 4. Measures of Relationship ----
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

### STEP 21. Create a Correlation Plot ----
if (!is.element("corrplot", installed.packages()[, 1])) {
  install.packages("corrplot", dependencies = TRUE)
}
require("corrplot")
corrplot(cor(dataset[, 4]), method = "circle")

### STEP 23. Create Multivariate Box and Whisker Plots by Class ----
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

# DATASET 1 (Splitting the dataset): Dataset ----

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
