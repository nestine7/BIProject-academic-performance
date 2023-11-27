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
require("caret")
featurePlot(x = dataset[, 1:4], y = dataset[, 5], plot = "box")
