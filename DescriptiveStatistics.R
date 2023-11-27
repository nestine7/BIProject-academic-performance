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

# 1. Measures of frequency
dataset_freq <- dataset$`Daytime/evening attendance`
cbind(frequency = table(dataset_freq),
      percentage = prop.table(table(dataset_freq)) * 100)

# 2. Measures of central tendency
dataset_mode <- names(table(dataset$`Daytime/evening attendance`))[
  which(table(dataset$`Daytime/evening attendance`) == max(table(dataset$`Daytime/evening attendance`)))
]
print(dataset_mode)

# 3. Measures of distribution/dispersion/spread/scatter/variability
summary(dataset)

## Measures of Relationship ----
dataset_cov <- cov(dataset[, 4])
View(dataset_cov)

#Perform ANOVA on the “dataset” dataset ----
dataset_one_way_anova <- aov(GDP ~ `Curricular units 1st sem (grade)` + `Curricular units 2nd sem (grade)`, data = dataset)
summary(dataset_one_way_anova)
