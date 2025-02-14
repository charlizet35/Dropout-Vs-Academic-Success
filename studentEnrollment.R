library(randomForest)
library(ggplot2)
library(GGally)
library(neuralnet)
library(NeuralNetTools)
library(nnet)
library(knitr)


set.seed(28)

students = read.csv('C:/spring_25/student_enrollment_project/students.csv')

head(students)
dim(students)
str(students)

students$Target = ifelse(students$Target == "Dropout", "Dropout", "Non-Dropout")
students$Target = as.factor(students$Target)

#Below code is for generating pairplots of potential features to remove due to collinearity
# cirricular1stSem = students[ , grep("^Curricular.units.1st.sem", names(students))]
# colnames(cirricular1stSem) = gsub("^Curricular.units.1st.sem", "", colnames(cirricular1stSem))

# ggpairs(cirricular1stSem)

# parents = students[ , c("Mother.s.qualification", "Father.s.qualification", "Mother.s.occupation", "Father.s.occupation")]
# ggpairs(parents)

students = students[, c(
  "Marital.status",
  "Application.mode",
  "Application.order",
  "Course",
  "Daytime.evening.attendance",
  "Previous.qualification",
  "Mother.s.qualification",
  "Mother.s.occupation",
  "Displaced",
  "Debtor",
  "Tuition.fees.up.to.date",
  "Gender",
  "Scholarship.holder",
  "Age.at.enrollment",
  "Curricular.units.1st.sem..enrolled.",
  "Curricular.units.2nd.sem..enrolled.",
  "Target"
)]


# Random Forest

prelim.rf = randomForest(Target ~ ., data = students, mtry = sqrt(16), importance = TRUE)

# Plot important predictors
varImpPlot(prelim.rf, sort = TRUE, main = NA)
p = ncol(students) - 1

test_errors = NULL
for (i in 0:9) {
  train_index = sample(1:nrow(students), size = 0.8 * nrow(students))
  train_data = students[train_index, ]
  test_data = students[-train_index, ]
  
  rf_model = randomForest(Target~., data = train_data, mtry = sqrt(p), importance = TRUE)
  
  predictions = predict(rf_model, test_data, type="class")
  
  MSE = mean((as.numeric(test_data$Target) - as.numeric(predictions))^2)
  MSEs[i] = MSE
  
  test_error = mean(predictions != test_data$Target)
  test_errors[i] = test_error
}
rf_mean_test_error = mean(rf_test_errors)

students_rf = students[, c(
  "Application.mode",
  "Course",
  "Mother.s.occupation",
  "Debtor",
  "Tuition.fees.up.to.date",
  "Scholarship.holder",
  "Age.at.enrollment",
  "Curricular.units.1st.sem..enrolled.",
  "Curricular.units.2nd.sem..enrolled.",
  "Target"
)]

# Neural Network

library(neuralnet)
library(NeuralNetTools)
library(nnet)

test_errors = NULL
for (i in 0:9) {
  # split the data (80% train, 20% test)
  train_index = sample(1:nrow(students), size = 0.8 * nrow(students))
  train_data = students[train_index, ]
  test_data = students[-train_index, ]
  
  
  model = nnet(Target ~.,
                data = train_data, 
                size = 4, # hidden layers
                rang = 0.01, 
                decay = 5e-2,
                maxit = 1000)
  
  
  pred = predict(model, test_data, type = "class")
  
  test_error = mean(pred != test_data$Target)
  test_errors[i] = test_error
}
nn_mean_test_error = mean(test_errors)

nn_confusion_matrix = table(Actual = test_data$Target, Predicted = pred)

plotnet(model, alpha = 0.8)

# Put the results in a table

library(knitr)

results = data.frame(
  Model = c("Random Forest", "Neural Network"),
  TestError = c(nn_mean_test_error, rf_test_error)
)

# Print results to html table (screenshot taken of html to be placed in report)
kable(
  results,
  col.names = c("Model", "Test Error"),
  format = "html",
  digits = 4
)


