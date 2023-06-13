#loading all the required packages
install.packages('readr')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('caret')
library(readr)
library(dplyr)
library(ggplot2)
library(caret)


#reading the csv file life expectency data
data <- read_csv("C:\\Users\\ganes\\Documents\\ISL\\ISL_PROJ\\dataset_1\\Life Expectancy Data.csv")
data

#display top rows
head(data)

#removing NA values
sum(is.na(data$"Life expectancy"))
data <- na.omit(data)

#split the data into training and testing sets using a 70/30 split
set.seed(531)
trainIndex <- createDataPartition(data$"Life expectancy", p = .7, list = FALSE)
training <- data[trainIndex,]
testing <- data[-trainIndex,]
training
testing

#a)Linear Regression

#simple linear regression model using the lm() function
model_lm <- lm(`Life expectancy` ~ `Adult Mortality` + Alcohol + BMI + `HIV/AIDS` + `Income composition of resources` + Schooling + Status, data = training)
summary(model_lm)
#output of the summary provides us with information on the coefficients
model_lm


#visualize the model using a scatter plot of the predicted values against the actual values:
predicted_lm <- predict(model_lm, newdata = testing)
ggplot(testing, aes(x = `Life expectancy`, y = predicted_lm)) + geom_point() + geom_abline(intercept = 0, slope = 1, color = "red")

#This plot shows how well the predicted values match up with the actual values. A perfect model would have all the points lying on the red line.


#b)Polynomial Regression
#we can try a polynomial regression model using the poly() function:
#The poly() function allows us to include polynomial terms in our model, which can help capture more complex relationships between the features and the response.
model_poly <- lm(`Life expectancy` ~ poly(`Adult Mortality`, 2) + poly(Alcohol, 2) + poly(BMI, 2) + poly(`HIV/AIDS`, 2) + poly(`Income composition of resources`, 2) + poly(Schooling, 2) + Status, data = training)

summary(model_poly)


#visualize the model using a scatter plot of the predicted values against the actual values:
predicted_poly <- predict(model_poly, newdata = testing)
ggplot(testing, aes(x = `Life expectancy`, y = predicted_poly)) + geom_point() + geom_abline(intercept = 0, slope = 1, color = "yellow")


#c)Multilinear Regression
model_multi <- lm(`Life expectancy` ~ `Adult Mortality` + Alcohol + BMI + `HIV/AIDS` + `Income composition of resources` + Schooling + Status, data = training)

summary(model_multi)


predicted_multi <- predict(model_multi, newdata = testing)
ggplot(testing, aes(x = `Life expectancy`, y = predicted_multi)) + geom_point() + geom_abline(intercept = 0, slope = 1, color = "green")


#d) Natural Cubic Spline
install.packages("splines")
install.packages("dplyr")
library(splines)
library(dplyr)


model <- lm(`Life expectancy` ~ ns(`Life expectancy`, df = 4), data = data)
summary(model)

plot(data$`Life expectancy`, data$`Life expectancy`, xlab = "Life Expectancy", ylab = "Fitted Values")
lines(data$`Life expectancy`, predict(model), col = "red", lwd = 2)



#------------------------------------------------------------------------------------------------------



#2

install.packages("caret")
install.packages("MASS")
# Load the necessary libraries and read in the data
library(caret)
library(MASS)
wine <- read.csv("C:\\Users\\ganes\\Documents\\ISL\\ISL_PROJ\\dataset_2\\winequality-red.csv")

head(wine)

# Split the data into training and testing sets
set.seed(531)
trainIndex1 <- createDataPartition(wine$quality, p = .7, list = FALSE)
train1 <- wine[trainIndex1, ]
test1 <- wine[-trainIndex1, ]

# Forward Stepwise Selection
fit.fs <- lm(quality ~ 1, data = train1)
for (i in 2:ncol(train1)) {
  fit.temp <- lm(quality ~ ., data = train1[, c(names(train1)[i], names(fit.fs$model))])
  if (AIC(fit.temp) < AIC(fit.fs)) {
    fit.fs <- fit.temp
  } else {
    break
  }
}
summary(fit.fs)

# Backward Stepwise Selection
fit.bs <- lm(quality ~ ., data = train1)
while (length(coefficients(fit.bs)) > 1) {
  pvals <- summary(fit.bs)$coefficients[, 4]
  maxp <- max(pvals[-1])
  if (maxp > 0.05) {
    exclude <- names(coefficients(fit.bs))[pvals == maxp]
    formula <- as.formula(paste("quality ~", paste(setdiff(names(train1), exclude), collapse = "+")))
    fit.bs <- lm(formula, data = train1)
  } else {
    break
  }
}
summary(fit.bs)


# Forward Stepwise Selection
fit <- lm(quality ~ ., data = train1)
forward_fit <- step(fit, direction = "forward")

# Backward Stepwise Selection
fit <- lm(quality ~ ., data = train1)
backward_fit <- step(fit, direction = "backward")

# Evaluate on test set
forward_pred <- predict(forward_fit, newdata = test1)
forward_rmse <- sqrt(mean((test1$quality - forward_pred)^2))
backward_pred <- predict(backward_fit, newdata = test1)
backward_rmse <- sqrt(mean((test1$quality - backward_pred)^2))

forward_pred
forward_rmse
backward_pred
backward_rmse

library(leaps)
regfit.fwd <- regsubsets(quality ~ ., data = train1, nvmax = 14, method = "forward")
summary(regfit.fwd)          


regfit.bwd<- regsubsets(quality ~ ., data = train1, nvmax = 14, method = "backward")
summary(regfit.bwd)


reg.summaryfwd <- summary(regfit.fwd)
par(mfrow = c(1, 2))
plot(reg.summaryfwd $rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summaryfwd $adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

reg.summarybwd <- summary(regfit.bwd)
par(mfrow = c(1, 2))
plot(reg.summarybwd $rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summarybwd $adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

plot(reg.summaryfwd$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")
points(7, reg.summaryfwd$adjr2[7], col = "red", cex = 2, 
       pch = 20)

plot(regfit.full, scale = "r2")

#pcr
install.packages("pls")
library(pls)

pcr_model <- pcr(quality ~ ., data = train1, scale = TRUE, validation = "CV")
summary(pcr_model)

# Plot the components relative to their fit target
plot(1:10, pcr_model$validation$R2, type = "b", xlab = "Number of Components", ylab = "Adjusted R-Squared")

#pcr2
library(pls)
set.seed(531)
pcr.fit <- pcr(quality ~ ., data = train1, scale = TRUE,
               validation = "CV")

summary(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP")

#---------------------------------------------------------------------




#3


data5 <- read.csv("C:\\Users\\ganes\\Documents\\ISL\\ISL_PROJ\\dataset_3\\data.csv")
data5



bc_data <- data5 %>%
  mutate(diagnosis_bin = if_else(diagnosis == "B", 0, 1)) %>%
  select(-id, -diagnosis)

# Split the data into training and testing datasets using a 70/30 split ratio
set.seed(531)
train_index5 <- sample(nrow(bc_data), nrow(bc_data)*0.7)
train_data5<- bc_data[train_index5, ]
test_data5 <- bc_data[-train_index5, ]

train_data5


# Train the logistic regression model
logit_model <- glm(diagnosis_bin ~ radius_mean + texture_mean + perimeter_mean + area_mean + smoothness_mean + compactness_mean + concavity_mean + concave.points_mean + symmetry_mean + fractal_dimension_mean, data = train_data5, family = "binomial")

# Make predictions on the test data
logit_pred <- predict(logit_model, newdata = test_data5, type = "response")

# Convert predictions to diagnoses (malignant or benign)
logit_diag <- ifelse(logit_pred > 0.5, "M", "B")


length(logit_diag)
length(test_data5$diagnosis_bin)
length(bc_data$diagnosis_bin)
bc_data

test_data5$diagnosis
# Generate the confusion matrix
logit_cm <- table(logit_diag, test_data5$diagnosis_bin)
logit_cm



summary(logit_cm)



#LDA

library(MASS)
lda_model <- lda(diagnosis_bin ~ radius_mean + texture_mean + perimeter_mean + area_mean + smoothness_mean + compactness_mean + concavity_mean + concave.points_mean + symmetry_mean + fractal_dimension_mean, data = train_data5)

# Make predictions on the test data
lda_pred <- predict(lda_model, newdata = test_data5)

# Convert predictions to diagnoses (malignant or benign)
lda_diag <- lda_pred$class

# Generate the confusion matrix
lda_cm <- table(lda_diag, test_data5$diagnosis_bin)
lda_cm
summary(lda_cm)


#2 tree classifier
#A)
# Load required packages

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Fit the decision tree model using the training data
tree_model <- rpart(diagnosis_bin ~ ., data = train_data5, method = "class")

# Print the summary of the tree model
summary(tree_model)

#---------------------------------------------

#b)
# Plot the decision tree
rpart.plot(tree_model)



#b)other way
install.packages("tree")
library(tree)
# Fit a decision tree classifier
library(tree)
my_tree <- tree(diagnosis_bin ~ radius_mean + perimeter_mean + concavity_mean, data = bc_data)

# Print the tree
print(my_tree)
plot(my_tree)
text(my_tree, pretty = 0)

#-----------------------------------------------------------

#svm

install.packages("e1071")




library(e1071)



# Select two classes to classify (e.g. M and B)
svm_data <- subset(data5, diagnosis %in% c("M", "B"))

# Convert diagnosis to a binary factor (M = 1, B = -1)
svm_data$diagnosis_bin <- ifelse(svm_data$diagnosis == "M", 1, -1)
svm_data$diagnosis_bin <- as.factor(svm_data$diagnosis_bin)

# Split data into training and test sets
set.seed(531)
train_index_svm1 <- sample(1:nrow(svm_data), size = round(0.7 * nrow(svm_data)))
train_data_svm1 <- svm_data[train_index_svm1, ]
test_data_svm1 <- svm_data[-train_index_svm1, ]


summary(train_data_svm1)

#sum(is.na(train_data_svm1))
#train_data_svm1 <- na.omit(train_data_svm1)



dim(train_data_svm1)
sapply(train_data_svm1, function(x) length(unique(x)))
train_data_svm1 <- train_data_svm1[, -33]
dim(train_data_svm1)
sapply(train_data_svm1, function(x) length(unique(x)))



dim(test_data_svm1)
sapply(test_data_svm1, function(x) length(unique(x)))
test_data_svm1 <- test_data_svm1[, -33]
dim(train_data_svm1)
sapply(train_data_svm1, function(x) length(unique(x)))


# Train the SVM using a radial basis kernel function
svm_model <- svm(diagnosis_bin ~ ., data = train_data_svm1, kernel = "radial")

# Make predictions on the test data
svm_pred <- predict(svm_model, newdata = test_data_svm1)

# Calculate accuracy and confusion matrix
svm_accuracy <- mean(svm_pred == test_data_svm1$diagnosis_bin)
svm_cm <- table(svm_pred, test_data_svm1$diagnosis_bin)
svm_cm

summary(svm_model)

train_data_svm1
dat <- data.frame(x = train_data_svm1$texture_mean, y = train_data_svm1$diagnosis_bin)
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = TRUE)
plot(svmfit, dat)

tune.out <- tune(svm, y ~ ., data = dat, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

bestmod <- tune.out$best.model
summary(bestmod)
