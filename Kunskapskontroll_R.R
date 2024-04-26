library(readxl)
library(caret)
library(Metrics)
library(car)

# Read data from Excel file
car_data <- read_excel("C:\\Users\\erikf\\ec\\R programmering\\Kunskapskontroll\\Data\\Blocket_all_data_klar.xlsx")

# EDA ---------------------------------------------------------------------


dim(car_data)
head(car_data)
str(car_data)
summary(car_data)

unique_Bränsle <- unique(car_data$Bränsle)
unique_Bränsle
Växellåda <- unique(car_data$Växellåda)
Växellåda
Biltyp <- unique(car_data$Biltyp)
Biltyp
Märke <- unique(car_data$Märke)
Märke
# inspecting classes of columns
column_classes <- sapply(car_data, class)
print(column_classes)               

# Check for missing values
missing_values <- colSums(is.na(car_data))
print(missing_values)

# Encoding ----------------------------------------------------------------


car_data$Bränsle <- as.factor(car_data$Bränsle)
car_data$Växellåda <- as.factor(car_data$Växellåda)
car_data$Biltyp <- as.factor(car_data$Biltyp)
car_data$Drivning <- as.factor(car_data$Drivning)
car_data$Märke <- as.factor(car_data$Märke)


column_classes <- sapply(car_data, class)
print(column_classes)
car_data

# Splitting ---------------------------------------------------------------

#Splitting the data to train, val, and test sets
spec = c(train = .6, validate = .2, test = .2)

set.seed(120)
g = sample(cut(
  seq(nrow(car_data)), 
  nrow(car_data)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(car_data, g)

car_data_train <- res$train
car_data_val <- res$validate
car_data_test <- res$test


# Linear Regression -------------------------------------------------------


lm_1 <- lm(Pris ~., data = car_data_train)
 
summary(lm_1)


par(mfrow = c(2, 2))
plot(lm_1)
#information on multicollinearity
vif(lm_1)

# Log Linear model --------------------------------------------------------
encoded_car_data_log <- car_data_train

# Apply the transformation (logarithm) to the dependent variable
encoded_car_data_log$Pris <- log(encoded_car_data_log$Pris)

# Fit the linear regression model using the transformed variables
Log_lm <- lm(Pris ~ ., data = encoded_car_data_log)
summary(Log_lm)



par(mfrow = c(2, 2))
plot(Log_lm)
#information on multicollinearity
vif(Log_lm) 

# Remove outliers ---------------------------------------------------------

indices_to_remove <- c(17, 555, 61)

# Remove the specified data points from car_data_train
car_data_train_filtered <- car_data_train[-indices_to_remove, ]

# Rerunning the models ----------------------------------------------------

# Linear Regression filtered-------------------------------------------------------


lm_3 <- lm(Pris ~., data = car_data_train_filtered)
 
summary(lm_3)


par(mfrow = c(2, 2))
plot(lm_3)
vif(lm_3)

# Log Linear model filtered--------------------------------------------------------
encoded_car_data_log <- car_data_train_filtered

# Apply the transformation (logarithm) to the dependent variable
encoded_car_data_log$Pris <- log(encoded_car_data_log$Pris)

# Fit the linear regression model
Log_lm2 <- lm(Pris ~ ., data = encoded_car_data_log)
summary(Log_lm2)


par(mfrow = c(2, 2))
plot(Log_lm2) 
#information on multicollinearity
vif(Log_lm2)


# Validation ---------------------------------------------------------
val_pred_m1 <- predict(lm_3, newdata = car_data_val)
val_pred_m2 <- exp(predict(Log_lm2, newdata = car_data_val))


results <- data.frame(
  Model = c("lm_3", "Log_lm2"),
  RMSE_val_data = c(rmse(car_data_val$Pris, val_pred_m1),
                    rmse(car_data_val$Pris, val_pred_m2)),
  Adj_R_squared = c(summary(lm_3)$adj.r.squared,
                    summary(Log_lm2)$adj.r.squared),
  BIC = c(BIC(lm_3), BIC(Log_lm2))
)  


results


# Checking for Correlation of error terms ---------------------------------


plot(residuals(Log_lm2))

# Perform Durbin-Watson test
#The test statistic ranges from 0 to 4,
#with values close to 2 indicating no autocorrelation.
durbinWatsonTest(Log_lm2)

# Testing Log_lm2 --------------------------------------------------------------------

# Predicting on the test data
test_pred <- exp(predict(Log_lm2, newdata = car_data_test))

# Calculate residuals for the test data
test_residuals <- car_data_test$Pris - test_pred

# Calculate the number of observations and predictors for the model
n_test <- nrow(car_data_test)
p_test <- length(coef(Log_lm2)) - 1  # Number of predictors (excluding intercept)

# Calculate the adjusted R^2 for the test set
test_adj_rsq <- 1 - (sum(test_residuals^2) / (n_test - p_test - 1)) / 
  (sum((car_data_test$Pris - mean(car_data_test$Pris))^2) / (n_test - 1))



# Calculate BIC for the test set
test_model <- lm(formula(Log_lm2), data = car_data_test)
test_BIC <- BIC(test_model)


# Creating a data frame to store the results
results_test <- data.frame(
  Model = "Log_lm2",
  RMSE_test_data = rmse(car_data_test$Pris, test_pred),
  Adj_R_squared = test_adj_rsq,
  BIC = test_BIC
)


results_test



#One observation 
test_observation_1 <- car_data_test[100, ]  

#Prediction using the normal scale
prediction_log_lm <- exp(predict(Log_lm, newdata = test_observation_1))

# Print the predictions
print(test_observation_1)
print(prediction_log_lm)

prediction_intervals <- exp(predict(Log_lm2, newdata = test_observation_1, interval = "prediction", level = 0.95))
prediction_intervals

#One observation 
test_observation_2 <- car_data_test[200, ]  

#Prediction using the normal scale
prediction_log_lm <- exp(predict(Log_lm, newdata = test_observation_2))

# Print the predictions
print(test_observation_2)
print(prediction_log_lm)

prediction_intervals <- exp(predict(Log_lm2, newdata = test_observation_2, interval = "prediction", level = 0.95))
prediction_intervals
