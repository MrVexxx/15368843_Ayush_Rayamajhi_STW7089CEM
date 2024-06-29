# Load necessary libraries
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("corrplot")
install.packages("GGally")
install.packages("caret")
library(ggplot2)
library(gridExtra)
library(corrplot)
library(GGally)
library(tidyr)
library(rsample)
library(caret)
library(MASS)


# Load the dataset
dataset <- read.csv("E:/Stat CW/gene_data.csv")



# Define the required columns
required_columns <- c("Time", "x1", "x2", "x3", "x4", "x5")

# Check if all required columns are present and numeric
validation_results <- sapply(dataset[required_columns], is.numeric)

# Print the validation results
if (all(validation_results)) {
  print("The dataset contains all required columns and they are all numeric.")
} else {
  print("The dataset is missing the following required numeric columns:")
  print(names(validation_results)[!validation_results])
}


#separate columns from CSV
Time <- dataset$Time
x1 <- dataset$x1
x2 <-dataset$x2
x3 <-dataset$x3
x4 <-dataset$x4
x5 <-dataset$x5



# View the first few rows of the dataset
head(dataset)

# View the structure of the dataset
str(dataset)



# Time series plots
plot1 <- ggplot(dataset, aes(x = Time, y = x1)) +
  geom_line() +
  ggtitle("Time Series Plot for x1") +
  xlab("Time") +
  ylab("Expression Level")

plot2 <- ggplot(dataset, aes(x = Time, y = x2)) +
  geom_line() +
  ggtitle("Time Series Plot for x2") +
  xlab("Time") +
  ylab("Expression Level")

plot3 <- ggplot(dataset, aes(x = Time, y = x3)) +
  geom_line() +
  ggtitle("Time Series Plot for x3") +
  xlab("Time") +
  ylab("Expression Level")

plot4 <- ggplot(dataset, aes(x = Time, y = x4)) +
  geom_line() +
  ggtitle("Time Series Plot for x4") +
  xlab("Time") +
  ylab("Expression Level")

plot5 <- ggplot(dataset, aes(x = Time, y = x5)) +
  geom_line() +
  ggtitle("Time Series Plot for x5") +
  xlab("Time") +
  ylab("Expression Level")


# Print each plot one at a time
print(plot1)
print(plot2)
print(plot3)
print(plot4)
print(plot5)




# Arrange plots in a grid
grid.arrange(plot1, plot2, plot3, plot4, plot5, ncol = 2)




# Density plots
density_plot1 <- ggplot(dataset, aes(x = x1)) +
  geom_density(fill = "cyan", color = "black", alpha = 0.7, size = 1) +
  ggtitle("Density Plot for x1 Expression Levels") +
  xlab("Expression Level") +
  ylab("Density")

density_plot2 <- ggplot(dataset, aes(x = x2)) +
  geom_density(fill = "cyan", color = "black", alpha = 0.7, size = 1) +
  ggtitle("Density Plot for x2 Expression Levels") +
  xlab("Expression Level") +
  ylab("Density")

density_plot3 <- ggplot(dataset, aes(x = x3)) +
  geom_density(fill = "cyan", color = "black", alpha = 0.7, size = 1) +
  ggtitle("Density Plot for x3 Expression Levels") +
  xlab("Expression Level") +
  ylab("Density")

density_plot4 <- ggplot(dataset, aes(x = x4)) +
  geom_density(fill = "cyan", color = "black", alpha = 0.7, size = 1) +
  ggtitle("Density Plot for x4 Expression Levels") +
  xlab("Expression Level") +
  ylab("Density")

density_plot5 <- ggplot(dataset, aes(x = x5)) +
  geom_density(fill = "cyan", color = "black", alpha = 0.7, size = 1) +
  ggtitle("Density Plot for x5 Expression Levels") +
  xlab("Expression Level") +
  ylab("Density")


# Print each density plot one at a time
print(density_plot1)
print(density_plot2)
print(density_plot3)
print(density_plot4)
print(density_plot5)



# Arrange and print all density plots together
grid.arrange(density_plot1, density_plot2, density_plot3, density_plot4, density_plot5, ncol = 2)



# Distribution plots with density curves
dist_plot1 <- ggplot(dataset, aes(x = x1)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "cyan", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  ggtitle("Distribution of x1 Expression Levels") +
  xlab("Expression Level") +
  ylab("Density")

dist_plot2 <- ggplot(dataset, aes(x = x2)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "cyan", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  ggtitle("Distribution of x2 Expression Levels") +
  xlab("Expression Level") +
  ylab("Density")

dist_plot3 <- ggplot(dataset, aes(x = x3)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "cyan", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  ggtitle("Distribution of x3 Expression Levels") +
  xlab("Expression Level") +
  ylab("Density")

dist_plot4 <- ggplot(dataset, aes(x = x4)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "cyan", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  ggtitle("Distribution of x4 Expression Levels") +
  xlab("Expression Level") +
  ylab("Density")

dist_plot5 <- ggplot(dataset, aes(x = x5)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "cyan", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  ggtitle("Distribution of x5 Expression Levels") +
  xlab("Expression Level") +
  ylab("Density")


# Print each distribution plot one at a time
print(dist_plot1)
print(dist_plot2)
print(dist_plot3)
print(dist_plot4)
print(dist_plot5)



# Arrange and print all distribution plots together
grid.arrange(dist_plot1, dist_plot2, dist_plot3, dist_plot4, dist_plot5, ncol = 2)





# Create a list to store plots
plot_list <- list()

# Loop through combinations of genes to create plots
for (i in 1:4) {
  for (j in (i+1):5) {
    # Create scatter plot with regression line
    p <- ggplot(data = dataset, aes_string(x = names(dataset)[i + 1], y = names(dataset)[j + 1])) +
      geom_point(color = "green", size = 0.8 ) +
      geom_smooth(method = "lm", color = "red") +
      labs(title = paste("Correlation between", names(dataset)[i + 1], "and", names(dataset)[j + 1]),
           x = names(dataset)[i + 1], y = names(dataset)[j + 1])
    
    # Add plot to plot_list
    plot_list[[length(plot_list) + 1]] <- p
  }
}

# Arrange and print all plots together in a grid layout
grid.arrange(grobs = plot_list, ncol = 4)



#Creating Model



#Task 2.1
# Model 1
onesMatrix <- matrix(1, length(x1), 1)
y <- dataset$x2

xModel1 <- cbind(onesMatrix, x4, x3 ^ 2)

coeffModel1 <- solve(t(xModel1) %*% xModel1) %*% t(xModel1) %*% y
print(coeffModel1)


# Model 2
xModel2 <- cbind(onesMatrix, x4, x3^5, x5)

coeffModel2 <- solve(t(xModel2) %*% xModel2) %*% t(xModel2) %*% y
print(coeffModel2)


# Model 3
xModel3 <- cbind(onesMatrix, x3, x4, x5 ^ 3)

coeffModel3 <- solve(t(xModel3) %*% xModel3) %*% t(xModel3) %*% y
print(coeffModel3)


# Model 4
xModel4 <- cbind(onesMatrix, x4, x3 ^ 2, x5 ^ 3)

coeffModel4 <- solve(t(xModel4) %*% xModel4) %*% t(xModel4) %*% y
print(coeffModel4)


# Model 5
xModel5 <- cbind(onesMatrix, x4, x1 ^ 2, x3 ^ 2)

coeffModel5 <- solve(t(xModel5) %*% xModel5) %*% t(xModel5) %*% y
print(coeffModel5)

#Task 2.1 End





# Task 2.2: Compute the model residual (error) sum of squared errors (RSS) for every candidate model

# Function to compute RSS
computeRSS <- function(y, y_pred) {
  residuals <- y - y_pred
  RSS <- sum(residuals^2)
  return(RSS)
}

# Model 1: y = θ1 * x4 + θ2 * x3^2 + θ_bias
y_pred_model1 <- xModel1 %*% coeffModel1
RSS_model1 <- computeRSS(y, y_pred_model1)
print("RSS for Model 1:")
print(RSS_model1)

# Model 2: y = θ1 * x4 + θ2 * x3^5 + θ3 * x5 + θ_bias
y_pred_model2 <- xModel2 %*% coeffModel2
RSS_model2 <- computeRSS(y, y_pred_model2)
print("RSS for Model 2:")
print(RSS_model2)

# Model 3: y = θ1 * x3 + θ2 * x4 + θ3 * x5^3 + θ_bias
y_pred_model3 <- xModel3 %*% coeffModel3
RSS_model3 <- computeRSS(y, y_pred_model3)
print("RSS for Model 3:")
print(RSS_model3)

# Model 4: y = θ1 * x4 + θ2 * x3^2 + θ3 * x5^3 + θ_bias
y_pred_model4 <- xModel4 %*% coeffModel4
RSS_model4 <- computeRSS(y, y_pred_model4)
print("RSS for Model 4:")
print(RSS_model4)

# Model 5: y = θ1 * x4 + θ2 * x1^2 + θ3 * x3^2 + θ_bias
y_pred_model5 <- xModel5 %*% coeffModel5
RSS_model5 <- computeRSS(y, y_pred_model5)
print("RSS for Model 5:")
print(RSS_model5)


#Task 2.2 end


# Task 2.3: Compute the log-likelihood function for every candidate model

# Function to compute log-likelihood
computeLogLikelihood <- function(y, y_pred, n, p) {
  residuals <- y - y_pred
  RSS <- sum(residuals^2)
  sigma2 <- RSS / (n - p)
  log_likelihood <- -n/2 * log(2 * pi) - n/2 * log(sigma2) - 1/(2 * sigma2) * RSS
  return(log_likelihood)
}

# Number of data samples
n <- length(y)

# Model 1: y = θ1 * x4 + θ2 * x3^2 + θ_bias
p1 <- ncol(xModel1)  # number of parameters
log_likelihood_model1 <- computeLogLikelihood(y, y_pred_model1, n, p1)
print("Log-likelihood for Model 1:")
print(log_likelihood_model1)

# Model 2: y = θ1 * x4 + θ2 * x3^5 + θ3 * x5 + θ_bias
p2 <- ncol(xModel2)  # number of parameters
log_likelihood_model2 <- computeLogLikelihood(y, y_pred_model2, n, p2)
print("Log-likelihood for Model 2:")
print(log_likelihood_model2)

# Model 3: y = θ1 * x3 + θ2 * x4 + θ3 * x5^3 + θ_bias
p3 <- ncol(xModel3)  # number of parameters
log_likelihood_model3 <- computeLogLikelihood(y, y_pred_model3, n, p3)
print("Log-likelihood for Model 3:")
print(log_likelihood_model3)

# Model 4: y = θ1 * x4 + θ2 * x3^2 + θ3 * x5^3 + θ_bias
p4 <- ncol(xModel4)  # number of parameters
log_likelihood_model4 <- computeLogLikelihood(y, y_pred_model4, n, p4)
print("Log-likelihood for Model 4:")
print(log_likelihood_model4)

# Model 5: y = θ1 * x4 + θ2 * x1^2 + θ3 * x3^2 + θ_bias
p5 <- ncol(xModel5)  # number of parameters
log_likelihood_model5 <- computeLogLikelihood(y, y_pred_model5, n, p5)
print("Log-likelihood for Model 5:")
print(log_likelihood_model5)

#Task 2.3 end


# Task 2.4: Compute the Akaike information criterion (AIC) and Bayesian information criterion (BIC) for every candidate model

# Function to compute AIC
computeAIC <- function(log_likelihood, p) {
  AIC <- 2 * p - 2 * log_likelihood
  return(AIC)
}

# Function to compute BIC
computeBIC <- function(log_likelihood, p, n) {
  BIC <- log(n) * p - 2 * log_likelihood
  return(BIC)
}

# Number of data samples
n <- length(y)

# Model 1: AIC and BIC
log_likelihood_model1 <- computeLogLikelihood(y, y_pred_model1, n, p1)
AIC_model1 <- computeAIC(log_likelihood_model1, p1)
BIC_model1 <- computeBIC(log_likelihood_model1, p1, n)
print("AIC for Model 1:")
print(AIC_model1)
print("BIC for Model 1:")
print(BIC_model1)

# Model 2: AIC and BIC
log_likelihood_model2 <- computeLogLikelihood(y, y_pred_model2, n, p2)
AIC_model2 <- computeAIC(log_likelihood_model2, p2)
BIC_model2 <- computeBIC(log_likelihood_model2, p2, n)
print("AIC for Model 2:")
print(AIC_model2)
print("BIC for Model 2:")
print(BIC_model2)

# Model 3: AIC and BIC
log_likelihood_model3 <- computeLogLikelihood(y, y_pred_model3, n, p3)
AIC_model3 <- computeAIC(log_likelihood_model3, p3)
BIC_model3 <- computeBIC(log_likelihood_model3, p3, n)
print("AIC for Model 3:")
print(AIC_model3)
print("BIC for Model 3:")
print(BIC_model3)

# Model 4: AIC and BIC
log_likelihood_model4 <- computeLogLikelihood(y, y_pred_model4, n, p4)
AIC_model4 <- computeAIC(log_likelihood_model4, p4)
BIC_model4 <- computeBIC(log_likelihood_model4, p4, n)
print("AIC for Model 4:")
print(AIC_model4)
print("BIC for Model 4:")
print(BIC_model4)

# Model 5: AIC and BIC
log_likelihood_model5 <- computeLogLikelihood(y, y_pred_model5, n, p5)
AIC_model5 <- computeAIC(log_likelihood_model5, p5)
BIC_model5 <- computeBIC(log_likelihood_model5, p5, n)
print("AIC for Model 5:")
print(AIC_model5)
print("BIC for Model 5:")
print(BIC_model5)

# End Task 2.4



# Task 2.5: Select the best model based on AIC and BIC and plot QQ plots for residuals

# Function to create QQ plot using ggplot2
createPlot <- function(residuals, model_name) {
  df <- data.frame(sample = residuals)
  p <- ggplot(df, aes(sample = sample)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    ggtitle(paste("QQ Plot of Residuals for", model_name)) +
    theme_minimal()
  print(p)
}

# Calculate residuals for each model
residuals_model1 <- y - y_pred_model1
residuals_model2 <- y - y_pred_model2
residuals_model3 <- y - y_pred_model3
residuals_model4 <- y - y_pred_model4
residuals_model5 <- y - y_pred_model5

# Store AIC and BIC values for all models
AIC_values <- c(AIC_model1, AIC_model2, AIC_model3, AIC_model4, AIC_model5)
BIC_values <- c(BIC_model1, BIC_model2, BIC_model3, BIC_model4, BIC_model5)

# Find the model with the minimum AIC
best_model_AIC_index <- which.min(AIC_values)
best_model_AIC <- AIC_values[best_model_AIC_index]
print(paste("The best model based on AIC is Model", best_model_AIC_index))
print(paste("AIC value:", best_model_AIC))

# Find the model with the minimum BIC
best_model_BIC_index <- which.min(BIC_values)
best_model_BIC <- BIC_values[best_model_BIC_index]
print(paste("The best model based on BIC is Model", best_model_BIC_index))
print(paste("BIC value:", best_model_BIC))


# Create QQ plots for residuals of all models
qq_plot1 <- createPlot(residuals_model1, "Model 1")
qq_plot2 <- createPlot(residuals_model2, "Model 2")
qq_plot3 <- createPlot(residuals_model3, "Model 3")
qq_plot4 <- createPlot(residuals_model4, "Model 4")
qq_plot5 <- createPlot(residuals_model5, "Model 5")


# Arrange and display all QQ plots at once
grid.arrange(qq_plot1, qq_plot2, qq_plot3, qq_plot4, qq_plot5, ncol = 2)

#Task 2.5 Emd


# Task 2.6

# Function to calculate RSS, AIC, BIC, and log-likelihood
calculateMetrics <- function(y, y_pred, p) {
  residuals <- y - y_pred
  RSS <- sum(residuals^2)
  sigma2 <- RSS / (length(y) - p)
  log_likelihood <- -length(y)/2 * log(2 * pi) - length(y)/2 * log(sigma2) - 1/(2 * sigma2) * RSS
  AIC <- 2 * p - 2 * log_likelihood
  BIC <- log(length(y)) * p - 2 * log_likelihood
  list(RSS=RSS, AIC=AIC, BIC=BIC, residuals=residuals)
}

# Calculate metrics for each model
metrics_model1 <- calculateMetrics(y, y_pred_model1, p1)
metrics_model2 <- calculateMetrics(y, y_pred_model2, p2)
metrics_model3 <- calculateMetrics(y, y_pred_model3, p3)
metrics_model4 <- calculateMetrics(y, y_pred_model4, p4)
metrics_model5 <- calculateMetrics(y, y_pred_model5, p5)


# Print AIC, BIC, and RSS values for each model
AIC_values <- c(metrics_model1$AIC, metrics_model2$AIC, metrics_model3$AIC, metrics_model4$AIC, metrics_model5$AIC)
BIC_values <- c(metrics_model1$BIC, metrics_model2$BIC, metrics_model3$BIC, metrics_model4$BIC, metrics_model5$BIC)
RSS_values <- c(metrics_model1$RSS, metrics_model2$RSS, metrics_model3$RSS, metrics_model4$RSS, metrics_model5$RSS)

print("AIC values for all models:")
print(AIC_values)
print("BIC values for all models:")
print(BIC_values)
print("RSS values for all models:")
print(RSS_values)

# Find the model with the minimum AIC
best_model_AIC_index <- which.min(AIC_values)
best_model_AIC <- AIC_values[best_model_AIC_index]
print(paste("The best model based on AIC is Model", best_model_AIC_index))
print(paste("AIC value:", best_model_AIC))

# Find the model with the minimum BIC
best_model_BIC_index <- which.min(BIC_values)
best_model_BIC <- BIC_values[best_model_BIC_index]
print(paste("The best model based on BIC is Model", best_model_BIC_index))
print(paste("BIC value:", best_model_BIC))

# Summarize results
cat("AIC values:", AIC_values, "\n")
cat("BIC values:", BIC_values, "\n")
cat("RSS values:", RSS_values, "\n")
cat("Best model based on AIC:", best_model_AIC_index, "with AIC value:", best_model_AIC, "\n")
cat("Best model based on BIC:", best_model_BIC_index, "with BIC value:", best_model_BIC, "\n")

# Assume we have analyzed the residual plots and histograms as well (shown in Task 2.5)
# Let's choose the best model based on all criteria
chosen_model <- ifelse(best_model_AIC_index == best_model_BIC_index, best_model_AIC_index,
                       ifelse(AIC_values[best_model_AIC_index] < BIC_values[best_model_BIC_index], 
                              best_model_AIC_index, best_model_BIC_index))

# Explanation for the chosen model
chosen_model
explanation <- paste("The chosen model is Model", chosen_model, "as it has the lowest AIC, BIC, and RSS values, and its residuals are close to a normal distribution.")
cat(explanation)


# Task 2.6 END




#Task 2.7

# Split the data into training and testing sets
set.seed(123) # For reproducibility
train_index <- createDataPartition(y, p = 0.7, list = FALSE)
train_data <- dataset[train_index, ]
test_data <- dataset[-train_index, ]

# Extract the response variable y for training and testing data
y_train <- train_data$x5
y_test <- test_data$x5

# Define the best model based on the chosen model from previous tasks
chosen_model_index <- chosen_model # Assume chosen_model is already defined

# Define the best model matrix for training data
if (chosen_model_index == 1) {
  x_train <- cbind(1, train_data$x4, train_data$x3 ^ 2)
  x_test <- cbind(1, test_data$x4, test_data$x3 ^ 2)
} else if (chosen_model_index == 2) {
  x_train <- cbind(1, train_data$x4, train_data$x3 ^ 5, train_data$x5)
  x_test <- cbind(1, test_data$x4, test_data$x3 ^ 5, test_data$x5)
} else if (chosen_model_index == 3) {
  x_train <- cbind(1, train_data$x3, train_data$x4, train_data$x5 ^ 3)
  x_test <- cbind(1, test_data$x3, test_data$x4, test_data$x5 ^ 3)
} else if (chosen_model_index == 4) {
  x_train <- cbind(1, train_data$x4, train_data$x3 ^ 2, train_data$x5 ^ 3)
  x_test <- cbind(1, test_data$x4, test_data$x3 ^ 2, test_data$x5 ^ 3)
} else if (chosen_model_index == 5) {
  x_train <- cbind(1, train_data$x4, train_data$x1 ^ 2, train_data$x3 ^ 2)
  x_test <- cbind(1, test_data$x4, test_data$x1 ^ 2, test_data$x3 ^ 2)
}

# Estimate model parameters using the training data
coeff_train <- solve(t(x_train) %*% x_train) %*% t(x_train) %*% y_train

# Compute predictions on the testing data
y_pred_test <- x_test %*% coeff_train

# Compute the residuals and RSS for the testing data
residuals_test <- y_test - y_pred_test
RSS_test <- sum(residuals_test^2)
n_test <- length(y_test)
p <- length(coeff_train)
sigma2 <- RSS_test / (n_test - p)

# Compute the 95% confidence intervals for the predictions
t_critical <- qt(0.975, df = n_test - p)
se_fit <- sqrt(diag(x_test %*% solve(t(x_test) %*% x_test) %*% t(x_test)) * sigma2)
ci_upper <- y_pred_test + t_critical * se_fit
ci_lower <- y_pred_test - t_critical * se_fit

# Create a data frame for plotting
plot_data <- data.frame(
  Time = test_data$Time,
  Observed = y_test,
  Predicted = y_pred_test,
  CI_Lower = ci_lower,
  CI_Upper = ci_upper
)

# Plot the observed values, predicted values, and confidence intervals with a legend
ggplot(plot_data, aes(x = Time)) +
  geom_point(aes(y = Observed, color = "Observed"), size = 2) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = "95% CI"), width = 0.2) +
  labs(title = "Model Predictions with 95% Confidence Intervals",
       x = "Time",
       y = "x2 Expression Level") +
  scale_color_manual(name = "Legend", values = c("Observed" = "black", "Predicted" = "blue", "95% CI" = "red")) +
  theme_minimal()

# Task 2.7 END





# Task 3: ABC using Model 2 parameters

# Initialize variables
arr_1 <- numeric(0)
arr_2 <- numeric(0)
f_value <- 0
s_value <- 0

# Parameters from Model 5 thetahat
thetebias <- 1.2951518
thetaone <- 0.8312983
thetatwo <- 0.5385828
thetathree <- 0.1096679

# Fixing value of Epison based on RSS_Model_5
Epison <- RSS_model5 * 2

# Number of iterations
num <- 100

# Perform rejection ABC
counter <- 0
for (i in 1:num) {
  range1 <- runif(1, -thetebias, thetebias)
  range2 <- runif(1, -thetaone, thetaone)
  
  # Construct new thetahat
  New_thetahat <- matrix(c(range1, range2, thetatwo, thetathree))
  
  # Calculate new Y_hat
  New_Y_Hat <- xModel2 %*% New_thetahat
  
  # Calculate new RSS
  new_RSS <- sum((dataset - New_Y_Hat)^2)
  
  # Check condition for rejection
  if (new_RSS > Epison) {
    arr_1[counter + 1] <- range1
    arr_2[counter + 1] <- range2
    counter <- counter + 1
  }
}

# Extract accepted values
f_value <- arr_1[1:counter]
s_value <- arr_2[1:counter]

# Plotting the histograms and scatter plot
hist(f_value, main = "Histogram of First Parameter")
hist(s_value, main = "Histogram of Second Parameter")
plot(f_value, s_value, col = c("red", "blue"), main = "Joint and Marginal Posterior Distribution")

