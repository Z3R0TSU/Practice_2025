# Generate some sample data
set.seed(123)
n <- 100
x <- rnorm(n)
y <- rbinom(n, 1, 1 / (1 + exp(-(0.5 * x + 0.1)) + 0.1 * rnorm(n)))

# Initialize coefficients
beta <- c(0, 0)

# Sigmoid function
sigmoid <- function(z) {
  1 / (1 + exp(-z))
}

# Cost function
cost <- function(beta, x, y) {
  N <- length(y)
  y_hat <- sigmoid(beta[1] + beta[2] * x)
  -sum(y * log(y_hat) + (1 - y) * log(1 - y_hat)) / N
}

# Gradient descent
gradient_descent <- function(beta, x, y, alpha, iterations = 100) {
  N <- length(y)
  cost_history <- numeric(iterations)

  for (i in 1:iterations) {
    y_hat <- sigmoid(beta[1] + beta[2] * x)
    print(paste("y_hat len: ", length(y_hat)))
    print(paste("y len: ", length(y)))

    gradient <- c(sum(y_hat - y) / N, sum((y_hat - y) * x) / N)

    beta <- beta - alpha * gradient
    cost_history[i] <- cost(beta, x, y)
  }

  list(beta = beta, cost_history = cost_history)
}

# Run gradient descent
result <- gradient_descent(beta, x, y, alpha = 0.01, iterations = 1000)

# Print coefficients and cost history
print(result$beta)
plot(result$cost_history, type = 'l', xlab = 'Iterations', ylab = 'Cost')

#########################################################################

# Load the iris dataset
data(iris)

# Convert the species to binary: setosa as 0 and versicolor as 1
iris$Species <- ifelse(iris$Species == "setosa", 0, 1)

# Extract sepal length (Sepal.Length) and sepal width (Sepal.Width) as predictors
x <- iris$Sepal.Length
y <- iris$Species

# Convert data to data frame
data_df <- data.frame(x = x, y = y)

# Custom Logistic Regression Function
custom_logistic_regression <- function(x, y, alpha = 0.01, iterations = 1000) {
  beta <- c(0, 0)

  for (i in 1:iterations) {
    y_hat <- sigmoid(beta[1] + beta[2] * x)

    gradient <- c(sum(y_hat - y), sum((y_hat - y) * x))

    beta <- beta - alpha * gradient
  }

  beta
}

# Run custom logistic regression
beta_custom <- custom_logistic_regression(x, y)

# Fit logistic regression model using glm
model <- glm(y ~ x, data = data_df, family = binomial())

# Print coefficients from custom and glm
print("Custom Logistic Regression Coefficients:")
print(beta_custom)

print("glm() Coefficients:")
print(coef(model))

# Predict using custom and glm
y_pred_custom <- ifelse(sigmoid(beta_custom[1] + beta_custom[2] * x) > 0.5, 1, 0)
y_pred_glm <- ifelse(predict(model, type = "response") > 0.5, 1, 0)

# Calculate accuracy
accuracy_custom <- mean(y_pred_custom == y)
accuracy_glm <- mean(y_pred_glm == y)

# Print accuracy
cat("Custom Logistic Regression Accuracy:", accuracy_custom, "\n")
cat("glm() Accuracy:", accuracy_glm, "\n")


