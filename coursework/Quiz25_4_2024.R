n <- 10000

# Generate n random samples from a standard normal distribution
samples <- rnorm(n)

# Compute ω for each sample
function_values <- pnorm(-2 - samples)

# Compute the Monte Carlo estimates
omega_hat <- mean(function_values)

# Compute the variance of the Monte Carlo estimates
var_hat_omega <- var(function_values)

print(paste("Estimated value of ω_hat =", omega_hat))
print(paste("Estimated variance of ω_hat =", var_hat_omega))
analytical_value <- pnorm(--2)
print(paste("Analytical value of Φ(--2) =", analytical_value))

# Number of random samples
n <- 100000

# Generate random samples from Cauchy distribution
cauchy_samples <- rcauchy(n, location = 0, scale = 1)

# Compute weights
weights <- dnorm(cauchy_samples) / dcauchy(cauchy_samples, location = 0, scale = 1)

# Compute the importance sampling estimates
importance_sampling_estimates <- (cauchy_samples <= -1.645) * weights

# Estimate Φ(-2) using importance sampling
phi_minus_2_estimate <- mean(importance_sampling_estimates)

print(paste("Estimated value of Φ(-2) using importance sampling =", phi_minus_2_estimate))
  

# Number of random samples
N <- 1000000

# Generate N random numbers between -infinity and -1.645
t <- rnorm(N)

# Compute the indicator function values
indicator <- ifelse(t < -1.645, 1, 0)

# Compute the Monte Carlo estimate
Phi_approx <- mean(indicator) / sqrt(2 * pi)

print(paste("Monte Carlo estimate of Phi(-1.645):", Phi_approx))
