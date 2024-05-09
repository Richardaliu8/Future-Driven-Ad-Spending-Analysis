# Load necessary libraries
library(MASS)

#import dataset
Gillette_data <- read.csv("Gillette_data.csv")

# Linear regression model & variance-covariance matrix
lm_model <- lm(Revenues ~ Margin + Sales_Multiple - 1, data = Gillette_data)
vcov_lm <- vcov(lm_model)
summary(lm_model)
vcov_lm

#nonlinear regression model
nls_model <- nls(Sqrt_Advtg ~ A * Margin + B * Sales_Multiple,
                 data = Gillette_data,
                 start = list(A = 1, B = 1))
summary(nls_model)

print("The coefficient for long-term margin (A) is 120.1183, and the coefficient for sales multiple (B) is 1.6430.")

# Monte Carlo simulation
set.seed(123)
draws <- 1000

simulated <- mvrnorm(n = draws, mu = coef(nls_model), Sigma = vcov_lm)
simulated

# Compute 95% CIs for A and B from the simulated parameters
A_ci <- quantile(simulated[, "A"], probs = c(0.025, 0.975))
B_ci <- quantile(simulated[, "B"], probs = c(0.025, 0.975))
A_ci
B_ci

print("The 95% confidence interval for A (long-temr margin) is (-2270.911,2398.685), and the CI for B(sales multiple) is (-190.4638, 199.7748).")

#### Question 3####
sprintf("The coefficients for both margin and sales multiple are both statistically significant,and these 2 metrics influenced Gillette's advertising spending decisions. This indicates that the Gillette managers were focused on the company’s financial situation and market/sales research, which are factors that will affect a future acquisition.")
sprintf("Thus, from the model, we can see the financial situation and market/sales factors that are influencing advertising spending for Gillette. However, only focusing on the model and the matrix, it’s hard to get an absolute conclusion that Gillette's managers were explicitly preparing for a future acquisition through their advertising expenditure decisions.")
sprintf("As a result, we can conclude that the Gillette managers might have set their advertising spending to take account of a future possibility of acquisition but it's not clear based on the results.")
