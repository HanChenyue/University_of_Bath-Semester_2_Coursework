# What is the design matrix for this model?
x = cbind(1, durham$Year - 1879)
x

# Derive and interpret the estimates ˆβ and ˆσ2.
y = durham$Temperature
n = nrow(x)
p = ncol(x)
beta_hat = solve(t(x) %*% x) %*% t(x) %*% y
beta_hat
# 12.728479872 is the mean temperature in 1879 in Durham
# 0.007967154 is the increase in temperature per year in Durham
sigma_2_hat = sum(((y - x %*% beta_hat)^2))/(n - p)
sigma_2_hat
# Figure out what does sigma_2_hat means?


# Calculate a 90% confidence interval for β2.
standard_error_beta2 = sqrt(sigma_2_hat * solve(t(x) %*% x)[2,2])
standard_error_beta2
beta_hat[2] + c(-1, 1) * standard_error_beta2 * qt(p = 0.95, df = n - p)


durham$Year2 = durham$Year - 1879
durham.lm = lm(Temperature ~ Year2, data = durham)
summary(durham.lm)


# Q3
q3x = cbind(1, olympics$LongJump - 308.3, olympics$DiscusThrow - 2145.0)
q3x

beta_hat_olympic = solve(t(q3x) %*% q3x) %*% t(q3x) %*% olympics$HighJump
beta_hat_olympic
vector_of_estimated_residual = olympics$HighJump - q3x %*% beta_hat_olympic
vector_of_estimated_residual
sigma_2_hat_olympic = sum(vector_of_estimated_residual^2)/(nrow(q3x) - ncol(q3x))
sigma_2_hat_olympic

q4a = beta_hat_olympic[1] + beta_hat_olympic[2] * (308 - 308.3) + beta_hat_olympic[3] * (2078 - 2145.0)
q4a