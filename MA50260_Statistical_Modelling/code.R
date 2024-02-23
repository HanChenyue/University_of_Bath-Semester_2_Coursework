### WEEK 2 - R code

par(mai = c(0.9, 0.9, 0.2, 0.9))
load("bwt0.Rdata")
load("gas.Rdata")

## EXAMPLE 1:
head(bwt)
dim(bwt)
# 1. Set up the design matrix X
X = matrix(cbind(rep(1, nrow(bwt)), bwt$Age), 
           ncol = 2)
head(X)
# 2. Find estimates of beta 
beta = solve(t(X) %*% X) %*% t(X) %*% bwt$Weight
beta
# 3. Plot it
plot(bwt$Age, bwt$Weight, type = "p", ylab = "Birthweight", xlab = "Gestational Age", 
     pch = 19, cex.lab = 1.5, cex.axis = 1.5, cex = 1.5)
abline(a = beta[1], b = beta[2], col = "red", lwd = 2)
# ... OR
bwtlm = lm(Weight ~ Age, data = bwt)
plot(bwt$Age, bwt$Weight, type = "p", ylab = "Birthweight", xlab = "Gestational Age", 
     pch = 19, cex.lab = 1.5, cex.axis = 1.5, cex = 1.5)
abline(bwtlm$coef, col = "red", lwd = 2)
# 4. Fitted or predicted values
expected_weight = X %*% beta
expected_weight
# for a single observation:
single = beta[1] + beta[2]*34
single
# 5. Residuals and estimation of common variance
estimated_residuals = bwt$Weight - expected_weight
estimated_variance = sum(estimated_residuals^2)/(nrow(bwt) - length(beta))
estimated_variance

# 6. Hypothesis test: is gestational age increasing the weights of children?
b = 0; beta_age = beta[2]
t_observed = (beta_age - b)/sqrt(estimated_variance * (solve(t(X) %*% X)[2,2]))
DF = nrow(X)-ncol(X)
cat("Observed t-statistic:", t_observed, 
    "\nTheoretical quantile of t with", DF, "df:", qt(p = 0.05, df = DF, lower.tail = F))

# 7. Confidence intervals
quantile_t = qt(p = 0.95, df = DF)
cat("Lower:", beta_age - quantile_t * sqrt(estimated_variance * (solve(t(X) %*% X)[2,2])),
    "\nUpper:", beta_age + quantile_t * sqrt(estimated_variance * (solve(t(X) %*% X)[2,2])))
    
    
    

## EXAMPLE 2: 
head(gas)
dim(gas)
# 1. Set up the design matrix X
X = matrix(cbind(rep(1, nrow(gas)), gas$Temp, gas$Insulate2, gas$Insulate2*gas$Temp), 
           ncol = 4)
head(X)
# 2. Find estimates of beta 
beta = solve(t(X) %*% X) %*% t(X) %*% gas$Gas
beta
# 3. Plot it
Insulate21 = which(gas$Insulate2==0)
Insulate22 = which(gas$Insulate2==1)
plot(gas$Temp, gas$Gas, type = "n", xlab = "Outside Temperature", 
     ylab = "Gas consumption", cex.lab = 2, cex.axis = 1.5)
points(gas$Temp[Insulate21], gas$Gas[Insulate21], pch = 1, col = 4, cex = 1.9)
abline(a = beta[1], b = beta[2], col = 4, lwd = 3)
points(gas$Temp[Insulate22], gas$Gas[Insulate22], pch = 4, col = 2, cex = 1.9)
abline(a = beta[1]+beta[3], b = beta[2]+beta[4], col = 2, lty = 2, lwd = 3)












