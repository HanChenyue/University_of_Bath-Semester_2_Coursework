df <- read.csv("FEV.csv")
head(df)
# a: Define a linear regression model which relates a child’s FEV to their age and height.
model <- lm(FEV ~ Age + Height, data = df)

# b: Without looking at the data, consider whether collinearity could arise.
# Yes, age and height are correlated.
# This is likely to be the case as older children are likely to be taller and younger children are likely to be shorter.

# c: Fit your linear regression model from part a.
summary(model)

# d: Derive confidence intervals for the regression coefficients and state which explanatory variables are significant at the 5% significance level. Do your conclusions agree with your answer in part b?
confint(model, level = 0.95)
# No, the confidence intervals do not agree with the answer in part b. Both age and height are significant at the 5% level.