library(tidyverse)
library(ggplot2)


replicates_r <- 10
treatment_levels_t <- 6
number_of_experimental_units_n <- replicates_r * treatment_levels_t
overall_response_average_mu <- 500
treatment_effects_tau <- c(-20, 50, 0, -30, -10, 100)
mean_response <- overall_response_average_mu + treatment_effects_tau %>% 
  rep(each = replicates_r)
sd <- 10
levels <- c("level 1","level 2","level 3","level 4", "level 5","level 6")
factor_f <- factor(rep(levels, each = replicates_r))



set.seed(5678)
fac <- sample(factor_f, size = number_of_experimental_units_n, replace = FALSE)
dataframe <- tibble(units=1:number_of_experimental_units_n, treatment=fac)
dataframe_rearranged_by_treatment_level <- arrange(dataframe, treatment)
dataframe_with_original_factor <- tibble(units=1:number_of_experimental_units_n,
                                         treatment=factor_f)
response_y <- rnorm(n = number_of_experimental_units_n, mean = mean_response,
                    sd = sd) 
# note the units should be arranged by treatment level (What does this mean that
# the unit should be arranged by treatment level and not any other way? 
# Mind elaborate further?)
dataframe_with_original_factor$individual_response <- response_y



ggplot(dataframe_with_original_factor, 
       aes(x = treatment, y = individual_response)) + geom_point()
ggplot(dataframe_with_original_factor, 
       aes(x = treatment, y = individual_response)) + geom_boxplot()



linear_model.dataframe_with_original_factor <- lm(
  individual_response ~ treatment, data = dataframe_with_original_factor)  
summary(model.dataframe_with_original_factor)
linear_model_coefficients <- coef(linear_model.dataframe_with_original_factor)
c(overall_response_average_mu, treatment_effects_tau[-1]) # why ignore tau_1?
new_taus <- c(0, coefficients[-1]) # why do we make tau_1 = 0?
new_means <- coefficients[1] + new_taus
new_means
overall_response_average_mu + treatment_effects_tau

by_group <- group_by(dataframe_with_original_factor, treatment)
by_group
summaries.dataframe_with_original_factor <- 
  summarize(by_group, mean = mean(individual_response),
            sd = sd(individual_response))
glimpse(summaries.dataframe_with_original_factor) 