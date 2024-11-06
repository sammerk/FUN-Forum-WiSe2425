## Simulation Post-Treatment Bias
library(tidyverse)
library(GGally)
library(sjPlot)

set.seed(19300)
n <- 4000
data_PTB <-
  tibble(
    # simulate randomized treatment assignment
    treatment = as.factor(sample(c("civic education", "control"), 
                                 n, 
                                 replace = T)),
    # simulate independently standard normally distributed confounder (interest)
    interest_in_politics = rnorm(n, 0, 1),
    # simulate appreciation as affected by treatment and confounder
    appreciation = scale(
      rnorm(n, 0, 1) + interest_in_politics - 2*as.numeric(treatment)
    )[,1],
    # simulate outcome as affected by treatment, confounder and covariate
    outcome = scale(
      rnorm(n, 0, 1) + interest_in_politics -
        2*as.numeric(as.factor(treatment)) + appreciation
    )[,1],
    # derive dichotomous low-appreciater variable
    low_appreciator = ifelse(appreciation < -1, "yes", "no")
  )

# look at the bivariate associations
ggpairs(data_PTB) + theme_minimal()

# look at unbalancedness of confounder over treatment after adjustment of 
# post-treatment variable
ggplot(data_PTB, aes(treatment, interest_in_politics, color = low_appreciator)) +
  geom_point(position = position_jitterdodge(), alpha = .6) +
  geom_boxplot(alpha = .5, outlier.shape = NA ) +
  theme_minimal()

# look on treatment of treatment effect on outcome conditional to post-treatment
ggplot(data_PTB, aes(x = treatment, y = outcome, color = low_appreciator)) +
  geom_point(position = position_jitterdodge(), alpha = .6) +
  geom_boxplot(alpha = .5, outlier.shape = NA ) +
  theme_minimal()
  
tab_model(
  lm(outcome ~ treatment, data = data_PTB),
  lm(outcome ~ treatment + interest_in_politics, data = data_PTB),
  lm(outcome ~ treatment + appreciation, data = data_PTB),
  lm(outcome ~ treatment + appreciation + interest_in_politics, data = data_PTB),
  lm(outcome ~ treatment, data = data_PTB %>% filter(low_appreciator == "yes")),
  show.ci = F,
  show.std = T,
  show.est = F
)



