library(tidyverse)
library(modelr)
options(na.action = na.warn)

#################
# Pre-Modeling #
#################

# show data with an obvious relationship
ggplot(sim1, aes(x, y)) +
  geom_point()

# the idea is to find something that describes this relationship 
# most basic is correlation which describes the strength of a linear relationship
ggplot(sim1, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

cor(sim1$x, sim1$y)
cor.test(sim1$x, sim1$y)

# visualizing your relationships is very important
library(gridExtra)
library(grid)

p1 <- ggplot(anscombe, aes(x1, y1)) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = .5) +
  geom_point()
p2 <- ggplot(anscombe, aes(x2, y2)) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = .5) +
  geom_point()
p3 <- ggplot(anscombe, aes(x3, y3)) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = .5) +
  geom_point()
p4 <- ggplot(anscombe, aes(x4, y4)) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = .5) +
  geom_point()

grid.arrange(p1, p2, p3, p4, ncol = 2, 
             top = textGrob("Anscombe's Quartet"))

cor(anscombe$x1, anscombe$y1)
cor(anscombe$x2, anscombe$y2)

##############
# Your Turn! #
##############
# assess the linear relationship between mpg and wt in the mtcars data set
cor.test(mtcars$mpg, mtcars$wt)

# how could you do this using dplyr (think summarise())?
mtcars %>%
  group_by(cyl) %>%
  summarise(correlation = cor(mpg, wt),
            p_value = cor.test(mpg, wt)$p.value)

# how could you visually compare a linear vs non-linear relationship (hint: geom_smooth)
ggplot(mtcars, aes(wt, mpg)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(se = "FALSE", color = "red") +
  geom_point()


# what if we want to observe multiple correlation relationships?
sim4
cor(sim4)
pairs(sim4)
# note corrplot & corrgram are worth checking out for visualizing correlations
# across a full data set

# but what if we want to get the significance
sim4 %>% 
  gather(var, value, -y) %>%
  group_by(var) %>%
  summarise(corr = cor(y, value),
            p_value = cor.test(y, value)$p.value) %>%
  filter(p_value < 0.05)


##############
# Your Turn! #
##############
# visualize relationships across all mtcars variables; which ones have appear to
# have the strongest relationship to mpg?
pairs(mtcars)

# quantify the correlations and find the ones that are statistically significant
mtcars %>%
  gather(var, value, - mpg) %>%
  group_by(var) %>%
  summarise(corr = cor(mpg, value),
            p_value = cor.test(mpg, value)$p.value) %>%
  filter(p_value <= 0.05) %>%
  arrange(desc(abs(corr)))

# Many questions remain such as:
# - what can I infer from these relationships (other than just strength)
# - how should we treat the categorical relationships
# - are there interactions between variables
# - and a host of others

###################
# Modeling Basics #
###################

# We can start to answer some of these questions with regression
# 1. there are lots of potential relationships - show messy regression plot
# 2. we want to reduce this to find the best relationships for our data

# show image with best fitted lines and then the best fit line
ggplot(sim1, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# illustrate regression model
sim1_mod <- lm(y ~ x, data = sim1)
sim1_mod
summary(sim1_mod)
str(sim1_mod)
coef(sim1_mod)
residuals(sim1_mod)
fitted.values(sim1_mod)

##############
# Your Turn! #
##############
# fit a linear model that regresses mpg onto wt with the mtcars data
mtcars_mod1 <- lm(mpg ~ wt, data = mtcars)

# how does this model appear to fit (hint: summary)?
summary(mtcars_mod1)

# can you access the fitted values and residuals
mtcars_mod1$residuals
mtcars_mod1$fitted.values

residuals(mtcars_mod1)
fitted.values(mtcars_mod1)

######################
# Visualizing Models #
######################
# we can add our modeling results to our data
sim1_mod <- lm(y ~ x, data = sim1)
sim1 %>%
  add_predictions(sim1_mod) %>%
  add_residuals(sim1_mod)

# and we can extend this right into visualizing it
sim1 %>%
  add_predictions(sim1_mod) %>%
  ggplot(aes(x, y)) +
  geom_point() +
  geom_line(aes(y = pred), color = "red", size = 1)

# show how this relates to
ggplot(sim1, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm")

# by visualizing residuals we can assess 
sim1 %>%
  add_residuals(sim1_mod) %>%
  ggplot(aes(resid)) +
  geom_histogram(binwidth = .5)

sim1 %>%
  add_residuals(sim1_mod) %>%
  ggplot(aes(x, resid)) +
  geom_ref_line(h = 0) +
  geom_point()


# This looks like random noise, suggesting that our model has done a good job of 
# capturing the patterns in the dataset.

##############
# Your Turn! #
##############
# fit a linear model that regresses mpg onto wt with the mtcars data
mtcars_mod1 <- lm(mpg ~ wt, data = mtcars)

# how does this model appear to fit?
summary(mtcars_mod1)

# add the predictions and residuals to the mtcars data set and plot them
mtcars %>%
  select(mpg, wt) %>%
  add_predictions(mtcars_mod1) %>%
  add_residuals(mtcars_mod1) %>%
  ggplot(aes(wt, mpg)) +
  geom_point() +
  geom_line(aes(y = pred), color = "red")

mtcars %>%
  select(mpg, wt) %>%
  add_predictions(mtcars_mod1) %>%
  add_residuals(mtcars_mod1) %>%
  ggplot(aes(wt, resid)) +
  geom_ref_line(h = 0) +
  geom_point() +
  geom_smooth(se = FALSE)


######################################
# Dealing with categorical variables #
######################################
sim2
ggplot(sim2, aes(x, y)) + 
  geom_point()

sim2_mod <- lm(y ~ x, data = sim2)
summary(sim2_mod)

# Effectively, a model with a categorical x will predict the mean value for each 
# category. We can easily see this with
sim2 %>% 
  data_grid(x) %>% 
  add_predictions(sim2_mod)

# visualize
# sim2 %>%
sim2 %>%
  add_predictions(sim2_mod) %>%
  ggplot(aes(x, y)) +
  geom_point() +
  geom_point(aes(y = pred), color = "red", size = 4)

# what if we want to change the reference factor?
sim2b <- sim2 %>% 
  mutate(x = as.factor(x),
         x = relevel(x, ref = "b"))

sim2b_mod <- lm(y ~ x, data = sim2b)
summary(sim2b_mod)

##############
# Your Turn! #
##############
# model mpg ~ cyl with cyl being categorical
mtcars_mod2 <- lm(mpg ~ as.factor(cyl), data = mtcars)

# plot the predictions
mtcars %>%
  add_predictions(mtcars_mod2) %>%
  ggplot(aes(as.factor(cyl), mpg)) +
  geom_boxplot() +
  geom_point() +
  geom_point(aes(y = pred), color = "red", size = 4)

# plot the residuals
mtcars %>%
  add_residuals(mtcars_mod2) %>%
  ggplot(aes(as.factor(cyl))) +
  geom_ref_line(h = 0) +
  geom_point(aes(y = resid))

################################################
# Interactions btwn continuous and categorical #
################################################
sim3
ggplot(sim3, aes(x1, y, color = x2)) + 
  geom_point() +
  facet_wrap(~ x2, nrow = 1)

# two potential model specifications, explain difference btwn + and *
mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)

model_matrix(sim3, y ~ x1 + x2)
model_matrix(sim3, y ~ x1 * x2)

summary(mod1)
summary(mod2)

sim3 %>% 
  gather_predictions(mod1, mod2) %>%
  ggplot(aes(x1, y, color = x2)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  facet_grid(model ~ x2)

sim3 %>%
  gather_residuals(mod1, mod2) %>%
  ggplot(aes(x1, resid, color = x2)) +
  geom_ref_line(h = 0, size = 1) +
  geom_point() +
  facet_grid(model ~ x2)



##############
# Your Turn! #
##############
# compare the following two models for the mtcars data
mtcars_mod3 <- lm(mpg ~ wt + as.factor(cyl), data = mtcars)
mtcars_mod4 <- lm(mpg ~ wt * as.factor(cyl), data = mtcars)

# compare summaries
summary(mtcars_mod3)
summary(mtcars_mod4)

# compare model matrix
model_matrix(mtcars, mpg ~ wt + as.factor(cyl))
model_matrix(mtcars, mpg ~ wt * as.factor(cyl))

# compare predicted values
mtcars %>%
  mutate(cyl = as.factor(cyl)) %>%
  select(mpg, wt, cyl) %>%
  gather_predictions(mtcars_mod3, mtcars_mod4) %>%
  ggplot(aes(wt, mpg, color = cyl)) +
  geom_point() +
  geom_line(aes(y = pred, color = cyl)) +
  facet_grid(model ~ cyl, scales = "free")

# compare residuals
mtcars %>%
  mutate(cyl = as.factor(cyl)) %>%
  select(mpg, wt, cyl) %>%
  gather_residuals(mtcars_mod3, mtcars_mod4) %>%
  ggplot(aes(wt, resid, color = cyl)) +
  geom_ref_line(h = 0) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(model ~ cyl, scales = "free")

# what are your thoughts?

########################
# Model Specifications #
########################

# at any time we can see the translation of our model with
model_matrix(sim1, y ~ x)
model_matrix(sim2, y ~ x)
model_matrix(sim3, y ~ x1 + x2)
model_matrix(sim3, y ~ x1 * x2)

##############
# Your Turn! #
##############
# get the model matrix for our mtcars_mod1 model
mtcars_mod1 <- lm(mpg ~ wt, data = mtcars)

model_matrix(mtcars, mpg ~ wt)

# what happens if you add two new variables as in 
mtcars_mod2 <- lm(mpg ~ wt + cyl + hp, data = mtcars)
model_matrix(mtcars, mpg ~ wt + cyl + hp)


###################
# Transformations #
###################
# You can also perform transformations inside the model formula
# lm(log(y) ~ sqrt(x1) + x2)
# If your transformation involves +, *, ^, or -, you’ll need to wrap it in I() 
# so R doesn’t treat it like part of the model specification. For example, 
# y ~ x + I(x ^ 2) is translated to y = a_1 + a_2 * x + a_3 * x^2

model_matrix(sim1, y ~ x + x^2)
model_matrix(sim1, y ~ x + I(x^2))

