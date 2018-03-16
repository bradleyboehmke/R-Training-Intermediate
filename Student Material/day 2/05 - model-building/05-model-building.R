# prerequisites
library(tidyverse)
library(modelr)
options(na.action = na.warn)

##############
# the set-up #
##############
# why are low quality diamonds more expensive?
p1 <- ggplot(diamonds, aes(cut, price)) + geom_boxplot() + scale_y_continuous(labels = scales::dollar) + ggtitle("Price vs Cut")
p2 <- ggplot(diamonds, aes(color, price)) + geom_boxplot() + scale_y_continuous(labels = scales::dollar) + ggtitle("Price vs Color")
p3 <- ggplot(diamonds, aes(clarity, price)) + geom_boxplot() + scale_y_continuous(labels = scales::dollar) + ggtitle("Price vs Clarity")

gridExtra::grid.arrange(p1, p2, p3, nrow = 1)

# spend a few minutes discussing the logic behind this with your neighbor


################################
# a major confounding variable #
################################
# carat has a big impact on price
ggplot(diamonds, aes(carat, price)) + 
  geom_hex(bins = 50)

# how can we visualize this trend better?
ggplot(diamonds, aes(carat, price)) + 
  geom_hex(bins = 50) +
  scale_x_log10() +
  scale_y_log10()

# what is the strength of this linear trend? Does it differ depending on the 
# different levels of cut, color, and clarity
cor.test(log10(diamonds$carat), log10(diamonds$price))

diamonds %>%
  group_by(cut) %>%
  summarise(corr = cor(log10(carat), log10(price)),
            p_value = cor.test(log10(carat), log10(price))$p.value)

# let's remove the price-carat relationship so we can see if there is still an
# unusual relationship between price and cut, color, and clarity

# step 1: fit model
mod_carat <- lm(log10(price) ~ log10(carat), data = diamonds)

# step 2: assess model numerically
summary(mod_carat)

# step 3: get prediction and residual data
diamonds2 <- diamonds %>%
  add_predictions(mod_carat) %>%
  add_residuals(mod_carat) %>%
  mutate(trans_pred = 10 ^ pred)


# step 4: assess model predictions visually
ggplot(diamonds2, aes(carat, price)) + 
  geom_hex(bins = 75) + 
  geom_line(aes(y = trans_pred), color = "red") +
  coord_cartesian(xlim = c(0, 3), ylim = c(0, 20000)) # show w/o this first

# step 5: assess model residuals visually
ggplot(diamonds2, aes(carat, resid)) +
  geom_hex(bins = 50) +
  geom_ref_line(h = 0) +
  scale_x_log10()

# step 6: reassess relationship between residuals and other characteristics
p1 <- ggplot(diamonds2, aes(cut, resid)) + geom_boxplot() + ggtitle("Residual vs Cut")
p2 <- ggplot(diamonds2, aes(color, resid)) + geom_boxplot() + ggtitle("Residual vs Color")
p3 <- ggplot(diamonds2, aes(clarity, resid)) + geom_boxplot() + ggtitle("Residual vs Clarity")
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)

# now we see the relationships we expect as the quality of the diamond increases, 
# so to does it’s relative price.  How would you interpret these plots?


############################
# building onto this model #
############################
# create a larger model that incorporates cut, color, and clarity....with and 
# without interaction
diamonds3 <- diamonds %>% 
  select(price, carat, color, cut, clarity)
mod_diamond <- lm(log10(price) ~ log10(carat) + color + cut + clarity, 
                  data = diamonds3)

# how does this model appear to fit numerically
summary(mod_diamond)

# assess predictions
  # example with cut
diamonds3 %>% 
  data_grid(cut, .model = mod_diamond) %>% 
  add_predictions(mod_diamond) %>%
  mutate(trans_pred = 10 ^ pred) %>%
  ggplot(aes(cut, trans_pred)) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar)

  # example with color
diamonds3 %>% 
  data_grid(color, .model = mod_diamond) %>% 
  add_predictions(mod_diamond) %>%
  mutate(trans_pred = 10 ^ pred) %>%
  ggplot(aes(color, trans_pred)) +
  geom_point()

 # example with clarity
diamonds3 %>% 
  data_grid(clarity, .model = mod_diamond) %>% 
  add_predictions(mod_diamond) %>%
  mutate(trans_pred = 10 ^ pred) %>%
  ggplot(aes(clarity, trans_pred)) +
  geom_point()

# assess residuals
diamonds3 %>% 
  add_residuals(mod_diamond) %>%
  ggplot(aes(price, resid)) +
  geom_hex(bins = 50) +
  geom_ref_line(h = 0) +
  scale_x_log10()
