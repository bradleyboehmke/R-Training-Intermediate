#################
# prerequisites #
#################
library(modelr)
library(tidyverse)
library(gapminder)
gapminder


#############################################
# How does life expectancy change over time #
#############################################
gapminder %>% 
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)

# model this relationship
full_mod <- lm(lifeExp ~ year + country, data = gapminder)
summary(full_mod)

# what if we want to focus on a single country
usa <- filter(gapminder, country == "United States")
usa %>% 
  ggplot(aes(year, lifeExp)) + 
  geom_line() + 
  ggtitle("Full data = ")

usa_mod <- lm(lifeExp ~ year, data = usa)
usa %>% 
  add_predictions(usa_mod) %>%
  ggplot(aes(year, pred)) + 
  geom_line() + 
  ggtitle("Linear trend + ")

usa %>% 
  add_residuals(usa_mod) %>% 
  ggplot(aes(year, resid)) + 
  geom_hline(yintercept = 0, colour = "white", size = 3) + 
  geom_line() + 
  ggtitle("Remaining pattern")


# But what if we want to compare this same model across every country?

###############
# Nested data #
###############
# introducing a new data structure = the nested data frame
by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()

by_country

# what is in each data column element? -- these kind of columns are called "list-columns"
by_country$data[[1]]



###############################
# Iterative model application #
###############################
country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

# using the map function we can apply this model over every element in the
# data column
map(by_country$data, country_model)

# however, if we use mutate we can actually save the results in a new list-column
by_country <- by_country %>% 
  mutate(model = map(data, country_model))
by_country


#############
# Unnesting #
#############
# if we want to pull information out of our models (i.e. predicted values or
# residuals) we can apply map2()
by_country %>% 
  mutate(resids = map2(data, model, add_residuals))

# we can then unnest our data
by_country %>% 
  mutate(resids = map2(data, model, add_residuals)) %>%
  unnest(resids)

# and do our normal visualization
by_country %>% 
  mutate(resids = map2(data, model, add_residuals)) %>%
  unnest(resids) %>%
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) + 
  geom_smooth(se = FALSE)

# if we facet by continent we get even better insight to where models are 
# performing poorly
by_country %>% 
  mutate(resids = map2(data, model, add_residuals)) %>%
  unnest(resids) %>%
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) + 
  geom_smooth(se = FALSE) +
  facet_wrap(~ continent)


#################
# Model Quality #
#################
# we can use broom::glance() to extract some model quality metrics
broom::glance(usa_mod)

# to apply this over all our models
by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE) # show without .drop first and then with it

# this allows us to do some quick assessment of our many models
by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE) %>%
  arrange(adj.r.squared)

by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE) %>%
  ggplot(aes(continent, adj.r.squared)) + 
  geom_jitter(width = 0.25)

# We could pull out the countries with particularly bad  R2R2  and plot the data:
bad_fit <- by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE) %>%
  filter(r.squared < 0.25)

gapminder %>% 
  semi_join(bad_fit, by = "country") %>% 
  ggplot(aes(year, lifeExp, colour = country)) +
  geom_line()

# what could this be from?
# likely the tragedies of the HIV/AIDS epidemic and the Rwandan genocide



