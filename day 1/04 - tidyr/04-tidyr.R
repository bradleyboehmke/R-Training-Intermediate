# Load required packages
library(tidyverse)

# import data for examples
load("data/tidy_data.RData")

###############################################
# gather --> transform data from wide to long #
###############################################
# example data
cases

# gather data
gather(cases, key = Year, value = n, 2:4)
cases %>% gather(Year, n, 2:4)

# multiple ways to define columns
cases %>% gather(Year, n, `2011`:`2013`)
cases %>% gather(Year, n, `2011`, `2012`, `2013`)
cases %>% gather(Year, n, 2:4)
cases %>% gather(Year, n, -country)


#############
# YOUR TURN #
#############




###############################################
# spread --> transform data from long to wide #
###############################################
# create long data set for example
cases2 <- gather(cases, key = Year, value = n, 2:4)
cases2

# spread data
spread(cases2, key = Year, value = n)
cases2 %>% spread(Year, n)


#############
# YOUR TURN #
#############




####################################################
# separate --> split a single column into multiple #
####################################################
# example data
storms

# separate date column
separate(storms, col = date, into = c("year", "month", "day"), sep = "-")
storms %>% separate(date, c("year", "month", "day"), sep = "-")

# by default, separate will identify any non-alphanumeric character
storms %>% separate(date, c("year", "month", "day"))


#############
# YOUR TURN #
#############



###############################################
# unite --> Combine multiple columnS into one #
###############################################
# example data
storms2 <- storms %>% separate(date, c("year", "month", "day"))
storms2

# unite year, month, & day columns
unite(storms2, col = date, year, month, day, sep = "-")
storms2 %>% unite(date, year, month, day, sep = "-")

# can specify with or without quotes/explicit vector
unite(storms2, col = date, year, month, day, sep = "-")
unite(storms2, col = date, c("year", "month", "day"), sep = "-")

# default separator is "_"
storms2 %>% unite(date, year, month, day)


#############
# YOUR TURN #
#############





##############
# CHALLENGE! #
##############






