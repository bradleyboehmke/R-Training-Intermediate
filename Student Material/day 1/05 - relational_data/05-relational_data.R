# packages required
library(nycflights13)
library(tidyverse)

# example data
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

x
y

# exercise data
flights
tailnum
airlines
airports
weather

##################
# Mutating Joins #
##################
# inner join
inner_join(x, y, by = "key")
x %>% inner_join(y, by = "key")

# left join
x %>% left_join(y, by = "key")

# right join
x %>% right_join(y, by = "key")

# full join
x %>% full_join(y, by = "key")

#############
# YOUR TURN #
#############




###################
# Filtering Joins #
###################
# semi join
x %>% semi_join(y, by = "key")
y %>% semi_join(x, by = "key")

# anti join
x %>% anti_join(y, by = "key")
y %>% anti_join(x, by = "key")

#############
# YOUR TURN #
#############




##################
# Set Operations #
##################
# example data
df1 <- tribble(
  ~x, ~y,
  1,  1,
  2,  1
)
df2 <- tribble(
  ~x, ~y,
  1,  1,
  1,  2
)

df1; df2

intersect(df1, df2)
union(df1, df2)
setdiff(df1, df2)

#############
# CHALLENGE #
#############







