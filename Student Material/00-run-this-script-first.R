###############################
# Setting Up Your Environment #
###############################
# the following packages will be used
list.of.packages <- c(
  "tidyverse",      # provides dplyr, tidyr, purrr, ggplot, etc for common data analysis tasks used in day 1
  "nycflights13",   # provides example data sets for day 1
  "devtools",       # allows you to download my github package
  "broom"
  )

# run the following line of code to install the packages you currently do not have
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# I have also created an interactive tutorial we'll use on day 1, which you can 
# download from my GitHub. To use this tutorial we need to use a specific rmarkdown
# version.  Both of these can be accomplished with the next two lines of code.
devtools::install_version("rmarkdown", version = "1.6")
devtools::install_github("bradleyboehmke/rbootcamp")

# Once you have downloaded my rbootcamp package, run this code. If it works, great.
# If not then send me an email at bradleyboehmke@gmail.com so we can get it 
# resolved prior to class.
library(rbootcamp)
get_tutorial("hello")

# See you on Feb 8th!





