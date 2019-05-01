pacman::p_load(tidyverse, magrittr)
Xy = read.csv("jfk_airport_wind_speed_data.csv")

head(Xy)
Xy = Xy[, c("PRCP", "SNOW", "WSF2")]
Xy = na.omit(Xy)
Xy %<>% sample_n(20)
y = Xy$WSF2
X = data.frame(Xy)
X$WSF2 = NULL

summary(y)
hist(y, br = 500)


options(java.parameters = "-Xmx8000m")
library(extremeBartMachine)

bart_mod = build_extreme_bart_machine(X, y, 
            num_burn_in = 5, num_iterations_after_burn_in = 10, num_trees = 2, 
            debug_log = TRUE)

bart_mod

