Xy = read.csv("jfk_airport_wind_speed_data.csv")

head(Xy)
Xy = Xy[, c("PRCP", "SNOW", "WSF2")]
Xy = na.omit(Xy)
y = Xy$WSF2
X = data.frame(Xy)
X$WSF2 = NULL

summary(y)
hist(y, br = 500)

options(java.parameters = "-Xmx8000m")
library(extremeBartMachine)

build_extreme_bart_machine(X, y)




