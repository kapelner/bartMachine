pacman::p_load(survival)
mod = survreg(Surv(futime, fustat) ~ ecog.ps + rx, ovarian, dist='weibull')
mod


k_hat = 1 / summary(mod)$scale


##TODO Bracha write function that calculates k_max based on q.



get_k_max = function(q) {
	k_max = k_hat / (1-q)
	return(k_max)
}



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




