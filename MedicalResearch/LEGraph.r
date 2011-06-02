require(zoo)

LEGraph <- function() {
	le <- rev(read.table("life expectancy")[,1])
	delta <- (le[2:length(le)] - le[1:length(le)-1]) / le[1:length(le)-1]
	x = 1930:2006
	data <- data.frame(years = x, lifeExpectancy = le[2:length(le)], delta = delta)
	plot(data$years, data$delta)
}
