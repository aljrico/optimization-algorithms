# Truck Content -----------------------------------------------------------


# Library -----------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(dplyr)
library(data.table)


# Gather Data -------------------------------------------------------------

raw.data <- fread("data/content-data.dat")


# Parameters and variables --------------------------------------------------------------

temp.min <- 1
temp.max <- 400000
values <- raw.data$values
weights <- raw.data$weights
ind.s <- c()
ind <- seq(from =1, to = 1, length.out= length(values))
t <- c()
p <- c()
ite <- 100000
# Calculating value --------------------------------------------------------------

value <- function(ind, values, weights){
	a <- sum(ind*weights)
	if (a <= 600) b <- sum(ind*values)
	else b <- - sum(ind*weights)^100
	return(b)
}


# Moving to a Neighbour ---------------------------------------------------

move <- function() {
	a <- runif(1)
	if(a<=0.33) {sum <- -1
	} else if(a <=0.66) {sum <- 0
	} else sum <- 1
	return (sum)
}


# Simulation --------------------------------------------------------------

for(j in 1:ite){
	#temp <- temp.max*(tanh(-log(j)+5) +1)
	temp <- temp.max/((log(j^2+1)))
	t[j] <- temp
	ind.s <- sapply(ind, function(x) x + move())
	ind[ind<0] <- 0
	ind.s[ind.s<0] <- 0
	pr <- exp(-(value(ind,values,weights)-value(ind.s,values,weights))/temp)
	p[j] <- pr
	a <- runif(1)
	if(a < pr) ind <- ind.s
}



# Results -----------------------------------------------------------------

value(ind,values,weights)
ind

sum(ind*weights)
plot(t)
