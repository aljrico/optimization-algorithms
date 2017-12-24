# Truck Content -----------------------------------------------------------

# Library -----------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(dplyr)
library(data.table)


# Gather Data -------------------------------------------------------------

raw.data <- fread("data/content-data.dat")


# Parameters and variables --------------------------------------------------------------

temp.max <- 100
values <- raw.data$values
weights <- raw.data$weights
ind.s <- c()
ind <- sample(c(1,0), length(values), replace = TRUE)
t <- c()
p <- c()
sol <- c()
ite <- 1000000

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
	if(a<=0.5) {sum <- 0
	} else sum <- 1
	return (sum)
}

# Simulation  -----------------------------------------------------------------

for(j in 1:ite){
	temp <- temp.max*(tanh(-log(j)+11) +1)*0.5
	t[j] <- temp
	ind.s <- ind

	for(m in 1:5){
		b <- sample(1:length(ind),1)
		ind.s[b] <- move()
	}

	ind[ind<0] <- 0
	ind.s[ind.s<0] <- 0
	pr <- exp(-(value(ind,values,weights)-value(ind.s,values,weights))/temp)
	p[j] <- pr
	a <- runif(1)
	if(a < pr) ind <- ind.s
	sol[j] <- value(ind,values,weights)
}


value(ind,values,weights)
ind

sum(ind*weights)
plot(t)
plot(sol[sol>1])
