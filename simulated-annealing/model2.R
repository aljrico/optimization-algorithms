# Truck Content -----------------------------------------------------------

# Library -----------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(dplyr)
library(data.table)
library(viridis)


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
ite <- 500000

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


# Moving Average ----------------------------------------------------------

ma <- function(arr, n=100){
	res = arr
	for(i in n:length(arr)){
		res[i] = mean(arr[(i-n):i])
	}
	res
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

	oldv <- value(ind,values,weights)
	newv <- value(ind.s,values,weights)

	if(oldv < newv) {
		pr <- 1
		}else{
		pr <- exp(-(oldv-newv)/temp)
		}
	p[j] <- pr
	a <- runif(1)
	if(a < pr) ind <- ind.s
	sol[j] <- value(ind,values,weights)
}



# Results -----------------------------------------------------------------

value(ind,values,weights)
ind
sum(ind*weights)


# Graphics ----------------------------------------------------------------

df <- as.data.frame(t)
df$steps <- as.numeric(rownames(df))
df$p <- p
df$sol <- sol

dfplot <- melt(df, id.vars = "steps", variable.name = "label")



dfplot %>%
	filter(label == "sol") %>%
	ggplot(aes(x=steps)) +
	geom_jitter(aes(y = value, colour = value))+
	scale_colour_viridis(option="B",begin=0.05, end =0.99, direction = -1)	+
	labs(x = "Time Steps", y = "Value of the Best Solution") +
	theme_bw() +
	theme(legend.position = "none")

dfplot %>%
	filter(label == "p") %>%
	ggplot(aes(x=steps)) +
	geom_jitter(aes(y = ma(value), colour = ma(value)))+
	scale_colour_viridis(option="C",begin=0, end =1, direction = 1)	+
	labs(x = "Time Steps", y = "Probability of Change (Moving Average)") +
	theme_bw() +
	theme(legend.position = "none")
