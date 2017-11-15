
# 8-Queen Problem using a Genetic Algorithm ---------------------------------------------------------
# Author: Alejandro Jiménez Rico <aljrico@gmail.com>

# Magnitude of the Problem
nproblem <- 8

# Seed for generating initial parents
seed <- seq(nproblem)

# List to be filled
offspring <- list()

# Arrays to be filled
son <- c()
subject <- c()
fitness <- c()

# Number of toddlers by couple
noff <- nproblem*2

# Initial parents
par1 <- sample(seed)
par2 <- sample(seed)

# Find common pattern in parents (function)
noncommon <- function(par1,par2){
	noncommon <- which(!par1-par2 == 0)
}

# Generate offspring (function)
gen.offspring <- function(par1,par2, noff, seed){
	for (j in 1:noff){
		offseed <- sample(par1[noncommon(par1,par2)])
		randomdummy <- runif(1)
		if (randomdummy<=0.75){
			for (i in 1:length(seed)){

				if (i %in% noncommon(par1,par2)) {
					son[i] <- offseed[1]
					offseed <- offseed[-1]
				}
				else {son[i] <- par1[i]}
			}
		}
		else {son <- sample(par1)}
		offspring[[j]] <- son
	}
	return (offspring)
}

# Measuring fitness (function)
meas.fitness <- function(subject){
	error <- 0
	for (i in 1: length(subject)){
		x <- i
		y <- subject[i]
		for(j in 1:length(subject)){
			dx <- j
			dy <- subject[j]

			if (abs(dx-x)==abs(dy-y)){error = error+1}
		}
	}
	return(error)
}

# Select the 2 most fittest elements of the offspring to be the new parents
for (i in 1:noff){
	subject <- gen.offspring(par1,par2,noff,seed)[[i]]
	fitness[i] <- (1/(1+meas.fitness(subject)))
}

offspring <- gen.offspring(par1,par2,noff,seed)
par1 <- offspring[sort(-fitness,index.return=TRUE)[[2]][1]][[1]]
par2 <- offspring[sort(-fitness,index.return=TRUE)[[2]][2]][[1]]


