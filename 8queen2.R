
# 8-Queen Problem using a Genetic Algorithm (Version 2) ---------------------------------------------------------
# Author: Alejandro Jiménez Rico <aljrico@gmail.com>

# Load functions
source("genetic_functions.R")

# Magnitude of the Problem
nproblem <- 8

# Probability of mutation
pmut <- 0.1

# Mortality and maxpopulation multiplicator
mort <- 0.9

# Seed for generating initial parents
seed <- seq(nproblem)

# List to be filled
offspring <- list()
population <- list()

# Arrays to be filled
son <- c()
subject <- c()
fitness <- c()
av.fitness <- c()

# Number of toddlers by couple
noff <- 3

# Initial population
for(i in 1:nproblem){
	population[[i]] <- sample(seq(1:nproblem))
}


# Reproduction ---------------------------------------------------
b <- 0
repeat{
	m <- 1
	while(length(population)>1){
		par1 <- population[[1]]; population <- population[-1]
		par2 <- population[[1]]; population <- population[-1]

		# Measuring fitness of every child
		for (i in 1:noff){
			offspring[[m]] <- gen.offspring(par1,par2,noff,pmut)[[i]]
			subject <- offspring[[m]]
			fitness[m] <- (1/(1+meas.error(subject)))
			m <- m+1
		}
	}
	av.fitness <- append(av.fitness, mean(fitness))
	bestguy <- offspring[max(fitness,index.return=TRUE)]
	if (max(fitness) == 1) break
	population <- list()

	# Populating the new world

	a <- round((length(offspring)*0.5)/mort)*2
	k <- 1
	while (length(offspring) != 0){
		population[[k]] <- offspring[[max(fitness,index.return=TRUE)]]
		fitness <- fitness[-max(fitness,index.return=TRUE)]
		offspring <- offspring[-max(fitness,index.return=TRUE)]
		k <- k +1
		if(k > a) break
	}
	b <- b+1
	offspring <- list()

	if (b > 5000) break

}
ts.plot(av.fitness)
bestguy
