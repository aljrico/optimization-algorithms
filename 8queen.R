
# 8-Queen Problem using a Genetic Algorithm ---------------------------------------------------------
# Author: Alejandro Jiménez Rico <aljrico@gmail.com>

# Load functions
source("genetic_functions.R")

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
max.fitness <- c()

# Number of toddlers by couple
noff <- nproblem*2

# Initial parents
par1 <- sample(seed)
par2 <- sample(seed)


b <- 0
# Creating further population ---------------------------------------------------
repeat{
	# Measuring fitness of every child
	for (i in 1:noff){
		subject <- gen.offspring(par1,par2,noff,seed)[[i]]
		fitness[i] <- nproblem*(nproblem-1)-meas.error(subject)
	}

	# Selecting most fittest childs as new parents
	offspring <- gen.offspring(par1,par2,noff,seed)
	par1 <- offspring[sort(-fitness,index.return=TRUE)[[2]][1]][[1]]
	par2 <- offspring[sort(-fitness,index.return=TRUE)[[2]][2]][[1]]
	max.fitness <- append(max.fitness, max(fitness))

	b <- b+1

	if (max(fitness)== nproblem*(nproblem-1)) break
	if (length(max.fitness)> 10000) break
	if (b > 10000) break
}

