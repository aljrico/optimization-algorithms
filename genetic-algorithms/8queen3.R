
# 8-Queen Problem using a Genetic Algorithm (Version 3) ---------------------------------------------------------
# Author: Alejandro Jiménez Rico <aljrico@gmail.com>

# Load functions
source("genetic_functions.R")

# Define Variables --------------------------------------------------------

# Number of toddlers by couple
noff <- 5

# Magnitude of the Problem
nproblem <- 10

# Probability of mutation
pmut <- 0.01

# Mortality
mort <- 0.5

# List to be filled
offspring <- list()
population <- list()

# Arrays to be filled
son <- c()
subject <- c()
fitness <- c()
av.fitness <- c()
max.fitness <- c()

# Count Variables
b <- 0
ind <- 0

# Initial Population ------------------------------------------------------
ipop <- (nproblem)^2*2
for(i in 1:ipop){
	population[[i]] <- sample(seq(1:nproblem))
}

# Reproduction ---------------------------------------------------

repeat{
	m <- 1
	ind <- ind + length(population)

	# Measuring fitness of every member of the population for statistical purposes
	for (i in 1:length(population)){
		subject <- population[[i]]
		fitness[i] <- (1/(1+meas.error(subject)))
	}


	# Performance measures
	av.fitness <- append(av.fitness, mean(fitness))
	max.fitness <- append(max.fitness, max(fitness))

	# Get the most fittest individual
	bestguy <- population[max(fitness,index.return=TRUE)]
	if (max(fitness) == 1) {
		print <- 'SUCCESS!'
		break
	}

	# Killing festivity
	population <- kill(population,pmut,mort)
	b <- b+1

	while(length(population)>1){
		# Selecting parents from population
		par1 <- population[[1]]; population <- population[-1]
		par2 <- population[[1]]; population <- population[-1]
		for(i in 1:noff){
			offspring[[m]] <- gen.offspring(par1,par2,noff,pmut)[[i]]
		}
		m <- m+1
	}

	population <- offspring

	if (b > 5000) {
		print <- "FAILURE"
		break
	}
	if (length(population)<2){
		print <- 'EXTINCTION'
		break
	}
}

# Print results
par()
ts.plot(as.ts(max.fitness), xlab="Generations", ylab="Fitness")
bestguy[[1]]
ind
print
