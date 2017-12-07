# Genetic Models ----------------------------------------------------------

# N-Queen Problem using different Genetic Algorithms
# Author: Alejandro Jiménez Rico <aljrico@gmail.com>



# Libraries ---------------------------------------------------------------

library(reshape)
library(ggplot2)
library(viridis)
library(dplyr)



# Kill Half Model ---------------------------------------------------------
half <- function(nproblem,noff,pmut,mort){
	source("genetic_functions.R")

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

	# Initial Population
	ipop <- (nproblem)^2*2
	for(i in 1:ipop){
		population[[i]] <- sample(seq(1:nproblem))
	}

	# Reproduction

	repeat{
		m <- 1
		ind <- ind + length(population)
		while(length(population)>1){
			# Selecting parents from population
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

		# Performance measures
		av.fitness <- append(av.fitness, mean(fitness))
		max.fitness <- append(max.fitness, max(fitness))

		# Get the most fittest individual
		bestguy <- offspring[max(fitness,index.return=TRUE)]
		if (max(fitness) == 1) {
			print <- 'SUCCESS!'
			break
		}

		# Kill all parents
		population <- list()
		rm(par1,par2)

		# Populating the new world
		a <- round(length(offspring)*(1-mort)*0.5)*2
		k <- 1
		while (length(offspring) != 0){
			population[[k]] <- offspring[[max(fitness,index.return=TRUE)]]
			fitness <- fitness[-max(fitness,index.return=TRUE)]
			offspring <- offspring[-max(fitness,index.return=TRUE)]
			k <- k +1
			if(k > a) break
		}
		b <- b+1
		offspring <- list() # Kill all children not fitted enough

		if (b > 5000) {
			print <- "FAILURE"
			break
		}
		if (length(population)<2){
			print <- 'EXTINCTION'
			break
		}
	}
	return("ind" = ind)
}


# Evaluation --------------------------------------------------------------

nproblem <- 8
noff <- 4
pmut <- 0.01
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

# Results
results <- list()
ind <- c()
nqueens <- c()
mutate <- c()
ite <- 25
data.list <- list()
pmuts <- c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,1)
lnq <- c(4,5,6,7,8,9,10,11,12)



# Evaluation of Half Model ------------------------------------------------

for(k in 1:length(lnq))	{
	nproblem <- lnq[k]
	for(j in 1:length(pmuts)){
		for(i in 1:ite){
			pmut <- pmuts[j]
			ind [i] <- half(nproblem,noff,pmut,mort)
			nqueens[i] <- nproblem
			mutate[i] <- pmut
			model <- "half"
			print("half")
			print(i)
			print(nproblem)
			print(pmut)
		}
		d1 <- data.frame(ind,nqueens,mutate, model)
		data.list[[j + length(pmuts)*(k-1)]] <- d1
	}
}

dat.half <- do.call(rbind, data.list)


write.csv(dat.half, file = "data/half.csv", row.names = FALSE)
write.csv(dat.half, file = "data/half-further.csv",row.names=FALSE)


