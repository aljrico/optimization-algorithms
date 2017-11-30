
# Genetic Models ----------------------------------------------------------

# N-Queen Problem using different Genetic Algorithms
# Author: Alejandro Jiménez Rico <aljrico@gmail.com>



# Libraries ---------------------------------------------------------------

library(reshape)
library(viridis)


# Tournament Model --------------------------------------------------------

tournament <- function(nproblem,noff,pmut,mort){
	# Load functions
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
	return("ind" = ind)
}


nproblem <- 8
noff <- 5
pmut <- 0.01
mort <- 0.85

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
ite <- 100
data.list <- list()
pmuts <- c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,1)


for(j in 1:length(pmuts)){
	for(i in 1:ite){
		pmut <- pmuts[j]
		ind [i] <- tournament(nproblem,noff,pmut,mort)
		nqueens[i] <- nproblem
		mutate[i] <- pmut
		mort[i] <- mort
		print(i*100/ite)
	}
	d1 <- data.frame(ind,nqueens,mutate,mort)
	data.list[[j]] <- d1
}

dat.mut <- do.call(rbind, data.list)

ggplot(dat.mut, aes(x=as.factor(mutate), y = ind, fill = as.factor(mutate))) +
	geom_violin(bw=1000) +
	stat_summary(fun.y=mean, geom="point", size=2) +
	scale_fill_viridis(discrete=TRUE)



