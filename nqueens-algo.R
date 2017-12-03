
# Genetic Models ----------------------------------------------------------

# N-Queen Problem using different Genetic Algorithms
# Author: Alejandro Jiménez Rico <aljrico@gmail.com>



# Libraries ---------------------------------------------------------------

library(reshape)
library(ggplot2)
library(viridis)
library(dplyr)


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


# Kill Half Model ---------------------------------------------------------
half <- function(nproblem,noff,pmut,mort){

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


# Kill all but two Model --------------------------------------------------
allbut2 <- function(nproblem,noff,pmut){
	# Load functions
	source("genetic_functions.R")

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
	noff <- nproblem*5

	# Initial parents
	par1 <- sample(seed)
	par2 <- sample(seed)

	# Count variables
	b <- 0
	ind <- 0

	# Creating further population
	repeat{
		# Measuring fitness of every child
		for (i in 1:noff){
			offspring[[i]] <- gen.offspring(par1,par2,noff, pmut)[[i]]
			subject <- offspring[[i]]
			fitness[i] <- (1/(1+meas.error(subject)))
		}

		ind <- ind + length(offspring)

		# Selecting most fittest childs as new parents
		par1 <- offspring[sort(-fitness,index.return=TRUE)[[2]][1]][[1]]
		par2 <- offspring[sort(-fitness,index.return=TRUE)[[2]][2]][[1]]
		max.fitness <- append(max.fitness, max(fitness))

		b <- b+1

		if (max(fitness)== nproblem*(nproblem-1)) break
		if (b > 5000) break
		if (max(fitness) == 1) break
	}
	return("ind" = ind)
}




# Evaluation --------------------------------------------------------------

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
ite <- 10
data.list <- list()
pmuts <- c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,1)
lnq <- c(6,8,9,10,11,12)



# Evaluation of Tournament Model ------------------------------------------

for(k in 1:length(lnq))	{
	nproblem <- lnq[k]
	for(j in 1:length(pmuts)){
		for(i in 1:ite){
			pmut <- pmuts[j]
			ind [i] <- tournament(nproblem,noff,pmut,mort)
			nqueens[i] <- nproblem
			mutate[i] <- pmut
			model = "tourn"
			print("tourn")
			print(i)
			print(nproblem)
			print(pmut)
		}
		d1 <- data.frame(ind,nqueens,mutate,model)
		data.list[[j + length(pmuts)*(k-1)]] <- d1
	}
}

dat.tourn <- do.call(rbind, data.list)

ggplot(dat.tourn, aes(x=as.factor(mutate), y = ind, fill = as.factor(mutate))) +
	geom_violin(bw=1000, scale="area") +
	stat_summary(fun.y=mean, geom="point", size=2) +
	scale_fill_viridis(discrete=TRUE)

ggplot(dat.tourn, aes(x=as.factor(nqueens), y = ind, colour = as.factor(mutate))) +
	geom_jitter() +
	scale_colour_viridis(discrete=TRUE)


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

ggplot(dat.half, aes(x=as.factor(mutate), y = ind, fill = as.factor(mutate))) +
	geom_violin(bw=8, scale="area") +
	stat_summary(fun.y=mean, geom="point", size=2) +
	scale_fill_viridis(discrete=TRUE)

ggplot(dat.half, aes(x=as.factor(nqueens), y = ind, colour = as.factor(mutate))) +
	geom_jitter() +
	scale_colour_viridis(discrete=TRUE)

# Evaluation of Kill All But Two Model ------------------------------------------------

pmuts <- c(0.1,0.25,0.5,0.75,0.9,1)

for(k in 1:length(lnq))	{
	nproblem <- lnq[k]
	for(j in 1:length(pmuts)){
		for(i in 1:ite){
			pmut <- pmuts[j]
			ind[i] <- allbut2(nproblem,noff,pmut)
			nqueens[i] <- nproblem
			mutate[i] <- pmut
			model <- "allbut2"
			print("allbut2")
			print(i)
			print(nproblem)
			print(pmut)
		}
		d1 <- data.frame(ind,nqueens,mutate, model)
		data.list[[j + length(pmuts)*(k-1)]] <- d1
	}
}

dat.allbut2 <- do.call(rbind, data.list)




# Save data
write.csv(dat.allbut2, file = "allbut2.csv",row.names = FALSE)
write.csv(dat.half, file = "half.csv",row.names=FALSE)
write.csv(dat.tourn, file = "tourn.csv", row.names=FALSE)


