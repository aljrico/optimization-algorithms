# Functions needed to compute Genetic Algorithms --------------------------

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
meas.error <- function(subject){
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
