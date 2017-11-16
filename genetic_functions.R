# Functions needed to compute Genetic Algorithms --------------------------

# Find common pattern in parents (function)
noncommon <- function(par1,par2){
	noncommon <- which(!par1-par2 == 0)
}

# Generate offspring (function)
gen.offspring <- function(par1,par2, noff,nproblem, pmut){
	for (j in 1:noff){
		offseed <- sample(par1[noncommon(par1,par2)])
		randomdummy <- runif(1)
		if (randomdummy<=(1-pmut)){
			for (i in 1:nproblem){

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
			if (i !=j){
				dx <- j
				dy <- subject[j]
				if (abs(dx-x)==abs(dy-y)){error = error+1}
			}

		}
	}
	return(error)
}

# Dummy crossover
gen.offspring2 <- function(par1,par2, noff,nproblem, pmut){
	i <- 1
	repeat{
		np <-  length(par1)
		nc <- sample(seq(1:np), 3)
		rpar1 <- par1[-nc]
		rpar2 <- par2[-nc]
		rndm1 <- runif(1)
		rndm2 <- runif(1)

		if(rndm1>=0.5){
			for(j in 1:np){
				if (j %in% nc) {son[j] <- par1[nc[1]]; nc[-1]}
				else {son[j] <- sample(rpar1, 1); rpar1[-1]}
			}
		}
		if(rndm1<0.5){
			for(j in 1:np){
				if (j %in% nc) {son[j] <- par2[nc[1]]; nc[-1]}
				else {son[j] <- sample(rpar2, 1); rpar2[-1]}
			}
		}
		offspring[[i]] <- son
		i <- i + 1
		if (length(offspring) >= noff) break
	}
	return (offspring)
}
