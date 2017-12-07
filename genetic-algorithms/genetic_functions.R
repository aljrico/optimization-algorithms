# Functions needed to compute Genetic Algorithms --------------------------

# Find common pattern in parents (function)
noncommon <- function(par1,par2){
	noncommon <- which(!par1-par2 == 0)
}

# Generate offspring (function)
gen.offspring <- function(par1,par2, noff, pmut){
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

kill <- function(population,pmut,mort){
	newpopulation <- list()
	b <- length(population)
	a <- 1
	while(a < b & length(population)>1){
		w1 <- sample(population,1)[[1]]
		w2 <- sample(population,1)[[1]]

		newpopulation[[a]] <- fight(w1,w2,pmut,mort)
		a <- a+1
	}
	return(newpopulation)
}


fight <- function(w1, w2,pmut,mort){
	e1 <- meas.error(w1)
	e2 <- meas.error(w2)

	if(e1 > e2) {
		a <- runif(1)
		if(a < mort) vic <- w2
		else  vic <- gen.offspring (w1,w2,1,pmut)[[1]]
	}
	if(e1< e2){
		a <- runif(1)
		if(a< mort) vic <- w1
		else vic <- gen.offspring (w1,w2,1,pmut)[[1]]
	}
	if(e1 == e2) vic <- gen.offspring (w1,w2,1,pmut)[[1]]
	return(vic)
}
