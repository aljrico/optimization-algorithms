
# Dummy function ----------------------------------------------------------

library(ggplot2)

source("genetic_functions.R")

df <- data.frame(1,1)
colnames(df) <- c("ind","nqueens")

nqueens <- c()
ind <- c()
for(k in 4:14){
	n <- k
	for(j in 1:200){
		i <- 1
		repeat{
			a <- sample(seq(1:n))
			err <- meas.error(a)
			if (err == 0) break
			i <- i+1
		}
		nqueens[j] <- k
		ind[j] <- i
	}
	d1 <- data.frame(ind,nqueens)
	df <- rbind(df, d1)
}

print(a)
print(i)

random <- as.data.frame(record)

df %>%
	filter() %>%
	ggplot(aes(x=as.factor(nqueens), y = log(ind), colour = as.factor(nqueens))) +
	geom_jitter() +
	scale_colour_viridis(discrete=TRUE)

df$model <- "random"

write.csv(df, file = "data/random-try.csv", row.names = FALSE)
