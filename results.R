
# Analysis of Genetic Algorithms Data -------------------------------------
# Author: Alejandro Jiménez Rico <aljrico@gmail.com>


# Libraries ---------------------------------------------------------------

library(reshape)
library(ggplot2)
library(viridis)
library(dplyr)
library(data.table)


# Load Data ---------------------------------------------------------------

tourn.data <- fread("data/tourn.csv")
allbut2.data <- fread("data/allbut2.csv")
half.data <- fread("data/half.csv")
random.data <- fread("data/random-try.csv")

tourn.data$model <- rep("tourn",nrow(tourn.data))
allbut2.data$model <- rep("allbut2", nrow(allbut2.data))
half.data$model <- rep("half", nrow(half.data))
random.data$mutate <- 1
random.data <- random.data[random.data$nqueens !=1,]

f.half <- fread("data/half-further.csv")

raw.data <- rbind(tourn.data, allbut2.data)
raw.data <- rbind(raw.data, random.data)
df <- as.data.frame(rbind(raw.data, half.data))

#df <- as.data.frame(f.half)



# Fancy Plots -------------------------------------------------------------


# Violin Plots ------------------------------------------------------------

df %>%
	filter() %>%
	filter(mutate != 0.01 & mutate != 0.05) %>%
	ggplot(aes(x=as.factor(mutate), y = log(ind), fill = as.factor(mutate))) +
	geom_violin(bw=0.5, scale="area") +
	stat_summary(fun.y=mean, geom="point", size=2) +
	scale_fill_viridis(discrete=TRUE)


df %>%
	filter(model == "allbut2") %>%
	filter(mutate != 0.01 & mutate != 0.05) %>%
	ggplot(aes(x=as.factor(mutate), y = log(ind), fill = as.factor(mutate))) +
		geom_violin(bw=0.5, scale="area") +
		stat_summary(fun.y=mean, geom="point", size=2) +
		scale_fill_viridis(discrete=TRUE)

df %>%
	filter(model == "tourn") %>%
	ggplot(aes(x=as.factor(mutate), y = log(ind), fill = as.factor(mutate))) +
	geom_violin(bw=0.15, scale="area") +
	stat_summary(fun.y=mean, geom="point", size=2) +
	scale_fill_viridis(discrete=TRUE)

df %>%
	filter(model == "half") %>%
	ggplot(aes(x=as.factor(mutate), y = log(ind), fill = as.factor(mutate))) +
	geom_violin(bw=0.05, scale="area") +
	stat_summary(fun.y=mean, geom="point", size=2) +
	scale_fill_viridis(discrete=TRUE)

df %>%
	filter(model != "allbut2") %>%
	ggplot(aes(x=as.factor(mutate), y = log(ind), fill = as.factor(model))) +
	geom_violin(bw=0.25, scale="area") +
	scale_fill_viridis(discrete=TRUE)

df %>%
	filter(mutate != 0.01 & mutate != 0.05) %>%
	ggplot(aes(x=as.factor(mutate), y = log(ind), fill = as.factor(model))) +
	geom_violin(bw=1, scale="area") +
	scale_fill_viridis(discrete=TRUE)

df %>%
	filter(mutate != 0.01 & mutate != 0.05) %>%
	ggplot(aes(x=as.factor(nqueens), y = log(ind), fill = as.factor(model))) +
	geom_violin(bw=0.45, scale="area") +
	scale_fill_viridis(discrete=TRUE)+
	labs(x = "Number of Queens", y = "Log(Individuals)", fill = "Model used") +
	theme(panel.grid.major = element_line(colour="grey"))

df %>%
	filter(model == "half") %>%
	ggplot(aes(x=as.factor(nqueens), y = log(ind), fill = as.factor(nqueens))) +
	geom_violin(bw=0.085, scale="area") +
	scale_fill_viridis(discrete=TRUE)

df %>% # Used
	filter(model != "random") %>%
	ggplot(aes(x=as.factor(nqueens), y = log(ind), fill = as.factor(nqueens))) +
	geom_violin(bw=0.2, scale="area") +
	scale_fill_viridis(discrete=TRUE) +
	labs(x = "Number of Queens", y = "Log(Individuals)") +
	theme_bw() +
	theme(legend.position="none")

df %>%
	filter(nqueens != 13 & nqueens != 14 & nqueens > 3) %>%
	filter(mutate !=0.01 & mutate !=0.05) %>%
	ggplot(aes(x=as.factor(nqueens), y = log(ind), fill = as.factor(model))) +
	geom_violin(bw = 1) +
	scale_fill_viridis(discrete=TRUE) +
	labs(x = "Number of Queens", y = "Log(Individuals)", fill = "Model") +
	theme_bw()

df %>% # Used
	filter(model == "random") %>%
	ggplot(aes(x=as.factor(nqueens), y = log(ind), fill = as.factor(nqueens))) +
	geom_violin(bw=0.25) +
	scale_fill_viridis(discrete=TRUE, option = "B", begin=0.1,end =0.9) +
	labs(x = "Number of Queens", y = "Log(Individuals)") +
	theme_bw() +
	theme(legend.position = "none")


# Points Plots ------------------------------------------------------------

df %>%
	filter() %>%
	ggplot(aes(x=as.factor(mutate), y = log(ind), colour = as.factor(model))) +
	geom_jitter() +
	scale_colour_viridis(discrete=TRUE) +
	labs(x = "Probabilty of Mutation", y = "Log(Individuals)", colour = "Model") +
	theme(panel.grid.major = element_line(colour="grey"))

df %>% # Used
	filter(model != "random") %>%
	ggplot(aes(x=as.factor(nqueens), y = log(ind), colour = as.factor(model))) +
	geom_jitter() +
	scale_colour_viridis(discrete=TRUE) +
	labs(x = "Number of Queens", y = "Log(Individuals)", colour = "Model") +
	theme_bw()

# Histograms and Heatmaps--------------------------------------------------------------

df %>%
	filter() %>%
	ggplot(aes(x=log(ind), fill = as.factor(nqueens))) +
	geom_histogram() +
	scale_fill_viridis(discrete=TRUE)

df %>% # Used
	filter(model != "random") %>%
	ggplot(aes(x=as.factor(nqueens),y=as.factor(mutate))) +
	geom_tile(aes(fill=log(ind))) +
	facet_wrap(~model) +
	scale_fill_viridis(discrete=FALSE) +
	labs(x = "Number of Queens", y = "Mutation Probability", fill = "Log(Individuals)") +
	theme_bw()

df %>%
	filter(mutate != 0.01 & mutate != 0.05) %>%
	ggplot(aes(x=as.factor(nqueens),y=model)) +
	geom_tile(aes(fill=log(ind))) +
	scale_fill_viridis(discrete=FALSE)

df %>%
	filter() %>%
	ggplot(aes(x=as.factor(nqueens),y=as.factor(mutate))) +
	geom_tile(aes(fill=log(ind))) +
	scale_fill_viridis(discrete=FALSE)




# Just for Half -----------------------------------------------------------

df %>% # Used
	filter() %>%
	ggplot(aes(x=as.factor(nqueens), y = log(ind), colour = as.factor(mutate))) +
	geom_jitter() +
	scale_colour_viridis(discrete=TRUE) +
	labs(x = "Number of Queens", y = "Number of Individuals") +
	theme(panel.grid.major = element_line(colour="grey"))

