
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

tourn.data$model <- rep("tourn",nrow(tourn.data))
allbut2.data$model <- rep("allbut2", nrow(allbut2.data))
half.data$model <- rep("half", nrow(half.data))

raw.data <- rbind(tourn.data, allbut2.data)
df <- as.data.frame(rbind(raw.data, half.data))



# Fancy Plots -------------------------------------------------------------


# Violin Plots ------------------------------------------------------------

df %>%
	filter(model == "allbut2") %>%
	ggplot(aes(x=as.factor(mutate), y = log(ind), fill = as.factor(mutate))) +
		geom_violin(bw=0.5, scale="area") +
		stat_summary(fun.y=mean, geom="point", size=2) +
		scale_fill_viridis(discrete=TRUE)

df %>%
	filter(model == "tourn") %>%
	ggplot(aes(x=as.factor(mutate), y = log(ind), fill = as.factor(mutate))) +
	geom_violin(bw=0.5, scale="area") +
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

# Points Plots ------------------------------------------------------------

df %>%
	filter(model == "allbut2") %>%
	ggplot(aes(x=as.factor(nqueens), y = log(ind), colour = as.factor(mutate))) +
	geom_jitter() +
	scale_colour_viridis(discrete=TRUE)

df %>%
	filter() %>%
	ggplot(aes(x=as.factor(nqueens), y = log(ind), colour = as.factor(model))) +
	geom_jitter() +
	scale_colour_viridis(discrete=TRUE)

df %>%
	filter(model == "half") %>%
	ggplot(aes(x=as.factor(nqueens), y = log(ind), colour = as.factor(mutate))) +
	geom_jitter() +
	scale_colour_viridis(discrete=TRUE)


# Histograms and Heatmaps--------------------------------------------------------------

df %>%
	filter(model == "half") %>%
	ggplot(aes(x=ind, fill = as.factor(nqueens))) +
	geom_histogram() +
	scale_fill_viridis(discrete=TRUE)

df %>%
	filter(model == "half") %>%
	ggplot(aes(x=nqueens,y=mutate)) +
	geom_tile(aes(fill=log(ind))) +
	scale_fill_viridis(discrete=FALSE)


df %>%
	filter() %>%
	ggplot(aes(x=nqueens,y=model)) +
	geom_tile(aes(fill=log(ind))) +
	scale_fill_viridis(discrete=FALSE)



