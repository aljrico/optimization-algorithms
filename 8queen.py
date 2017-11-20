import numpy as np
import random as rndm

execfile("genfunctions.py")

# Definiton of variables
noff = 5 # Number of toddles by couple
nproblem = 8 # Magnitutde of the Problem
pmut = 0.1 # Probability of Mutation
mort = 0.5 # Mortality

# Lists to be filled
offspring = []
population = []
son = []
fitness = []
av_fitness = []
max_fitness = []

# Count variables
b = 0
ind = 0

# Initial Population
for i in range(1, nproblem):
	c = list(range(1, nproblem+1))
	c= rndm.sample(c,8)
	population.append(c)

# Reproduction
while (b < 10000):
	m = 0
	ind = ind + len(population)
	while ( len(population)>1):
		par1 = population[0]
		population.pop(0)

		par2 = population[0]
		population.pop(0)

		# Measuring fitness of every child
		for l in range(0,noff-1):
			a = gen_offspring(par1,par2,noff,pmut)
			offspring.append(a[l])
			subject = a[l]
			fitness.append = (1/(1+meas_error(subject)))
			m = m + 1

	# Performance Measures
	av_fitness.append(np.mean(fitness))
	max_fitness.append(max(fitness))

	# Get the fittest individuals
	bestguy = offspring[fitness.index(max(fitness))]
	if (max(fitness) == 1):
		break

