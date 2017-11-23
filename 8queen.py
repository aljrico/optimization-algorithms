import numpy as np
import random as rndm


# exec(open("genfunctions.py").read())

# Functions needed to compute Genetic Algorithms

# Find common pattern in parents
def noncommon(par1,par2):
	l = []
	p = []
	l.append(par1)
	l.append(par2)
	for i,j in enumerate(zip(*l)):
		if all(j[0] !=k for k in j[1:]):
			p.append(i)
	return p

# Generate Offspring
def gen_offspring(par1,par2,noff,pmut):
	son = par1
	for j in range(0,noff-1):
		a = list(noncommon(par1,par2))
		pardum = np.array(par1)
		parlist = list(pardum[a])
		offseed = rndm.sample(parlist, len(parlist))
		randomdummy = np.random.uniform(0,1,1)
		a =set(a)
		if(randomdummy <= (1-pmut)):
			for i in range(0,nproblem):
				if(i in a):
					son[i] = offseed[0]
					offseed.pop(0)
				else:
					son[i] = par1[i]
		else:
			son = rndm.sample(par1,len(par1))
		offspring.append(son)
	return offspring


# Measuring Fitness
def meas_error(subject):
	error = 0
	for i in range(0,len(subject)-1):
		x = i+1
		y = subject[i]
		for j in range(0, len(subject)):
			dx = j+1
			dy = subject[j]
			if((dx-x)^2 == (dy-y)^2):
				error = error + 1
	return error


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
	c= rndm.sample(c,nproblem)
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
			nerrors = meas_error(subject)
			fitness.append(1/(1+nerrors))
			m = m + 1

	# Performance Measures
	av_fitness.append(np.mean(fitness))
	max_fitness.append(max(fitness))

	# Get the fittest individuals
	bestguy = offspring[fitness.index(max(fitness))]
	if (max(fitness) == 1):
		b = 99999999

	# Kill all Parents
	population = []

	# Populating the new world
	a = round(len(offspring)*(1-mort)*0.5)*2
	k = 1
	while (len(offspring) != 0 and len(fitness) != 0):
		maxloc = fitness.index(max(fitness))
		population.append(offspring[maxloc])
		fitness.pop(maxloc)
		offspring.pop(maxloc)
		k = k +1
		if(k>a):
			break
	b = b+1

	#Kill all children not fitted enough to get to the population list
	offspring = []

	if(len(population)<2):
		print("EXCTINCTION!!")
		break

print(bestguy)
print(ind)

