# Functions needed to compute Genetic Algorithms

# Find common pattern in parents
def noncommon(par1,par2):
	test1 = np.array(par1)
	test2 = np.array(par2)
	return [dmi for dmi, dmj in zip(test1, test2) if dmi == dmj]

# Generate Offspring
def gen_offspring(par1,par2,noff,pmut):
	son = par1
	for j in range(0,noff-1):
		a = noncommon(par1,par2)
		parlist = [par1[k] for k in a]
		offseed = rndm.sample(parlist, len(parlist))
		randomdummy = np.random.uniform(0,1,1)
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
