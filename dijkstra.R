
# Dijkstra Algorithm implementation ---------------------------------------
# Author: Alejandro Jiménez <aljrico@gmail.com>


# Libraries ---------------------------------------------------------------

library(dplyr)


# Define distance matrix --------------------------------------------------

S=matrix(999,5,5)
S[1,2]=2
S[1,3]=7
S[2,3]=3 #wip
S[2,4]=8
S[2,5]=5
S[3,2]=2 #wip
S[3,4]=1
S[4,5]=4
S[5,4]=5



# Input variables ---------------------------------------------------------

# Number of nodes
n=length(S[,1])

# Source node
v=1

# Distance matrix
cost=S

#create empty variables to store data
dest = numeric(n)
flag = numeric(n)
prev = numeric(n)
path = numeric()

for (i in 1:n){
	cost [i,i] <- 0 #= distance from one node to the same node is 0
	dest[i] = cost[v,i] #= distance from start node v to every other node i in the network
}

location <- v
while(length(path)<n){
	
	# Check lowest distance from actual node
	path <- path %>% append(location)
	dist.at.sight <- cost[path[length(path)],]
	next.n <- which.min(dist.at.sight)
	
	# Updating known distances
	for (j in 1:n){
		if (dist.at.sight[j] + dest[location]  < dest[j]) {dest[j] <- dist.at.sight[j] + dest[location]}
	}
	
	if (length(path)==n){break}
	
	# Move to the nearest non-visited node
	ind <- 0
	while(next.n %in% path){
		ind = ind +1
		next.n <- which(dist.at.sight == sort(dist.at.sight,partial=ind)[ind])
	}
	
	# Update our new position
	location <- next.n 
}
print(path)
print(dest)
