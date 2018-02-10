#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>
#include <limits.h>
#include <float.h>

typedef struct {
	unsigned long id; // Node identification
	unsigned short namelen;
	char *name;
	double lat, lon; // Node position
	unsigned short nsucc; // Number of node successors; i. e. length of successors
	unsigned long *successors;
} node;

typedef char Queue;
	enum whichQueue {NONE, OPEN, CLOSED};

typedef struct {
	double g, h;
	unsigned long parent;
	Queue whq;
} AStarStatus;

typedef struct node{
	double f;
	unsigned long index;
	struct node * next;
} node_t;

void push(node_t ** head, double f,unsigned long index){
	node_t * new_node;
	new_node = malloc(sizeof(node_t));

	new_node->f = f;
	new_node->index = index;
	new_node->next = *head;
	*head = new_node;
}

int remove_first(node_t ** head){
	node_t * next_node = NULL;

	if(*head == NULL){
		return -1;}

	next_node = (*head)->next;
	free(*head);
	*head= next_node;
	return 1;
}

int remove_by_index(node_t ** head,unsigned long index){
	node_t * current = *head;
	if(current == NULL) return -1;
	node_t * temp_node = NULL;

	if(index == current->index){
		remove_first(head);
		return 1;
	}

	while(current->next->index != index){
		current = current->next;
		if(current == NULL) return -1;
	}

	temp_node = current->next;
	current->next = temp_node->next;
	free(temp_node);
	return 1;
}

unsigned long index_minimum(node_t * head){
	node_t * current = head;
	if(current == NULL) return ULONG_MAX;
	node_t * minimum = NULL;
	double min = 9999999;

	while(current != NULL){
		if(current->f < min){
			minimum=current;
			min = current->f;
		}
		current = current->next;
	}
	return minimum->index;
}

unsigned long BinarySearch(unsigned long key, node *nodes, unsigned long lenlist)
{
	register unsigned long start=0UL, afterend=lenlist, middle;
	register unsigned long try;
	while(afterend > start)
		{
			middle = start + ((afterend-start-1)>>1); try = nodes[middle].id;
			if (key == try) return middle;
			else if ( key > try ) start=middle+1;
			else afterend=middle;
		}
	return ULONG_MAX;
}

double distance(node * nodes, unsigned long node_start, unsigned long node_goal)
{
	double R=6371000;
	double lat1=nodes[node_start].lat*(M_PI/180);
	double lat2=nodes[node_goal].lat*(M_PI/180);
	double lon1=nodes[node_start].lon*(M_PI/180);
	double lon2=nodes[node_goal].lon*(M_PI/180);
	double d_lat=lat2-lat1;
	double d_lon=lon2-lon1;
	double a = sin(d_lat/2)*sin(d_lat/2) + cos(lat1)*cos(lat2)*sin(d_lon/2)*sin(d_lon/2);
	double c = 2*atan2(sqrt(a),sqrt(1-a));
	return R*c;
}


void read_csv(node * nodes, unsigned long nnodes) {

	char *buffer, *field;
	size_t bufsize = 79857; //Maximum length of a line
	size_t bytes_read;
	buffer = (char *)malloc(sizeof(char) * bufsize);
	field = (char *)malloc(sizeof(char) * 184); //Maximum length of a field

	unsigned long nways = 1417363UL;
	unsigned long *way;
	way = (unsigned long *)malloc(sizeof(unsigned long) * 5306); //Maximum number of fields

	FILE *file;
	file = fopen("spa-nodes.csv", "r");

	int i, j, n, l;

	printf("Starting to read nodes.\n");
	while ((bytes_read = getline(&buffer, &bufsize, file)) != -1)
	{
		field = strsep(&buffer, "|");
		nodes[i].id = strtoul(field, NULL, 10);
		field = strsep(&buffer, "|");
		l = strlen(field);
		if (l>0) {
			nodes[i].namelen = l;
			nodes[i].name = (char *)malloc(l * sizeof(char)+1);
			strcpy(nodes[i].name, field);
		}
		field = strsep(&buffer, "|");
		nodes[i].lat = atof(field);
		field = strsep(&buffer, "|");
		nodes[i].lon = atof(field);
		i++;
		if (i % 500000 == 0) {
			double x = ((100 * (double)i) / (double)nnodes);
			printf("%.2f percent of the nodes have been read.\n", x);
		}

	}
	i = 0;
	printf("100 percent of the nodes have been read.\n");
	fclose(file);

	file = fopen("spa-ways.csv", "r");

	unsigned long a, b, A, B, member, waylen, index;
	bool oneway;

	for (i = 0; i<nnodes; i++)
	{
		if ((nodes[i].successors = (unsigned long *)malloc(sizeof(unsigned long) * 16)) == NULL) { //Maximum number of successors
			printf("Memory allocation for the successors failed at node %d\n", i);}
	}
	printf("Starting to read ways.\n");
	while ((bytes_read = getline(&buffer, &bufsize, file)) != -1)
	{
		j++;
		field = strsep(&buffer, "|");
		if (strlen(field)>0) oneway = 1;
		else oneway = 0;
		while ((field = strsep(&buffer, "|")) != NULL)
		{
			member = strtoul(field, '\0', 10);
			if ((index = BinarySearch(member, nodes, nnodes)) != ULONG_MAX)
			{
				way[n] = index;
				n++;
			}
		}
		waylen = n;
		n = 0;

		for (i = 0; i<waylen - 1; i++)
		{
			A = way[i];
			B = way[i + 1];

			nodes[A].successors[nodes[A].nsucc] = B;
			nodes[A].nsucc++;

			if (oneway == 0)
			{
				nodes[B].successors[nodes[B].nsucc] = A;
				nodes[B].nsucc++;
			}
		}
		if (j % 500000 == 0) {
			double x = ((100 * (double)j) / (double)nways);
			printf("%.2f percent of the ways have been read.\n", x);
		}
	}

	printf("100 percent of the ways have been read.\n");
	fclose(file);
	free(buffer); free(field);
}

void write_binary(node * nodes, unsigned long nnodes) {

	int i;
	unsigned long ntotnsucc = 0UL;
	for (i = 0; i < nnodes; i++) ntotnsucc += nodes[i].nsucc;

	unsigned long ntotchar = 0UL;
	for (i = 0; i<nnodes; i++) ntotchar += nodes[i].namelen;

	printf("Starting to write the bin file.\n");

	FILE * file;
	if ((file = fopen("spa-data.bin", "wb")) == NULL) printf("The output binary data file cannot be opened.\n");

	// Global data --- header
	if (fwrite(&nnodes, sizeof(unsigned long), 1, file) +
		fwrite(&ntotnsucc, sizeof(unsigned long), 1, file) +
		fwrite(&ntotchar, sizeof(unsigned long), 1, file) != 3)
		printf("Error when initializing the output binary data file.\n");

	// Writing all nodes
	if (fwrite(nodes, sizeof(node), nnodes, file) != nnodes)
		printf("Error when writing nodes to the output binary data file.\n");

	// Writing successors in blocks
	for (i = 0; i < nnodes; i++) if (nodes[i].nsucc) {
		if (fwrite(nodes[i].successors, sizeof(unsigned long), nodes[i].nsucc, file) != nodes[i].nsucc)
			printf("Error when writing edges to the output binary data file.\n");
	}

	// Writing names in blocks
	for (i = 0; i < nnodes; i++) if (nodes[i].namelen) {
		if (fwrite(nodes[i].name, sizeof(char), nodes[i].namelen, file) != nodes[i].namelen)
			printf("Error when writing names to the output binary data file.\n");
	}

	fclose(file);
}

void read_binary(node * nodes) {

	int i;
	unsigned long nnodes,ntotnsucc, ntotchar,num;
	unsigned long *allsuccessors;
	char *allnames;

	FILE *file;

	if ((file = fopen("spa-data.bin", "r")) == NULL) printf("the data file does not exist or cannot be opened\n");

	/* Global data --- header */
	if (fread(&nnodes, sizeof(unsigned long), 1, file) +
		fread(&ntotnsucc, sizeof(unsigned long), 1, file) +
		fread(&ntotchar, sizeof(unsigned long), 1, file) != 3) printf("when reading the header of the binary data file\n");

	/* getting memory for all data */
	//if ((nodes = (node *)malloc(nnodes * sizeof(node))) == NULL) printf("when allocating memory for the nodes vector\n");
	if ((allsuccessors = (unsigned long *)malloc(ntotnsucc * sizeof(unsigned long))) == NULL) printf("when allocating memory for the edges vector\n");
	if ((allnames = (char *)malloc(ntotchar * sizeof(char))) == NULL) printf("when allocating memory for the names\n");

	/* Reading all data from file */
	if (fread(nodes, sizeof(node), nnodes, file) != nnodes) printf("when reading nodes from the binary data file\n");
	if (fread(allsuccessors, sizeof(unsigned long), ntotnsucc, file) != ntotnsucc) printf("when reading successors from the binary data file\n");
	if (fread(allnames, sizeof(char), ntotchar, file) != ntotchar) printf("when reading names from the binary data file\n");

	fclose(file);

	//Setting pointers to successors
	for (i = 0; i < nnodes; i++) {
		if (nodes[i].nsucc) {
			nodes[i].successors = allsuccessors;
			allsuccessors += nodes[i].nsucc;
		}
	}

	//Setting pointers to names
	for (i = 0; i < nnodes; i++) {
		if (nodes[i].namelen) {
			nodes[i].name = allnames;
			allnames += nodes[i].namelen+1;
		}
	}
}

void AStar_algorithm(node * nodes, AStarStatus * status, unsigned long node_start, unsigned long node_goal) {

	int i;
	unsigned long node_current, node_successor;


	node_t * open_list = NULL;

	status[node_start].g = 0;
	status[node_start].h = distance(nodes, node_start, node_goal);
	status[node_start].whq = OPEN;
	push(&open_list, status[node_start].h, node_start);

	double successor_current_cost;


	while ((node_current = index_minimum(open_list)) != ULONG_MAX) {

		if (node_current == node_goal) break;

		for (i = 0; i<nodes[node_current].nsucc; i++) {

			node_successor = nodes[node_current].successors[i];
			successor_current_cost = status[node_current].g + distance(nodes, node_current, node_successor);

			if (status[node_successor].whq == OPEN) {
				if (status[node_successor].g <= successor_current_cost) continue;
			}

			else if (status[node_successor].whq == CLOSED) {
				if (status[node_successor].g <= successor_current_cost) continue;

				status[node_successor].whq = OPEN;
				push(&open_list, successor_current_cost + status[node_successor].h, node_successor);
			}

			else {
				status[node_successor].whq = OPEN;
				status[node_successor].h = distance(nodes, node_successor, node_goal);
				push(&open_list, (successor_current_cost + status[node_successor].h), node_successor);
			}

			status[node_successor].g = successor_current_cost;
			status[node_successor].parent = node_current;
		}
		status[node_current].whq = CLOSED;

		if ((remove_by_index(&open_list, node_current)) != 1) printf("Remove failed.\n");
	}
	if (node_current != node_goal) printf("OPEN list is empty.\n");
}

void print_path(node * nodes, AStarStatus * status, unsigned long node_start, unsigned long node_goal) {

	int i=0,j;
	unsigned long * path;
	unsigned long next_node;
	next_node = node_goal;
	path = (unsigned long *)malloc(66536 * sizeof(unsigned long));
	path[0] = node_goal;
	while (next_node != node_start) {
		i++;
		next_node = status[next_node].parent;
		path[i] = next_node;
	}
	int path_length = i + 1;

	for (i = path_length - 1; i >= 0; i--) {
		//printf("Node id: %lu,  Distance: %.2f m, Name: %s\n", nodes[path[i]].id, status[path[i]].g,nodes[path[i]].name);
        printf("Node id: %lu,  Distance: %.2f m, Name: ", nodes[path[i]].id, status[path[i]].g);
        for(j=0;j<nodes[i].namelen;j++){
        printf ("%c", nodes[i].name[j]);}
        printf ("\n");
	}
}
