#include "functions.h"


int main(int argc, char* argv[])
{
	int i=0,n=0;

	node *nodes;
	unsigned long nnodes = 23895681UL;
	nodes = (node *) malloc(nnodes*sizeof(node));

	read_csv(nodes, nnodes);
	write_binary(nodes, nnodes);

	return 0;
}
