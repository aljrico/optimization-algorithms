#include "functions.h"

int main(int argc, char* argv[])
{
	unsigned long ID_start = 240949599, ID_goal = 195977239;

	unsigned long nnodes = 23895681UL;
	unsigned long node_start,node_goal,node_current,node_successor;

	node *nodes;
	nodes = (node*)malloc(sizeof(node)*nnodes);

	AStarStatus *status;
	status = (AStarStatus *)malloc(sizeof(AStarStatus)*nnodes);


	read_binary(nodes);

	if((node_start=BinarySearch(ID_start,nodes,nnodes)) == ULONG_MAX) printf("Node start not found.\n");
	if((node_goal=BinarySearch(ID_goal,nodes,nnodes)) == ULONG_MAX) printf("Node goal not found.\n");

	AStar_algorithm(nodes, status, node_start, node_goal);
	print_path(nodes, status,node_start,node_goal);
}
