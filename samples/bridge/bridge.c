
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

//==============================================================================

#define IFILE 5

#if IFILE == 0
	char* filename_glbl = "samples/bridge/test-input-0.txt";
#elif IFILE == 1
	char* filename_glbl = "samples/bridge/test-input-1.txt";
#elif IFILE == 2
	char* filename_glbl = "samples/bridge/test-input-2.txt";
#elif IFILE == 3
	char* filename_glbl = "samples/bridge/input-0.txt";
#elif IFILE == 4
	char* filename_glbl = "samples/bridge/input-1.txt";
#elif IFILE == 5
	char* filename_glbl = "samples/bridge/input-2.txt";
#endif

#define NNAMES_CAP 4 * 1024  // 1493 for my input
#define NAME_CAP 8           // max name length

#define NADJ_CAP 16

char names_glbl[NNAMES_CAP][NAME_CAP];
//char* names_glbl[NNAMES_CAP];

// FIXME: remember to call memset() on any arrays declared here
int name_ids[NNAMES_CAP];
int left    [NNAMES_CAP];
int right   [NNAMES_CAP];

int num_nodes  = 0;

//==============================================================================

void println(char* str)
{
	// TODO: remove in final version.  Only to aid translation from syntran
	printf("%s", str);
	printf("\n");
}

//==============================================================================

void push_name(char* name_)
{
	//if (name_ == "") return;
	//printf("name_ = \"%s\"\n", name_);

	if (strlen(name_) >= NAME_CAP)
	{
		// Greater than *or equal* because we need 1 more char for null terminator
		println("\nError: NAME_CAP overflow!\n");
		exit(-1);
	}

	int i = 0;
	bool listed = false;
	bool done_search = false;
	while (!done_search)
	{
		if (i >= NNAMES_CAP)
		{
			println("\nError: NNAMES_CAP overflow!\n");
			exit(-1);
		}

		if (name_ids[i] < 0)
		{
			//names_glbl[i] = name_;
			strcpy(names_glbl[num_nodes], name_);
			name_ids[i] = num_nodes;
			done_search = true;
		}
		else
		{
			if (strcmp(name_, names_glbl[i]) == 0)
				listed = done_search = true;
			else if (strcmp(name_, names_glbl[i]) < 0)
			{
				// Iterate into left sub-tree
				if (left[i] < 0) left[i] = num_nodes;
				i = left[i];
			}
			else // greater than
			{
				// Iterate into right sub-tree
				if (right[i] < 0) right[i] = num_nodes;
				i = right[i];
			}
		}
	}

	if (!listed)
		num_nodes += 1;
}

//==============================================================================

int lookup_name(char* name_)
{
	int i = 0;
	bool done_search = false;
	while (!done_search)
	{
		if (i >= NNAMES_CAP)
		{
			println("\nError: NNAMES_CAP overflow!\n");
			exit(-1);
		}

		if (name_ids[i] < 0)
		{
			println("\nError: name lookup failed!\n");
			exit(-1);
		}
		else
		{
			if (strcmp(name_, names_glbl[i]) == 0)
				done_search = true;
			else if (strcmp(name_, names_glbl[i]) < 0)
				// Iterate into left sub-tree
				i = left[i];
			else // greater than
				// Iterate into right sub-tree
				i = right[i];
		}
	}
	return name_ids[i];
}

//==============================================================================

//let g_glbl = [0; 0, 0];
//int** g_glbl;
int* g_glbl;
int* num_adj_glbl;
void read_aoc_graph(char* filename)
{
	// First pass: save list of unique names
	printf("Reading graph from file ""%s"" ...\n", filename);
	println("1st pass ...");

	memset(name_ids, -1, sizeof(name_ids));
	memset(left    , -1, sizeof(left    ));
	memset(right   , -1, sizeof(right   ));

	FILE* f;
	uint MAX_LEN = 128;
	char str_[MAX_LEN];
	f = fopen(filename, "r");
	if (f == NULL)
	{
		perror("Error opening file");
		exit(-1);
	}
	char* dlms = ": \n";

	while (fgets(str_, MAX_LEN, f) != NULL)
	{
		//println(str_);
		char* substr = strtok(str_, dlms);
		while (substr != NULL)
		{
			push_name(substr);
			substr = strtok(NULL, dlms);
		}
	}
	//printf("Number of nodes = %d\n", num_nodes);
	//for (int i = 0; i < num_nodes; i++)
	//	printf("\"%s\"\n", names_glbl[i]);
	//printf("name_ids = \n");
	//for (int i = 0; i < num_nodes; i++)
	//	printf("%d  ", name_ids[i]);
	//printf("\n");

	// Second pass: save one-way directed connections as graph gdir
	println("2nd pass ...");

	//int gdir[num_nodes][NADJ_CAP];
	int gdir[num_nodes * NADJ_CAP];

	memset(gdir, -1, sizeof(gdir));

	int num_adjdir[num_nodes];
	memset(num_adjdir, 0, sizeof(num_adjdir));

	rewind(f);
	while (fgets(str_, MAX_LEN, f) != NULL)
	{
		//println(str_);
		char* substr = strtok(str_, dlms);
		int from = lookup_name(substr);
		//printf("name, id = %s, %d\n", substr, from);

		substr = strtok(NULL, dlms);
		while (substr != NULL)
		{
			int to = lookup_name(substr);
			//printf("to = %d\n", to);

			if (num_adjdir[from] > NADJ_CAP)
			{
				printf("\nError: NADJ_CAP overflow 1!\n");
				exit(-1);
			}

			//gdir[ num_adjdir[from], from ] = to;

			//gdir[from][ num_adjdir[from] ] = to;
			gdir[ from * NADJ_CAP + num_adjdir[from] ] = to;

			num_adjdir[from] += 1;

			substr = strtok(NULL, dlms);
		}
	}
	//printf("num_adjdir = \n");
	//for (int i = 0; i < num_nodes; i++)
	//	printf("%d  ", num_adjdir[i]);
	//printf("\n");

	//printf("gdir = \n");
	//for (int i = 0; i < num_nodes; i++)
	//{
	//	for (int j = 0; j < num_adjdir[i]; j++)
	//	{
	//		//printf("%d  ", gdir[i][j]);
	//		printf("%d  ", gdir[i * NADJ_CAP + j]);
	//	}
	//	printf("\n");
	//}
	//printf("\n");

	g_glbl = malloc(num_nodes * NADJ_CAP * sizeof(int));
	memcpy(g_glbl, gdir, sizeof(gdir));

	num_adj_glbl = malloc(num_nodes * sizeof(int));
	memcpy(num_adj_glbl, num_adjdir, sizeof(num_adjdir));

	// Double each edge to convert the directed graph to an undirected graph
	for (int i = 0; i < num_nodes; i++)
	for (int j = 0; j < num_adjdir[i]; j++)
	{
		int from = i;
		//int to   = gdir[j, i];
		int to   = gdir[i * NADJ_CAP + j];
		//printf("from, to = %d, %d\n", from, to);

		if (num_adj_glbl[to] > NADJ_CAP)
		{
			println("\nError: NADJ_CAP overflow 2!\n");
			exit(-1);
		}
		//g_glbl[ num_adj_glbl[to], to ] = from;
		g_glbl[ to * NADJ_CAP + num_adj_glbl[to] ] = from;

		num_adj_glbl[to] += 1;
	}

	//printf("num_adj_glbl = \n");
	//for (int i = 0; i < num_nodes; i++)
	//	printf("%d  ", num_adj_glbl[i]);
	//printf("\n");

	//printf("g_glbl = \n");
	//for (int i = 0; i < num_nodes; i++)
	//{
	//	for (int j = 0; j < num_adj_glbl[i]; j++)
	//		printf("%d  ", g_glbl[i * NADJ_CAP + j]);
	//	printf("\n");
	//}
	//printf("\n");

	fclose(f);
}

//==============================================================================

int min(int a, int b)
{
	return a < b ? a : b;
}

//********

int* get_bridge(int* g, int* num_adj, int nn)
{
	// Stack for iterative (non-recursive) depth-first search
	int STACK_CAP = 16 * nn;
	int* stack = malloc(STACK_CAP * sizeof(int));
	memset(stack, 0, sizeof(stack));
	int sp = -1; // stack "pointer"

	// Push node 0 as root
	stack[(sp += 1)] = 0;

	bool* visited = malloc(nn * sizeof(bool));
	bool* defer   = malloc(nn * sizeof(bool));
	int*  parent  = malloc(nn * sizeof(int));
	int*  low     = malloc(nn * sizeof(int));
	int*  dists   = malloc(nn * sizeof(int));

	memset(visited, false, nn * sizeof(bool));
	memset(defer  , false, nn * sizeof(bool));
	memset(parent , -1   , nn * sizeof(int));
	memset(low    , -1   , nn * sizeof(int));
	memset(dists  , -1   , nn * sizeof(int));

	int dist = 0;
	bool found = false;
	int* bridge = malloc(2 * sizeof(int)); bridge[0] = -1; bridge[1] = -1;
	while (sp >= 0 && !found)
	{
		int v = stack[(sp -= 1) + 1]; // pop
		if (!visited[v])
		{
			low[v]   = dist;
			dists[v] = dist;
			dist += 1;
		}
		visited[v] = true;

		for (int iw = 0; iw < num_adj[v]; iw++)
		{
			int w = g[v * NADJ_CAP + iw];
			if (w == parent[v])
			{
				// Do nothing
			}
			else if (visited[w])
				low[v] = min(low[v], dists[w]);
			else
			{
				parent[w] = v;

				// Re-push parent for deferred processing
				stack[(sp += 1)] = v;
				//check_stack_cap(sp, STACK_CAP); // TODO
				defer[v] = true;

				// Push child. order matters wrt parent
				stack[(sp += 1)] = w;
				//check_stack_cap(sp, STACK_CAP); // TODO
			}
		}

		if (defer[v])
		{
			for (int iw = 0; iw < num_adj[v]; iw++)
			{
				int w = g[v * NADJ_CAP + iw];
				if ((w != parent[v]) && visited[w])
				{
					low[v] = min(low[v], low[w]);
					if (low[w] > dists[v])
					{
						bridge[0] = v; bridge[1] = w;
						found = true;
					}
				}
			}
		}
	}
	return bridge;
}

//==============================================================================

int main()
{
	read_aoc_graph(filename_glbl);

	int* bridge = get_bridge(g_glbl, num_adj_glbl, num_nodes);
	if (bridge[0] < 0)
		println("No bridges were found");
	else
		printf("Found bridge between nodes [%d, %d] = [%s, %s]\n",
				bridge[0], bridge[1],
				names_glbl[bridge[0]], names_glbl[bridge[1]]);

	return 0;
}

/*

//==============================================================================

fn check_stack_cap(n: i32, cap: i64)
{
	if n >= cap
	{
		println("");
		println("Error: STACK_CAP overflow!");
		println("");
		exit(-1);
	}
}

//==============================================================================

*/

