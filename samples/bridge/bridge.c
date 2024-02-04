
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

//==============================================================================

#define IFILE 0

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
	printf("name_ = \"%s\"\n", name_);

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

//let g_glbl = [0; 0, 0];
//let num_adj_glbl = [0; 0];
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
	while (fgets(str_, MAX_LEN, f) != NULL)
	{
		//println(str_);
		char* dlms = ": \n";
		char* substr = strtok(str_, dlms);
		while (substr != NULL)
		{
			push_name(substr);
			substr = strtok(NULL, dlms);
		}
	}
	printf("Number of nodes = %d\n", num_nodes);
	for (int i = 0; i < num_nodes; i++)
		printf("\"%s\"\n", names_glbl[i]);
	printf("name_ids = \n");
	for (int i = 0; i < num_nodes; i++)
		printf("%d  ", name_ids[i]);
	printf("\n");

	fclose(f);

	/*

	// Second pass: save one-way directed connections as graph gdir
	//
	// TODO: 2nd pass could probably be optimized by not using lookup_name().
	// Need to return id from push_name() and save it somewhere in 1st pass
	let NADJ_CAP = 16;
	let gdir = [-1; NADJ_CAP, num_nodes];
	let num_adjdir = [0; num_nodes];
	close(f);
	println("2nd pass ...");
	f = open(filename);
	str_ = readln(f);
	while not eof(f)
	{
		//println("str_ = ", str_);

		let strs = split_(str_, ": ");
		let from = lookup_name(strs[0]);
		for i in [1: size(strs, 0)]
		{
			let to = lookup_name(strs[i]);

			if num_adjdir[from] > NADJ_CAP
			{
				println("");
				println("Error: NADJ_CAP overflow 1!");
				println("");
				exit(-1);
			}

			gdir[ num_adjdir[from], from ] = to;
			num_adjdir[from] += 1;
		}

		str_ = readln(f);
	}
	//println("gdir = ", gdir);

	g_glbl = gdir;
	num_adj_glbl = num_adjdir;

	// Double each edge to convert the directed graph to an undirected graph
	for i in [0: num_nodes]
	for j in [0: num_adjdir[i]]
	{
		let from = i;
		let to   = gdir[j, i];
		if num_adj_glbl[to] > NADJ_CAP
		{
			println("");
			println("Error: NADJ_CAP overflow 2!");
			println("");
			exit(-1);
		}
		g_glbl[ num_adj_glbl[to], to ] = from;
		num_adj_glbl[to] += 1;
	}
	//println("g_glbl = ", g_glbl);
	//println("num_adj_glbl = ", num_adj_glbl);
	*/
}

//==============================================================================

int main()
{
	println("");
	println("Starting C Tarjan's bridge sample");

	read_aoc_graph(filename_glbl);

	println("Running C Tarjan's bridge algorithm ...");
	//let bridge = get_bridge(g_glbl, num_adj_glbl);
	//if bridge[0] < 0
	//	println("No bridges were found");
	//else
	//{
	//	println("Found bridge between nodes ", bridge, " = ",
	//			[names_glbl[bridge[0]], names_glbl[bridge[1]]]);
	//}

	println("Ending C Tarjan's bridge sample");
	println("");
	return 0;
}

/*

fn lookup_name(name_: str): i32
{
	let i = 0;
	let done_search = false;
	while not done_search
	{
		if i >= NNAMES_CAP
		{
			println("");
			println("Error: NNAMES_CAP overflow!");
			println("");
			exit(-1);
		}

		if name_ids[i] < 0
		{
			println("");
			println("Error: name lookup failed!");
			println("");
			exit(-1);
		}
		else
		{
			if names_glbl[i] == name_
				done_search = true;
			else if is_str_lt(name_, names_glbl[i])
				// Iterate into left sub-tree
				i = left[i];
			else // greater than
				// Iterate into right sub-tree
				i = right[i];
		}
	}
	let ans = name_ids[i];
}

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

fn get_bridge(g: [i32; :, :], num_adj: [i32; :]): [i32; :]
{
	// Search for a bridge in graph g in the component connected to node 0.
	// Return the pair of nodes that make up the first bridge found, or [-1, -1]
	// if no bridges are found

	let nn = size(num_adj, 0);

	// Stack for iterative (non-recursive) depth-first search
	let STACK_CAP = 16 * nn;
	let stack = [0; STACK_CAP];
	let sp = -1; // stack "pointer"

	// Push node 0 as root
	stack[(sp += 1)] = 0;

	let visited = [false; nn];
	let defer   = [false; nn];
	let parent  = [-1   ; nn];
	let low	    = [-1   ; nn];
	let dists   = [-1   ; nn];

	let dist = 0;
	let found = false;
	let bridge = [-1, -1];
	while sp >= 0 and not found
	{
		let v = stack[(sp -= 1) + 1]; // pop
		if not visited[v]
		{
			low[v]   = dist;
			dists[v] = dist;
			dist += 1;
		}
		visited[v] = true;

		for iw in [0: num_adj[v]]
		{
			let w = g[iw, v];
			if w == parent[v]
			{
				// Do nothing
			}
			else if visited[w]
			{
				low[v] = min(low[v], dists[w]);
			}
			else
			{
				parent[w] = v;

				stack[(sp += 1)] = v;  // re-push parent for deferred processing
				check_stack_cap(sp, STACK_CAP);
				defer[v] = true;

				stack[(sp += 1)] = w; // push child. order matters wrt parent
				check_stack_cap(sp, STACK_CAP);
			}
		}

		if defer[v]
		{
			for iw in [0: num_adj[v]]
			{
				let w = g[iw, v];
				if w != parent[v] and visited[w]
				{
					low[v] = min(low[v], low[w]);
					if (low[w] > dists[v])
					{
						//println("Found bridge = ", [v, w], " = ", [names_glbl[v], names_glbl[w]]);
						bridge = [v, w];
						found = true;
					}
				}
			}
		}
	}
	//println("visited = ", visited);
	//println("parent = ", parent);
	//println("dists    = ", dists);
	//println("low  = ", low);

	let ans = bridge;
}

//==============================================================================

*/

