#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <sys/time.h>
#include <ctype.h>

typedef enum 
{
	OP_INSERT = 0,
	OP_DELETE = 1,
	OP_REPLACE = 2,
	OP_TWIDDLE = 3,
	OP_MATCH = 4,
	OP_COUNT = 5
} edit_operation_type_t;

typedef struct
{
	int cost[OP_COUNT];
} editcosts_t;

typedef struct
{
	edit_operation_type_t type;
	int parent;
	int ed;
} edit_operations_t;

//Map (0,0) (M,N) to a single integer between 0 and ((M+1)*(N+1)) - 1.
inline int INDEX(int i, int j, int N)
{
	return (i*(N+1) + j);
}

//The inverse of the above mapping.
inline void REVERSE_INDEX(int k, int N, int& i, int& j)
{
	i = k / (N+1);
	j = k % (N+1);
}

void printactions(const edit_operations_t* op, int index, const int N)
{
	const char* messages[] = { "INSERT", "DELETE", "REPLACE", "TWIDDLE" };
	if(op[index].parent == -1)
		return;
	else
	{
		int i, j;
		REVERSE_INDEX(index, N, i, j);
		printactions(op, op[index].parent, N);
		if(op[index].type <= OP_TWIDDLE)
			printf("(%s,%2d) ", messages[op[index].type], i); 
	}
}

int calculate_ed(const char* p, const char* q, edit_operations_t* e, int M, int N, const editcosts_t* costs)
{
#define MIN(i, j) (((i) < (j)) ? (i) : (j))
#define MIN4(i, j, k, l) (MIN(MIN(i,j),MIN(k,l)))
	//Basic parameter validation.
	if(p == NULL || q == NULL || e == NULL || M < 0 || N < 0 || costs == NULL) return -1;

	if(M == 0) return N;
	if(N == 0) return M;

	//Initialize the base cases for the recurrence relation.
	e[0].ed = 0;
	e[0].parent = -1;
	e[0].type = OP_COUNT;
	for(int k = 1; k <= N; ++k)
	{
		e[INDEX(0, k, N)].ed = k;
		e[INDEX(0, k, N)].parent = INDEX(0, k-1, N);
		e[INDEX(0, k, N)].type = OP_INSERT; 
	}
	for(int k = 1; k <= M; ++k)
	{
		e[INDEX(k, 0, N)].ed = k;
		e[INDEX(k, 0, N)].parent = INDEX(k-1, 0, N);
		e[INDEX(k, 0, N)].type = OP_DELETE;
	}

	//Find e(M, N) using the recurrence relation.
	for(int i = 1; i <= M; ++i)
	{
		for(int j = 1; j <= N; ++j)
		{
			if(p[i] == q[j])
			{
				e[INDEX(i,j,N)].ed = e[INDEX(i-1,j-1,N)].ed;
				e[INDEX(i,j,N)].parent = INDEX(i-1,j-1,N);
				e[INDEX(i,j,N)].type = OP_MATCH;
			}				
			else
			{
				int del, ins, rep, twid;
				del = ins = rep = twid = INT_MAX;

				//Check if delete operation is possible
				if(i > 0) 
					del = e[INDEX(i-1,j,N)]  .ed + costs->cost[OP_DELETE];

				//Check if insert operation is possible
				if(j > 0)
					ins = e[INDEX(i,j-1,N)]  .ed + costs->cost[OP_INSERT];
				
				rep = e[INDEX(i-1,j-1,N)].ed + costs->cost[OP_REPLACE];
				
				int minop;

				//Check if twiddle operation is possible.
				if(i > 0 && j > 0 && q[j] == p[i-1] && q[j-1] == p[i])
					twid = e[INDEX(i-2,j-2,N)].ed + costs->cost[OP_TWIDDLE];

				e[INDEX(i,j,N)].ed = minop = MIN4(del, ins, rep, twid);
				
				//Which operation did we perform? Or what operation yielded the minimum cost.
				if(minop == del)
				{
					e[INDEX(i,j,N)].parent = INDEX(i-1,j,N);
					e[INDEX(i,j,N)].type = OP_DELETE;
				}
				else if(minop == ins)
				{
					e[INDEX(i,j,N)].parent = INDEX(i,j-1,N);
					e[INDEX(i,j,N)].type = OP_INSERT;
				}
				else if(minop == rep)
				{
					e[INDEX(i,j,N)].parent = INDEX(i-1,j-1,N);
					e[INDEX(i,j,N)].type = OP_REPLACE;
				}
				else
				{
					e[INDEX(i,j,N)].parent = INDEX(i-2,j-2,N);
					e[INDEX(i,j,N)].type = OP_TWIDDLE;
				}
			}		
		}
	}
	return e[INDEX(M,N,N)].ed;
#undef MIN4
#undef MIN
}

int editdistance(const char* p, const char* q, const editcosts_t *costs_in = NULL)
{
#define MAX(i,j) (((i) > (j)) ? (i) : (j))
	editcosts_t costs;
	for(int i = 0; i < OP_COUNT; ++i)
		costs.cost[i] = 1;

	int m, n;
	char * p_ = new char [ (m = (strlen(p) + 2)) ];
	char * q_ = new char [ (n = (strlen(q) + 2)) ];

	--m, --n;
	p_[0] = q_[0] = ' ' ;
	strcpy(p_ + 1, p);
	strcpy(q_ + 1, q);
	
	edit_operations_t *e = (edit_operations_t*) malloc(sizeof(edit_operations_t) * (m*n));

	int ret = calculate_ed(p_, q_, e, --m, --n, costs_in ? costs_in : &costs);

#ifdef DEBUG_SPELLER
	printactions(e, INDEX(m,n,n), n);
	printf("\n");
#endif
	free(e);
	free(q_);
	free(p_);

	return ret;
#undef MAX
}

enum { KDTREE_CHILD_NODES = 16, MAXIMUM_SEARCH_RESULTS = 8 };
struct kdtree_node
{
	char* s;
	kdtree_node* children[KDTREE_CHILD_NODES];
};

typedef int (*distance_function) (const char*, const char*) ;

class kdtree
{
	public:
	kdtree(char* s, distance_function editdistance_function)
	{
		root = allocate_kdtree_node(s);	
		f = editdistance_function;
	}

	void insert(char* in)
	{
		kdtree_insert_recursive(root, in);	
	}

	/*
	query	: the query string
	n	: maximum admissible edit distance i.e. look for strings with an edit distance <=n to the query string.
		  (if n == 0, exact search is made for the query string).
	result  : the result will be written to this array.
	m	: the search stops after m strings are written.

	return value gives the actual number of strings written in the result array.
	*/
	int search(const char* query, int n, char** result, int m)
	{
		int m_ = (m <= 0 || m > MAXIMUM_SEARCH_RESULTS) ? MAXIMUM_SEARCH_RESULTS : m;
		int i_ = 0;
		kdtree_search_recursive(root, query, n, result, i_, m_);
		return i_;
	}

	void print()
	{
		printf("printing kd tree @%p\n", root);
		print_kdtree_recursive(root);
		printf("end of kd tree @%p\n", root);
	}

	private:
	kdtree_node* root;
	distance_function f;

	kdtree_node* allocate_kdtree_node(char* s)
	{
		kdtree_node * newnode = new kdtree_node();
		newnode->s = s;
		for(int i = 0; i < KDTREE_CHILD_NODES; ++i)
			newnode->children[i] = NULL;
		return newnode;
	}

	void kdtree_insert_recursive(kdtree_node* r, char* in)
	{
		int d = f(r->s, in);
		if(d >= 0 && d < KDTREE_CHILD_NODES)
		{
			if(r->children[d])
				kdtree_insert_recursive(r->children[d], in);
			else
				r->children[d] = allocate_kdtree_node(in);
		}
	}

	void kdtree_search_recursive(kdtree_node* r, const char* query, int n, char** result, int& i, int N)
	{
		int d = f(r->s, query);

		if(i >= N) return;

		if(d <= n)
			result[i++] = r->s;

		for(int l = (d - n); l <= (d + n); ++l)
			if( l >= 0 && l < KDTREE_CHILD_NODES && r->children[l])
				kdtree_search_recursive(r->children[l], query, n, result, i, N);
	}

	void print_kdtree_recursive(kdtree_node* r)
	{
		int i;
		printf("[%p] [%s]\n", r, r->s);
		printf("[");
		for(i = 0; i < KDTREE_CHILD_NODES; ++i)
			if(r->children[i]) printf("(i=%2d, %p) ", i, r->children[i]);
		printf("]\n");

		for(i = 0; i < KDTREE_CHILD_NODES; ++i)
			if(r->children[i]) print_kdtree_recursive(r->children[i]);
	}
};

void remove_trailing_newline(char* s)
{
	if(s[strlen(s) - 1] == '\n')
		s[strlen(s) - 1] = '\0';
	if(s[strlen(s) - 1] == '\r')
		s[strlen(s) - 1] = '\0';
}

int editdistance_wrapper(const char* p, const char* q)
{
	return editdistance(p, q);
}

#define START_TIMER(tv) do { \
	gettimeofday(&tv, NULL); \
} while(0)

#define STOP_TIMER_AND_PRINT(tv) do { \
	timeval t, tr; \
	gettimeofday(&t, NULL); \
	timersub(&t, &tv, &tr); \
	printf("time taken = (%ld) seconds, (%ld) microseconds\n", tr.tv_sec, tr.tv_usec); \
} while(0)

#define MAXIMUM_WORDS (128 * 1024)

#define CHECK_MALLOC(p) do { \
	if(p == NULL) { \
		printf("memory allocation error\n"); \
		exit(-1); \
	} \
} while(0)

int main(int argc, char** argv)
{
	distance_function f = editdistance_wrapper;
	
	char buffer[128];
	FILE* wordlist;
	timeval tv;

	char ** words = (char**) malloc(sizeof(char*) * MAXIMUM_WORDS);
	CHECK_MALLOC(words);
	int wordcount = 0;

	srand(gettimeofday(NULL, NULL));

	if(argc != 2)
	{
		printf("usage: %s <wordlist>\n", argv[0]);
		return -1;
	}
	else
	{
		if((wordlist = fopen(argv[1], "r")) == NULL)
		{
			printf("unable to open file %s\n", argv[1]);
			return -1;
		}
	}

	while(fgets(buffer, sizeof(buffer), wordlist))
	{
		remove_trailing_newline(buffer);

		if(wordcount >= MAXIMUM_WORDS)
			break;

		words[wordcount] = (char*) malloc(sizeof(char) * (strlen(buffer) + 1));
		int k;
		for(k = 0; k < strlen(buffer); ++k)
			words[wordcount][k] = tolower(buffer[k]);
		words[wordcount][k] = '\0';
		++wordcount;

	}
	fclose(wordlist);

	if(wordcount == 0)
	{
		printf("no words were found in the file %s\n", argv[1]);
		return -1;
	}
	//the words need to be randomly permuted so that the expected height of the kd tree is reasonable.
	for(int k = 0; k < wordcount; ++k)
	{
		int r = rand() % wordcount;
		char* temp = words[k];
		words[k] = words[r];
		words[r] = temp;
	}

	START_TIMER(tv);
	kdtree kd(words[0], f);
	for(int k = 1; k < wordcount; ++k)
		kd.insert(words[k]);
	STOP_TIMER_AND_PRINT(tv);

#define RESULT_SET_SIZE (4)
	char ** resultset = (char**) malloc(sizeof(char*) * RESULT_SET_SIZE);

	while(fgets(buffer, sizeof(buffer), stdin))
	{
		if(!strcmp(buffer, "quit"))
			return 0;
		remove_trailing_newline(buffer);
	
		//get a maximum of RESULT_SET_SIZE spelling suggestions subject to the constraint that the
		//edit distance to the string in buffer is <= 1.	
		START_TIMER(tv);
		int r = kd.search(buffer, 1, resultset, RESULT_SET_SIZE);
		STOP_TIMER_AND_PRINT(tv);

		printf("[");
		for(int k = 0; k < r; ++k)
		{
			if(k == r - 1)
				printf("'%s'", resultset[k]);
			else
				printf("'%s', ", resultset[k]); 
		}
		printf("]");

		printf("\n");
	}
	return 0;
}
