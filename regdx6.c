#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>
void die(char *s) { printf(s); exit(0); }
/*#define die(...) do { \
		fprintf(stderr, "PANIC: "); \
		fprintf(stderr, __VA_ARGS__); \
		fprintf(stderr, "\n"); \
		exit(-1); \
	} while(0)*/
enum type { UNUSED = 0, EMPTY, ALL, NONE, LIT, MARK, INF, NOT, SEQ, OR, AND };
struct reg {
	enum type type; // what kind of node?
	bool null; // does the regex match the empty string
	union { // each type uses only one of these:
		struct { struct reg *head, *tail; }; // types with children
		struct { int ch, len; }; // lit
		struct { char *name; }; // mark
	};
	struct reg *next[256]; // cache of derivitives for each byte
	int id; // unique id for each state, 0 if undefined
	bool done; // have we printed this state yet?
};
typedef struct reg Reg;
typedef enum type Type;
#if 1 // allocation
#define CACHE_SIZE (50000)
Reg *make0(Type type, uint ch, uint len) {
	static Reg cache[CACHE_SIZE];
	for(uint i = 0; i < CACHE_SIZE; i++) {
		Reg *r = &cache[i];
		if(r->type == type && r->ch == ch && r->len == len)
			return r;
		if(r->type == UNUSED) {
			r->type = type;
			r->ch = ch;
			r->len = len;
			assert(type == LIT || type == MARK);
			if(type == LIT)
				r->null = false;
			else
				r->null = true;
			return r;
		}
	}
	die("out of pair nodes");
}
Reg *make1(Type type, Reg *head) {
	static Reg cache[CACHE_SIZE];
	for(uint i = 0; i < CACHE_SIZE; i++) {
		Reg *r = &cache[i];
		if(r->type == type && r->head == head)
			return r;
		if(r->type == UNUSED) {
			r->type = type;
			r->head = head;
			assert(type == NOT || type == INF);
			if(type == NOT)
				r->null = !head->null;
			else
				r->null = true;
			return r;
		}
	}
	die("out of pair nodes");
}
Reg *make2(Type type, Reg *head, Reg *tail) {
	static Reg cache[CACHE_SIZE];
	for(uint i = 0; i < CACHE_SIZE; i++) {
		Reg *r = &cache[i];
		if(r->type == type && r->head == head && r->tail == tail)
			return r;
		if(r->type == UNUSED) {
			r->type = type;
			r->head = head;
			r->tail = tail;
			assert(type == SEQ || type == OR || type == AND);
			if(type == SEQ || type == AND)
				r->null = head->null && tail->null;
			else
				r->null = head->null || tail->null;
			return r;
		}
	}
	die("out of pair nodes");
}
#endif
#if 1 // merging
void print(Reg *x);
Reg *merge(Type type, Reg *head, Reg *tail);
Reg *merge_one(Type type, Reg *head, Reg *tail) {
	//printf("MERGE ONE %s ", type == SEQ ? "SEQ" : type == AND ? "AND" : "OR");
	//print(head);
	//printf(" WITH ");
	//print(tail);
	//printf(": ");
	if(head < tail) { //printf("<LT>");
		return make2(type, head, tail); }
	if(tail < head) { //printf("<GT>");
		return make2(type, tail, head); }
	 { //printf("<EQ>");
	return head; }
}
Reg *merge_single(Type type, Reg *head, Reg *tail) {
	if(head < tail->head)
		return make2(type, head, tail);
	if(tail->head < head)
		return make2(type, tail->head, merge(type, head, tail->tail));
	return make2(type, head, tail->tail);
}
Reg *merge_pair(Type type, Reg *head, Reg *tail) {
	if(head->head < tail->head)
		return make2(type, head->head, merge(type, head->tail, tail));
	if(tail->head < head->head)
		return make2(type, tail->head, merge(type, head, tail->tail));
	return make2(type, head->head, merge(type, head->tail, tail->tail));
}
Reg *merge(Type type, Reg *head, Reg *tail) {
	if(head->type == type)
		if(tail->type == type)
			return merge_pair(type, head, tail);
		else
			return merge_single(type, tail, head);
	else
		if(tail->type == type)
			return merge_single(type, head, tail);
		else
			return merge_one(type, head, tail);
}
Reg *append(Type type, Reg *head, Reg *tail) {
	if(head->type != type)
		return make2(type, head, tail);
	else
		return make2(type, head->head, append(type, head->tail, tail));
}
#endif
#if 1 // constructors
Reg *Empty() {
	static Reg cache;
	cache.type = EMPTY;
	cache.null = true;
	return &cache;
}
Reg *All() {
	static Reg cache;
	cache.type = ALL;
	cache.null = true;
	return &cache;
}
Reg *None() {
	static Reg cache;
	cache.type = NONE;
	cache.null = false;
	return &cache;
}
Reg *Lit(uint ch, uint len) {
	return make0(LIT, ch, len);
}
Reg *Mark(uint id) {
	return make0(MARK, id, 0);
}
Reg *Inf(Reg *head) {
	if(head->type == EMPTY) return Empty();
	if(head->type == ALL) return All();
	if(head->type == NONE) return Empty();
	if(head->type == INF) return head;
	if(head->type == LIT && head->ch == 0 && head->len == 256) return All();
	return make1(INF, head);
}
Reg *Not(Reg *head) {
	if(head->type == ALL) return None();
	if(head->type == NONE) return All();
	return make1(NOT, head);
}
Reg *Seq(Reg *head, Reg *tail) {
	if(head->type == EMPTY) return tail;
	if(tail->type == EMPTY) return head;
	if(head->type == NONE) return None();
	if(tail->type == NONE) return None();
	return append(SEQ, head, tail);
}
Reg *Or(Reg *head, Reg *tail) {
	if(head->type == ALL) return All();
	if(tail->type == ALL) return All();
	if(head->type == NONE) return tail;
	if(tail->type == NONE) return head;
	return merge(OR, head, tail);
}
Reg *And(Reg *head, Reg *tail) {
	if(head->type == ALL) return tail;
	if(tail->type == ALL) return head;
	if(head->type == NONE) return None();
	if(tail->type == NONE) return None();
	return merge(AND, head, tail);
}
#endif
#if 1 // parse
char *reg = NULL;
bool more() { return *reg != '\0'; }
char peek() { return *reg; }
void eat(char c) { if(c == *reg) reg++; else die("expected %c got %c", c, *reg); }
char next() { return *reg++; }
char *until(char c) {
	char *ret = reg;
	while(peek() != c)
		if(next() == '\0')
			die("unexpected end of regex");
	*reg = '\0';
	reg++;
	return ret;
}
bool ate(char c) {
	if(more() && peek() == c) {
		eat(c);
		return true;
	}
	return false;
}
char parse_esc() {
	if(peek() != '\\') return next();
	eat('\\');
	char c = next();
	switch(c) {
		case 'r': c = '\r'; break;
		case 'n': c = '\n'; break;
		case 't': c = '\t'; break;
		case '\\': c = '\\'; break;
	}
	return c;
}
Reg *parse_class() {
	eat('[');
	bool inv = ate('^');
	Reg *r = None();
	for(;;) {
		if(!more())
			die("unexpected end of class");
		if(ate(']')) {
			if(inv)
				r = And(Not(r), Lit(0, 256));
			return r;
		}
		char hold = parse_esc();
		if(ate('-')) {
			if(!more())
				die("unexpected end of class");
			char new = parse_esc();
			if(hold > new) { char t = hold; hold = new; new = t; }
			r = Or(r, Lit(hold, new - hold + 1));
		} else {
			r = Or(r, Lit(hold, 1));
		}
	}
}
int marks = 0;
char *names[CACHE_SIZE];
Reg *parse_mark() {
	eat('`');
	static char buf[CACHE_SIZE];
	static int pos = 0;
	int len = 0;
	while(peek() != '`')
		if(more())
			buf[pos + len++] = next();
		else
			die("unexpected end of mark");
	eat('`');
	buf[pos + len] = '\0';
	Reg *r = Mark(marks);
	names[marks++] = &buf[pos];
	pos = pos + len + 1;
	return r;
}
Reg *parse_and();
Reg *parse_atom() {
	switch(peek()) {
		case '(':
			eat('(');
			Reg *r = parse_and();
			eat(')');
			return r;
		case '[':
			return parse_class();
		case '`':
			return parse_mark();
		case '.':
			eat('.');
			return Lit(0, 256);
		case '\\':
			return Lit(parse_esc(), 1);
		default:
			return Lit(next(), 1);
	}
}
Reg *parse_post() {
	Reg *r = parse_atom();
	while(more() && (peek() == '*' || peek() == '+' || peek() == '!' || peek() == '?')) {
		if(ate('*')) r = Inf(r);
		else if(ate('+')) r = Seq(r, Inf(r));
		else if(ate('!')) r = Not(r);
		else if(ate('?')) r = Or(r, Empty());
		else die("should never happen");
	}
	return r;
}
Reg *parse_seq() {
	Reg *r = parse_post();
	while(more() && peek() != ')' && peek() != '|' && peek() != '&')
		r = Seq(r, parse_post());
	return r;
}
Reg *parse_or() {
	Reg *r = parse_seq();
	while(ate('|'))
		r = Or(r, parse_seq());
	return r;
}
Reg *parse_and() {
	Reg *r = parse_or();
	while(ate('&'))
		r = And(r, parse_or());
	return r;
}
Reg *parse(char *s) {
	reg = s;
	return parse_and();
}
#endif
#if 1 // dfa
void print(Reg *r) {
	switch(r->type) {
		case UNUSED: printf("Unused()"); break;
		case EMPTY: printf("Empty()"); break;
		case ALL: printf("All()"); break;
		case NONE: printf("None()"); break;
		case LIT: printf("Lit(%i, %i)", r->ch, r->len); break;
		case MARK: printf("Mark(%s)", names[r->ch]); break;
		case INF: printf("Inf("); print(r->head); printf(")"); break;
		case NOT: printf("Not("); print(r->head); printf(")"); break;
		case SEQ: printf("Seq("); print(r->head); printf(", "); print(r->tail); printf(")"); break;
		case OR: printf("Or("); print(r->head); printf(", "); print(r->tail); printf(")"); break;
		case AND: printf("And("); print(r->head); printf(", "); print(r->tail); printf(")"); break;
	}
}
Reg *derive(int ch, Reg *r) {
	if(r->next[ch] == NULL) {
		//printf("DERIVE %i ", ch);
		//print(r);
		//printf("\n");
		switch(r->type) {
			case UNUSED:
				die("deriving UNUSED node");
			break;
			case EMPTY:
				r->next[ch] = None();
			break;
			case ALL:
				r->next[ch] = All();
			break;
			case NONE:
				r->next[ch] = None();
			break;
			case LIT:
				if(ch >= r->ch && ch < r->ch + r->len)
					r->next[ch] = Empty();
				else
					r->next[ch] = None();
			break;
			case MARK:
				r->next[ch] = None();
			break;
			case INF:
				r->next[ch] = Seq(derive(ch, r->head), r);
			break;
			case NOT:
				r->next[ch] = Not(derive(ch, r->head));
			break;
			case SEQ:
				r->next[ch] = Seq(derive(ch, r->head), r->tail);
				if(r->head->null)
					r->next[ch] = Or(r->next[ch], derive(ch, r->tail));
			break;
			case OR:
				r->next[ch] = Or(derive(ch, r->head), derive(ch, r->tail));
			break;
			case AND:
				r->next[ch] = And(derive(ch, r->head), derive(ch, r->tail));
			break;
		}
	}
	return r->next[ch];
}
bool marked(Reg *r, uint mark) {
	switch(r->type) {
		case UNUSED:
			die("mark testing UNUSED node");
		case EMPTY:
			return false;
		case ALL:
			return false;
		case NONE:
			return false;
		case LIT:
			return false;
		case MARK:
			return r->ch == mark;
		case INF:
			return marked(r->head, mark);
		case NOT:
			return !marked(r->head, mark);
		case SEQ:
			return marked(r->head, mark) || (r->head->null && marked(r->tail, mark));
		case OR:
			return marked(r->head, mark) || marked(r->tail, mark);
		case AND:
			return marked(r->head, mark) && marked(r->tail, mark);
	}
}
#if 0
void marks(Reg *r) {
	switch(r->type) {
		case UNUSED:
			die("mark testing UNUSED node");
		break;
		// nodes with no children obviously don't contain any marks
		case EMPTY:
		break;
		case ALL:
		break;
		case NONE:
		break;
		case LIT:
		break;
		case MARK: // print the mark, since we found one
			printf("mark %s\n", &names[r->ch]);
		break;
		case INF: // whatever's on the inside will be at the front in every iteration
			marks(r->head);
		break;
		case NOT:
		break;
		case SEQ: // same as with the SEQ case for derive, do the head, and if the head is null, do the tail also
			marks(r->head);
			if(r->head->null)
				marks(r->tail);
		break;
		case OR: // either side could be at the front
			marks(r->head);
			marks(r->tail);
		break;
		case AND:
		break;
	}
}
#endif
// assign a unique nonzero id to every state in the regex, in depth first order
void label(Reg *r) {
	static int id = 1; // static counter persists accross calls
	if(r->id > 0) // already did this node, don't loop forever
		return;
	r->id = id++; // assign an id
	// label the regex we get from deriving by each character
	// this is effectively doing a graph traversal of the final DFA
	for(int ch = 0; ch < 256; ch++)
		label(derive(ch, r));
}
// print out the DFA from the given regex
void dfa(Reg *r) {
	// each Reg is marked so that we cover every state exactly once
	if(r->done)
		return;
	r->done = true;
	if(r->type == ALL) die("all in production");
	if(r->type == NONE) {
		printf("%i [label=\"default\"];\n", r->id - 1);
		return;
	}
	// print off the ID, this will always be in order starting with 1
	printf("%i [%slabel=\"", r->id - 1, r->id == 1 ? "shape=doublecircle," : "");
	//printf("%i [label=\"", r->id - 1);
	//print(r);
	//printf("\", xlabel=\"");
	/*printf("expr ");
	print(r);
	printf("\n");*/
	for(uint i = 0; i < marks; i++)
		if(marked(r, i))
			printf("%s ", names[i]);
	//print(r);
	printf("\"];\n");
	// print off the transition table, for some basic compaction of the output, we print all transitions that are different from the transition on '\0', and default to that
	for(int ch = 0; ch < 256; ch++)
		if(r->next[ch] != r->next[0])
			printf("%i -> %i [label=\"%i\"];\n", r->id - 1, r->next[ch]->id - 1, ch);
	//printf("default %i\n\n", r->next[0]->id - 1);
	printf("%i -> %i;\n", r->id - 1, r->next[0]->id - 1);
	// walk the DFA in the same order as in label()
	for(int ch = 0; ch < 256; ch++)
		dfa(r->next[ch]);
}
void pr(Reg *r) {
	printf("reg %p ", r);
	print(r);
	printf("\n");
}
#endif
int main(int argc, char *argv[]) {
	/*Reg *ab = Or(Lit('a', 1), Lit('b', 1));
	Reg *ba = Or(Lit('b', 1), Lit('a', 1));
	//pr(ab);
	//pr(ba);
	pr(merge(OR, ab, ba));
	//pr(merge(OR, Lit('a', 1), Lit('b', 1)));
	return 0;*/
	if(argc < 2) die("need at least 1 arg");
	if(strcmp(argv[1], "dfa") == 0) {
		Reg *r = parse(argv[2]);
		label(r);
		printf("digraph dfa {\n");
		dfa(r);
		printf("}\n");
		return 0;
	}
	if(strcmp(argv[1], "derive") == 0) {
		Reg *r = parse(argv[3]);
		char *s = argv[2];
		while(*s != '\0') {
			print(r);
			printf("\n");
			r = derive(*s++, r);
		}
		print(r);
		printf("\n");
		return 0;
	}
	die("bad args");
}
/*
Inf(Or(Lit(97, 1), Or(Seq(Lit(97, 1), Lit(97, 1)), Seq(Lit(97, 1), Seq(Lit(97, 1), Seq(Lit(97, 1), Lit(97, 1)))))))
Seq(Or(Lit(97, 1), Or(Seq(Lit(97, 1), Seq(Lit(97, 1), Lit(97, 1))), Empty())), Inf(Or(Lit(97, 1), Or(Seq(Lit(97, 1), Lit(97, 1)), Seq(Lit(97, 1), Seq(Lit(97, 1), Seq(Lit(97, 1), Lit(97, 1))))))))
Or(Seq(Or(Lit(97, 1), Or(Seq(Lit(97, 1), Seq(Lit(97, 1), Lit(97, 1))), Empty())), Inf(Or(Lit(97, 1), Or(Seq(Lit(97, 1), Lit(97, 1)), Seq(Lit(97, 1), Seq(Lit(97, 1), Seq(Lit(97, 1), Lit(97, 1)))))))), Seq(Or(Seq(Lit(97, 1), Lit(97, 1)), Empty()), Inf(Or(Lit(97, 1), Or(Seq(Lit(97, 1), Lit(97, 1)), Seq(Lit(97, 1), Seq(Lit(97, 1), Seq(Lit(97, 1), Lit(97, 1)))))))))
Or(Or(Seq(Or(Lit(97, 1), Or(Seq(Lit(97, 1), Seq(Lit(97, 1), Lit(97, 1))), Empty())), Inf(Or(Lit(97, 1), Or(Seq(Lit(97, 1), Lit(97, 1)), Seq(Lit(97, 1), Seq(Lit(97, 1), Seq(Lit(97, 1), Lit(97, 1)))))))), Seq(Or(Seq(Lit(97, 1), Lit(97, 1)), Empty()), Inf(Or(Lit(97, 1), Or(Seq(Lit(97, 1), Lit(97, 1)), Seq(Lit(97, 1), Seq(Lit(97, 1), Seq(Lit(97, 1), Lit(97, 1))))))))), Or(Seq(Or(Seq(Lit(97, 1), Lit(97, 1)), Empty()), Inf(Or(Lit(97, 1), Or(Seq(Lit(97, 1), Lit(97, 1)), Seq(Lit(97, 1), Seq(Lit(97, 1), Seq(Lit(97, 1), Lit(97, 1)))))))), Seq(Lit(97, 1), Inf(Or(Lit(97, 1), Or(Seq(Lit(97, 1), Lit(97, 1)), Seq(Lit(97, 1), Seq(Lit(97, 1), Seq(Lit(97, 1), Lit(97, 1))))))))))
*/
