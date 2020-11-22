/* -- PRELUDE -- */

#include <stdio.h> // for fprintf, printf
#include <stdlib.h> // for exit, NULL
#include <stdbool.h> // for bool, obviously :)

// display an error message and exit
#define panic(...) do { \
		fprintf(stderr, "PANIC: "); \
		fprintf(stderr, __VA_ARGS__); \
		fprintf(stderr, "\n"); \
		exit(-1); \
	} while(0)

// size of the node cache, at most this many nodes can be in play
#define CACHE_SIZE (4096)

// all possible node types
// UNUSED is 0 to identify unused nodes
enum type { UNUSED = 0, EMPTY, ALL, NONE, LIT, MARK, INF, NOT, SEQ, OR, AND };

// a regular expression
// every regex exists exactly once, so equality
// can be tested with ==. this also lets us
// cache derivitives in the struct
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

typedef struct reg Reg; // for the sake of smaller code

/* -- CONSTRUCTORS -- */

// the core of the program
// every regex node exists exactly once in here
// we could add some logic to expand it when we run
// out, but this is cleaner and works well enough
Reg cache[CACHE_SIZE];

// constuct Reg nodes and simplify
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
Reg *Lit(int ch, int len) {
	// search the cache for an existing version of the node, or an unused one to fill
	for(Reg *r = &cache[0]; r < &cache[CACHE_SIZE]; r++)
		if(r->type == UNUSED || (r->type == LIT && r->ch == ch && r->len == len)) {
			r->type = LIT;
			r->null = false;
			r->ch = ch;
			r->len = len;
			return r;
		}
	panic("out of nodes");
}
Reg *Mark(char *name) {
	for(Reg *r = &cache[0]; r < &cache[CACHE_SIZE]; r++)
		if(r->type == UNUSED || (r->type == MARK && r->name == name)) {
			r->type = MARK;
			r->null = true;
			r->name = name;
			return r;
		}
	panic("out of nodes");
}
Reg *Inf(Reg *head) {
	// run some simplifications
	if(head->type == EMPTY) return Empty(); // infinitely repeating an empty string is still just an empty string
	if(head->type == ALL) return All(); // infinitely repeating every possible string is still just every possible string
	if(head->type == NONE) return Empty(); // infinitely repeating nothing is still nothing, plus repeating it no times just gives us the empty string
	if(head->type == INF) return head; // infinitely repeating an infinite repitition gives us nothing new
	if(head->type == LIT && head->ch == 0 && head->len == 256) return All(); // if ch is 0 and len is 256, that's every single character, so repeating any character any number of times gives every string
	for(Reg *r = &cache[0]; r < &cache[CACHE_SIZE]; r++)
		if(r->type == UNUSED || (r->type == INF && r->head == head)) {
			r->type = INF;
			r->null = true;
			r->head = head;
			return r;
		}
	panic("out of nodes");
}
Reg *Not(Reg *head) {
	if(head->type == ALL) return None();
	if(head->type == NONE) return All();
	for(Reg *r = &cache[0]; r < &cache[CACHE_SIZE]; r++)
		if(r->type == UNUSED || (r->type == NOT && r->head == head)) {
			r->type = NOT;
			r->null = !head->null;
			r->head = head;
			return r;
		}
	panic("out of nodes");
}
Reg *Seq(Reg *head, Reg *tail) {
	// in order to make sure we don't end up with many copies of the same regex, with the nodes ordered differently, we make some attempt to normilize the ordering of the nodes: a(bc) -> (ab)c
	if(tail->type == SEQ) return Seq(Seq(head, tail->head), tail->tail);
	if(head->type == EMPTY) return tail; // empty string on the front does nothing
	if(tail->type == EMPTY) return head; // empty string on the back does nothing
	if(head->type == NONE) return None(); // nothing to match at the beginning means we'll never match anything
	if(tail->type == NONE) return None(); // nothing to match at the end means we'll never match anything
	for(Reg *r = &cache[0]; r < &cache[CACHE_SIZE]; r++)
		if(r->type == UNUSED || (r->type == SEQ && r->head == head && r->tail == tail)) {
			r->type = SEQ;
			r->null = head->null && tail->null;
			r->head = head;
			r->tail = tail;
			return r;
		}
	panic("out of nodes");
}
Reg *Or(Reg *head, Reg *tail) {
	if(head == tail) return head; // X or X is just X
	// same as above, but we restructure the other way, to give the above rule the maximum chance to work
	// (A|B)|B will not match the above case, but A|(B|B) will
	if(head > tail) {
		Reg *tmp = tail;
		tail = head;
		head = tmp;
	}
	if(head->type == OR) return Or(head->head, Or(head->tail, tail));
	if(head->type == ALL) return All(); // ORring will never match fewer strings, so if we already match all of them, we're already there
	if(tail->type == ALL) return All();
	if(head->type == NONE) return tail; // ORring with nothing will never match, so just try to match the other side
	if(tail->type == NONE) return head;
	for(Reg *r = &cache[0]; r < &cache[CACHE_SIZE]; r++)
		if(r->type == UNUSED || (r->type == OR && r->head == head && r->tail == tail)) {
			r->type = OR;
			r->null = head->null || tail->null;
			r->head = head;
			r->tail = tail;
			return r;
		}
	panic("out of nodes");
}
Reg *And(Reg *head, Reg *tail) {
	if(head == tail) return head; // AND between the same expressions is the same
	if(head->type == AND) return And(head->head, And(head->tail, tail)); // restructure
	if(head->type == ALL) return tail; // ANDing with everything will always work
	if(tail->type == ALL) return head;
	if(head->type == NONE) return None(); // ANDing with nothing will never work
	if(tail->type == NONE) return None();
	for(Reg *r = &cache[0]; r < &cache[CACHE_SIZE]; r++)
		if(r->type == UNUSED || (r->type == AND && r->head == head && r->tail == tail)) {
			r->type = AND;
			r->null = head->null && tail->null;
			r->head = head;
			r->tail = tail;
			return r;
		}
	panic("out of nodes");
}
/* -- OPERATION -- */
void dprint(struct reg *r) {
	switch(r->type) {
		case ALL: printf("All()"); return;
		case NONE: printf("None()"); return;
		case EMPTY: printf("Empty()"); return;
		case MARK: printf("Mark(%i)", r->ch); return;
		case LIT: printf("Lit(%i, %i)", r->ch, r->len); return;
		case INF: printf("Inf("); dprint(r->head); printf(")"); return;
		case NOT: printf("Not("); dprint(r->head); printf(")"); return;
		case SEQ: printf("Seq("); dprint(r->head); printf(", "); dprint(r->tail); printf(")"); return;
		case OR: printf("Or("); dprint(r->head); printf(", "); dprint(r->tail); printf(")"); return;
		case AND: printf("And("); dprint(r->head); printf(", "); dprint(r->tail); printf(")"); return;
		case UNUSED: printf("U"); return;
	}
	printf("@");
}
Reg *derive(int ch, Reg *r) {
	// if the derivitive we want isn't cached, we have to compute it
	if(r->next[ch] == NULL) {
		printf("DERIVE %i ", ch);
		dprint(r);
		printf("\n");
		switch(r->type) {
			case UNUSED: // should be impossible, hopefully
				panic("deriving UNUSED node");
			break;
			case EMPTY: // there are no characters to filter by for the derivitive, so it must be nothing
				r->next[ch] = None();
			break;
			case ALL: // removing from infinity just gives infinity
				r->next[ch] = All();
			break;
			case NONE: // removing from nothing is still nothing
				r->next[ch] = None();
			break;
			case LIT: // if the character is in range, we remove it and are left with an empty string
				if(ch >= r->ch && ch < r->ch + r->len)
					r->next[ch] = Empty();
				else // if not, nothing remains
					r->next[ch] = None();
			break;
			case MARK: // MARK follows the same rules as EMPTY
				r->next[ch] = None();
			break;
			case INF: // since X* is X repeated any number of times, it is equivilant to XX*, so we can 'peel off' one layer of repitition, derive that, and glue it back on with SEQ
				r->next[ch] = Seq(derive(ch, r->head), r);
			break;
			case NOT: // derivitive of a logical operator just derives the inner part
				r->next[ch] = Not(derive(ch, r->head));
			break;
			case SEQ:
				// derive the front end
				r->next[ch] = Seq(derive(ch, r->head), r->tail);
				// if the front part is null, that means that all of the strings from the back part are 'let through', so we should also derive the back part, and OR it together
				if(r->head->null)
					r->next[ch] = Or(r->next[ch], derive(ch, r->tail));
			break;
			case OR: // rules for logical operators still apply
				r->next[ch] = Or(derive(ch, r->head), derive(ch, r->tail));
			break;
			case AND:
				r->next[ch] = And(derive(ch, r->head), derive(ch, r->tail));
			break;
		}
	}
	// the derivitive we want is definitely cached now, so just return that
	return r->next[ch];
}
/* -- COMPILE -- */
// print off all the marks which are available in the regex
void marks(Reg *r) {
	switch(r->type) {
		case UNUSED:
			panic("mark testing UNUSED node");
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
			printf("mark %s\n", r->name);
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
	// print off the ID, this will always be in order starting with 1
	printf("state %i\n", r->id - 1);
	marks(r); // print off each mark for this state
	// print off the transition table, for some basic compaction of the output, we print all transitions that are different from the transition on '\0', and default to that
	for(int ch = 0; ch < 256; ch++)
		if(r->next[ch] != r->next[0])
			printf("transition %i %i\n", ch, r->next[ch]->id - 1);
	printf("default %i\n", r->next[0]->id - 1);
	// walk the DFA in the same order as in label()
	for(int ch = 0; ch < 256; ch++)
		dfa(r->next[ch]);
}
/* -- PARSE -- */
char *reg = NULL;
bool more() { return *reg != '\0'; }
char peek() { return *reg; }
void eat(char c) { if(c == *reg) reg++; else panic("expected %c got %c", c, *reg); }
char next() { return *reg++; }
char *until(char c) {
	char *ret = reg;
	while(peek() != c)
		if(next() == '\0')
			panic("unexpected end of regex");
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
			panic("unexpected end of class");
		if(ate(']')) {
			if(inv)
				r = And(Not(r), Lit(0, 256));
			return r;
		}
		char hold = parse_esc();
		if(ate('-')) {
			if(!more())
				panic("unexpected end of class");
			char new = parse_esc();
			if(hold > new) { char t = hold; hold = new; new = t; }
			r = Or(r, Lit(hold, new - hold + 1));
		} else {
			r = Or(r, Lit(hold, 1));
		}
	}
}
Reg *parse_mark() {
	eat('`');
	static char names[CACHE_SIZE];
	static int pos = 0;
	int len = 0;
	while(peek() != '`')
		if(more())
			names[pos + len++] = next();
		else
			panic("unexpected end of mark");
	eat('`');
	names[pos + len] = '\0';
	Reg *r = Mark(&names[pos]);
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
		else panic("should never happen");
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
/* -- DRIVER -- */
int main(int argc, char *argv[]) {
	if(argc != 2) {
		fprintf(stderr, "usage: %s <regex to compile>\n", argv[0]);
		exit(-1);
	}
	Reg *r = parse(argv[1]);
	label(r);
	dfa(r);
	exit(0);
}
