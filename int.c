#include <stdio.h>
#include <stdlib.h>
void die(char *msg) {
	fprintf(stderr, "ERROR: %s\n", msg);
	exit(-1);
}
#define ALL 0
#define NONE 1
#define EPS 2
#define RANGE 3
#define MARK 4
#define INF 5
#define REP 6
#define MAX 7
#define NOT 8
#define AND 9
#define OR 10
#define SEQ 11
#define CACHE_SIZE 40000
struct {
	unsigned int id: 24;
	unsigned int seen: 1;
	unsigned int null: 1;
	unsigned int type: 6;
	unsigned int head, tail;
} nodes[CACHE_SIZE];
unsigned int cache[CACHE_SIZE][256];
unsigned int used;
unsigned int make(unsigned int type, unsigned int head, unsigned int tail) {
	for(unsigned int i = 0; i < used; i++)
		if(nodes[i].type == type && nodes[i].head == head && nodes[i].tail == tail)
			return i;
	if(used == CACHE_SIZE)
		die("out of memory");
	nodes[used].type = type;
	nodes[used].head = head;
	nodes[used].tail = tail;
	nodes[used].seen = 0;
	nodes[used].id = ~0U;
	for(unsigned int byte = 0; byte < 256; byte++)
		cache[used][byte] = ~0U;
	switch(type) {
		case ALL: nodes[used].null = 1; break;
		case NONE: nodes[used].null = 0; break;
		case EPS: nodes[used].null = 1; break;
		case RANGE: nodes[used].null = 0; break;
		case MARK: nodes[used].null = 1; break;
		case INF: nodes[used].null = 1; break;
		case REP: nodes[used].null = nodes[head].null; break;
		case MAX: nodes[used].null = 1; break;
		case NOT: nodes[used].null = !nodes[head].null; break;
		case AND: nodes[used].null = nodes[head].null && nodes[tail].null; break;
		case OR: nodes[used].null = nodes[head].null || nodes[tail].null; break;
		case SEQ: nodes[used].null = nodes[head].null && nodes[tail].null; break;
	}
	return used++;
}
unsigned int merge(unsigned int type, unsigned int a, unsigned int b) {
	if(a == ~0U) return b;
	if(b == ~0U) return a;
	unsigned int a_head, a_tail;
	if(nodes[a].type == type) {
		a_head = nodes[a].head;
		a_tail = nodes[a].tail;
	} else {
		a_head = a;
		a_tail = ~0U;
	}
	unsigned int b_head, b_tail;
	if(nodes[b].type == type) {
		b_head = nodes[b].head;
		b_tail = nodes[b].tail;
	} else {
		b_head = b;
		b_tail = ~0U;
	}
	if(a_head < b_head) return make(type, a_head, merge(type, a_tail, b));
	if(a_head > b_head) return make(type, b_head, merge(type, a, b_tail));
	if(a_head == b_head) return make(type, a_head, merge(type, a_tail, b_tail));
	return -1;
}
unsigned int All() {
	return make(ALL, 0, 0);
}
unsigned int None() {
	return make(NONE, 0, 0);
}
unsigned int Eps() {
	return make(EPS, 0, 0);
}
unsigned int Range(unsigned int min, unsigned int max) {
	return make(RANGE, min, max);
}
unsigned int Mark(unsigned int kind, unsigned int number) {
	return make(MARK, kind, number);
}
unsigned int Inf(unsigned int body) {
	if(nodes[body].type == RANGE && nodes[body].head == 0 && nodes[body].tail == 255) return All();
	if(nodes[body].type == ALL) return All();
	if(nodes[body].type == NONE) return Eps();
	if(nodes[body].type == EPS) return Eps();
	if(nodes[body].type == MAX) return Inf(nodes[body].head);
	return make(INF, body, 0);
}
unsigned int Rep(unsigned int body, unsigned int times) {
	if(nodes[body].type == ALL) return All();
	if(nodes[body].type == NONE) return None();
	if(nodes[body].type == EPS) return Eps();
	if(nodes[body].type == INF) return body;
	return make(REP, body, times);
}
unsigned int Max(unsigned int body, unsigned int times) {
	if(nodes[body].type == ALL) return All();
	if(nodes[body].type == NONE) return Eps();
	if(nodes[body].type == EPS) return Eps();
	if(nodes[body].type == INF) return body;
	return make(MAX, body, times);
}
unsigned int Not(unsigned int body) {
	if(nodes[body].type == ALL) return None();
	if(nodes[body].type == NONE) return All();
	if(nodes[body].type == NOT) return nodes[body].head;
	return make(NOT, body, 0);
}
unsigned int And(unsigned int head, unsigned int tail) {
	if(nodes[head].type == ALL) return tail;
	if(nodes[tail].type == ALL) return head;
	if(nodes[head].type == NONE) return None();
	if(nodes[tail].type == NONE) return None();
	if(nodes[head].type == EPS) return nodes[tail].null ? Eps() : None();
	if(nodes[tail].type == EPS) return nodes[head].null ? Eps() : None();
	return merge(AND, head, tail);
}
unsigned int Or(unsigned int head, unsigned int tail) {
	if(nodes[head].type == NONE) return tail;
	if(nodes[tail].type == NONE) return head;
	if(nodes[head].type == ALL) return All();
	if(nodes[tail].type == ALL) return All();
	if(nodes[head].type == EPS && nodes[tail].null) return tail;
	if(nodes[tail].type == EPS && nodes[head].null) return head;
	return merge(OR, head, tail);
}
unsigned int Seq(unsigned int head, unsigned int tail) {
	if(nodes[head].type == NONE) return None();
	if(nodes[tail].type == NONE) return None();
	if(nodes[head].type == EPS) return tail;
	if(nodes[tail].type == EPS) return head;
	if(nodes[head].type != SEQ) return make(SEQ, head, tail);
	return make(SEQ, nodes[head].head, Seq(nodes[head].tail, tail));
}
unsigned int derive(unsigned int regex, unsigned int byte);
unsigned int derive_force(unsigned int regex, unsigned int byte) {
	switch(nodes[regex].type) {
		case ALL: return All();
		case NONE: return None();
		case EPS: return None();
		case RANGE: return byte >= nodes[regex].head && byte <= nodes[regex].tail ? Eps() : None();
		case MARK: return None();
		case INF: return Seq(derive(nodes[regex].head, byte), regex);
		case REP: return derive(Seq(nodes[regex].head, Rep(nodes[regex].head, nodes[regex].tail - 1)), byte);
		case MAX: return derive(Seq(nodes[regex].head, Max(nodes[regex].head, nodes[regex].tail - 1)), byte);
		case NOT: return Not(derive(nodes[regex].head, byte));
		case AND: return And(derive(nodes[regex].head, byte), derive(nodes[regex].tail, byte));
		case OR: return Or(derive(nodes[regex].head, byte), derive(nodes[regex].tail, byte));
		case SEQ: ;
			unsigned int dx = Seq(derive(nodes[regex].head, byte), nodes[regex].tail);
			if(nodes[nodes[regex].head].null)
				dx = Or(dx, derive(nodes[regex].tail, byte));
			return dx;
	}
	die("unknown type");
	return 0;
}
unsigned int derive(unsigned int regex, unsigned int byte) {
	if(cache[regex][byte] == ~0U)
		cache[regex][byte] = derive_force(regex, byte);
	return cache[regex][byte];
}
void print(unsigned int regex) {
	switch(nodes[regex].type) {
		case ALL: printf("All()"); break;
		case NONE: printf("None()"); break;
		case EPS: printf("Eps()"); break;
		case RANGE: printf("Range(%u, %u)", nodes[regex].head, nodes[regex].tail); break;
		case MARK: printf("Mark(%u, %u)", nodes[regex].head, nodes[regex].tail); break;
		case INF: printf("Inf("); print(nodes[regex].head); printf(")"); break;
		case REP: printf("Rep("); print(nodes[regex].head); printf(", %u)", nodes[regex].tail); break;
		case MAX: printf("Max("); print(nodes[regex].head); printf(", %u)", nodes[regex].tail); break;
		case NOT: printf("Not("); print(nodes[regex].head); printf(")"); break;
		case AND: printf("And("); print(nodes[regex].head); printf(", "); print(nodes[regex].tail); printf(")"); break;
		case OR:  printf("Or(");  print(nodes[regex].head); printf(", "); print(nodes[regex].tail); printf(")"); break;
		case SEQ: printf("Seq("); print(nodes[regex].head); printf(", "); print(nodes[regex].tail); printf(")"); break;
	}
}
void label(unsigned int regex) {
	static int id = 0;
	if(nodes[regex].seen == 1) return;
	nodes[regex].seen = 1;
	nodes[regex].id = id++;
	for(unsigned int byte = 0; byte < 256; byte++)
		label(derive(regex, byte));
}
unsigned int marks(unsigned int regex, unsigned int number) {
	switch(nodes[regex].type) {
		case ALL: return 0;
		case NONE: return 0;
		case EPS: return 0;
		case RANGE: return 0;
		case MARK: return nodes[regex].tail == number ? nodes[regex].head : 0;
		case INF: return marks(nodes[regex].head, number);
		case REP: return marks(nodes[regex].head, number);
		case MAX: return marks(nodes[regex].head, number);
		case NOT: return ~marks(nodes[regex].head, number);
		case AND: return marks(nodes[regex].head, number) & marks(nodes[regex].tail, number);
		case OR: return marks(nodes[regex].head, number) | marks(nodes[regex].tail, number);
		case SEQ:;
			unsigned int ret = marks(nodes[regex].head, number);
			if(nodes[nodes[regex].head].null)
				ret |= marks(nodes[regex].tail, number);
			return ret;
	}
	die("fuck");
	return 0;
}
unsigned int mode(unsigned int data[256]) {
	static struct {
		unsigned int value, count;
	} ht[256];
	for(int i = 0; i < 256; i++) {
		ht[i].value = ~0U;
		ht[i].count = 0;
	}
	for(int i = 0; i < 256; i++)
		for(int j = 0; j < 256; j++) {
			if(ht[j].value == ~0U)
				ht[j].value = data[i];
			if(ht[j].value == data[i]) {
				ht[j].count++;
				break;
			}
		}
	unsigned int best = ht[0].value;
	unsigned int best_count = ht[0].count;
	for(int i = 1; i < 256; i++)
		if(ht[i].count > best_count) {
			best = ht[i].value;
			best_count = ht[i].count;
		}
	return best;
}
void compile(unsigned int regex) {
	if(nodes[regex].seen == 0) return;
	nodes[regex].seen = 0;
	printf("st_%u: {\n", nodes[regex].id);
	if(regex == None()) {
		printf("\treturn match_none();\n");
		printf("}\n");
		return;
	}
	for(unsigned int mark = 0; mark < 999; mark++) {
		unsigned int types = marks(regex, mark);
		if(types & MATCH) {
			printf("return match_%u();\n", mark);
			return;
		}
		if(types & TAG)
			printf("match_pos[%u] = pos;\n", mark);
	}
	unsigned int next0 = mode(cache[regex]);
	//unsigned int next0 = derive(regex, 0);
	for(unsigned int byte = 0; byte < 256; byte++) {
		unsigned int next = derive(regex, byte);
		if(next != next0)
			printf("if(current == %u) goto st_%u;\n", byte, nodes[next].id);
	}
	printf("goto st_%u;\n", nodes[next0].id);
	for(unsigned int byte = 0; byte < 256; byte++)
		compile(derive(regex, byte));
}
void print_flat() {
	static char *names[] = {"ALL", "NONE", "EPS", "RANGE", "MARK", "INF", "REP", "MAX", "NOT", "AND", "OR", "SEQ"};
	for(unsigned int i = 0; i < used; i++) {
		char *type = nodes[i].type < 12 ? names[nodes[i].type] : "???";
		printf("%02u: %5s(%02u, %02u) - %u\n", i, type, nodes[i].head, nodes[i].tail, nodes[i].id);
	}
}
int main() {
	unsigned int initial = None();
	initial = Or(initial, Range('a', 'a'));
	unsigned int after = Or(initial, Range('b', 'b'));
	unsigned int r = Seq(initial, Inf(after));
	//unsigned int not_after = And(Range(0, 255), Not(after));
	unsigned int not_after = And(Range(0, 255), Not(Range('a','b')));
	//unsigned int not_after = Or(Range(0, 'a' - 1), Range('b' + 1, 255));
	r = Seq(r, Seq(Mark(2, 42), Seq(not_after, Mark(1, 42))));
	
	label(r);
	compile(r);
	return 0;
}
