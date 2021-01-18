#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
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
#define NOT 8
#define AND 9
#define OR 10
#define SEQ 11
#define UNDEFINED 65535
#define CACHE_BITS 16
#define CACHE_SIZE (1 << 16)
typedef enum {
	ALL,
	NONE,
	EPS,
	RANGE,
	MARK,
	INF,
	NOT,
	AND,
	OR,
	SEQ
} Type;
typedef uint16_t Reg;
typedef struct {
	Type type;
	Reg head, tail;
	bool null;
	uint16_t id;
	state;
} Node;
Node nodes[65535];
Reg cache[65535][256];
unsigned int used;

// allocate and init a new Node, and return its Reg
Reg make(Type type, Reg head, Reg tail) {
	// if the node already exists, reuse it
	for(unsigned int i = 0; i < used; i++) {
		if(nodes[i].type != type) continue;
		if(nodes[i].head != head) continue;
		if(nodes[i].tail != tail) continue;
		return i;
	}
	
	// out of memory; UNDEFINED is the biggest Reg
	if(used >= UNDEFINED)
		die("out of memory");
	
	Reg new = used++; // allocation :)
	
	// basic data setup
	n->type = type;
	n->head = head;
	n->tail = tail;
	n->id = UNDEFINED;
	n->state = CREATED;
	
	// unsure what any derivitives are yet
	for(unsigned int byte = 0; byte < 256; byte++)
		cache[new][byte] = UNDEFINED;
	
	// shorten code
	Node *r = &nodes[new];
	Node *h = &nodes[head];
	Node *t = &nodes[tail];
	
	// determine if the node is nullable
	switch(type) {
		case ALL:   n->null = true;  break;
		case NONE:  n->null = false; break;
		case EPS:   n->null = true;  break;
		case RANGE: n->null = false; break;
		case MARK:  n->null = true;  break;
		case INF:   n->null = true;  break;
		case NOT:   n->null = !h->null; break;
		case AND:   n->null = h->null && t->null; break;
		case OR:    n->null = h->null || t->null; break;
		case SEQ:   n->null = h->null && t->null; break;
	}
	return new;
}

Reg merge(Type type, Reg a, Reg b) {
	if(a == UNDEFINED) return b;
	if(b == UNDEFINED) return a;
	Reg ah = nodes[a].type == type ? nodes[a].head : a;
	Reg at = nodes[a].type == type ? nodes[a].tail : UNDEFINED;
	Reg bh = nodes[b].type == type ? nodes[b].head : b;
	Reg bt = nodes[b].type == type ? nodes[b].tail : UNDEFINED;
	if(ah < bh) return make(type, ah, merge(type, at, b));
	if(bh < ah) return make(type, bh, merge(type, a, bt));
	return make(type, ah, merge(type, at, bt));
}

Reg All() {
	return make(ALL, 0, 0);
}
Reg None() {
	return make(NONE, 0, 0);
}
Reg Eps() {
	return make(EPS, 0, 0);
}
Reg Range(unsigned int min, unsigned int max) {
	return make(RANGE, min, max);
}
Reg Mark(unsigned int kind, unsigned int number) {
	return make(MARK, kind, number);
}
Reg Inf(Reg body) {
	Node *n = &nodes[body];
	if(n->type == RANGE && n->head == 0 && n->tail == 255)
		return All();
	if(n->type == ALL) return All();
	if(n->type == NONE) return Eps();
	if(n->type == EPS) return Eps();
	if(n->type == MAX) return Inf(n->head);
	return make(INF, body, 0);
}
Reg Not(Reg body) {
	if(nodes[body].type == ALL) return None();
	if(nodes[body].type == NONE) return All();
	if(nodes[body].type == NOT) return nodes[body].head;
	return make(NOT, body, 0);
}
Reg And(Reg head, Reg tail) {
	if(nodes[head].type == ALL) return tail;
	if(nodes[tail].type == ALL) return head;
	if(nodes[head].type == NONE) return None();
	if(nodes[tail].type == NONE) return None();
	if(nodes[head].type == EPS) return nodes[tail].null ? Eps() : None();
	if(nodes[tail].type == EPS) return nodes[head].null ? Eps() : None();
	return merge(AND, head, tail);
}
Reg Or(unsigned int head, unsigned int tail) {
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

Reg derive(Reg regex, Byte byte);
Reg derive_force(Reg regex, Byte byte) {
	Reg head = nodes[regex].head;
	Reg tail = nodes[regex].tail;
	switch(nodes[regex].type) {
		case ALL: return All();
		case NONE: return None();
		case EPS: return None();
		case RANGE: return byte >= head && byte <= tail ? Eps() : None();
		case MARK: return None();
		case INF: return Seq(derive(head, byte), regex);
		case NOT: return Not(derive(head, byte));
		case AND: return And(derive(head, byte), derive(tail, byte));
		case OR: return Or(derive(head, byte), derive(tail, byte));
		case SEQ: ;
			Reg dx = Seq(derive(head, byte), tail);
			if(nodes[head].null)
				dx = Or(dx, derive(tail, byte));
			return dx;
	}
	return 0;
}
Reg derive(Reg regex, Reg byte) {
	if(cache[regex][byte] == UNDEFINED)
		cache[regex][byte] = derive_force(regex, byte);
	return cache[regex][byte];
}

let <name> = <expr>;
