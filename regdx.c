#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <ctype.h>
enum type { UNUSED, LIT, MARK, EMPTY, NONE, INF, SEQ, OR };
struct reg {
	enum type type;
	union {
		struct { struct reg *head, *tail; };
		struct { int ch, len; };
	};
	bool null;
	struct reg *next[256];
	uint64_t mask;
	int id;
	bool done;
};
void dprint(struct reg *r) {
	switch(r->type) {
		case NONE: printf("None"); return;
		case EMPTY: printf("Empty()"); return;
		case MARK: printf("Mark(%i)", r->ch); return;
		case LIT: printf("Lit(%i, %i)", r->ch, r->len); return;
		case INF: printf("Inf("); dprint(r->head); printf(")"); return;
		case SEQ: printf("Seq("); dprint(r->head); printf(", "); dprint(r->tail); printf(")"); return;
		case OR: printf("Or("); dprint(r->head); printf(", "); dprint(r->tail); printf(")"); return;
		case UNUSED: printf("U"); return;
	}
	printf("@");
}
#define fatal(ARGS...) (fprintf(stderr, "FATAL: "), fprintf(stderr, ARGS), fprintf(stderr, "\n"), exit(-1))
#define CACHE_SIZE (512)
struct reg *insert1(struct reg *cache, enum type type, struct reg *body, bool null, uint64_t mask) {
	for(int i = 0; i < CACHE_SIZE; i++)
		if(cache[i].type == UNUSED) {
			cache[i].type = type;
			cache[i].head = body;
			cache[i].null = null;
			cache[i].mask = mask;
			return &cache[i];
		} else if(cache[i].head == body)
			return &cache[i];
	fatal("out of single nodes");
}
struct reg *insert2(struct reg *cache, enum type type, struct reg *head, struct reg *tail, bool null, uint64_t mask) {
	for(int i = 0; i < CACHE_SIZE; i++)
		if(cache[i].type == UNUSED) {
			cache[i].type = type;
			cache[i].head = head;
			cache[i].tail = tail;
			cache[i].null = null;
			cache[i].mask = mask;
			return &cache[i];
		} else if(cache[i].head == head && cache[i].tail == tail)
			return &cache[i];
	fatal("out of double nodes");
}
struct reg *Lit(int ch, int len) {
	static struct reg cache[CACHE_SIZE];
	for(int i = 0; i < CACHE_SIZE; i++)
		if(cache[i].type == UNUSED) {
			cache[i].type = LIT;
			cache[i].ch = ch;
			cache[i].len = len;
			cache[i].null = false;
			return &cache[i];
		} else if(cache[i].ch == ch && cache[i].len == len)
			return &cache[i];
	fatal("out of lit nodes");
}
struct reg *Mark(int n) {
	static struct reg cache[64];
	cache[n].type = MARK;
	cache[n].ch = n;
	cache[n].null = true;
	cache[n].mask = 1 << n;
	return &cache[n];
}
struct reg *Empty() {
	static struct reg cache;
	cache.type = EMPTY;
	cache.null = true;
	return &cache;
}
struct reg *None() {
	static struct reg cache;
	cache.type = NONE;
	cache.null = false;
	return &cache;
}
struct reg *Inf(struct reg *body) {
	if(body->type == EMPTY) return Empty();
	if(body->type == NONE) return Empty();
	if(body->type == INF) return body;
	static struct reg cache[CACHE_SIZE];
	return insert1(cache, INF, body, true, body->mask);
}
struct reg *Seq(struct reg *head, struct reg *tail) {
	if(tail->type == SEQ) return Seq(Seq(head, tail->head), tail->tail);
	if(head->type == EMPTY) return tail;
	if(tail->type == EMPTY) return head;
	if(head->type == NONE) return None();
	if(tail->type == NONE) return None();
	static struct reg cache[CACHE_SIZE];
	return insert2(cache, SEQ, head, tail, head->null && tail->null, head->null ? head->mask | tail->mask : head->mask);
}
struct reg *Or(struct reg *head, struct reg *tail) {
	if(head == tail) return head;
	if(head->type == OR) return Or(head->head, Or(head->tail, tail));
	if(head->type == NONE) return tail;
	if(tail->type == NONE) return head;
	static struct reg cache[CACHE_SIZE];
	return insert2(cache, OR, head, tail, head->null || tail->null, head->mask | tail->mask);
}
struct reg *derive(int ch, struct reg *r) {
	if(r->next[ch] == NULL)
		switch(r->type) {
			case LIT: r->next[ch] = (ch >= r->ch && ch < r->ch + r->len) ? Empty() : None(); break;
			case MARK: r->next[ch] = None(); break;
			case EMPTY: r->next[ch] = None(); break;
			case NONE: r->next[ch] = None(); break;
			case INF: r->next[ch] = Seq(derive(ch, r->head), r); break;
			case SEQ:
				r->next[ch] = Seq(derive(ch, r->head), r->tail);
				if(r->head->null)
					r->next[ch] = Or(r->next[ch], derive(ch, r->tail));
			break;
			case OR: r->next[ch] = Or(derive(ch, r->head), derive(ch, r->tail)); break;
			case UNUSED: fatal("deriving UNUSED node"); break;
		}
	return r->next[ch];
}
void explore(struct reg *r) {
	dprint(r);
	printf("\n");
	for(int ch = 0; ch < 256; ch++)
		if(r->next[ch] == NULL)
			explore(derive(ch, r));
}
void label(struct reg *r) {
	static int id = 1;
	if(r->id > 0) return;
	r->id = id++;
	for(int ch = 0; ch < 256; ch++)
		label(derive(ch, r));
}
void dfa(struct reg *r) {
	if(r->done) return;
	r->done = true;
	/*printf("{%lu,{", r->mask);
	for(int ch = 0; ch < 256; ch++)
		printf("%i,", derive(ch, r)->id - 1);
	printf("}}, // ");*/
	printf("ST_%i: // ", r->id - 1);
	dprint(r);
	printf("\n");
	if(r == None()) {
		printf("stuck();\n\n");
		return;
	}
	for(int i = 0; i < 64; i++)
		if((r->mask >> i) & 1)
			printf("mask(%i);\n", i);
	printf("ch = getchar();\n");
	for(int ch = 0; ch < 256; ch++)
		if(derive(ch, r) != derive(0, r))
			printf("if(ch == %i) goto ST_%i;\n", ch, derive(ch, r)->id - 1);
	printf("goto ST_%i;\n\n", derive(0, r)->id - 1);
	for(int ch = 0; ch < 256; ch++)
		dfa(derive(ch, r));
}
int main(int argc, char *argv[]) {
	if(argc != 2) {
		fprintf(stderr, "usage: %s <text to match>\n", argv[0]);
		return -1;
	}
	reg = argv[1];
	struct reg *r = regex();
	explore(r);
	label(r);
	printf("#include <stdio.h>\n");
	printf("int main() {\n");
	printf("int ch;\n\n");
	dfa(r);
	printf("}\n");
	/*printf("#include <stdio.h>\n");
	printf("struct{int done;int next[256];}table[]={\n");
	dfa(r);
	printf("};\n");
	printf("int main(int argc, char *argv[]) {\n");
	printf("\tint st = 0, ch;\n");
	printf("\twhile(ch = *argv[1]++)\n");
	printf("\t\tst = table[st].next[ch];\n");
	printf("\treturn table[st].done;\n");
	printf("}\n");*/
	return 0;
}
//("([^"]|\\.)*"`string)
//((/\*(.*\*/.*)!\*/|)`comment)
//\[^?-?]?
