#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#define fatal(ARGS...) (fprintf(stderr, "FATAL: "), fprintf(stderr, ARGS), fprintf(stderr, "\n"), exit(-1))
enum type { UNUSED, LIT, MARK, EMPTY, NONE, INF, SEQ, OR };
struct reg {
	enum type type;
	union {
		struct reg *head;
		int ch;
		int mark;
	};
	union {
		struct reg *tail;
		int len;
	};
	bool null;
	struct reg *next[256];
	int id;
	bool done;
};
typedef struct reg Reg;
typedef enum type Type;
#define CACHE_SIZE (4096)
Reg cache[CACHE_SIZE];
Reg *make0(Type type, int ch, int len, bool null) {
	for(int i = 0; i < CACHE_SIZE; i++)
		if(cache[i].type == type && cache[i].ch == ch && cache[i].len == len) {
			return &cache[i];
		} else if(cache[i].type == UNUSED) {
			cache[i].type = type;
			cache[i].ch = ch;
			cache[i].len = len;
			cache[i].null = null;
			return &cache[i];
		}
	fatal("out of nodes");
}
#define make1(type,body,null) make2(type, body, NULL, null)
Reg *make2(Type type, Reg *head, Reg *tail, bool null) {
	for(int i = 0; i < CACHE_SIZE; i++)
		if(cache[i].type == type && cache[i].head == head && cache[i].tail == tail) {
			return &cache[i];
		} else if(cache[i].type == UNUSED) {
			cache[i].type = type;
			cache[i].head = head;
			cache[i].tail = tail;
			cache[i].null = null;
			return &cache[i];
		}
	fatal("out of nodes");
}
Reg *None() {
	return make0(NONE, 0, 0, false);
}
Reg *Empty() {
	return make0(EMPTY, 0, 0, true);
}
Reg *Lit(int ch, int len) {
	return make0(LIT, ch, len, false);
}
Reg *Mark(int mark) {
	return make0(MARK, mark, 0, true);
}
Reg *Inf(Reg *body) {
	if(body->type == EMPTY) return Empty();
	if(body->type == NONE) return Empty();
	if(body->type == INF) return body;
	return make1(INF, body, true);
}
Reg *Seq(Reg *head, Reg *tail) {
	if(tail->type == SEQ) return Seq(Seq(head, tail->head), tail->tail);
	if(head->type == EMPTY) return tail;
	if(tail->type == EMPTY) return head;
	if(head->type == NONE) return None();
	if(tail->type == NONE) return None();
	return make2(SEQ, head, tail, head->null && tail->null);
}
Reg *Or(Reg *head, Reg *tail) {
	if(head == tail) return head;
	if(head->type == OR) return Or(head->head, Or(head->tail, tail));
	if(head->type == NONE) return tail;
	if(tail->type == NONE) return head;
	return make2(OR, head, tail, head->null || tail->null);
}
Reg *derive(int ch, Reg *r) {
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
int mark_count = 0;
char *marks[256];
Reg *add_mark(char *name) {
	if(mark_count == 256)
		fatal("too many marks");
	marks[mark_count] = name;
	return Mark(mark_count++);
}
bool marked(Reg *r, int mark) {
	switch(r->type) {
		case LIT: return false;
		case MARK: return r->mark == mark;
		case EMPTY: return false;
		case NONE: return false;
		case INF: return marked(r->head, mark);
		case SEQ: return marked(r->head, mark) || (r->null && marked(r->tail, mark));
		case OR: return marked(r->head, mark) || marked(r->tail, mark);
		case UNUSED: fatal("mark testing UNUSED node"); break;
	}
}
void label(Reg *r) {
	static int id = 1;
	if(r->id > 0) return;
	r->id = id++;
	for(int ch = 0; ch < 256; ch++)
		label(derive(ch, r));
}
void dfa(struct reg *r) {
	if(r->done) return;
	r->done = true;
	printf("state %i\n", r->id - 1);
	for(int mark = 0; mark < mark_count; mark++)
		if(marked(r, mark))
			printf("mark %s\n", marks[mark]);
	for(int ch = 0; ch < 256; ch++)
		if(r->next[ch] != r->next[0])
			printf("transition %i %i\n", ch, r->next[ch]->id - 1);
	printf("transition %i\n\n", r->next[0]->id - 1);
	for(int ch = 0; ch < 256; ch++)
		dfa(r->next[ch]);
}
char *reg = NULL;
bool more() { return *reg != '\0'; }
char peek() { return *reg; }
void eat(char c) { if(c == *reg) reg++; else fatal("expected %c got %c", c, *reg); }
char next() { return *reg++; }
struct reg *regex();
struct reg *base() {
	if(peek() == '(') {
		eat('(');
		struct reg *r = regex();
		eat(')');
		return r;
	}
	if(peek() == '[') {
		eat('[');
		bool inv = false;
		if(peek() == '^') {
			eat('^');
			inv = true;
		}
		struct reg *r = None();
		char last = 0;
		if(peek() == '-') {
			eat('-');
			last = '-';
			r = Or(r, Lit('-', 1));
		}
		if(peek() == ']') {
			eat(']');
			last = ']';
			r = Or(r, Lit(']', 1));
		}
		if(last == 0)
			r = Or(r, Lit(last = next(), 1));
		while(peek() != ']') {
			if(peek() == '-') {
				eat('-');
				char end = next();
				if(last > end)
			}
		}
	}
	if(peek() == '.') {
		eat('.');
		return Lit(0, 256);
	}
	if(peek() == '`') {
		eat('`');
		char *end = reg;
		while(*end != '`')
			if(*end == '\0')
				fatal("unexpected end of mark");
			else
				end++;
		*end = '\0';
		Reg *r = add_mark(reg);
		reg = end + 1;
		return r;
	}
	if(peek() == '\\')
		eat('\\');
	return Lit(next(), 1);
}
struct reg *factor() {
	struct reg *r = base();
	while(more() && peek() == '*') {
		eat('*');
		r = Inf(r);
	}
	return r;
}
struct reg *term() {
	struct reg *r = factor();
	while(more() && peek() != ')' && peek() != '|') {
		struct reg *r2 = factor();
		r = Seq(r, r2);
	}
	return r;
}
struct reg *regex() {
	struct reg *r = term();
	while(more() && peek() == '|') {
		eat('|');
		struct reg *r2 = term();
		r = Or(r, r2);
	}
	return r;
}
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
int main(int argc, char *argv[]) {
	if(argc != 2) {
		fprintf(stderr, "usage: %s <text to match>\n", argv[0]);
		return -1;
	}
	reg = argv[1];
	Reg *r = regex();
	dprint(r);
	//label(r);
	//dfa(r);
	exit(0);
}
