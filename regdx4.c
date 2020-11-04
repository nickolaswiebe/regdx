#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#define fatal(ARGS...) (fprintf(stderr, "FATAL: "), fprintf(stderr, ARGS), fprintf(stderr, "\n"), exit(-1))
enum type { UNUSED, LIT, MARK, EMPTY, ALL, NONE, INF, NOT, SEQ, OR, AND };
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
Reg *All() {
	return make0(ALL, 0, 0, true);
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
char mark_names[CACHE_SIZE];
int mark_pos = 0;
Reg *Mark(char *name) {
	int pos = mark_pos;
	for(;;) {
		if(mark_pos == CACHE_SIZE)
			fatal("out of space for mark names");
		mark_names[mark_pos++] = *name;
		if(*name == '\0')
			return make0(MARK, pos, 0, true);
		name++;
	}
}
Reg *Inf(Reg *body) {
	if(body->type == EMPTY) return Empty();
	if(body->type == ALL) return All();
	if(body->type == NONE) return Empty();
	if(body->type == INF) return body;
	if(body->type == LIT && body->ch == 0 && body->len == 256) return All();
	return make1(INF, body, true);
}
Reg *Not(Reg *body) {
	if(body->type == ALL) return None();
	if(body->type == NONE) return All();
	return make1(NOT, body, !body->null);
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
	if(head->type == ALL) return All();
	if(tail->type == ALL) return All();
	if(head->type == NONE) return tail;
	if(tail->type == NONE) return head;
	return make2(OR, head, tail, head->null || tail->null);
}
Reg *And(Reg *head, Reg *tail) {
	if(head == tail) return head;
	if(head->type == AND) return And(head->head, And(head->tail, tail));
	if(head->type == ALL) return tail;
	if(tail->type == ALL) return head;
	if(head->type == NONE) return None();
	if(tail->type == NONE) return None();
	return make2(AND, head, tail, head->null && tail->null);
}
Reg *derive(int ch, Reg *r) {
	if(r->next[ch] == NULL)
		switch(r->type) {
			case LIT: r->next[ch] = (ch >= r->ch && ch < r->ch + r->len) ? Empty() : None(); break;
			case MARK: r->next[ch] = None(); break;
			case EMPTY: r->next[ch] = None(); break;
			case ALL: r->next[ch] = All(); break;
			case NONE: r->next[ch] = None(); break;
			case INF: r->next[ch] = Seq(derive(ch, r->head), r); break;
			case NOT: r->next[ch] = Not(derive(ch, r->head)); break;
			case SEQ:
				r->next[ch] = Seq(derive(ch, r->head), r->tail);
				if(r->head->null)
					r->next[ch] = Or(r->next[ch], derive(ch, r->tail));
			break;
			case OR: r->next[ch] = Or(derive(ch, r->head), derive(ch, r->tail)); break;
			case AND: r->next[ch] = And(derive(ch, r->head), derive(ch, r->tail)); break;
			case UNUSED: fatal("deriving UNUSED node"); break;
		}
	return r->next[ch];
}
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
void marks(Reg *r) {
	switch(r->type) {
		case LIT: return;
		case MARK: printf("%s();\n", &mark_names[r->mark]); return;
		case EMPTY: return;
		case ALL: return;
		case NONE: return;
		case INF: marks(r->head); return;
		case NOT: return;
		case SEQ:
			marks(r->head);
			if(r->head->null)
				marks(r->tail);
			return;
		case OR:
			marks(r->head);
			marks(r->tail);
			return;
		case AND: return;
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
void dfa(Reg *r) {
	if(r->done) return;
	r->done = true;
	printf("st_%i:\n", r->id - 1);
	/*printf("// ");
	dprint(r);
	printf("\n");*/
	marks(r);
	printf("ch = getchar();\n");
	for(int ch = 0; ch < 256; ch++)
		if(r->next[ch] != r->next[0])
			printf("if(ch == %i) goto st_%i;\n", ch, r->next[ch]->id - 1);
	printf("goto st_%i;\n", r->next[0]->id - 1);
	for(int ch = 0; ch < 256; ch++)
		dfa(r->next[ch]);
}
char *reg = NULL;
bool more() { return *reg != '\0'; }
char peek() { return *reg; }
void eat(char c) { if(c == *reg) reg++; else fatal("expected %c got %c", c, *reg); }
char next() { return *reg++; }
char *until(char c) {
	char *ret = reg;
	while(peek() != c)
		if(next() == '\0')
			fatal("unexpected end of regex");
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
			fatal("unexpected end of class");
		if(ate(']')) {
			if(inv)
				r = And(Not(r), Lit(0, 256));
			return r;
		}
		char hold = parse_esc();
		if(ate('-')) {
			if(!more())
				fatal("unexpected end of class");
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
	static char name[256];
	int len = 0;
	while(peek() != '`')
		if(more())
			name[len++] = next();
		else
			fatal("unexpected end of mark");
	eat('`');
	name[len] = '\0';
	return Mark(name);
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
		else fatal("should never happen");
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
int main(int argc, char *argv[]) {
	if(argc != 2) {
		fprintf(stderr, "usage: %s <text to match>\n", argv[0]);
		return -1;
	}
	reg = argv[1];
	Reg *r = parse_and();
	printf("{\n");
	printf("char ch;\n");
	label(r);
	dfa(r);
	printf("}\n");
	exit(0);
}
// (/\*(.*\*/.*)!\*/`block_comment`)|(//[^\n]*\n`block_comment`)|(.`any`)
/*

whitespace [\n\t ]+
block_comment /\*(.*\*/.*)!\*/
line_comment //[^\n]*\n
preprocess #(\\\n|[^\n])+\n
string "(\\.|[^"\\])*"
char '(\\.|[^\\'])'
flow if|else|do|while|for|return|break|continue|switch|case|default
type static|typedef|enum|struct|union|signed|unsigned|short|int|long|float|double|bool|char
number [1-9][0-9]*
number 0x[0-9a-fA-F]+

*/
