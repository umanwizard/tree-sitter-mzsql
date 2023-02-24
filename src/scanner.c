#include <tree_sitter/parser.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

enum TokenType { CMP_OP, PM_OP, MD_OP, OTHER_OP };

const char *OP_CHARS = "+-*/<>=~!@#%^&|`?";
const char *OP_SPECIALS = "~!@#%^&|`?";

void *tree_sitter_mzsql_external_scanner_create() {
        return NULL;
}
void tree_sitter_mzsql_external_scanner_destroy(void *payload) {}
unsigned tree_sitter_mzsql_external_scanner_serialize(void *payload,
                                                      char *buffer) {
        return 0;
}

void tree_sitter_mzsql_external_scanner_deserialize(void *payload,
                                                    const char *buffer,
                                                    unsigned length) {}

// Whether the two given characters represent at comparison operator.
// If `last` is 0, only `next` is considered.
static bool is_cmp(int last, int next) {
        //     cmp_op: $ => /<|<=|<>|!=|>|>=|=/,
        if (last >= 128 || next >= 128)
                return false;
        char lc = last, nc = next;
        if (!lc) {
               return nc ==  '<'
                       || nc == '>'
                       || nc == '=';
        }
        char arr[3] = {lc,nc,0};
        return !(strcmp(arr, "<=")
                 && strcmp(arr, "<>")
                 && strcmp(arr, "!=")
                 && strcmp(arr, ">="));
}

bool scan_operator(int (*lookahead)(void *), void (*accept)(void *),
                   void (*advance)(void *), void *state, enum TokenType *tt_out) {
        char last = 0;
        int matched = 0;
        int advanced = 0;
        bool any_special = false;
        bool maybe_cmp = false;
        bool maybe_pm = false;
        bool maybe_md = false;

        for (;;) {
                int next = (lookahead)(state);
                // All op symbols are ASCII
                bool is_next_valid = (next != 0) && (next < 128) && (strchr(OP_CHARS, next) != NULL);
                bool is_last_special = last && (strchr(OP_SPECIALS, last) != NULL);
                if ((last == '-' && next == '-') || (last == '/' && next == '*')) {
                        break;
                }
                any_special |= is_last_special;
                if (any_special || !(last == '-' || last == '+') || advanced <= 1) {
                        // Accept `last`.
                        (accept)(state);
                        matched = advanced;
                }                
                if (is_next_valid) {
                        (advance)(state);
                        ++advanced;
                }                
                else
                        break;
                maybe_cmp = is_cmp(last, next);
                maybe_pm = next == '+' || next == '-';
                maybe_md = next == '*' || next == '/' || next == '%';

                last = next;
        }
        if (matched > 0) {
                assert(tt_out);
                if (matched <= 2 && maybe_cmp)
                        *tt_out = CMP_OP;
                else if (matched == 1 && maybe_pm)
                        *tt_out = PM_OP;
                else if (matched == 1 && maybe_md)
                        *tt_out = MD_OP;
                else
                        *tt_out = OTHER_OP;                
                return true;
        }
        return false;
}

static void advance_non_whitespace(void *state) {
        TSLexer *lexer = (TSLexer *)state;
        (lexer->advance)(lexer, false);
}

static int get_lookahead(void *state) {
        TSLexer *lexer = (TSLexer *)state;
        return lexer->lookahead;
}

static void mark_end(void *state) {
        TSLexer *lexer = (TSLexer *)state;
        (lexer->mark_end)(lexer);
}

bool tree_sitter_mzsql_external_scanner_scan(void *payload, TSLexer *lexer,
                                              const bool *valid_symbols) {
        // skip ws
        for (;;) {
                int next = lexer->lookahead;
                if (next == ' ' || next == '\t' || next == '\r' || next == '\n') {
                        (lexer->mark_end)(lexer);
                        (lexer->advance)(lexer, true);
                }
                else {
                        break;
                }
        }
        enum TokenType tt;
        bool result = scan_operator(get_lookahead, mark_end, advance_non_whitespace, lexer, &tt);
        if (result && valid_symbols[tt]) {
                lexer->result_symbol = tt;
                return true;
        }
        return false;
}

