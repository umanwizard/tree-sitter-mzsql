#include <tree_sitter/parser.h>
#include <string.h>

enum TokenType { OP };

const char *OP_CHARS = "+-*/<>=~!@#%^&|`?";
const char *OP_SPECIALS = "~!@#%^&|`?";

void *tree_sitter_mzsql_external_scanner_create() {}
void tree_sitter_mzsql_external_scanner_destroy(void *payload) {}
unsigned tree_sitter_mzsql_external_scanner_serialize(void *payload,
                                                   char *buffer) {}

void tree_sitter_mzsql_external_scanner_deserialize(void *payload,
                                                    const char *buffer,
                                                    unsigned length) {}

bool scan_operator(int (*lookahead)(void *), void (*accept)(void *),
                          void (*advance)(void *), void *state) {
        char last = 0;
        int matched = 0;
        int advanced = 0;
        bool any_special = false;
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
                last = next;
                if (is_next_valid) {
                        (advance)(state);
                        ++advanced;
                }
                else
                        break;
        }
        return (matched > 0);  
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
        if (!valid_symbols[OP])
                return false;
        return scan_operator(get_lookahead, mark_end, advance_non_whitespace, lexer);
}

