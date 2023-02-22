#include <stdbool.h>
#include <string.h>
#include <assert.h>

bool scan_operator(int (*lookahead)(void *), void (*accept)(void *),
                   void (*advance)(void *), void *state);

typedef struct {
        const char *input;
        const char *accepted_frontier;
        const char *point;
} State;

static int lookahead(void *state) {
        State *st = (State *)state;
        return *(st->point);
}

static void accept(void *state) {
        State *st = (State *)state;
        st->accepted_frontier = st->point;
}

static void advance(void *state) {
        State *st = (State *)state;
        ++st->point;
}

static bool test_scan_operator(const char *input, const char *expected) {
        State st = { input, input, input };
        if (!scan_operator(lookahead, accept, advance, &st))
                return false;
        int n = st.accepted_frontier - st.input;
        return (strlen(expected) == n) && (strncmp(input, expected, n) == 0);
}

int main(int argc, char *argv[]) {
        assert(test_scan_operator("++foo", "+"));
        assert(test_scan_operator("++-+", "+"));
        assert(test_scan_operator("++--@+", "+"));        
        assert(test_scan_operator("@++-+foo", "@++-+"));
        assert(test_scan_operator("++@-+--foo", "++@-+"));
}
