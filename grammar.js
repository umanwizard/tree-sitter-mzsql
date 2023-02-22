const PREC = {
  Or: 1,
  And: 2,
  PrefixNot: 3,
  Is: 4,
  Cmp: 5,
  Like: 6,
  Other: 7,
  PlusMinus: 8,
  MultiplyDivide: 9,
  PostfixCollateAt: 10,
  PrefixPlusMinus: 11,
  PostfixSubscriptCast: 12,
  
  Override: 999,
};

module.exports = grammar({
  name: 'mzsql',
  externals: $ => [
    $.op
  ],
  rules: {
    query: $ => seq(
      optional($.ctes),
      $.body,
      $.query_tail,
    ),
    body: $ => choice(
      $.select,
      prec(2, $.intersect),
      prec(1, $.union_ish),
    ),
    // UNION, EXCEPT, or UNION EXCEPT;
    // i.e., those set exprs that bind less tightly than `INTERSECT`.
    union_ish: $ => "FAIL!union_ish",
    intersect: $ => "FAIL!intersect",
    select: $ => seq(
      "SELECT",
      optional(choice(
        "ALL",
        seq("DISTINCT", $.distinct),
      )),
      sepBy(",", $.select_item),
      optional(seq("FROM", sepBy(",", $.table_and_joins))),
      optional(seq("WHERE", $.expr)),
      optional(seq("GROUP", "BY", sepBy(",", $.expr))),
      optional(seq("OPTIONS", "(", sepBy(",", $.select_option), ")"))
    ),
    select_item: $ => choice(
      field("wildcard", "*"),
      seq($.expr, optional($.alias))
    ),
    alias: $ => "FAIL!alias",
    table_and_joins: $ => seq(
      $.table_factor,
      repeat($.join),
    ),
    join_type: $ => choice(
          seq(optional("INNER"), "JOIN"),
          seq(choice("LEFT", "RIGHT", "FULL"), optional("OUTER"), "JOIN"),          
    ),
    join: $ => choice(
      seq("CROSS", "JOIN"),
      seq("NATURAL", $.join_type, $.table_factor),
      seq(
        $.join_type,
        $.table_factor,
        $.join_constraint,
      )
    ),
    table_factor: $ => choice(
      seq("LATERAL", $.lateral_factor),
      seq("(",
          choice($.non_lateral_derived_table_factor,
                 $.table_and_joins),
          ")"),
      seq("ROWS", "FROM", $.rows_from),
      seq($.raw_name, optional(
        seq("(",
            optional($.table_factor_args),
            optional($.table_alias),
            optional(seq("WITH", "ORDINALITY")),
            ")"))),
      seq($.object_name, optional($.table_alias)),
    ),
    non_lateral_derived_table_factor: $ => "FAIL!non_lateral_derived_table_factor",
    rows_from: $ => "FAIL!rows_from",
    table_factor_args: $ => "FAIL!table_factor_args",
    table_alias: $ => "FAIL!table_alias",
    lateral_factor: $ => "FAIL!lateral_factor",
    join_constraint: $ => choice(
      seq("ON", $.expr),
      seq("USING", $.parenthesized_column_list),
    ),
    raw_name: $ => seq("[", $.identifier, "AS", $.object_name, "]"),
    object_name: $ => sepBy1(".", $.identifier),
    parenthesized_column_list: $ => seq(
      "(",
      sepBy(",", $.identifier),
      ")",
    ),
    identifier: $ => choice(
      /"[^"]+"/,
      /[A-Za-z0-9$_\x80-]+/,
    ),
    expr: $ => choice(
      $.regular_binary_operator,
      prec(PREC.Is, $.is),
      prec(PREC.Is, $.is_null), // TODO check this
      prec(PREC.Like,$.like_ish),
      prec(PREC.And, $.and),
      prec(PREC.Or, $.or),
      prec(PREC.PostfixCollateAt, $.at),
      prec(PREC.PostfixCollateAt, $.collate),
      prec(PREC.PostfixSubscriptCast, $.subscript),
      prec(PREC.PostfixSubscriptCast, $.pg_cast),
      prec(PREC.PostfixSubscriptCast, $.access),
      prec(PREC.Override, $.prefix_expr),
    ),
    is: $ => "FAIL!is",
    is_null: $ => "FAIL!is_null",
    like_ish: $ => "FAIL!like_ish",
    and: $ => "FAIL!and",
    or: $ => "FAIL!or",
    at: $ => "FAIL!at",
    collate: $ => "FAIL!collate",
    subscript: $ => "FAIL!subscript",
    pg_cast: $ => "FAIL!pg_cast",
    access: $ => "FAIL!access",
    prefix_expr: $ => choice(
      // TODO - typename-preceded literals 
      kwRule("ARRAY", $.array),
      kwRule("LIST", $.list),
      kwRule("CASE", $.case),
      kwRule("CAST", $.cast),
      kwRule("COALESCE", $.homog),
      kwRule("GREATEST", $.homog),
      kwRule("LEAST", $.homog),
      kwRule("NULLIF", $.nullif),
      kwRule("EXISTS", $.exists),
      kwRule("NOT", $.expr),
      kwRule("ROW", $.row),
      kwRule("TRIM", $.trim),
      field("position", seq("POSITION", "(", $.position_special_form, ")")),
      kwRule("SUBSTRING", $.substring),
      $.qualified_id,
      field("unary_minus", seq("-", $.expr)),
      field("unary_plus", seq("+", $.expr)),
      field("unary_bitwise_not", seq("~", $.expr)),
      $.value,
      $.parameter,
      $.parenthesized_expr,
    ),
    // TODO -- Test this extra carefully -- see
    // `parse_parenthesized_expression` in mz
    parenthesized_expr: $ => seq(
      "(", choice($.expr, $.query), ")"
    ),
    value: $ => choice(
      "TRUE", "FALSE", "NULL",
      kwRule("INTERVAL", $.interval_value),
      // TODO - do we need unary +/- here,
      // or is it enough for it to be in `prefix_expr` ?
      $.number,
      $.string,
      $.hex_string,
    ),
    number: $ => /[0-9]*((e|E)(\+|-)[0-9]*)?/,
    // Quoth Nikhil:
    // 
    // > Adjacent string literals that are separated by whitespace are
    // > concatenated if and only if that whitespace contains at least one newline
    // > character. This bizarre rule matches PostgreSQL and the SQL standard.
    string: $ => /'([^']|'')*'(\s*\n\s*'([^']|'')*')*/,
    hex_string: $ => seq('[xX]', $.string),
    parameter: $ => "FAIL!parameter",
    // TODO -- deal with keywords
    qualified_id: $ => seq(
      sepBy1(".", $.identifier),
      optional(choice(seq(".", "*"), $.funcall)),
    ),
    funcall: $ => "FAIL!funcall",
    kw_op: $ => "FAIL!kw_op",
    array: $ => "FAIL!array",
    list: $ => "FAIL!list",
    case: $ => "FAIL!case",
    cast: $ => "FAIL!cast",
    homog: $ => "FAIL!homog",
    nullif: $ => "FAIL!nullif",
    exists: $ => "FAIL!exists",
    interval_value: $ => "FAIL!interval_value",
    row: $ => "FAIL!row",
    trim: $ => "FAIL!trim",
    position_special_form: $ => "FAIL!position_special_form",
    substring: $ => "FAIL!substring",
    
    regular_binary_operator: $ => choice(
      $.any_ish,
      $.normal_binary_operator,
      kwRule("OPERATOR", $.kw_op)
    ),
    normal_binary_operator: $ => choice(
      prec.left(PREC.Cmp, seq($.expr, $.cmp_op, $.expr)),
      prec.left(PREC.PlusMinus, seq($.expr, /\+|-/, $.expr)),
      prec.left(PREC.MultiplyDivide, seq($.expr, /\/|%/, $.expr)),
      prec.left(PREC.Other, seq($.expr, $.op, $.expr)),
    ),
    cmp_op: $ => /<|<=|<>|!=|>|>=/,
    any_ish: $ => "FAIL!any_ish",
    select_option: $ => "FAIL!select_option",
    distinct: $ => "FAIL!distinct",
    query_tail: $ => "FAIL!query_tail",
    ctes: $ => "FAIL!ctes",
    as_of: $ => "FAIL!as_of",
    create: $ => "FAIL!create",
    discard: $ => "FAIL!discard",
    drop: $ => "FAIL!drop",
    delete: $ => "FAIL!delete",
    insert: $ => "FAIL!insert",
    update: $ => "FAIL!update",
    alter: $ => "FAIL!alter",
    copy: $ => "FAIL!copy",
    set: $ => "FAIL!set",
    reset: $ => "FAIL!reset",
    show: $ => "FAIL!show",
    start: $ => "FAIL!start",
    begin: $ => "FAIL!begin",
    commit: $ => "FAIL!commit",
    rollback: $ => "FAIL!rollback",
    subscribe: $ => "FAIL!subscribe",
    explain: $ => "FAIL!explain",
    declare: $ => "FAIL!declare",
    fetch: $ => "FAIL!fetch",
    close: $ => "FAIL!close",
    prepare: $ => "FAIL!prepare",
    execute: $ => "FAIL!execute",
    deallocate: $ => "FAIL!deallocate",
    raise: $ => "FAIL!raise",
    statement: $ => choice(
      seq($.query, optional($.as_of)),
      $.create,
      $.discard,
      $.drop,
      $.delete,
      $.insert,
      $.update,
      $.alter,
      $.copy,
      $.set,
      $.reset,
      $.show,
      $.start,
      $.begin,
      $.commit,
      $.rollback,
      $.subscribe,
      $.explain,
      $.declare,
      $.fetch,
      $.close,
      $.prepare,
      $.execute,
      $.deallocate,
      $.raise,
      seq('(', $.query, ')'),
    ),
    source_file: $ => seq(sepBy(';', optional($.statement)), optional(';'))
  }
});

function sepBy1(sep, rule) {
  return seq(rule, repeat(seq(sep, rule)));
}

function sepBy(sep, rule) {
  return optional(sepBy1(sep, rule));
}

function kwRule(kw, rule) {
  return field(kw.toLowerCase(), seq(kw, rule));
}
