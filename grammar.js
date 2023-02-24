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
  extras: $ => [
    /[ \t\r\n]/,
    $.line_comment,
    $.block_comment,
  ],
  externals: $ => [
    $.cmp_op,
    $.pm_op,
    $.md_op,
    $.other_op,
  ],
  rules: {
    source_file: $ => sepBy(';', optional($.statement)),
    statement: $ => choice(
      seq($.query, optional($.as_of)),
      $._create,
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
    line_comment: $ => seq("--", /[^\r\n]*/),
    block_comment: $ => seq(
      "/*",
      repeat(/.|\n|\r/),
      "*/"
    ),
    query: $ => seq(
      optional($._ctes),
      $._body,
      optional($.query_tail),
    ),
    _body: $ => choice(
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
      optional(seq("WHERE", $._expr)),
      optional(seq("GROUP", "BY", sepBy(",", $._expr))),
      optional(seq("OPTIONS", "(", sepBy(",", $.select_option), ")"))
    ),
    select_item: $ => choice(
      field("wildcard", "*"),
      seq($._expr, optional($.column_alias))
    ),
    // TODO - kw reservation, don't accept `AS OF`
    column_alias: $ => seq(
      optional("AS"),
      $.identifier,
    ),
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
    // TODO -- This should check for keyword reservation
    table_alias: $ => seq(
      optional("AS"),
      $._bare_table_alias,
    ),
    lateral_factor: $ => "FAIL!lateral_factor",
    join_constraint: $ => choice(
      seq("ON", $._expr),
      seq("USING", $.parenthesized_column_list),
    ),
    raw_name: $ => seq("[", $.identifier, "AS", $.object_name, "]"),
    object_name: $ => sepBy1(".", $.identifier),
    parenthesized_column_list: $ => seq(
      "(",
      sepBy(",", $.identifier),
      ")",
    ),
    // TODO: quoted IDs and arbitrary unicode
    identifier: $ => // choice(
      // /"[^"]+"/,
      /[A-Za-z_][A-Za-z_0-9]*/,
//    ),
    _expr: $ => choice(
      $.bin_expr,
      $._prefix_expr,
    ),
    bin_expr: $ => choice(
      $._regular_binary_operator,
      $.is,
      $.is_null,
      $.like_ish,
      $.and,
      $.or,
      $.at,
      $.collate,
      $.subscript,
      $.pg_cast,
      $.access,
    ),
    is: $ => "FAIL!is",
    is_null: $ => "FAIL!is_null",
    like_ish: $ => "FAIL!like_ish",
    and: $ => prec.left(PREC.And, seq($._expr, "AND", $._expr)),
    or: $ => "FAIL!or",
    at: $ => "FAIL!at",
    collate: $ => "FAIL!collate",
    subscript: $ => "FAIL!subscript",
    pg_cast: $ => "FAIL!pg_cast",
    access: $ => "FAIL!access",
    _prefix_expr: $ => choice(
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
      prec(PREC.Override, kwRule("NOT", $._expr)),
      kwRule("ROW", $.row),
      kwRule("TRIM", $.trim),
      field("position", seq("POSITION", "(", $.position_special_form, ")")),
      kwRule("SUBSTRING", $.substring),
      $.qualified_id,
      field("unary_minus", prec(PREC.Override, seq("-", $._expr))),
      field("unary_plus", prec(PREC.Override, seq("+", $._expr))),
      field("unary_bitwise_not", prec(PREC.Override, seq("~", $._expr))),
      $._value,
      $.parameter,
      $.parenthesized_expr,
    ),
    // TODO -- Test this extra carefully -- see
    // `parse_parenthesized_expression` in mz
    parenthesized_expr: $ => seq(
      "(", choice($._expr, $.query), ")"
    ),
    _value: $ => choice(
      "TRUE", "FALSE", "NULL",
      kwRule("INTERVAL", $.interval_value),
      // TODO - do we need unary +/- here,
      // or is it enough for it to be in `_prefix_expr` ?
      $.number,
      $.string,
      $.hex_string,
    ),
    number: $ => /[0-9]+((e|E)(\+|-)[0-9]+)?/,
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
    
    _regular_binary_operator: $ => choice(
      $.any_ish,
      $._normal_binary_operator,
      kwRule("OPERATOR", $.kw_op)
    ),
    _normal_binary_operator: $ => choice(
      prec.left(PREC.Cmp, seq($._expr, $.cmp_op, $._expr)),
      prec.left(PREC.PlusMinus, seq($._expr, $.pm_op, $._expr)),
      prec.left(PREC.MultiplyDivide, seq($._expr, $.md_op, $._expr)),
      prec.left(PREC.Other, seq($._expr, $.other_op, $._expr)),
    ),
    any_ish: $ => "FAIL!any_ish",
    select_option: $ => "FAIL!select_option",
    distinct: $ => "FAIL!distinct",
    query_tail: $ => "FAIL!query_tail",
    _ctes: $ => seq(
      "WITH",
      choice(
        field("WMR", seq("MUTUALLY", "RECURSIVE", sepBy(",", $.mut_rec_cte))),
        sepBy(",", $.cte),
      )
    ),
    cte: $ => seq(
      alias($._bare_table_alias, $.table_alias),
      "AS",
      "(",
      $.query,
      ")",
    ),
    // A table alias that can't be preceded by `AS`
    // and that doesn't check for keyword reservation
    _bare_table_alias: $ => seq(
      $.identifier,
      optional($.parenthesized_column_list),
    ),
    mut_rec_cte: $ => "FAIL!mut_rec_cte",
    as_of: $ => "FAIL!as_of",
    _create: $ => choice(
        $.create_database,
        $.create_schema,
        $.create_sink,
        $.create_type,
        $.create_role,
        $.create_cluster,
        $.create_cluster_replica,
        $.create_index,
        $.create_default_index,
        $.create_source,
        $.create_subsource,
        $.create_table,
        $.create_secret,
        $.create_connection,
        $.create_materialized_view,
        $.create_view,
    ),
    create_database: $ => "FAIL!create_database",
    create_schema: $ => "FAIL!create_schema",
    create_sink: $ => "FAIL!create_sink",
    create_type: $ => "FAIL!create_type",
    create_role: $ => "FAIL!create_role",
    create_cluster: $ => "FAIL!create_cluster",
    create_cluster_replica: $ => "FAIL!create_cluster_replica",
    create_index: $ => "FAIL!create_index",
    create_default_index: $ => "FAIL!create_default_index",
    create_source: $ => "FAIL!create_source",
    create_subsource: $ => "FAIL!create_subsource",
    create_table: $ => "FAIL!create_table",
    create_secret: $ => "FAIL!create_secret",
    create_connection: $ => "FAIL!create_connection",
    create_materialized_view: $ => "FAIL!create_materialized_view",
    create_view: $ => seq(
      "CREATE",
      optional(seq("OR", "REPLACE")),
      optional(choice("TEMP", "TEMPORARY")),
      "VIEW",
      $.view_definition,
    ),
    view_definition: $ => seq(
      $.object_name,
      optional($.parenthesized_column_list),
      "AS",
      $.query,
    ),
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
