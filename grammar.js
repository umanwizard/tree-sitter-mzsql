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
  // TODO - I don't actually
  // understand how parsers work, so IDK about this.
  // Read about LR(1)/GLR theory to understand what this actually means...
  conflicts: $ => [
    [$._body, $.parenthesized_expr],
    [$._body, $.non_lateral_derived_table_factor],
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
      $._grant,
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
      seq('(', $.query, ')'),
      // TODO: values, show, table
      prec(2, $.intersect),
      prec(1, $.union_ish),
    ),
    // UNION or EXCEPT;
    // i.e., those set exprs that bind less tightly than `INTERSECT`.
    union_ish: $ => prec.left(seq(
      $._body,
      choice("UNION", "EXCEPT"),
      optional(choice("ALL", "DISTINCT")),
      $._body,
    )),
    intersect: $ => "FAIL!intersect",
    select: $ => seq(
      "SELECT",
      optional(choice(
        "ALL",
        seq("DISTINCT", $.distinct),
      )),
      sepBy(",", $.select_item),
      optional(seq("FROM", sepBy1(",", $.table_and_joins))),
      optional(seq("WHERE", $._expr)),
      optional(seq("GROUP", "BY", sepBy(",", $._expr))),
      optional(seq("OPTIONS", "(", sepBy(",", $.select_option), ")"))
    ),
    select_item: $ => choice(
      alias("*", $.wildcard),
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
      $.non_lateral_derived_table_factor,
      seq("(", $.table_and_joins, ")"),
      seq("ROWS", "FROM", $.rows_from),
      seq($.raw_name, choice(
        seq("(",
            optional($.table_factor_args),
            optional($.table_alias),
            optional(seq("WITH", "ORDINALITY")),
            ")"),
        optional($.table_alias),
      )),
    ),
    // Assumes parens ARE NOT consumed by caller
    non_lateral_derived_table_factor: $ => seq(
      "(",
      $.query,
      ")",
      optional($.table_alias),
    ),
    rows_from: $ => seq(
      "(",
      sepBy1(",", seq($.raw_name, $.funcall)),
      ")",
      optional($.table_alias),
      optional(seq("WITH", "ORDINALITY")),
    ),
    // TODO - `ORDER BY` should only sometimes be allowed
    args: $ => choice(
      "*",
      seq(
        sepBy1(",", $._expr),
        optional(seq(
          "ORDER", "BY",
          sepBy1(",", $.order_by_expr),
        )),
      ),
    ),
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
    raw_name: $ => choice(
      seq("[", $.identifier, "AS", $.object_name, "]"),
      $.object_name,
    ),
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
      $.subscript_expr,
      $.pg_cast,
      $.access,
    ),
    subscript_expr: $ => prec.left(PREC.PostfixSubscriptCast, seq(
      $._expr,
      $._subscript,
    )),
    _subscript: $ => seq(
      "[",
      field("start", optional($._expr)),
      field("end", optional(seq(":", $._expr))),
      "]",
    ),
    is: $ => "FAIL!is",
    is_null: $ => "FAIL!is_null",
    like_ish: $ => "FAIL!like_ish",
    and: $ => prec.left(PREC.And, seq($._expr, "AND", $._expr)),
    or: $ => "FAIL!or",
    at: $ => "FAIL!at",
    collate: $ => "FAIL!collate",
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
      $.qualified_funcall,
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
      sepBy1(".", $.identifier), optional(seq(".", alias("*", $.wildcard)))
    //   optional(choice(seq(".", "*"), $.funcall)),
    ),
    qualified_funcall: $ => seq(
      sepBy1(".", $.identifier),
      $.funcall,
    ),
    funcall: $ => seq(
      "(",
      optional(choice("DISTINCT", "ALL")),
      // TODO: ban `DISTINCT *`
      optional($.args),
      ")",
      optional($.filter),
      optional($.window_spec),      
    ),
    filter: $ => seq(
      "FILTER", "(", "WHERE", $._expr, ")"
    ),
    order_by_expr: $ => seq(
      $._expr,
      optional(choice("ASC", "DESC")),
      optional(seq("NULLS", choice("FIRST", "LAST"))),
    ),
    window_spec: $ => seq(      
      "OVER", "(",
      optional(seq("PARTITION", "BY", sepBy(",", $._expr))),
      optional(seq("ORDER", "BY", sepBy(",", $.order_by_expr))),
      optional($.window_frame),
      ")"
    ),
    window_frame: $ => seq(
      choice("ROWS", "RANGE", "GROUPS"),
      choice(
        seq("BETWEEN", $.wf_bound, "AND", $.wf_bound),
        $.wf_bound,
      ),
    ),
    wf_bound: $ => choice(
      seq("CURRENT", "ROW"),
      seq(
        choice("UNBOUNDED", $.number),
        choice("PRECEDING", "FOLLOWING")
      ),
    ),
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
    _grant: $ => seq(
      choice(
        $.grant_privileges,
        $.grant_role,
      ),
    ),
    grant_privileges: $ => seq(
      "GRANT",
      $.priv_spec,
      "ON",
      $.grant_target_spec,
      "TO",
      sepBy1(",", $.role_spec),
    ),
    priv_spec: $ => choice(
      seq("ALL", optional("PRIVILEGES")),
      sepBy1(",", choice("INSERT", "SELECT", "UPDATE", "DELETE", "USAGE", "CREATE", "CREATEROLE", "CREATEDB", "CREATECLUSTER")),
    ),
    role_spec: $ => seq(
      optional("GROUP"),
      $.identifier,
    ),
    grant_target_spec: $ => choice(
      "SYSTEM",
      $.grant_all,
      $.grant_non_all,
    ),
    grant_all: $ => seq(
      "ALL",
      choice("TABLES", "TYPES", "CLUSTERS", "SECRETS", "CONNECTIONS", "DATABASES", "SCHEMAS"),
      optional($.grant_all_in),
    ),
    grant_all_in: $ => seq(
      "IN",
      choice(
        seq("DATABASE", sepBy1(",", $.identifier)),
        seq("SCHEMA", sepBy1(",", $.object_name)),
      ),
    ),
    grant_non_all: $ => seq(
      optional(choice("TABLE", "TYPE", "CLUSTER", "SECRET", "CONNECTION", "DATABASE", "SCHEMA")),
      sepBy1(",", $.object_name),
    ),
    grant_role: $ => "FAIL!grant_role",
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
