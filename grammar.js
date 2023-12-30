const PREC = {
  Or: 1,
  And: 2,
  PrefixNot: 3,
  Is: 4,
  Cmp: 5,
  LikeInBetween: 6,
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
    [$._body, $.derived_table_factor],
  ],
  externals: $ => [
    $.cmp_op,
    $.pm_op,
    $.md_op,
    $.other_op,
  ],
  rules: {
    source_file: $ => optional(sepBy1(';', optional($.statement))),
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
      $.values,
      // TODO: show, table
      $.intersect,
      $.union_ish,
    ),
    values: $ => seq(
      k("VALUES"),
      sepBy1(",",
             seq(
               "(",
               sepBy1(",", $._expr),
               ")",
             )),
    ),
    // UNION or EXCEPT;
    // i.e., those set exprs that bind less tightly than `INTERSECT`.
    union_ish: $ => prec.left(1, seq(
      $._body,
      choice(k("UNION"), k("EXCEPT")),
      optional(choice(k("ALL"), k("DISTINCT"))),
      $._body,
    )),
    intersect: $ => prec.left(2, seq(
      $._body,
      choice(k("INTERSECT")),
      optional(choice(k("ALL"), k("DISTINCT"))),
      $._body,
    )),
    select: $ => seq(
      k("SELECT"),
      optional(choice(
        k("ALL"),
        seq(k("DISTINCT"), $.distinct),
      )),
      optional(sepBy1(",", $.select_item)),
      optional(seq(k("FROM"), sepBy1(",", $.table_and_joins))),
      optional(seq(k("WHERE"), $._expr)),
      optional(seq(k("GROUP"), k("BY"), sepBy1(",", $._expr))),
      optional(seq(k("HAVING"), $._expr)),
      optional(seq(k("OPTIONS"), "(", sepBy1(",", $.select_option), ")"))
    ),
    select_item: $ => choice(
      alias("*", $.wildcard),
      seq($._expr, optional($.column_alias))
    ),
    // TODO - kw reservation, don't accept `AS OF`
    column_alias: $ => seq(
      optional(k("AS")),
      $.identifier,
    ),
    table_and_joins: $ => seq(
      $.table_factor,
      repeat($.join),
    ),
    join_type: $ => choice(
          seq(optional(k("INNER")), k("JOIN")),
          seq(choice(k("LEFT"), k("RIGHT"), k("FULL")), optional(k("OUTER")), k("JOIN")),          
    ),
    join: $ => choice(
      seq(k("CROSS"), k("JOIN")),
      seq(k("NATURAL"), $.join_type, $.table_factor),
      seq(
        $.join_type,
        $.table_factor,
        $.join_constraint,
      )
    ),
    table_factor: $ => choice(
      seq(k("LATERAL"), $.lateral_factor),
      $.derived_table_factor,
      seq("(", $.table_and_joins, ")"),
      seq(k("ROWS"), k("FROM"), $.rows_from),
      seq($.raw_name, choice(
        seq("(",
            optional($.table_factor_args),
            ")",
            optional($.table_alias),
            optional(seq(k("WITH"), k("ORDINALITY"))),
           ),
        optional($.table_alias),
      )),
    ),
    // Assumes parens ARE NOT consumed by caller
    derived_table_factor: $ => seq(
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
      optional(seq(k("WITH"), k("ORDINALITY"))),
    ),
    table_factor_args: $ => choice(
      "*",
      sepBy1(",", $._expr),
    ),
    args: $ => choice(
      "*",
      seq(
        sepBy1(",", $._expr),
        optional(seq(
          k("ORDER"), k("BY"),
          sepBy1(",", $.order_by_expr),
        )),
      ),
    ),
    // TODO -- This should check for keyword reservation
    table_alias: $ => seq(
      optional(k("AS")),
      $._bare_table_alias,
    ),
    lateral_factor: $ => choice(
      $.derived_table_factor,
      seq(k("ROWS"), k("FROM"), $.rows_from),
      seq(
        $.raw_name,
        "(",
        optional($.table_factor_args),
        ")",
        optional($.table_alias),
        optional(seq(k("WITH"), k("ORDINALITY"))),
      ),
    ),
    join_constraint: $ => choice(
      seq(k("ON"), $._expr),
      seq(k("USING"), $.parenthesized_column_list),
    ),
    raw_name: $ => choice(
      seq("[", $.identifier, k("AS"), $.object_name, "]"),
      $.object_name,
    ),
    object_name: $ => sepBy1(".", $.identifier),
    parenthesized_column_list: $ => seq(
      "(",
      sepBy1(",", $.identifier),
      ")",
    ),
    _expr: $ => choice(
      $.bin_expr,
      $._prefix_expr,
    ),
    bin_expr: $ => choice(
      $._regular_binary_operator,
      $.is,
      $.isnull,
      $.like_ish,
      alias($._in, $.in),
      $.between,
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
    is: $ => prec.left(PREC.Is, seq(
      $._expr,
      k("IS"),
      optional(k("NOT")),
      $._is_right_side
    )),
    _is_right_side: $ => choice(
      k("NULL"),
      k("TRUE"),
      k("FALSE"),
      k("UNKNOWN"),
      $.distinct_from,
    ),
    distinct_from: $ => seq(
      k("DISTINCT"),
      k("FROM"),
      $._expr,
    ),
    isnull: $ => k("ISNULL"),
    like_ish: $ => prec.left(PREC.LikeInBetween, seq(
      $._expr,
      optional(k("NOT")),
      choice(k("LIKE"), k("ILIKE")),
      $._expr,
      optional(seq(k("ESCAPE"), $._expr)),
    )),
    _in: $ => prec.left(PREC.LikeInBetween,
      seq(
        $._expr,        
        optional(k("NOT")),
        k("IN"),
        "(",
        choice($.query, sepBy1(",", $._expr)),
        ")",
      ),
    ),
    between: $ => prec.left(PREC.LikeInBetween, "FAIL!between"),
    and: $ => prec.left(PREC.And, seq($._expr, k("AND"), $._expr)),
    or: $ => "FAIL!or",
    at: $ => "FAIL!at",
    collate: $ => "FAIL!collate",
    pg_cast: $ => "FAIL!pg_cast",
    access: $ => "FAIL!access",
    _prefix_expr: $ => choice(
      // TODO - typename-preceded literals
      $.array_expr,
      $.list_expr,
      $.case_expr,
      $.cast_expr,
      $.coalesce_expr,
      $.greatest_expr,
      $.least_expr,
      $.nullif_expr,
      $.exists_expr,
      $.not_expr,
      $.row_expr,
      $.trim_expr,
      $.position_expr,
      $.substring_expr,      
      $.qualified_id,
      $.qualified_funcall,
      alias(prec(PREC.Override, seq("-", $._expr)), $.unary_minus_expr),
      alias(prec(PREC.Override, seq("+", $._expr)), $.unary_plus_expr),
      alias(prec(PREC.Override, seq("~", $._expr)), $.unary_bitwise_not_expr),
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
      k("TRUE"), k("FALSE"), k("NULL"),
      $.interval_value,
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
      optional(choice(k("DISTINCT"), k("ALL"))),
      // TODO: ban `DISTINCT *`
      optional($.args),
      ")",
      optional($.filter),
      optional($.window_spec),      
    ),
    filter: $ => seq(
      k("FILTER"), "(", k("WHERE"), $._expr, ")"
    ),
    order_by_expr: $ => seq(
      $._expr,
      optional(choice(k("ASC"), k("DESC"))),
      optional(seq(k("NULLS"), choice(k("FIRST"), k("LAST")))),
    ),
    window_spec: $ => seq(      
      k("OVER"), "(",
      optional(seq(k("PARTITION"), k("BY"), sepBy1(",", $._expr))),
      optional(seq(k("ORDER"), k("BY"), sepBy1(",", $.order_by_expr))),
      optional($.window_frame),
      ")"
    ),
    window_frame: $ => seq(
      choice(k("ROWS"), k("RANGE"), k("GROUPS")),
      choice(
        seq(k("BETWEEN"), $.wf_bound, k("AND"), $.wf_bound),
        $.wf_bound,
      ),
    ),
    wf_bound: $ => choice(
      seq(k("CURRENT"), k("ROW")),
      seq(
        choice(k("UNBOUNDED"), $.number),
        choice(k("PRECEDING"), k("FOLLOWING"))
      ),
    ),
    kw_op_expr: $ => "FAIL!kw_op",
    array_expr: $ => "FAIL!array",
    list_expr: $ => "FAIL!list",
    coalesce_expr: $ => "FAIL!coalesce",
    greatest_expr: $ => "FAIL!greatest",
    least_expr: $ => "FAIL!least",
    not_expr: $ => "FAIL!not",
    case_expr: $ => "FAIL!case",
    cast_expr: $ => "FAIL!cast",
    homog_expr: $ => "FAIL!homog",
    nullif_expr: $ => "FAIL!nullif",
    exists_expr: $ => "FAIL!exists",
    interval_value: $ => "FAIL!interval_value",
    row_expr: $ => "FAIL!row",
    trim_expr: $ => "FAIL!trim",
    position_expr: $ => "FAIL!position",
    substring_expr: $ => "FAIL!substring",
    
    _regular_binary_operator: $ => choice(
      $.any_ish,
      $._normal_binary_operator,
      $.kw_op_expr,
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
      k("WITH"),
      choice(
        alias(
          seq(
            k("MUTUALLY"), k("RECURSIVE"),
            optional(seq(
              "(",
              sepBy1(",", $.mut_rec_block_option),
              ")"
            )),
            sepBy1(",", $.mut_rec_cte)),
          $.wmr),
        sepBy1(",", $.cte),
      )
    ),
    cte: $ => seq(
      alias($._bare_table_alias, $.table_alias),
      k("AS"),
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
    mut_rec_block_option: $ => seq(
      choice(
        seq(k("RECURSION"), k("LIMIT")),
        seq(k("RETURN"), k("AT"), k("RECURSION"), k("LIMIT")),
        seq(k("ERROR"), k("AT"), k("RECURSION"), k("LIMIT")),
      ),
      optional(seq("=", $.option_value)),
    ),
    option_value: $ => choice(
      seq("(", sepBy1(",", $.option_value), ")"),
      seq("[", sepBy1(",", $.option_value), "]"),
      seq(k("SECRET"), $.raw_name),
      $._value,
      $.identifier,
    ),
    mut_rec_cte: $ => seq(
      $.identifier,
      "(",
      sepBy1(",",
             alias(seq($.identifier, $.data_type), $.mut_rec_column_def)),
      ")",
      k("AS"),
      "(",
      $.query,
      ")",
    ),
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
    create_table: $ => seq(
      k("CREATE"),
      optional(choice(k("TEMP"), k("TEMPORARY"))),
      k("TABLE"),
      optional(seq(k("IF"), k("NOT"), k("EXISTS"))),
      $.object_name,
      $.columns_and_constraints,
    ),
    columns_and_constraints: $ => seq(
      "(",
      // optionality is a Postgres extension
      optional(sepBy1(",", choice($.constraint, $.column))),
      ")",
    ),
    constraint: $ => seq(
      optional(seq(k("CONSTRAINT"), $.identifier)),
      choice(
        seq(
          k("PRIMARY"), k("KEY"),
          $.parenthesized_column_list,
        ),
        seq(
          k("UNIQUE"),
          optional(seq(k("NULLS"), k("NOT"), k("DISTINCT"))),
          $.parenthesized_column_list,
        ),
        seq(
          k("FOREIGN"), k("KEY"),
          $.parenthesized_column_list,
          k("REFERENCES"),
          $.raw_name,
          $.parenthesized_column_list,
        ),
        seq(
          k("CHECK"),
          "(",
          $._expr,
          ")",
        ),
      ),
    ),
    column: $ => seq(
      $.identifier,
      $.data_type,
      optional(seq(k("COLLATE"), $.object_name)),
      repeat($.column_option),
    ),
    data_type: $ => choice(
      seq(choice(k("char"), k("character")), optional(k("varying")), optional($.typ_mod)),
      seq(k("bpchar"), optional($.typ_mod)),
      seq(k("varchar"), optional($.typ_mod)),
      k("string"),
      k("bigint"),
      k("smallint"),
      seq(choice(k("dec"), k("decimal")), optional($.typ_mod)),
      seq(k("double"), optional(k("precision"))),
      seq(k("float"), optional($.precision)),
      k("int"),
      k("integer"),
      k("real"),
      seq(k("time"), optional(seq(choice(k("WITH"), k("WITHOUT")), k("TIME"), k("ZONE")))),
      seq(k("timestamp"), optional($.timestamp_precision), optional(seq(choice(k("WITH"), k("WITHOUT")), k("TIME"), k("ZONE")))),
      seq(k("timestamptz"), optional($.timestamp_precision)),
      seq(k("map"), $.map),
      k("boolean"),
      k("bytes"),
      k("json"),
      seq($.raw_name, optional($.typ_mod)),
    ),
    column_option: $ => seq(
      optional(seq(k("CONSTRAINT"), $.identifier)),
      choice(
        seq(k("NOT"), k("NULL")),
        k("NULL"),
        seq(k("DEFAULT"), $._expr),
        seq(k("PRIMARY"), k("KEY")),
        k("UNIQUE"),
        seq(k("REFERENCES"), $.object_name, $.parenthesized_column_list),
        seq(k("CHECK"), "(", $._expr, ")"),
      ),
    ),
    typ_mod: $ => seq(
      "(",
      sepBy1(",", /-?[0-9]+/),
      ")",
    ),
    map: $ => seq(
      "(",
      $.data_type,
      "=>",
      $.data_type,
      ")",
    ),
    precision: $ => seq(
      "(",
      /[0-9]+/,
      ")",
    ),
    timestamp_precision: $ => seq(
      "(",
      /-?[0-9]+/,
      ")",
    ),
    create_secret: $ => "FAIL!create_secret",
    create_connection: $ => "FAIL!create_connection",
    create_materialized_view: $ => "FAIL!create_materialized_view",
    create_view: $ => seq(
      k("CREATE"),
      optional(seq(k("OR"), k("REPLACE"))),
      optional(choice(k("TEMP"), k("TEMPORARY"))),
      k("VIEW"),
      $.view_definition,
    ),
    view_definition: $ => seq(
      $.object_name,
      optional($.parenthesized_column_list),
      k("AS"),
      $.query,
    ),
    discard: $ => "FAIL!discard",
    drop: $ => "FAIL!drop",
    delete: $ => "FAIL!delete",
    insert: $ => seq(
      k("INSERT"), k("INTO"),
      $.raw_name,
      optional($.parenthesized_column_list),
      choice(
        seq(k("DEFAULT"), k("VALUES")),
        $.query,
      ),
      optional($.returning),
    ),
    returning: $ => seq(
      k("RETURNING"),
      sepBy1(",", $.select_item),
    ),
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
      k("GRANT"),
      $.priv_spec,
      k("ON"),
      $.grant_target_spec,
      k("TO"),
      sepBy1(",", $.role_spec),
    ),
    priv_spec: $ => choice(
      seq(k("ALL"), optional(k("PRIVILEGES"))),
      sepBy1(",", choice(k("INSERT"), k("SELECT"), k("UPDATE"), k("DELETE"), k("USAGE"), k("CREATE"), k("CREATEROLE"), k("CREATEDB"), k("CREATECLUSTER"))),
    ),
    role_spec: $ => seq(
      optional(k("GROUP")),
      $.identifier,
    ),
    grant_target_spec: $ => choice(
      k("SYSTEM"),
      $.grant_all,
      $.grant_non_all,
    ),
    grant_all: $ => seq(
      k("ALL"),
      choice(k("TABLES"), k("TYPES"), k("CLUSTERS"), k("SECRETS"), k("CONNECTIONS"), k("DATABASES"), k("SCHEMAS")),
      optional($.grant_all_in),
    ),
    grant_all_in: $ => seq(
      k("IN"),
      choice(
        seq(k("DATABASE"), sepBy1(",", $.identifier)),
        seq(k("SCHEMA"), sepBy1(",", $.object_name)),
      ),
    ),
    grant_non_all: $ => seq(
      optional(choice(k("TABLE"), k("TYPE"), k("CLUSTER"), k("SECRET"), k("CONNECTION"), k("DATABASE"), k("SCHEMA"))),
      sepBy1(",", $.object_name),
    ),
    grant_role: $ => "FAIL!grant_role",
    // TODO: quoted IDs and arbitrary unicode
    identifier: $ => token(prec(PREC.Override, /[A-Za-z_][A-Za-z_0-9]*/)),
  }
});

function sepBy1(sep, rule) {
  return seq(rule, repeat(seq(sep, rule)));
}

// tree-sitter doesn't yet support the /i flag
// (though there's an unreleased version that does...)
// so we need to use this instead.
function k(kw) {
  let r = '';
  for (let ch of kw) {
    r += ('[' + ch.toLowerCase() + ch.toUpperCase() + ']');
  }
  return alias(token(prec(PREC.Override, new RegExp(r))), kw);
}
