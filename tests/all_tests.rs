mod infra;

// Your tests go here!
success_tests! {
    // Boa Regression Tests
    {
        name: boa_quick_brown_fox,
        file: "boa/quick_brown_fox.snek",
        expected: "-3776",
    },
    {
        name: boa_binding1,
        file: "boa/binding1.snek",
        expected: "-5",
    },

        ////// BEGIN COBRA PROVIDED TESTS
    // Tests we gave students
    {
        name: cobra_false_val,
        file: "cobra/false_val.snek",
        expected: "false",
    },
    {
        name: cobra_example1,
        file: "cobra/example1.snek",
        expected: "6",
    },
    {
        name: cobra_example2,
        file: "cobra/example2.snek",
        expected: "-6",
    },
    {
        name: cobra_factorial0,
        file: "cobra/factorial.snek",
        input: "0",
        expected: "1",
    },
    {
        name: cobra_factorial1,
        file: "cobra/factorial.snek",
        input: "1",
        expected: "1",
    },
    {
        name: cobra_factorial2,
        file: "cobra/factorial.snek",
        input: "2",
        expected: "2",
    },
    {
        name: cobra_factorial3,
        file: "cobra/factorial.snek",
        input: "3",
        expected: "6",
    },
    {
        name: cobra_input_compare_1,
        file: "cobra/input_compare.snek",
        input: "2",
        expected: "false",
    },
    {
        name: cobra_input_compare_2,
        file: "cobra/input_compare.snek",
        input: "10",
        expected: "true",
    },
    // Number and Boolean Literals
    {
        name: cobra_num,
        file: "cobra/num.snek",
        expected: "644",
    },

    // Input Expression
    {
        name: cobra_input_default,
        file: "cobra/input0.snek",
        expected: "0",
    },
    {
        name: cobra_input_num,
        file: "cobra/input0.snek",
        input: "123",
        expected: "123",
    },

    // Simple Number Expressions
    {
        name: boa_add1,
        file: "boa/add1.snek",
        expected: "73",
    },
    {
        name: boa_add1_sub1,
        file: "boa/add1_sub1.snek",
        expected: "4",
    },
    {
        name: cobra_add_num,
        file: "cobra/add.snek",
        input: "10",
        expected: "15",
    },

    // Nested Arithmetic Expressions
    {
        name: boa_nested_arith0,
        file: "boa/nested_arith0.snek",
        expected: "35",
    },
    {
        name: boa_nested_arith1,
        file: "boa/nested_arith1.snek",
        expected: "25",
    },
    {
        name: boa_nested_arith2,
        file: "boa/nested_arith2.snek",
        expected: "0",
    },
    {
        name: boa_nested_arith3,
        file: "boa/nested_arith3.snek",
        input: "8",
        expected: "1117",
    },
    {
        name: boa_nested_arith4,
        file: "boa/nested_arith4.snek",
        expected: "-1",
    },

    // Comparison Expressions
    {
        name: cobra_equals_bool,
        file: "cobra/equals_bool.snek",
        expected: "true",
    },
    {
        name: cobra_compare_expr_succ0,
        file: "cobra/compare_expr_succ0.snek",
        expected: "true",
    },

    {
        name: cobra_compare_expr_succ2,
        file: "cobra/compare_expr_succ2.snek",
        expected: "true",
    },

    // Let expressions
    {
        name: cobra_binding0,
        file: "cobra/binding0.snek",
        expected: "5",
    },
    {
        name: cobra_binding1,
        file: "cobra/binding1.snek",
        expected: "-5",
    },

    {
        name: cobra_binding_expr,
        file: "cobra/binding_expr.snek",
        expected: "1225",
    },
    {
        name: boa_binding_nested,
        file: "boa/binding_nested.snek",
        expected: "1",
    },

    {
        name: cobra_binding_chain,
        file: "cobra/binding_chain.snek",
        expected: "3",
    },
    {
        name: cobra_binding_nested_chain,
        file: "cobra/binding_nested_chain.snek",
        expected: "12",
    },

    // Let expressions with shadowing
    {
        name: boa_shadowed_binding_succ0,
        file: "boa/shadowed_binding_succ0.snek",
        expected: "100",
    },
    {
        name: boa_shadowed_binding_succ1,
        file: "boa/shadowed_binding_succ1.snek",
        expected: "7",
    },
    {
        name: boa_shadowed_binding_succ2,
        file: "boa/shadowed_binding_succ2.snek",
        expected: "150",
    },
    {
        name: boa_shadowed_binding_succ3,
        file: "boa/shadowed_binding_succ3.snek",
        expected: "5",
    },
    {
        name: boa_shadowed_binding_succ4,
        file: "boa/shadowed_binding_succ4.snek",
        expected: "18",
    },
    {
        name: boa_shadowed_binding_succ5,
        file: "boa/shadowed_binding_succ5.snek",
        expected: "5",
    },
    {
        name: boa_shadowed_binding_succ6,
        file: "boa/shadowed_binding_succ6.snek",
        expected: "3",
    },
    {
        name: boa_shadowed_binding_succ7,
        file: "boa/shadowed_binding_succ7.snek",
        expected: "200",
    },

    // Misc complex expressions with arithmetic and let bindings
    {
        name: boa_complex_expr,
        file: "boa/complex_expr.snek",
        expected: "6",
    },

    // If expressions
    {
        name: cobra_if_expr_succ2,
        file: "cobra/if_expr_succ2.snek",
        expected: "8",
    },
    {
        name: cobra_if_expr_succ3,
        file: "cobra/if_expr_succ3.snek",
        expected: "7",
    },

    // Set expr
    {
        name: cobra_set_expr_succ0,
        file: "cobra/set_expr1.snek",
        expected: "10",
    },
    {
        name: cobra_set_expr_succ1,
        file: "cobra/set_expr2.snek",
        expected: "true",
    },
    {
        name: cobra_set_expr_succ2,
        file: "cobra/set_expr3.snek",
        input: "25",
        expected: "true",
    },
    {
        name: cobra_set_expr_succ3,
        file: "cobra/set_expr3.snek",
        input: "20",
        expected: "false",
    },

    {
        name: cobra_loop_expr_succ0,
        file: "cobra/loop_expr0.snek",
        input: "3",
        expected: "6",
    },
    {
        name: cobra_loop_expr_succ1,
        file: "cobra/loop_expr0.snek",
        input: "7",
        expected: "5040",
    },
    {
        name: cobra_loop_expr_succ2,
        file: "cobra/loop_expr1.snek",
        expected: "-6",
    },

    ////// END COBRA PROVIDED TESTS

    // Unit Tests
    {
        name: cobra_input_plus1_1,
        file: "cobra/input_plus1.snek",
        input: "10",
        expected: "11",
    },
    {
        name: cobra_input_plus1_2,
        file: "cobra/input_plus1.snek",
        input: "0",
        expected: "1",
    },
    {
        name: cobra_true_val,
        file: "cobra/true_val.snek",
        expected: "true",
    },
    {
        name: cobra_negative_one_val,
        file: "cobra/negative_one_val.snek",
        expected: "-1",
    },
    {
        name: cobra_zero_val,
        file: "cobra/zero_val.snek",
        expected: "0",
    },
    {
        name: cobra_one_val,
        file: "cobra/one_val.snek",
        expected: "1",
    },
    {
        name: cobra_five_val,
        file: "cobra/five_val.snek",
        expected: "5",
    },
    {
        name: cobra_compare_greater_1,
        file: "cobra/compare_greater.snek",
        input: "6",
        expected: "true",
    },
    {
        name: cobra_compare_greater_2,
        file: "cobra/compare_greater.snek",
        input: "5",
        expected: "false",
    },
    {
        name: cobra_compare_greater_3,
        file: "cobra/compare_greater.snek",
        input: "4",
        expected: "false",
    },
    {
        name: cobra_compare_greater_equal_1,
        file: "cobra/compare_greater_equal.snek",
        input: "6",
        expected: "true",
    },
    {
        name: cobra_compare_greater_equal_2,
        file: "cobra/compare_greater_equal.snek",
        input: "5",
        expected: "true",
    },
    {
        name: cobra_compare_greater_equal_3,
        file: "cobra/compare_greater_equal.snek",
        input: "4",
        expected: "false",
    },
    {
        name: cobra_compare_equal_1,
        file: "cobra/compare_equal.snek",
        input: "6",
        expected: "false",
    },
    {
        name: cobra_compare_equal_2,
        file: "cobra/compare_equal.snek",
        input: "5",
        expected: "true",
    },
    {
        name: cobra_compare_equal_3,
        file: "cobra/compare_equal.snek",
        input: "4",
        expected: "false",
    },
    {
        name: cobra_compare_less_equal_1,
        file: "cobra/compare_less_equal.snek",
        input: "6",
        expected: "false",
    },
    {
        name: cobra_compare_less_equal_2,
        file: "cobra/compare_less_equal.snek",
        input: "5",
        expected: "true",
    },
    {
        name: cobra_compare_less_equal_3,
        file: "cobra/compare_less_equal.snek",
        input: "4",
        expected: "true",
    },
    {
        name: cobra_compare_less_1,
        file: "cobra/compare_less.snek",
        input: "6",
        expected: "false",
    },
    {
        name: cobra_compare_less_2,
        file: "cobra/compare_less.snek",
        input: "5",
        expected: "false",
    },
    {
        name: cobra_compare_less_3,
        file: "cobra/compare_less.snek",
        input: "4",
        expected: "true",
    },
    {
        name: cobra_equal_bool,
        file: "cobra/equal_bool.snek",
        expected: "true",
    },
   {
        name: cobra_not_equal_bool,
        file: "cobra/not_equal_bool.snek",
        expected: "false",
    },
    {
        name: cobra_good_set,
        file: "cobra/good_set.snek",
        expected: "6",
    },
    {
        name: cobra_basic_block,
        file: "cobra/basic_block.snek",
        expected: "3",
    },
    {
        name: cobra_nested_loops_1,
        file: "cobra/nested_loops.snek",
        input: "1",
        expected: "1",
    },
    {
        name: cobra_nested_loops_2,
        file: "cobra/nested_loops.snek",
        input: "5",
        expected: "125",
    },
    {
        name: cobra_nested_loops_3,
        file: "cobra/nested_loops.snek",
        input: "0",
        expected: "0",
    },
    {
        name: cobra_nested_loops_4,
        file: "cobra/nested_loops.snek",
        input: "1000",
        expected: "1000000000",
    },

    // Ensemble Tests
    {
        name: cobra_big_negative_input,
        file: "cobra/just_input.snek",
        input: "-9223372036854775808",
        expected: "-9223372036854775808",
    },
    {
        name: cobra_equal_bool_compound,
        file: "cobra/equal_bool_compound.snek",
        expected: "true",
    },
    {
        name: cobra_equal_bool_let,
        file: "cobra/equal_bool_let.snek",
        expected: "true",
    },
    {
        name: cobra_if_nested,
        file: "cobra/if_nested.snek",
        expected: "5",
    },
    {
        name: cobra_if_let,
        file: "cobra/if_let.snek",
        expected: "5",
    },
    {
        name: cobra_ok_positive_input,
        file: "cobra/just_input.snek",
        input: "9223372036854775807",
        expected: "9223372036854775807",
    },
    {
        name: diamondback_print_int,
        file: "diamondback/print_int.snek",
        expected: "5\n5",
    },
    {
        name: diamondback_print_bool,
        file: "diamondback/print_bool.snek",
        expected: "true\ntrue",
    },
    {
        name: diamondback_print_test,
        file: "diamondback/print_test.snek",
        input: "3",
        expected: "5\n4\n3\n3",
    },
    {
        name: diamondback_let_print,
        file: "diamondback/let_print_test.snek",
        expected: "5\n5",
    },
    {
        name: diamondback_sophia_test,
        file: "diamondback/sophia_test.snek",
        expected: "243\n244",
    },
    {
        name: diamondback_fact_5,
        file: "diamondback/fact.snek",
        input: "5",
        expected: "120"
    },
    {
        name: diamondback_fact_10,
        file: "diamondback/fact.snek",
        input: "10",
        expected: "3628800"
    },
    {
        name: diamondback_even_odd_one_print,
        file: "diamondback/even_odd_one_print.snek",
        input: "1",
        expected: "false"
    },
    {
        name: diamondback_even_odd_11_print,
        file: "diamondback/even_odd_one_print.snek",
        input: "11",
        expected: "false"
    },
    {
        name: diamondback_even_odd_1000_print,
        file: "diamondback/even_odd.snek",
        input: "1000",
        expected: "1000\ntrue\ntrue"
    },
    {
        name: diamondback_fib_1,
        file: "diamondback/fib.snek",
        input: "2",
        expected: "1\n1\n2"
    },
    {
        name: diamondback_fib_2,
        file: "diamondback/fib.snek",
        input: "10",
        expected: "34\n55\n89"
    },
    {
        name: diamondback_fib_3,
        file: "diamondback/fib.snek",
        input: "18",
        expected: "1597\n2584\n4181"
    },
    {
        name: diamondback_many_args,
        file: "diamondback/many_args.snek",
        input: "5",
        expected: "760"
    },
    {
        name: diamondback_add_up_1,
        file: "diamondback/add_up.snek",
        input: "1",
        expected: "1"
    },
    {
        name: diamondback_add_up_2,
        file: "diamondback/add_up.snek",
        input: "5",
        expected: "15"
    },
    {
        name: diamondback_add_up_3,
        file: "diamondback/add_up.snek",
        input: "17",
        expected: "153"
    },
    {
        name: diamondback_print_up_to_1,
        file: "diamondback/print_up_to.snek",
        input: "2",
        expected: "1\n1\n2\n4"
    },
    {
        name: diamondback_print_up_to_2,
        file: "diamondback/print_up_to.snek",
        input: "4",
        expected: "1\n1\n2\n4\n3\n9\n4\n16"
    },
}

runtime_error_tests! {
    {
        name: cobra_mul_over,
        file: "cobra/mul_over.snek",
        expected: "overflow",
    },
    {
        name: cobra_too_negative_input,
        file: "cobra/just_input.snek",
        input: "-9223372036854775809",
        expected: "Invalid Input",
    },
    {
        name: cobra_too_positive_input,
        file: "cobra/just_input.snek",
        input: "9223372036854775808",
        expected: "Invalid Input",
    },
}

static_error_tests! {
    {
        name: diamondback_set_input,
        file: "diamondback/input_set_fail.snek",
        expected: "keyword"
    },

    {
        name: cobra_invalid_argument,
        file: "cobra/invalid_argument.snek",
        expected: "mismatch",
    },
    {
        name: cobra_invalid_argument_add1,
        file: "cobra/invalid_argument_add1.snek",
        expected: "mismatch",
    },
    {
        name: cobra_invalid_argument_greater,
        file: "cobra/invalid_argument_greater.snek",
        expected: "mismatch",
    },
        {
        name: cobra_invalid_argument_equal,
        file: "cobra/invalid_argument_equal.snek",
        expected: "mismatch",
    },
    {
        name: cobra_number_bounds_fail,
        file: "cobra/number_bounds_fail.snek",
        expected: "Invalid",
    },
    {
        name: cobra_failed_set,
        file: "cobra/failed_set.snek",
        expected: "mismatch",
    },
    {
        name: diamondback_duplicate_params,
        file: "diamondback/duplicate_params.snek",
        expected: "Duplicate Argument",
    },


    // Type errors
    {
        name: cobra_if_expr_succ0,
        file: "cobra/if_expr_succ0.snek",
        expected: "mismatch",
    },
    {
        name: cobra_invalid_argument_fail8,
        file: "cobra/if_expr_input.snek",
        expected: "mismatch",
    },
    {
        name: cobra_invalid_argument_fail0,
        file: "cobra/invalid_argument_fail0.snek",
        expected: "mismatch",
    },
    {
        name: cobra_invalid_argument_fail1,
        file: "cobra/invalid_argument_fail1.snek",
        expected: "mismatch",
    },
    {
        name: cobra_invalid_argument_fail2,
        file: "cobra/invalid_argument_fail2.snek",
        expected: "mismatch",
    },
    {
        name: cobra_invalid_argument_fail3,
        file: "cobra/invalid_argument_fail3.snek",
        expected: "mismatch",
    },
    {
        name: cobra_invalid_argument_fail4,
        file: "cobra/invalid_argument_fail4.snek",
        expected: "mismatch",
    },
    {
        name: cobra_invalid_argument_fail6,
        file: "cobra/invalid_argument_fail6.snek",
        expected: "mismatch",
    },
    {
        name: cobra_invalid_argument_fail11,
        file: "cobra/invalid_argument_fail11.snek",
        expected: "mismatch",
    },
    // Invalid S-expressions
    {
        name: cobra_parse_sexp_fail1,
        file: "cobra/parse_sexp_fail1.snek",
        expected: "Invalid",
    },
    {
        name: cobra_parse_sexp_fail2,
        file: "cobra/parse_sexp_fail2.snek",
        expected: "Invalid",
    },

    // Invalid tokens/operators
    {
        name: cobra_parse_token_fail1,
        file: "cobra/parse_token_fail1.snek",
        expected: "Invalid",
    },
    {
        name: cobra_parse_token_fail2,
        file: "cobra/parse_token_fail2.snek",
        expected: "Invalid",
    },
    {
        name: cobra_parse_token_fail3,
        file: "cobra/parse_token_fail3.snek",
        expected: "Invalid",
    },
    {
        name: cobra_parse_token_fail4,
        file: "cobra/parse_token_fail4.snek",
        expected: "Invalid",
    },


    // Invalid/Out of bounds Number Literal
    {
        name: cobra_number_bounds_fail0,
        file: "cobra/number_bounds_fail0.snek",
        expected: "Invalid",
    },
    {
        name: cobra_number_bounds_fail1,
        file: "cobra/number_bounds_fail1.snek",
        expected: "Invalid",
    },

    // Invalid operator arguments
    {
        name: cobra_parse_op_fail1,
        file: "cobra/parse_op_fail1.snek",
        expected: "Invalid",
    },
    {
        name: cobra_parse_op_fail2,
        file: "cobra/parse_op_fail2.snek",
        expected: "Invalid",
    },
    {
        name: cobra_parse_op_fail3,
        file: "cobra/parse_op_fail3.snek",
        expected: "Invalid",
    },
    {
        name: cobra_parse_op_fai4,
        file: "cobra/parse_op_fail4.snek",
        expected: "Invalid",
    },
    {
        name: cobra_parse_op_fail5,
        file: "cobra/parse_op_fail5.snek",
        expected: "Invalid",
    },
    {
        name: cobra_parse_op_fail6,
        file: "cobra/parse_op_fail6.snek",
        expected: "Invalid",
    },
    {
        name: cobra_parse_op_fail7,
        file: "cobra/parse_op_fail7.snek",
        expected: "Invalid",
    },
    {
        name: cobra_parse_op_fail8,
        file: "cobra/parse_op_fail8.snek",
        expected: "Invalid",
    },

    // Invalid let expressions
    {
        name: cobra_parse_let_nobindings_fail,
        file: "cobra/parse_let_nobindings_fail.snek",
        expected: "Invalid",
    },
    {
        name: cobra_parse_let_improperargs_fail1,
        file: "cobra/parse_let_improperargs_fail1.snek",
        expected: "Invalid",
    },
    {
        name: cobra_parse_let_improperargs_fail2,
        file: "cobra/parse_let_improperargs_fail2.snek",
        expected: "Invalid",
    },
    {
        name: cobra_parse_let_improperargs_fail3,
        file: "cobra/parse_let_improperargs_fail3.snek",
        expected: "Invalid",
    },
    {
        name: cobra_parse_let_improperargs_fail4,
        file: "cobra/parse_let_improperargs_fail4.snek",
        expected: "Invalid",
    },
    {
        name: cobra_parse_let_improperargs_fail5,
        file: "cobra/parse_let_improperargs_fail5.snek",
        expected: "keyword",
    },

    {
        name: boa_duplicate_binding_fail0,
        file: "boa/duplicate_binding_fail0.snek",
        expected: "Duplicate binding",
    },
    {
        name: boa_duplicate_binding_fail1,
        file: "boa/duplicate_binding_fail1.snek",
        expected: "Duplicate binding",
    },
    {
        name: boa_duplicate_binding_fail2,
        file: "boa/duplicate_binding_fail2.snek",
        expected: "Duplicate binding",
    },

    // Invalid if expressions
    {
        name: cobra_parse_if_fail0,
        file: "cobra/parse_if_fail0.snek",
        expected: "Invalid",
    },
    {
        name: cobra_parse_if_fail1,
        file: "cobra/parse_if_fail1.snek",
        expected: "Invalid",
    },

    // Unbound identifier
    {
        name: cobra_unbound_identifier_fail0,
        file: "cobra/unbound_identifier_fail0.snek",
        expected: "Unbound variable identifier x",
    },
    {
        name: cobra_unbound_identifier_fail1,
        file: "cobra/unbound_identifier_fail1.snek",
        expected: "Unbound variable identifier y",
    },
    {
        name: cobra_unbound_identifier_fail2,
        file: "cobra/unbound_identifier_fail2.snek",
        expected: "Unbound variable identifier x",
    },
    {
        name: cobra_unbound_identifier_fail3,
        file: "cobra/unbound_identifier_fail3.snek",
        expected: "Unbound variable identifier z",
    },
    {
        name: cobra_unbound_identifier_fail4,
        file: "cobra/unbound_identifier_fail4.snek",
        expected: "Unbound variable identifier t",
    },
    {
        name: cobra_unbound_identifier_fail5,
        file: "cobra/unbound_identifier_fail5.snek",
        expected: "Unbound variable identifier x",
    },

    // Invalid block
    {
        name: cobra_parse_block_fail0,
        file: "cobra/parse_block_fail0.snek",
        expected: "Invalid",
    },

    // Invalid break
    {
        name: cobra_invalid_break_fail0,
        file: "cobra/invalid_break_fail0.snek",
        expected: "break",
    },

    // Invalid loop
    {
        name: cobra_invalid_loop_fail0,
        file: "cobra/invalid_loop_fail0.snek",
        expected: "Invalid",
    }

}
