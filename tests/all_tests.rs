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
        name: boa_nested_arith1,
        file: "boa/nested_arith1.snek",
        expected: "25",
    },
    {
        name: boa_nested_arith3,
        file: "boa/nested_arith3.snek",
        expected: "1117",
    },
    {
        name: boa_binding1,
        file: "boa/binding1.snek",
        expected: "-5",
    },

    // Cobra Tests
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

}
