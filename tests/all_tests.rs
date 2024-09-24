mod infra;

// Your tests go here!
success_tests! {
    // Boa Regression Tests
    {
        name: quick_brown_fox_boa,
        file: "quick_brown_fox.snek",
        expected: "-3776",
    },
    {
        name: sophia_test_boa,
        file: "sophia_test.snek",
        expected: "243",
    },
    {
        name: nested_arith1_boa,
        file: "nested_arith1.snek",
        expected: "25",
    },
    {
        name: nested_arith3_boa,
        file: "nested_arith3.snek",
        expected: "1117",
    },
    {
        name: binding1_boa,
        file: "binding1.snek",
        expected: "-5",
    },

    // Provided Tests
    {
        name: false_val,
        file: "false_val.snek",
        expected: "false",
    },
    {
        name: example1,
        file: "example1.snek",
        expected: "6",
    },
    {
        name: example2,
        file: "example2.snek",
        expected: "-6",
    },
    {
        name: factorial0,
        file: "factorial.snek",
        input: "0",
        expected: "1",
    },
    {
        name: factorial1,
        file: "factorial.snek",
        input: "1",
        expected: "1",
    },
    {
        name: factorial2,
        file: "factorial.snek",
        input: "2",
        expected: "2",
    },
    {
        name: factorial3,
        file: "factorial.snek",
        input: "3",
        expected: "6",
    },
    {
        name: input_compare_1,
        file: "input_compare.snek",
        input: "2",
        expected: "false",
    },
    {
        name: input_compare_2,
        file: "input_compare.snek",
        input: "10",
        expected: "true",
    },

    // Unit Tests
    {
        name: input_plus1_1,
        file: "input_plus1.snek",
        input: "10",
        expected: "11",
    },
    {
        name: input_plus1_2,
        file: "input_plus1.snek",
        input: "0",
        expected: "1",
    },
    {
        name: true_val,
        file: "true_val.snek",
        expected: "true",
    },
    {
        name: negative_one_val,
        file: "negative_one_val.snek",
        expected: "-1",
    },
    {
        name: zero_val,
        file: "zero_val.snek",
        expected: "0",
    },
    {
        name: one_val,
        file: "one_val.snek",
        expected: "1",
    },
    {
        name: five_val,
        file: "five_val.snek",
        expected: "5",
    },
    {
        name: compare_greater_1,
        file: "compare_greater.snek",
        input: "6",
        expected: "true",
    },
    {
        name: compare_greater_2,
        file: "compare_greater.snek",
        input: "5",
        expected: "false",
    },
    {
        name: compare_greater_3,
        file: "compare_greater.snek",
        input: "4",
        expected: "false",
    },
    {
        name: compare_greater_equal_1,
        file: "compare_greater_equal.snek",
        input: "6",
        expected: "true",
    },
    {
        name: compare_greater_equal_2,
        file: "compare_greater_equal.snek",
        input: "5",
        expected: "true",
    },
    {
        name: compare_greater_equal_3,
        file: "compare_greater_equal.snek",
        input: "4",
        expected: "false",
    },
    {
        name: compare_equal_1,
        file: "compare_equal.snek",
        input: "6",
        expected: "false",
    },
    {
        name: compare_equal_2,
        file: "compare_equal.snek",
        input: "5",
        expected: "true",
    },
    {
        name: compare_equal_3,
        file: "compare_equal.snek",
        input: "4",
        expected: "false",
    },
    {
        name: compare_less_equal_1,
        file: "compare_less_equal.snek",
        input: "6",
        expected: "false",
    },
    {
        name: compare_less_equal_2,
        file: "compare_less_equal.snek",
        input: "5",
        expected: "true",
    },
    {
        name: compare_less_equal_3,
        file: "compare_less_equal.snek",
        input: "4",
        expected: "true",
    },
    {
        name: compare_less_1,
        file: "compare_less.snek",
        input: "6",
        expected: "false",
    },
    {
        name: compare_less_2,
        file: "compare_less.snek",
        input: "5",
        expected: "false",
    },
    {
        name: compare_less_3,
        file: "compare_less.snek",
        input: "4",
        expected: "true",
    },
    {
        name: equal_bool,
        file: "equal_bool.snek",
        expected: "true",
    },
   {
        name: not_equal_bool,
        file: "not_equal_bool.snek",
        expected: "false",
    },
    {
        name: good_set,
        file: "good_set.snek",
        expected: "6",
    },
    {
        name: basic_block,
        file: "basic_block.snek",
        expected: "3",
    },

    // Ensemble Tests
    {
        name: equal_bool_compound,
        file: "equal_bool_compound.snek",
        expected: "true",
    },
    {
        name: equal_bool_let,
        file: "equal_bool_let.snek",
        expected: "true",
    },
    {
        name: if_nested,
        file: "if_nested.snek",
        expected: "5",
    },
    {
        name: if_let,
        file: "if_let.snek",
        expected: "5",
    },
}

runtime_error_tests! {
    {
        name: mul_over,
        file: "mul_over.snek",
        expected: "overflow",
    },
}

static_error_tests! {
    {
        name: invalid_argument,
        file: "invalid_argument.snek",
        expected: "mismatch",
    },
    {
        name: invalid_argument_add1,
        file: "invalid_argument_add1.snek",
        expected: "mismatch",
    },
    {
        name: invalid_argument_greater,
        file: "invalid_argument_greater.snek",
        expected: "mismatch",
    },
        {
        name: invalid_argument_equal,
        file: "invalid_argument_equal.snek",
        expected: "mismatch",
    },
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    },
    {
        name: failed_set,
        file: "failed_set.snek",
        expected: "mismatch",
    },
}
