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
    
    // Given Tests
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

    // Custom Tests
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
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    }
}
