Tests :: {}.{
	test_already_sorted_even = |func| {
		test("test_already_sorted_even", sorted_even, sorted_even, func)
	}

	test_permutation_even = |func| {
		test("test_permutation_even", sorted_even, unsorted_even, func)
	}

	test_already_sorted_odd = |func| {
		test("test_already_sorted_odd", sorted_odd, sorted_odd, func)
	}

	test_permutation_odd = |func| {
		test("test_permutation_odd", sorted_odd, unsorted_odd, func)
	}

	test_empty = |func| {
		test("test_empty", [], [], func)
	}

	test_singleton = |func| {
		test("test_singleton", [42], [42], func)
	}

	test_duplicates = |func| {
		test("test_duplicates", [1, 1, 2, 2, 3], [2, 1, 3, 1, 2], func)
	}

	test_spare = |func| {
		test("test_spare", [0, 7, 100], [100, 0, 7], func)
	}

	test_reversed = |func| {
		test("test_singleton", [1, 2, 3, 4, 5], [5, 4, 3, 2, 1], func)
	}

	test_all_equal = |func| {
		test("test_all_equal", [3, 3, 3], [3, 3, 3], func)
	}
}

test = |name, expected, data, func| {
	result = func(data)
	passed = expected == result
	if !passed {
		dbg name
		dbg ("input:", data)
		dbg ("result:", result)
	}
	passed
}

unsorted_even : List(U64)
unsorted_even = [2, 4, 1, 3]

sorted_even : List(U64)
sorted_even = [1, 2, 3, 4]

unsorted_odd : List(U64)
unsorted_odd = [5, 2, 4, 1, 3]

sorted_odd : List(U64)
sorted_odd = [1, 2, 3, 4, 5]
