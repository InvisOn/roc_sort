import Utils exposing [random_list]

Tests :: {}.{
	run = |func| {
		tests.map(|test| test(func)).all(|passed| passed)
	}

	test_tail_call_optimization = |func, array| {
		expect func(array) == array.sort()
	}
}

tests = [
	|func| test("test_sorted_even", sorted_even, sorted_even, func),
	|func| test("test_unsorted_even", sorted_even, unsorted_even, func),
	|func| test("test_sorted_odd", sorted_odd, sorted_odd, func),
	|func| test("test_unsorted_odd", sorted_odd, unsorted_odd, func),
	|func| test("test_unsorted_long_list", sorted_long, unsorted_long, func),
	|func| test("test_sorted_long_list", sorted_long, sorted_long, func),
	|func| test("test_empty", [], [], func),
	|func| test("test_singleton", [42], [42], func),
	|func| test("test_duo", [1, 2], [2, 1], func),
	|func| test("test_trio", [1, 2, 3], [1, 3, 2], func),
	|func| test("test_duplicates", [1, 1, 2, 2, 3], [2, 1, 3, 1, 2], func),
	|func| test("test_spare", [0, 7, 100], [100, 0, 7], func),
	|func| test("test_reversed", [1, 2, 3, 4, 5], [5, 4, 3, 2, 1], func),
	|func| test("test_all_equal", [3, 3, 3], [3, 3, 3], func),
]

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

unsorted_long = random_list(0, 20, 12)

sorted_long = unsorted_long.sort()

unsorted_even = [2, 4, 1, 3]

sorted_even = [1, 2, 3, 4]

unsorted_odd = [5, 2, 4, 1, 3]

sorted_odd = [1, 2, 3, 4, 5]
