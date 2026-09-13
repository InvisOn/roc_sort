Utils :: {}.{
	get : List(U64), U64 -> U64
	get = |array, idx| {
		expect idx >= 0 or idx <= array.len()
		match array.get(idx) {
			Ok(arr) => arr
			_ => crash "get unreachable"
		}
	}

	swap = |array, idx, idx2| {
		expect {
			len = array.len()
			idx >= 0 or idx <= len and idx2 >= 0 or idx2 <= len
		}
		match array.swap(idx, idx2) {
			Ok(arr) => arr
			_ => crash "swap unreachable"
		}
	}

	replace = |array, idx, elem| {
		expect idx >= 0 or idx <= array.len()
		match array.replace(idx, elem) {
			Ok({ list: arr, prev: _ }) => arr
			_ => crash "replace unreachable"
		}
	}

	insert = |array, idx, elem| {
		expect idx >= 0 or idx <= array.len()
		match array.insert(idx, elem) {
			Ok(arr) => arr
			_ => crash "insert unreachable"
		}
	}

	test1 = |func| {
		test_helper("Test 1", sorted_4, sorted_4, func)
	}

	test2 = |func| {
		test_helper("Test 2", sorted_4, unsorted_4, func)
	}

	test3 = |func| {
		test_helper("Test 3", sorted_5, sorted_5, func)
	}

	test4 = |func| {
		test_helper("Test 4", sorted_5, unsorted_5, func)
	}

	test5 = |func| {
		test_helper("Test 5", [], [], func)
	}
}

test_helper = |name, expected, data, func| {
	result = func(data)
	passed = expected == result
	if !passed {
		dbg name
		dbg ("input:", data)
		dbg ("result:", result)
	}
	passed
}

unsorted_4 : List(U64)
unsorted_4 = [2, 4, 1, 3]

sorted_4 : List(U64)
sorted_4 = [1, 2, 3, 4]

unsorted_5 : List(U64)
unsorted_5 = [5, 2, 4, 1, 3]

sorted_5 : List(U64)
sorted_5 = [1, 2, 3, 4, 5]
