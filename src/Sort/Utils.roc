Utils :: {}.{
	get = |array, idx| {
		match array.get(idx) {
			Ok(arr) => arr
			_ => crash "get unreachable"
		}
	}

	swap = |array, i, j| {
		match array.swap(i, j) {
			Ok(arr) => arr
			_ => crash "swap unreachable"
		}
	}

	replace = |array, i, e| {
		match array.replace(i, e) {
			Ok(result) => result.list
			_ => crash "replace unreachable"
		}
	}

	test = |func| {
		if func(sorted_4) != sorted_4 {
			dbg "Fail 1"
			dbg sorted_4
			dbg func(sorted_4)
			return False
		}

		if func(unsorted_4) != sorted_4 {
			dbg "Fail 2"
			dbg unsorted_4
			dbg func(unsorted_4)
			return False
		}

		if func(sorted_5) != sorted_5 {
			dbg "Fail 3"
			dbg unsorted_5
			dbg func(unsorted_5)
			return False
		}

		if func(unsorted_5) != sorted_5 {
			dbg "Fail 4"
			dbg unsorted_5
			dbg func(unsorted_5)
			return False
		}

		True
	}
}

unsorted_4 : List(U64)
unsorted_4 = [2, 4, 1, 3]

sorted_4 = [1, 2, 3, 4]

unsorted_5 = [5, 2, 4, 1, 3]

sorted_5 = [1, 2, 3, 4, 5]
