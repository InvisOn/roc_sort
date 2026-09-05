OddEvenSort :: {}.{
	odd_even_sort : List(U64) -> List(U64)
	odd_even_sort = |array| {
		len = array.len()

		sort = |arr, done| {
			if done {
				return arr
			}

			var $sorted = True
			var $var_arr = arr

			for i in 0..<len - 1 {
				if i % 2 == 0 {
					j = i + 1
					if get($var_arr, i) > get($var_arr, j) {
						$var_arr = swap($var_arr, i, j)
						$sorted = False
					}
				}
			}

			for i in 0..<len - 1 {
				if i % 2 == 1 {
					j = i + 1
					if get($var_arr, i) > get($var_arr, j) {
						$var_arr = swap($var_arr, i, j)
						$sorted = False
					}
				}
			}

			sort($var_arr, $sorted)
		}

		sort(array, False)
	}
}

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

expect OddEvenSort.odd_even_sort([1, 2, 3, 4]) == [1, 2, 3, 4]
expect OddEvenSort.odd_even_sort([2, 4, 1, 3]) == [1, 2, 3, 4]

expect OddEvenSort.odd_even_sort([1, 2, 3, 4, 5]) == [1, 2, 3, 4, 5]
expect OddEvenSort.odd_even_sort([5, 2, 4, 1, 3]) == [1, 2, 3, 4, 5]
