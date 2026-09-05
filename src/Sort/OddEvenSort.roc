import Utils

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
					if Utils.get($var_arr, i) > Utils.get($var_arr, j) {
						$var_arr = Utils.swap($var_arr, i, j)
						$sorted = False
					}
				}
			}

			for i in 0..<len - 1 {
				if i % 2 == 1 {
					j = i + 1
					if Utils.get($var_arr, i) > Utils.get($var_arr, j) {
						$var_arr = Utils.swap($var_arr, i, j)
						$sorted = False
					}
				}
			}

			sort($var_arr, $sorted)
		}

		sort(array, False)
	}
}

expect Utils.test(OddEvenSort.odd_even_sort)
