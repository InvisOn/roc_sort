import Utils

OddEvenSort :: {}.{
	sort : List(U64) -> List(U64)
	sort = |array| {
		len = array.len()

		aux = |arr, done| {
			if done {
				return arr
			}

			var $auxed = True
			var $var_arr = arr

			for i in 0..<len - 1 {
				if i % 2 == 0 {
					j = i + 1
					if Utils.get($var_arr, i) > Utils.get($var_arr, j) {
						$var_arr = Utils.swap($var_arr, i, j)
						$auxed = False
					}
				}
			}

			for i in 0..<len - 1 {
				if i % 2 == 1 {
					j = i + 1
					if Utils.get($var_arr, i) > Utils.get($var_arr, j) {
						$var_arr = Utils.swap($var_arr, i, j)
						$auxed = False
					}
				}
			}

			aux($var_arr, $auxed)
		}

		aux(array, False)
	}
}

expect Utils.test(OddEvenSort.sort)
