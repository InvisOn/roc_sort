import Utils exposing [swap, get, test1, test2, test3, test4, test5]

OddEvenSort :: {}.{
	odd_even_sort : List(U64) -> List(U64)
	odd_even_sort = |array| {
		len = array.len()
		if len == 0 {
			return array
		}

		odd_even = |var $array, i, var $done, remainder| {
			if i >= len - 1 {
				return ($array, $done)
			}

			if i % 2 == remainder {
				j = i + 1
				if get($array, i) > get($array, j) {
					$array = swap($array, i, j)
					$done = False
				}
			}

			return odd_even($array, i + 1, $done, remainder)
		}

		sort = |var $array, var $done| {
			if $done {
				return $array
			}

			($array, $done) = odd_even($array, 0, True, 0)
			($array, $done) = odd_even($array, 0, True, 1)

			sort($array, $done)
		}

		sort(array, False)
	}

	odd_even_sort2 : List(U64) -> List(U64)
	odd_even_sort2 = |var $array| {
		len = $array.len()
		if len == 0 {
			return $array
		}

		var $done = False
		while !$done {
			$done = True

			for i in 0..<len - 1 {
				if i % 2 == 0 {
					j = i + 1
					if get($array, i) > get($array, j) {
						$array = swap($array, i, j)
						$done = False
					}
				}
			}

			for i in 0..<len - 1 {
				if i % 2 == 1 {
					j = i + 1
					if get($array, i) > get($array, j) {
						$array = swap($array, i, j)
						$done = False
					}
				}
			}
		}

		$array
	}
}

expect test1(OddEvenSort.odd_even_sort)
expect test2(OddEvenSort.odd_even_sort)
expect test3(OddEvenSort.odd_even_sort)
expect test4(OddEvenSort.odd_even_sort)
expect test5(OddEvenSort.odd_even_sort)

expect test1(OddEvenSort.odd_even_sort2)
expect test2(OddEvenSort.odd_even_sort2)
expect test3(OddEvenSort.odd_even_sort2)
expect test4(OddEvenSort.odd_even_sort2)
expect test5(OddEvenSort.odd_even_sort2)
