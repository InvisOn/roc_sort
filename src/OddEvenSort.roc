import Utils exposing [get, swap]
import Tests

OddEvenSort :: {}.{
	odd_even_sort = |array| {
		len = array.len()
		if len < 2 {
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

			odd_even($array, i + 1, $done, remainder)
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

	odd_even_sort2 = |var $array| {
		len = $array.len()
		if len < 2 {
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

expect Tests.run(OddEvenSort.odd_even_sort)
expect Tests.run(OddEvenSort.odd_even_sort2)
