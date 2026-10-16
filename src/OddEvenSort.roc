import Utils exposing [get, swap]
import Tests

OddEvenSort :: {}.{
	odd_even_sort = |array| {
		len = array.len()
		if len < 2 {
			return array
		}

		# Compare-and-swap the pairs (i, i + 1), (i + 2, i + 3), ...
		odd_even = |var $array, i, var $done| {
			if i + 1 >= len {
				return ($array, $done)
			}

			j = i + 1
			if get($array, i) > get($array, j) {
				$array = swap($array, i, j)
				$done = False
			}

			odd_even($array, i + 2, $done)
		}

		sort = |arr| {
			(after_even, even_done) = odd_even(arr, 0, True)
			(after_odd, odd_done) = odd_even(after_even, 1, True)
			if even_done and odd_done after_odd else sort(after_odd)
		}

		sort(array)
	}

	odd_even_sort2 = |var $array| {
		len = $array.len()
		if len < 2 {
			return $array
		}

		var $done = False
		while !$done {
			$done = True

			for start in [0, 1] {
				var $i = start
				while $i + 1 < len {
					j = $i + 1
					if get($array, $i) > get($array, j) {
						$array = swap($array, $i, j)
						$done = False
					}
					$i = $i + 2
				}
			}
		}

		$array
	}
}

expect Tests.run(OddEvenSort.odd_even_sort)
expect Tests.run(OddEvenSort.odd_even_sort2)
