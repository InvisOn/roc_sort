import Utils exposing [get, pop, swap]
import Tests

StoogeSort :: {}.{
	stooge_sort = |array| {
		len = array.len()
		if len < 2 {
			return array
		}

		stooge(array, 0, len - 1)
	}

	stooge_sort2 = |var $array| {
		len = $array.len()
		if len < 2 {
			return $array
		}

		var $stack = [(0, len - 1)]
		while $stack.len() > 0 {
			((i, j), $stack) = pop($stack)
			if get($array, i) > get($array, j) {
				$array = swap($array, i, j)
			}
			if j - i + 1 > 2 {
				third = (j - i + 1) // 3
				# Prepend so sub-ranges run depth-first, in the same order as the recursion.
				$stack = [(i, j - third), (i + third, j), (i, j - third)].concat($stack)
			}
		}

		$array
	}
}

stooge = |array, i, j| {
	arr = if get(array, i) > get(array, j) {
		swap(array, i, j)
	} else {
		array
	}

	if j - i + 1 > 2 {
		third = (j - i + 1) // 3
		stooge(arr, i, j - third)
			|> stooge(i + third, j)
			|> stooge(i, j - third)
	} else {
		arr
	}
}

expect Tests.run(StoogeSort.stooge_sort)
expect Tests.run(StoogeSort.stooge_sort2)
