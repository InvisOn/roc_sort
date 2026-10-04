import Utils exposing [get, pop, swap]
import Tests

StoogeSort :: {}.{
	stooge_sort : List(U64) -> List(U64)
	stooge_sort = |array| {
		len = array.len()
		if len < 2 {
			return array
		}

		stooge(array, 0, len - 1)
	}

	stooge_sort2 : List(U64) -> List(U64)
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
				$stack = $stack.append((i, j - third))
					.append((i + third, j))
					.append((i, j - third))
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
