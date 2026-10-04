import Utils exposing [get, replace]
import Tests

InsertionSort :: {}.{
	insertion_sort : List(U64) -> List(U64)
	insertion_sort = |array| {
		sort_from(array, 1, array.len())
	}

	insertion_sort2 : List(U64) -> List(U64)
	insertion_sort2 = |var $array| {
		var $idx = 1
		while $idx < $array.len() {
			x = get($array, $idx)
			var $j = $idx
			while $j > 0 and get($array, $j - 1) > x {
				$array = replace($array, $j, get($array, $j - 1))
				$j = $j - 1
			}
			$array = replace($array, $j, x)
			$idx = $idx + 1
		}

		$array
	}
}

sort_from = |arr, sorted_len, len| {
	if sorted_len < len {
		x = get(arr, sorted_len)
		(ar, j) = shift_larger_right(arr, x, sorted_len)
		return sort_from(replace(ar, j, x), sorted_len + 1, len)
	} else {
		return arr
	}
}

shift_larger_right = |array, x, j| {
	if j > 0 and get(array, j - 1) > x {
		return shift_larger_right(replace(array, j, get(array, j - 1)), x, j - 1)
	} else {
		return (array, j)
	}
}

expect Tests.run(InsertionSort.insertion_sort)
expect Tests.run(InsertionSort.insertion_sort2)
