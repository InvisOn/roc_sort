import Utils exposing [get, replace]
import Tests

InsertionSort :: {}.{
	insertion_sort : List(U64) -> List(U64)
	insertion_sort = |array| {
		len = array.len()

		sort_from = |var $arr, sorted_len| {
			if sorted_len < len {
				x = get($arr, sorted_len)
				($arr, j) = shift_larger_right($arr, x, sorted_len)
				return sort_from(replace($arr, j, x), sorted_len + 1)
			} else {
				return $arr
			}
		}

		sort_from(array, 1)
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

shift_larger_right = |array, x, j| {
	if j > 0 and get(array, j - 1) > x {
		return shift_larger_right(replace(array, j, get(array, j - 1)), x, j - 1)
	} else {
		return (array, j)
	}
}

expect Tests.run(InsertionSort.insertion_sort)
expect Tests.run(InsertionSort.insertion_sort2)
