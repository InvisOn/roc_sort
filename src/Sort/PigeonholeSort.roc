import Utils exposing [replace, min, max, get]
import Tests

PigeonholeSort :: {}.{
	pigeonhole_sort : List(U64) -> List(U64)
	pigeonhole_sort = |array| {
		len = array.len()
		if len == 0 {
			return array
		}

		minimum = min(array)
		range = max(array) - minimum + 1
		tmp = List.repeat(0, range)

		return fill_temp(tmp, array, len, minimum, 0)
			|> aux(array, 0, range, minimum, 0)
	}

	pigeonhole_sort2 : List(U64) -> List(U64)
	pigeonhole_sort2 = |var $array| {
		len = $array.len()
		if len == 0 {
			return $array
		}

		minimum = min($array)
		range = max($array) - minimum + 1
		var $tmp = List.repeat(0, range)

		for i in 0..<len {
			j = get($array, i) - minimum
			k = get($tmp, j) + 1
			$tmp = replace($tmp, j, k)
		}

		var $idx = 0
		for i in 0..<range {
			while get($tmp, i) > 0 {
				j = get($tmp, i) - 1
				$tmp = replace($tmp, i, j)
				$array = replace($array, $idx, i + minimum)
				$idx = $idx + 1
			}
		}

		$array
	}
}

fill_temp = |tmp, array, len, minimum, idx| {
	if idx == len {
		return tmp
	}

	j = get(array, idx) - minimum
	k = get(tmp, j) + 1
	return replace(tmp, j, k) |> fill_temp(array, len, minimum, idx + 1)
}

aux = |tmp, array, idx, range, minimum, i| {
	if i == range {
		return array
	}
	return sort(tmp, array, minimum, i, idx)
		|> (|(arr, t, j)| aux(t, arr, j, range, minimum, i + 1))
}

sort = |tmp, var $array, minimum, i, idx| {
	elem = get(tmp, i)
	if !(elem > 0) {
		return ($array, tmp, idx)
	}
	j = elem - 1
	$array = replace($array, idx, i + minimum)

	return replace(tmp, i, j)
		|> sort($array, minimum, i, idx + 1)
}

expect Tests.test_already_sorted_even(PigeonholeSort.pigeonhole_sort)
expect Tests.test_permutation_even(PigeonholeSort.pigeonhole_sort)
expect Tests.test_already_sorted_odd(PigeonholeSort.pigeonhole_sort)
expect Tests.test_permutation_odd(PigeonholeSort.pigeonhole_sort)
expect Tests.test_empty(PigeonholeSort.pigeonhole_sort)
expect Tests.test_singleton(PigeonholeSort.pigeonhole_sort)
expect Tests.test_duplicates(PigeonholeSort.pigeonhole_sort)
expect Tests.test_spare(PigeonholeSort.pigeonhole_sort)
expect Tests.test_reversed(PigeonholeSort.pigeonhole_sort)
expect Tests.test_all_equal(PigeonholeSort.pigeonhole_sort)

expect Tests.test_already_sorted_even(PigeonholeSort.pigeonhole_sort2)
expect Tests.test_permutation_even(PigeonholeSort.pigeonhole_sort2)
expect Tests.test_already_sorted_odd(PigeonholeSort.pigeonhole_sort2)
expect Tests.test_permutation_odd(PigeonholeSort.pigeonhole_sort2)
expect Tests.test_empty(PigeonholeSort.pigeonhole_sort2)
expect Tests.test_singleton(PigeonholeSort.pigeonhole_sort2)
expect Tests.test_duplicates(PigeonholeSort.pigeonhole_sort2)
expect Tests.test_spare(PigeonholeSort.pigeonhole_sort2)
expect Tests.test_reversed(PigeonholeSort.pigeonhole_sort2)
expect Tests.test_all_equal(PigeonholeSort.pigeonhole_sort2)
