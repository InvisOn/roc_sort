import Utils exposing [swap, get]
import Tests

CombSort :: {}.{
	comb_sort : List(U64) -> List(U64)
	comb_sort = |var $array| {
		len = $array.len()
		if len == 0 {
			return $array
		}

		return sort($array, len, False, len)
	}

	comb_sort2 : List(U64) -> List(U64)
	comb_sort2 = |var $array| {
		len = $array.len()
		if len == 0 {
			return $array
		}
		var $gap = len
		var $sorted = False

		while !$sorted {
			$gap = $gap.to_f64().div_floor_by(1.3).to_u64_wrap()

			if $gap <= 1 {
				$gap = 1
				$sorted = True
			} else if $gap == 0 or $gap == 10 {
				$gap = 11
			}

			var $i = 0
			while $i + $gap < len {
				j = $i + $gap

				if get($array, $i) > get($array, $i + $gap) {
					$array = swap($array, $i, j)
					$sorted = False
				}

				$i = $i + 1
			}
		}

		$array
	}
}

sort = |var $array, var $gap, var $sorted, len| {
	if $sorted {
		return $array
	}

	$gap = $gap.to_f64().div_floor_by(1.3).to_u64_wrap()

	if $gap <= 1 {
		$gap = 1
		$sorted = True
	} else if $gap == 0 or $gap == 10 {
		$gap = 11
	}

	($array, $gap, _, _, $sorted) = aux($array, $gap, 0, len, $sorted)

	return sort($array, $gap, $sorted, len)
}

aux = |var $array, gap, i, len, var $sorted| {
	if !(i + gap < len) {
		return ($array, gap, i, len, $sorted)
	}
	j = i + gap

	if get($array, i) > get($array, i + gap) {
		$array = swap($array, i, j)
		$sorted = False
	}

	return aux($array, gap, i + 1, len, $sorted)
}

expect Tests.test_already_sorted_even(CombSort.comb_sort)
expect Tests.test_permutation_even(CombSort.comb_sort)
expect Tests.test_already_sorted_odd(CombSort.comb_sort)
expect Tests.test_permutation_odd(CombSort.comb_sort)
expect Tests.test_empty(CombSort.comb_sort)
expect Tests.test_empty(CombSort.comb_sort)
expect Tests.test_singleton(CombSort.comb_sort)
expect Tests.test_duplicates(CombSort.comb_sort)
expect Tests.test_spare(CombSort.comb_sort)
expect Tests.test_reversed(CombSort.comb_sort)
expect Tests.test_all_equal(CombSort.comb_sort)

expect Tests.test_already_sorted_even(CombSort.comb_sort2)
expect Tests.test_permutation_even(CombSort.comb_sort2)
expect Tests.test_already_sorted_odd(CombSort.comb_sort2)
expect Tests.test_permutation_odd(CombSort.comb_sort2)
expect Tests.test_empty(CombSort.comb_sort2)
expect Tests.test_empty(CombSort.comb_sort2)
expect Tests.test_singleton(CombSort.comb_sort2)
expect Tests.test_duplicates(CombSort.comb_sort2)
expect Tests.test_spare(CombSort.comb_sort2)
expect Tests.test_reversed(CombSort.comb_sort2)
expect Tests.test_all_equal(CombSort.comb_sort2)
