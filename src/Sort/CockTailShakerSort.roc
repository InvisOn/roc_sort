import Utils exposing [swap, get]
import Tests

CockTailShakerSort :: {}.{
	cocktail_shaker_sort : List(U64) -> List(U64)
	cocktail_shaker_sort = |array| {
		len = array.len()
		if len == 0 or len == 1 {
			return array
		}

		alternate_sweeps = |var $arr, var $swapped| {
			forwards_stop = len - 1
			($arr, $swapped) = sweep($arr, 0, |i| i == forwards_stop, |i| i + 1)

			if !$swapped {
				return $arr
			}
			$swapped = False

			($arr, $swapped) = sweep($arr, len - 2, |i| i == 0, |i| i - 1)

			if !$swapped {
				return $arr
			}

			return alternate_sweeps($arr, $swapped)
		}

		alternate_sweeps(array, False)
	}

	cocktail_shaker_sort2 : List(U64) -> List(U64)
	cocktail_shaker_sort2 = |var $array| {
		len = $array.len()
		if len == 0 or len == 1 {
			return $array
		}

		while True {
			var $swapped = False

			for i in 0..=len - 2 {
				j = i + 1
				if get($array, i) > get($array, j) {
					$array = swap($array, i, j)
					$swapped = True
				}
			}

			if !$swapped {
				break
			}

			$swapped = False

			for i in (0..=len - 2).iter_rev() {
				j = i + 1
				if get($array, i) > get($array, j) {
					$array = swap($array, i, j)
					$swapped = True
				}
			}

			if !$swapped {
				break
			}
		}

		$array
	}
}

sweep = |array, i, stop_at, step| {
	if stop_at(i) {
		return (array, False)
	}

	(rest, swapped) = sweep(array, step(i), stop_at, step)

	j = i + 1
	if get(array, i) > get(array, j) {
		(swap(array, i, j), True)
	} else {
		(rest, swapped)
	}
}

expect Tests.test_duo(CockTailShakerSort.cocktail_shaker_sort)
expect Tests.test_trio(CockTailShakerSort.cocktail_shaker_sort)
expect Tests.test_already_sorted_even(CockTailShakerSort.cocktail_shaker_sort)
expect Tests.test_permutation_even(CockTailShakerSort.cocktail_shaker_sort)
expect Tests.test_already_sorted_odd(CockTailShakerSort.cocktail_shaker_sort)
expect Tests.test_permutation_odd(CockTailShakerSort.cocktail_shaker_sort)
expect Tests.test_empty(CockTailShakerSort.cocktail_shaker_sort)
expect Tests.test_singleton(CockTailShakerSort.cocktail_shaker_sort)
expect Tests.test_duplicates(CockTailShakerSort.cocktail_shaker_sort)
expect Tests.test_spare(CockTailShakerSort.cocktail_shaker_sort)
expect Tests.test_reversed(CockTailShakerSort.cocktail_shaker_sort)
expect Tests.test_all_equal(CockTailShakerSort.cocktail_shaker_sort)

expect Tests.test_duo(CockTailShakerSort.cocktail_shaker_sort2)
expect Tests.test_trio(CockTailShakerSort.cocktail_shaker_sort2)
expect Tests.test_already_sorted_even(CockTailShakerSort.cocktail_shaker_sort2)
expect Tests.test_permutation_even(CockTailShakerSort.cocktail_shaker_sort2)
expect Tests.test_already_sorted_odd(CockTailShakerSort.cocktail_shaker_sort2)
expect Tests.test_permutation_odd(CockTailShakerSort.cocktail_shaker_sort2)
expect Tests.test_empty(CockTailShakerSort.cocktail_shaker_sort2)
expect Tests.test_singleton(CockTailShakerSort.cocktail_shaker_sort2)
expect Tests.test_duplicates(CockTailShakerSort.cocktail_shaker_sort2)
expect Tests.test_spare(CockTailShakerSort.cocktail_shaker_sort2)
expect Tests.test_reversed(CockTailShakerSort.cocktail_shaker_sort2)
expect Tests.test_all_equal(CockTailShakerSort.cocktail_shaker_sort2)
