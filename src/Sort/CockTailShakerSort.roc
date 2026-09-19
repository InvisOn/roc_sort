import Utils exposing [swap, get]
import Tests

CockTailShakerSort :: {}.{
	cocktail_shaker_sort : List(U64) -> List(U64)
	cocktail_shaker_sort = |array| {
		len = array.len()
		if len == 0 or len == 1 {
			return array
		}

		shake = |var $arr, i, var $swapped, condition, step| {
			if condition(i) {
				return ($arr, $swapped)
			}

			j = i + 1
			if get($arr, i) > get($arr, j) {
				$arr = swap($arr, i, j)
				$swapped = True
			}

			return shake($arr, step(i), $swapped, condition, step)
		}

		sort = |var $arr, var $swapped| {
			end = len - 2
			($arr, $swapped) = shake($arr, 0, $swapped, |i| i == end, |i| i + 1)

			if !$swapped {
				return $arr
			}
			$swapped = False

			($arr, $swapped) = shake($arr, end, $swapped, |i| i == 0, |i| i - 1)

			if !$swapped {
				return $arr
			}

			return sort($arr, $swapped)
		}

		sort(array, False)
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

			if $swapped == False {
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

			if $swapped == False {
				break
			}
		}

		$array
	}
}

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
