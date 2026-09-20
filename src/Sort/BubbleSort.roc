import Utils exposing [swap, get]
import Tests

BubbleSort :: {}.{
	bubble_sort : List(U64) -> List(U64)
	bubble_sort = |array| {
		bubble = |arr, idx, swapped| {
			len = arr.len()
			if len == 0 {
				return arr
			}

			end = idx == len - 1

			if end and swapped {
				return bubble(arr, 0, False)
			}

			if end and !swapped {
				return arr
			}

			next = idx + 1
			if get(arr, idx) > get(arr, next) {
				return swap(arr, idx, next)
					|> bubble(next, True)
			}

			bubble(arr, next, swapped)
		}

		bubble(array, 0, False)
	}

	bubble_sort2 : List(U64) -> List(U64)
	bubble_sort2 = |var $array| {
		len = $array.len()
		if len == 0 {
			return $array
		}

		last = len - 1
		var $swapped = False
		while True {
			$swapped = False
			for i in 1..=last {
				j = i - 1
				prev = get($array, i - 1)
				current = get($array, i)
				if prev > current {
					$array = swap($array, j, i)
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

expect Tests.test_already_sorted_even(BubbleSort.bubble_sort)
expect Tests.test_permutation_even(BubbleSort.bubble_sort)
expect Tests.test_already_sorted_odd(BubbleSort.bubble_sort)
expect Tests.test_permutation_odd(BubbleSort.bubble_sort)
expect Tests.test_empty(BubbleSort.bubble_sort)
expect Tests.test_singleton(BubbleSort.bubble_sort)
expect Tests.test_duplicates(BubbleSort.bubble_sort)
expect Tests.test_spare(BubbleSort.bubble_sort)
expect Tests.test_reversed(BubbleSort.bubble_sort)
expect Tests.test_all_equal(BubbleSort.bubble_sort)

expect Tests.test_already_sorted_even(BubbleSort.bubble_sort2)
expect Tests.test_permutation_even(BubbleSort.bubble_sort2)
expect Tests.test_already_sorted_odd(BubbleSort.bubble_sort2)
expect Tests.test_permutation_odd(BubbleSort.bubble_sort2)
expect Tests.test_empty(BubbleSort.bubble_sort2)
expect Tests.test_singleton(BubbleSort.bubble_sort2)
expect Tests.test_duplicates(BubbleSort.bubble_sort2)
expect Tests.test_spare(BubbleSort.bubble_sort2)
expect Tests.test_reversed(BubbleSort.bubble_sort2)
expect Tests.test_all_equal(BubbleSort.bubble_sort2)
