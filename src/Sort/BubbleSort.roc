import Utils exposing [swap, get, test_already_sorted_even, test_permutation_even, test_already_sorted_odd, test_permutation_odd, test_empty]
import Tests

BubbleSort :: {}.{
	bubble_sort = |array| {
		aux = |arr, idx, swapped| {
			len = arr.len()
			if len == 0 {
				return arr
			}

			end = idx == len - 1

			if end and swapped {
				return aux(arr, 0, False)
			}

			if end and !swapped {
				return arr
			}

			next = idx + 1
			if get(arr, idx) > get(arr, next) {
				return swap(arr, idx, next)
					|> aux(next, True)
			}

			aux(arr, next, swapped)
		}

		aux(array, 0, False)
	}

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

expect Tests.test_already_sorted_even(BubbleSort.bubble_sort2)
expect Tests.test_permutation_even(BubbleSort.bubble_sort2)
expect Tests.test_already_sorted_odd(BubbleSort.bubble_sort2)
expect Tests.test_permutation_odd(BubbleSort.bubble_sort2)
expect Tests.test_empty(BubbleSort.bubble_sort2)
