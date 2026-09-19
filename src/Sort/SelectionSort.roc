import Utils exposing [get, swap, test_already_sorted_even, test_permutation_even, test_already_sorted_odd, test_permutation_odd, test_empty]

SelectionSort :: {}.{
	selection_sort = |array| {
		len = array.len()

		find_min = |arr, idx, min| {
			if idx >= len {
				return min
			}

			if get(arr, idx) < get(arr, min) {
				return find_min(arr, idx + 1, idx)
			}

			return find_min(arr, idx + 1, min)
		}

		aux = |arr, idx| {
			if idx == len {
				return arr
			}

			next = idx + 1
			minimum = find_min(arr, next, idx)

			if minimum != idx {
				return aux(swap(arr, idx, minimum), next)
			}

			aux(arr, next)
		}

		aux(array, 0)
	}

	selection_sort2 = |var $array| {
		len = $array.len()

		for idx in 0..<len {
			if idx == len {
				return $array
			}

			next = idx + 1
			var $minimum = idx
			for i in next..<len {
				if get($array, i) < get($array, $minimum) {
					$minimum = i
				}
			}

			if $minimum != idx {
				$array = swap($array, idx, $minimum)
			}
		}

		$array
	}

}

expect test_already_sorted_even(SelectionSort.selection_sort)
expect test_permutation_even(SelectionSort.selection_sort)
expect test_already_sorted_odd(SelectionSort.selection_sort)
expect test_permutation_odd(SelectionSort.selection_sort)
expect test_empty(SelectionSort.selection_sort)

expect test_already_sorted_even(SelectionSort.selection_sort2)
expect test_permutation_even(SelectionSort.selection_sort2)
expect test_already_sorted_odd(SelectionSort.selection_sort2)
expect test_permutation_odd(SelectionSort.selection_sort2)
expect test_empty(SelectionSort.selection_sort2)
