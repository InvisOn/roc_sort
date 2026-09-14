import Utils exposing [get, swap, test1, test2, test3, test4, test5]

SelectionSort :: {}.{
	selection_sort : List(U64) -> List(U64)
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

	selection_sort2 : List(U64) -> List(U64)
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

expect test1(SelectionSort.selection_sort)
expect test2(SelectionSort.selection_sort)
expect test3(SelectionSort.selection_sort)
expect test4(SelectionSort.selection_sort)
expect test5(SelectionSort.selection_sort)

expect test1(SelectionSort.selection_sort2)
expect test2(SelectionSort.selection_sort2)
expect test3(SelectionSort.selection_sort2)
expect test4(SelectionSort.selection_sort2)
expect test5(SelectionSort.selection_sort2)
