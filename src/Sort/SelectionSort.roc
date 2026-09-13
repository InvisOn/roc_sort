import Utils exposing [get, swap, test1, test2, test3, test4, test5]

SelectionSort :: {}.{
	selection_sort : List(U64) -> List(U64)
	selection_sort = |array| {
		len = array.len()

		aux = |arr, idx| {
			if idx == len {
				return arr
			}

			next = idx + 1
			var $minimum = idx
			for i in next..<len {
				if get(arr, i) < get(arr, $minimum) {
					$minimum = i
				}
			}

			if $minimum != idx {
				return aux(swap(arr, idx, $minimum), next)
			}

			aux(arr, next)
		}

		aux(array, 0)
	}
}

expect test1(SelectionSort.selection_sort)
expect test2(SelectionSort.selection_sort)
expect test3(SelectionSort.selection_sort)
expect test4(SelectionSort.selection_sort)
expect test5(SelectionSort.selection_sort)
