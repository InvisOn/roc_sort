import Utils

SelectionSort :: {}.{
	selection_sort : List(U64) -> List(U64)
	selection_sort = |array| {
		len = array.len()

		sort = |arr, idx| {
			if idx == len {
				return arr
			}

			next = idx + 1
			var $minimum = idx
			for i in next..<len {
				if Utils.get(arr, i) < Utils.get(arr, $minimum) {
					$minimum = i
				}
			}

			if $minimum != idx {
				return sort(Utils.swap(arr, idx, $minimum), next)
			}

			sort(arr, next)
		}

		sort(array, 0)
	}
}

expect Utils.test(SelectionSort.selection_sort)
