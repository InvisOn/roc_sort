import Utils

BubbleSort :: {}.{
	bubble_sort : List(U64) -> List(U64)
	bubble_sort = |array| {
		sort = |arr, idx, swapped| {
			end = idx == arr.len() - 1

			if end and swapped {
				return sort(arr, 0, False)
			}

			if end and !swapped {
				return arr
			}

			next = idx + 1
			if Utils.get(arr, idx) > Utils.get(arr, next) {
				return Utils.swap(arr, idx, next)
					|> sort(next, True)
			}

			sort(arr, next, swapped)
		}

		sort(array, 0, False)
	}

}

expect Utils.test(BubbleSort.bubble_sort)
