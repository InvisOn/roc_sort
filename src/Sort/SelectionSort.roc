import Utils exposing [get, swap, test]

SelectionSort :: {}.{
	sort : List(U64) -> List(U64)
	sort = |array| {
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

expect test(SelectionSort.sort)
