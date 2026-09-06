import Utils

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
				if Utils.get(arr, i) < Utils.get(arr, $minimum) {
					$minimum = i
				}
			}

			if $minimum != idx {
				return aux(Utils.swap(arr, idx, $minimum), next)
			}

			aux(arr, next)
		}

		aux(array, 0)
	}
}

expect Utils.test(SelectionSort.sort)
