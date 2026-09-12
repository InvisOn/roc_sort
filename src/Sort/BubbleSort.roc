import Utils exposing [swap, get, test]

BubbleSort :: {}.{
	sort : List(U64) -> List(U64)
	sort = |array| {
		aux = |arr, idx, swapped| {
			end = idx == arr.len() - 1

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

}

expect test(BubbleSort.sort)
