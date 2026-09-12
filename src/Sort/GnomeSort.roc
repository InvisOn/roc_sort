import Utils exposing [swap, get, test]

GnomeSort :: {}.{
	sort : List(U64) -> List(U64)
	sort = |array| {
		len = array.len()

		aux = |arr, idx| {
			if !(idx < len) {
				return arr
			}

			next = idx + 1
			if idx == 0 {
				return aux(arr, next)
			}

			prev = idx - 1
			if get(arr, idx) >= get(arr, prev) {
				return aux(arr, idx + 1)
			}

			return swap(arr, idx, prev) |> aux(prev)
		}

		aux(array, 0)
	}
}

expect test(GnomeSort.sort)
