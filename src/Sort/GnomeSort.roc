import Utils

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
			if Utils.get(arr, idx) >= Utils.get(arr, prev) {
				return aux(arr, idx + 1)
			}

			return Utils.swap(arr, idx, prev) |> aux(prev)
		}

		aux(array, 0)
	}
}

expect Utils.test(GnomeSort.sort)
