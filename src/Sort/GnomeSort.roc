import Utils

GnomeSort :: {}.{
	gnome_sort : List(U64) -> List(U64)
	gnome_sort = |array| {
		len = array.len()

		sort = |arr, idx| {
			if !(idx < len) {
				return arr
			}

			next = idx + 1
			if idx == 0 {
				return sort(arr, next)
			}

			prev = idx - 1
			if Utils.get(arr, idx) >= Utils.get(arr, prev) {
				return sort(arr, idx + 1)
			}

			return Utils.swap(arr, idx, prev) |> sort(prev)
		}

		sort(array, 0)
	}
}

expect Utils.test(GnomeSort.gnome_sort)
