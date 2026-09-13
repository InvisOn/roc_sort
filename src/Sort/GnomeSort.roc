import Utils exposing [swap, get, test1, test2, test3, test4, test5]

GnomeSort :: {}.{
	gnome_sort : List(U64) -> List(U64)
	gnome_sort = |array| {
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

expect test1(GnomeSort.gnome_sort)
expect test2(GnomeSort.gnome_sort)
expect test3(GnomeSort.gnome_sort)
expect test4(GnomeSort.gnome_sort)
expect test5(GnomeSort.gnome_sort)
