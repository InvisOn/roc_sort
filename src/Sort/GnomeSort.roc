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

	gnome_sort2 : List(U64) -> List(U64)
	gnome_sort2 = |var $array| {
		len = $array.len()

		var $idx = 1
		while $idx < len {
			if $idx == 0 or get($array, $idx) >= get($array, $idx - 1) {
				$idx = $idx + 1
			} else {
				prev = $idx - 1
				$array = swap($array, $idx, prev)
				$idx = prev
			}
		}

		return $array
	}
}

expect test1(GnomeSort.gnome_sort)
expect test2(GnomeSort.gnome_sort)
expect test3(GnomeSort.gnome_sort)
expect test4(GnomeSort.gnome_sort)
expect test5(GnomeSort.gnome_sort)

expect test1(GnomeSort.gnome_sort2)
expect test2(GnomeSort.gnome_sort2)
expect test3(GnomeSort.gnome_sort2)
expect test4(GnomeSort.gnome_sort2)
expect test5(GnomeSort.gnome_sort2)
