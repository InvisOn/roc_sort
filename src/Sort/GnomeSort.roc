import Utils exposing [swap, get, test_already_sorted_even, test_permutation_even, test_already_sorted_odd, test_permutation_odd, test_empty]

GnomeSort :: {}.{
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

expect test_already_sorted_even(GnomeSort.gnome_sort)
expect test_permutation_even(GnomeSort.gnome_sort)
expect test_already_sorted_odd(GnomeSort.gnome_sort)
expect test_permutation_odd(GnomeSort.gnome_sort)
expect test_empty(GnomeSort.gnome_sort)

expect test_already_sorted_even(GnomeSort.gnome_sort2)
expect test_permutation_even(GnomeSort.gnome_sort2)
expect test_already_sorted_odd(GnomeSort.gnome_sort2)
expect test_permutation_odd(GnomeSort.gnome_sort2)
expect test_empty(GnomeSort.gnome_sort2)
