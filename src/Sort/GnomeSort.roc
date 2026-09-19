import Utils exposing [swap, get]
import Tests

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

expect Tests.test_already_sorted_even(GnomeSort.gnome_sort)
expect Tests.test_permutation_even(GnomeSort.gnome_sort)
expect Tests.test_already_sorted_odd(GnomeSort.gnome_sort)
expect Tests.test_permutation_odd(GnomeSort.gnome_sort)
expect Tests.test_empty(GnomeSort.gnome_sort)
expect Tests.test_singleton(GnomeSort.gnome_sort)
expect Tests.test_duplicates(GnomeSort.gnome_sort)
expect Tests.test_spare(GnomeSort.gnome_sort)
expect Tests.test_reversed(GnomeSort.gnome_sort)
expect Tests.test_all_equal(GnomeSort.gnome_sort)

expect Tests.test_already_sorted_even(GnomeSort.gnome_sort2)
expect Tests.test_permutation_even(GnomeSort.gnome_sort2)
expect Tests.test_already_sorted_odd(GnomeSort.gnome_sort2)
expect Tests.test_permutation_odd(GnomeSort.gnome_sort2)
expect Tests.test_empty(GnomeSort.gnome_sort2)
expect Tests.test_singleton(GnomeSort.gnome_sort2)
expect Tests.test_duplicates(GnomeSort.gnome_sort2)
expect Tests.test_spare(GnomeSort.gnome_sort2)
expect Tests.test_reversed(GnomeSort.gnome_sort2)
expect Tests.test_all_equal(GnomeSort.gnome_sort2)
