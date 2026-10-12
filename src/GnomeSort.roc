import Utils exposing [get, swap]
import Tests

GnomeSort :: {}.{
	gnome_sort : List(U64) -> List(U64)
	gnome_sort = |array| {
		len = array.len()
		if len < 2 {
			return array
		}

		sweep = |arr, idx| {
			if idx >= len {
				return arr
			}

			if idx == 0 or get(arr, idx) >= get(arr, idx - 1) {
				return sweep(arr, idx + 1)
			}

			prev = idx - 1
			swap(arr, idx, prev) |> sweep(prev)
		}

		sweep(array, 1)
	}

	gnome_sort2 : List(U64) -> List(U64)
	gnome_sort2 = |var $array| {
		len = $array.len()
		if len < 2 {
			return $array
		}

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

		$array
	}
}

expect Tests.run(GnomeSort.gnome_sort)
expect Tests.run(GnomeSort.gnome_sort2)
