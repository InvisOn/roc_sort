GnomeSort :: {}.{
	gnome_sort : List(U64) -> List(U64)
	gnome_sort = |array| {
		len = array.len()

		gnome = |arr, idx| {
			if !(idx < len) {
				return arr
			}

			if idx == 0 {
				return gnome(arr, idx + 1)
			}

			prev_idx = idx - 1

			current = match arr.get(idx) {
				Ok(a) => a
				_ => crash "unreachable"
			}
			previous = match arr.get(prev_idx) {
				Ok(a) => a
				_ => crash "unreachable"
			}

			if current >= previous {
				return gnome(arr, idx + 1)
			}

			return match arr.swap(idx, prev_idx) {
				Ok(swap) => swap
				_ => crash "unreachable"
			}
				|> gnome(prev_idx)
		}

		gnome(array, 1)
	}
}

expect GnomeSort.gnome_sort([1, 2, 3, 4]) == [1, 2, 3, 4]
expect GnomeSort.gnome_sort([2, 4, 1, 3]) == [1, 2, 3, 4]

expect GnomeSort.gnome_sort([1, 2, 3, 4, 5]) == [1, 2, 3, 4, 5]
expect GnomeSort.gnome_sort([5, 2, 4, 1, 3]) == [1, 2, 3, 4, 5]
