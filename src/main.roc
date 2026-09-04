main! = |_args| {
	arr = [5, 2, 4, 1, 3]

	sorted = bubble_sort(arr)

	dbg sorted

	Ok({})
}

bubble_sort : List(U64) -> List(U64)
bubble_sort = |array| {
	bubble = |arr, idx, swapped| {
		end = idx == arr.len() - 1

		if end and swapped {
			return bubble(arr, 0, False)
		}

		if end and !swapped {
			return arr
		}

		next_idx = idx + 1

		current = match arr.get(idx) {
			Ok(elem) => elem
			_ => crash "unreachable"
		}
		next = match arr.get(next_idx) {
			Ok(elem) => elem
			_ => crash "unreachable"
		}

		if current > next {
			return match arr.swap(idx, next_idx) {
				Ok(swap) => swap
				_ => crash "unreachable"
			}
				|> bubble(next_idx, True)
		}

		bubble(arr, next_idx, swapped)
	}

	bubble(array, 0, False)
}

sorted_4 = [1, 2, 3, 4]

sorted_5 = [1, 2, 3, 4, 5]

expect bubble_sort([1, 2, 3, 4]) == sorted_4
expect bubble_sort([1, 2, 3, 4, 5]) == sorted_5

expect bubble_sort([2, 4, 1, 3]) == sorted_4
expect bubble_sort([5, 2, 4, 1, 3]) == sorted_5
