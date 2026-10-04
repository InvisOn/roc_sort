import Utils exposing [get, swap]
import Tests

SelectionSort :: {}.{
	selection_sort = |array| {
		len = array.len()
		if len < 2 {
			return array
		}

		find_min = |arr, idx, min| {
			if idx >= len {
				min
			} else if get(arr, idx) < get(arr, min) {
				find_min(arr, idx + 1, idx)
			} else {
				find_min(arr, idx + 1, min)
			}
		}

		sweep = |arr, idx| {
			if idx == len {
				arr
			} else {
				min = find_min(arr, idx + 1, idx)
				sweep(swap(arr, idx, min), idx + 1)
			}
		}

		sweep(array, 0)
	}

	selection_sort2 = |var $array| {
		len = $array.len()

		for idx in 0..<len {
			next = idx + 1
			var $minimum = idx
			for i in next..<len {
				if get($array, i) < get($array, $minimum) {
					$minimum = i
				}
			}

			if $minimum != idx {
				$array = swap($array, idx, $minimum)
			}
		}

		$array
	}

}

expect Tests.run(SelectionSort.selection_sort)
expect Tests.run(SelectionSort.selection_sort2)
