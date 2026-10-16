import Utils exposing [get, swap]
import Tests

BubbleSort :: {}.{
	bubble_sort = |array| {
		len = array.len()
		if len < 2 {
			return array
		}

		bubble = |arr, idx, swapped| {
			end = idx == len - 1

			if end {
				return if swapped bubble(arr, 0, False) else arr
			}

			next = idx + 1
			if get(arr, idx) > get(arr, next) {
				return swap(arr, idx, next)
					|> bubble(next, True)
			}

			bubble(arr, next, swapped)
		}

		bubble(array, 0, False)
	}

	bubble_sort2 = |var $array| {
		len = $array.len()
		if len < 2 {
			return $array
		}

		last = len - 1
		var $swapped = False
		while True {
			$swapped = False
			for i in 1..=last {
				prev = i - 1
				if get($array, prev) > get($array, i) {
					$array = swap($array, prev, i)
					$swapped = True
				}
			}

			if !$swapped {
				break
			}
		}

		$array
	}
}

expect Tests.run(BubbleSort.bubble_sort)
expect Tests.run(BubbleSort.bubble_sort2)
