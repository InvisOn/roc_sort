import Utils exposing [swap, get, test1, test2, test3, test4, test5]

BubbleSort :: {}.{
	bubble_sort : List(U64) -> List(U64)
	bubble_sort = |array| {
		aux = |arr, idx, swapped| {
			len = arr.len()
			if len == 0 {
				return arr
			}

			end = idx == len - 1

			if end and swapped {
				return aux(arr, 0, False)
			}

			if end and !swapped {
				return arr
			}

			next = idx + 1
			if get(arr, idx) > get(arr, next) {
				return swap(arr, idx, next)
					|> aux(next, True)
			}

			aux(arr, next, swapped)
		}

		aux(array, 0, False)
	}

	bubble_sort2 : List(U64) -> List(U64)
	bubble_sort2 = |var $array| {
		len = $array.len()
		if len == 0 {
			return $array
		}

		last = len - 1
		var $swapped = False
		while True {
			$swapped = False
			for i in 1..=last {
				j = i - 1
				prev = get($array, i - 1)
				current = get($array, i)
				if prev > current {
					$array = swap($array, j, i)
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

expect test1(BubbleSort.bubble_sort)
expect test2(BubbleSort.bubble_sort)
expect test3(BubbleSort.bubble_sort)
expect test4(BubbleSort.bubble_sort)
expect test5(BubbleSort.bubble_sort)

expect test1(BubbleSort.bubble_sort2)
expect test2(BubbleSort.bubble_sort2)
expect test3(BubbleSort.bubble_sort2)
expect test4(BubbleSort.bubble_sort2)
expect test5(BubbleSort.bubble_sort2)
