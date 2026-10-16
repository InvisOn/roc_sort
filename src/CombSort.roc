import Utils exposing [get, swap]
import Tests

CombSort :: {}.{
	comb_sort = |array| {
		len = array.len()
		if len < 2 {
			array
		} else {
			comb_sort_pass(array, len, len)
		}
	}

	comb_sort2 = |var $array| {
		len = $array.len()
		if len < 2 {
			return $array
		}

		var $gap = len
		var $sorted = False

		while !$sorted {
			$gap = next_gap($gap)
			# Only a pass with gap 1 and no swaps proves the list is sorted.
			$sorted = $gap == 1

			for i in 0..<len - $gap {
				if get($array, i) > get($array, i + $gap) {
					$array = swap($array, i, i + $gap)
					$sorted = False
				}
			}
		}

		$array
	}
}

comb_sort_pass = |array, prev_gap, len| {
	gap = next_gap(prev_gap)
	(scanned, sorted) = scan_gap(array, gap == 1, 0, len, gap)
	if sorted {
		scanned
	} else {
		comb_sort_pass(scanned, gap, len)
	}
}

scan_gap = |array, is_sorted, i, len, gap| {
	if !(i + gap < len) {
		(array, is_sorted)
	} else if get(array, i) > get(array, i + gap) {
		swap(array, i, i + gap)
			|> scan_gap(False, i + 1, len, gap)
	} else {
		scan_gap(array, is_sorted, i + 1, len, gap)
	}
}

## Shrink by the usual factor of 1.3, never below 1. Gaps of 9 and 10 jump to
## 11 ("Combsort11"), which avoids some slow worst cases.
next_gap = |gap| {
	shrunk = gap * 10 // 13
	if shrunk <= 1 {
		1
	} else if shrunk == 9 or shrunk == 10 {
		11
	} else {
		shrunk
	}
}

expect Tests.run(CombSort.comb_sort)
expect Tests.run(CombSort.comb_sort2)
