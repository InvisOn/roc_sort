import Utils exposing [get, swap]
import Tests

CombSort :: {}.{
	comb_sort = |array| {
		len = array.len()
		if len < 2 {
			array
		} else {
			comb_sort_pass(array, shrink_gap(len), False, len)
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
			$gap = shrink_gap($gap)

			if $gap <= 1 {
				$gap = 1
				$sorted = True
			} else if $gap == 9 or $gap == 10 {
				$gap = 11
			}

			var $i = 0
			while $i + $gap < len {
				if get($array, $i) > get($array, $i + $gap) {
					$array = swap($array, $i, $i + $gap)
					$sorted = False
				}

				$i = $i + 1
			}
		}

		$array
	}
}

comb_sort_pass = |array, gap, is_sorted, len| {
	if is_sorted {
		return array
	}

	(new_gap, new_is_sorted) = if gap <= 1 {
		(1, True)
	} else if gap == 9 or gap == 10 {
		(11, is_sorted)
	} else {
		(gap, is_sorted)
	}

	(scanned, scan_sorted) = scan_gap(array, new_is_sorted, 0, len, new_gap)
	comb_sort_pass(scanned, shrink_gap(new_gap), scan_sorted, len)
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

shrink_gap = |gap| gap * 10 // 13

expect Tests.run(CombSort.comb_sort)
expect Tests.run(CombSort.comb_sort2)
