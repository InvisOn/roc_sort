import Utils exposing [get, swap]
import Tests

CombSort :: {}.{
	comb_sort : List(U64) -> List(U64)
	comb_sort = |var $array| {
		len = $array.len()
		if len == 0 {
			return $array
		}

		return comb_sort_pass($array, len, False, len)
	}

	comb_sort2 : List(U64) -> List(U64)
	comb_sort2 = |var $array| {
		len = $array.len()
		if len == 0 {
			return $array
		}
		var $gap = len
		var $sorted = False

		while !$sorted {
			$gap = $gap.to_f64().div_floor_by(1.3).to_u64_wrap()

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

comb_sort_pass = |var $array, var $gap, var $sorted, len| {
	if $sorted {
		return $array
	}

	$gap = $gap.to_f64().div_floor_by(1.3).to_u64_wrap()

	if $gap <= 1 {
		$gap = 1
		$sorted = True
	} else if $gap == 9 or $gap == 10 {
		$gap = 11
	}

	($array, $gap, _, _, $sorted) = scan_gap($array, $gap, 0, len, $sorted)

	return comb_sort_pass($array, $gap, $sorted, len)
}

scan_gap = |array, gap, i, len, sorted| {
	if !(i + gap < len) {
		return (array, gap, i, len, sorted)
	}

	(arr, srtd) = compare_and_swap(array, i, gap, sorted)
	return scan_gap(arr, gap, i + 1, len, srtd)
	# BUG: Lambdas that capture a variable are not tail call optimized
	# return compare_and_swap(array, i, gap, sorted)
	# 	|> (|(arr, srtd)| scan_gap(arr, gap, i + 1, len, srtd))
}

compare_and_swap = |array, i, gap, sorted| {
	if get(array, i) > get(array, i + gap) {
		(swap(array, i, i + gap), False)
	} else {
		(array, sorted)
	}
}

expect Tests.run(CombSort.comb_sort)
expect Tests.run(CombSort.comb_sort2)
