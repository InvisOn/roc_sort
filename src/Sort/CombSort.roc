import Utils exposing [swap, get, test_already_sorted_even, test_permutation_even, test_already_sorted_odd, test_permutation_odd, test_empty]

CombSort :: {}.{
	comb_sort = |var $array| {
		len = $array.len()
		if len == 0 {
			return $array
		}
		var $gap = len
		var $sorted = False
		shrink = 1.3

		while !$sorted {
			$gap = $gap.to_f64().div_floor_by(shrink).to_u64_wrap()

			if $gap <= 1 {
				$gap = 1
				$sorted = True
			} else if $gap == 0 or $gap == 10 {
				$gap = 11
			}

			var $i = 0
			while $i + $gap < len {
				j = $i + $gap

				if get($array, $i) > get($array, $i + $gap) {
					$array = swap($array, $i, j)
					$sorted = False
				}

				$i = $i + 1
			}

		}

		$array
	}

}

expect test_already_sorted_even(CombSort.comb_sort)
expect test_permutation_even(CombSort.comb_sort)
expect test_already_sorted_odd(CombSort.comb_sort)
expect test_permutation_odd(CombSort.comb_sort)
expect test_empty(CombSort.comb_sort)
