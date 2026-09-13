import Utils exposing [swap, get, test1, test2, test3, test4, test5]

CombSort :: {}.{
	comb_sort : List(U64) -> List(U64)
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

expect test1(CombSort.comb_sort)
expect test2(CombSort.comb_sort)
expect test3(CombSort.comb_sort)
expect test4(CombSort.comb_sort)
expect test5(CombSort.comb_sort)
