import Utils exposing [swap, get, test]

CockTailShakerSort :: {}.{
	sort : List(U64) -> List(U64)
	sort = |var $array| {
		len = $array.len()

		while True {
			var $swapped = False

			for i in 0..=len - 2 {
				j = i + 1
				if get($array, i) > get($array, j) {
					$array = swap($array, i, j)
					$swapped = True
				}
			}

			if $swapped == False {
				break
			}

			$swapped = False

			for i in (0..=len - 2).iter_rev() {
				j = i + 1
				if get($array, i) > get($array, j) {
					$array = swap($array, i, j)
					$swapped = True
				}
			}

			if $swapped == False {
				break
			}
		}

		$array
	}
}

expect test(CockTailShakerSort.sort)
