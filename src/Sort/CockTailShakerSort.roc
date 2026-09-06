import Utils

CockTailShakerSort :: {}.{
	sort : List(U64) -> List(U64)
	sort = |var $array| {
		len = $array.len()

		while True {
			var $swapped = False

			for i in 0..=len - 2 {
				j = i + 1
				if Utils.get($array, i) > Utils.get($array, j) {
					$array = Utils.swap($array, i, j)
					$swapped = True
				}
			}

			if $swapped == False {
				break
			}

			$swapped = False

			for i in (0..=len - 2).iter_rev() {
				j = i + 1
				if Utils.get($array, i) > Utils.get($array, j) {
					$array = Utils.swap($array, i, j)
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

expect Utils.test(CockTailShakerSort.sort)
