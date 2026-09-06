import Utils

CockTailShakerSort :: {}.{
	sort : List(U64) -> List(U64)
	sort = |var $array| {
		len = $array.len()

		var $swapped = False
		while True {

			for i in 0..<len - 1 {
				j = i + 1
				if Utils.get($array, i) > Utils.get($array, j) {
					$array = Utils.swap($array, i, j)
					$swapped = True
				}
			}

			if !$swapped {
				break
			}

			for i in len - 2..=0 {
				j = i + 1
				if Utils.get($array, i) > Utils.get($array, j) {
					$array = Utils.swap($array, i, j)
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

expect {
	array = [4, 3, 2, 1]
	result = CockTailShakerSort.sort(array)
	dbg array
	dbg result
	[1, 2, 3, 4] == result

}
# expect Utils.test(CockTailShakerSort.sort)
