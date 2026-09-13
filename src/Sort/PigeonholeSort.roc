import Utils exposing [replace, swap, get, test1, test2, test3, test4, test5]

PigeonholeSort :: {}.{
	pigeonhole_sort : List(U64) -> List(U64)
	pigeonhole_sort = |var $array| {
		if $array.len() == 0 {
			return $array
		}

		min = match $array.min() {
			Ok(m) => m
			Err(_) => crash "unreachable"
		}
		max = match $array.max() {
			Ok(m) => m
			Err(_) => crash "unreachable"
		}

		range = max - min + 1
		var $tmp = List.repeat(0, range)

		for i in 0..<range {
			j = get($array, i) - min
			k = get($tmp, j) + 1
			$tmp = replace($tmp, j, k)
		}

		var $idx = 0
		for i in 0..<range {
			while get($tmp, i) > 0 {
				j = get($tmp, i) - 1
				$tmp = replace($tmp, i, j)
				$array = replace($array, $idx, i + min)
				$idx = $idx + 1
			}
		}

		$array
	}

}

expect test1(PigeonholeSort.pigeonhole_sort)
expect test2(PigeonholeSort.pigeonhole_sort)
expect test3(PigeonholeSort.pigeonhole_sort)
expect test4(PigeonholeSort.pigeonhole_sort)
expect test5(PigeonholeSort.pigeonhole_sort)
