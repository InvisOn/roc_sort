import Utils exposing [replace, swap, get, test]

PigeonholeSort :: {}.{
	sort : List(U64) -> List(U64)
	sort = |var $array| {
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

expect test(PigeonholeSort.sort)
