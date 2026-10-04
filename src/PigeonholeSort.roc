import Utils exposing [get, max, min, replace]
import Tests

PigeonholeSort :: {}.{
	pigeonhole_sort = |array| {
		len = array.len()
		if len < 2 {
			return array
		}

		minimum = min(array)
		range = max(array) - minimum + 1

		fill_temp(List.repeat(0, range), array, len, minimum, 0)
			|> sweep(array, 0, range, minimum, 0)
	}

	pigeonhole_sort2 = |var $array| {
		len = $array.len()
		if len < 2 {
			return $array
		}

		minimum = min($array)
		range = max($array) - minimum + 1
		var $tmp = List.repeat(0, range)

		for i in 0..<len {
			j = get($array, i) - minimum
			k = get($tmp, j) + 1
			$tmp = replace($tmp, j, k)
		}

		var $idx = 0
		for i in 0..<range {
			while get($tmp, i) > 0 {
				j = get($tmp, i) - 1
				$tmp = replace($tmp, i, j)
				$array = replace($array, $idx, i + minimum)
				$idx = $idx + 1
			}
		}

		$array
	}
}

fill_temp = |tmp, array, len, minimum, idx| {
	if idx == len {
		tmp
	} else {
		j = get(array, idx) - minimum
		replace(tmp, j, get(tmp, j) + 1)
			|> fill_temp(array, len, minimum, idx + 1)
	}
}

sweep = |temp, array, idx, range, minimum, i| {
	if i == range {
		array
	} else {
		(arr, tmp, j) = sort(temp, array, minimum, i, idx)
		sweep(tmp, arr, j, range, minimum, i + 1)
	}
}

sort = |tmp, var $array, minimum, i, idx| {
	elem = get(tmp, i)
	if !(elem > 0) {
		return ($array, tmp, idx)
	}
	j = elem - 1
	$array = replace($array, idx, i + minimum)

	replace(tmp, i, j)
		|> sort($array, minimum, i, idx + 1)
}

expect Tests.run(PigeonholeSort.pigeonhole_sort)
expect Tests.run(PigeonholeSort.pigeonhole_sort2)
