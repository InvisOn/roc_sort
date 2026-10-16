import Utils exposing [get, swap]
import Tests

CockTailShakerSort :: {}.{
	cocktail_shaker_sort = |array| {
		len = array.len()
		if len < 2 {
			return array
		}

		alternate_sweeps = |var $arr| {
			forward_stop = len - 2
			($arr, var $swapped) = sweep($arr, False, 0, |i| i == forward_stop, |i| i + 1)

			if !$swapped {
				return $arr
			}

			($arr, $swapped) = sweep($arr, False, len - 2, |i| i == 0, |i| i - 1)

			if !$swapped {
				return $arr
			}

			alternate_sweeps($arr)
		}

		alternate_sweeps(array)
	}

	cocktail_shaker_sort2 = |var $array| {
		len = $array.len()
		if len < 2 {
			return $array
		}

		while True {
			var $swapped = False

			for i in 0..<len - 1 {
				j = i + 1
				if get($array, i) > get($array, j) {
					$array = swap($array, i, j)
					$swapped = True
				}
			}

			if !$swapped {
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

			if !$swapped {
				break
			}
		}

		$array
	}
}

sweep = |var $array, var $swapped, i, stop_at, step| {
	j = i + 1
	if get($array, i) > get($array, j) {
		$array = swap($array, i, j)
		$swapped = True
	}

	if stop_at(i) {
		return ($array, $swapped)
	}

	sweep($array, $swapped, step(i), stop_at, step)
}

expect Tests.run(CockTailShakerSort.cocktail_shaker_sort)
expect Tests.run(CockTailShakerSort.cocktail_shaker_sort2)
