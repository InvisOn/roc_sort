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

		holes = fill_holes(List.repeat(0, range), array, minimum)
		empty_holes(holes, array, minimum, 0, 0)
	}

	pigeonhole_sort2 = |var $array| {
		len = $array.len()
		if len < 2 {
			return $array
		}

		minimum = min($array)
		range = max($array) - minimum + 1
		var $holes = List.repeat(0, range)

		for value in $array {
			hole = value - minimum
			$holes = replace($holes, hole, get($holes, hole) + 1)
		}

		var $idx = 0
		for hole in 0..<range {
			for _ in 0..<get($holes, hole) {
				$array = replace($array, $idx, hole + minimum)
				$idx = $idx + 1
			}
		}

		$array
	}
}

## Count how many of each value fall into its hole (value - minimum).
fill_holes = |holes, values, minimum| {
	match values {
		[] => holes
		[value, .. as rest] => {
			hole = value - minimum
			replace(holes, hole, get(holes, hole) + 1)
				|> fill_holes(rest, minimum)
		}
	}
}

## Write each hole's values back into `array`, smallest hole first, starting at `idx`.
empty_holes = |holes, array, minimum, hole, idx| {
	if hole == holes.len() {
		return array
	}

	count = get(holes, hole)
	if count == 0 {
		return empty_holes(holes, array, minimum, hole + 1, idx)
	}

	empty_holes(
		replace(holes, hole, count - 1),
		replace(array, idx, hole + minimum),
		minimum,
		hole,
		idx + 1,
	)
}

expect Tests.run(PigeonholeSort.pigeonhole_sort)
expect Tests.run(PigeonholeSort.pigeonhole_sort2)
