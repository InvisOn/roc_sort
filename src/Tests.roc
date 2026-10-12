import Utils exposing [random_list]

Tests :: {}.{
	run = |func| {
		# Evaluate every case so all failures get reported, not just the first.
		fixed = cases.map(|(name, input)| check(name, input, func))
		random = random_cases.map(|input| check("random", input, func))
		fixed.concat(random).all(|passed| passed)
	}
}

cases = [
	("sorted_even", [1, 2, 3, 4]),
	("unsorted_even", [2, 4, 1, 3]),
	("sorted_odd", [1, 2, 3, 4, 5]),
	("unsorted_odd", [5, 2, 4, 1, 3]),
	("unsorted_long", unsorted_long),
	("sorted_long", unsorted_long.sort()),
	("empty", []),
	("singleton", [42]),
	("duo", [2, 1]),
	("trio", [1, 3, 2]),
	("duplicates", [2, 1, 3, 1, 2]),
	("sparse", [100, 0, 7]),
	("reversed", [5, 4, 3, 2, 1]),
	("all_equal", [3, 3, 3]),
]

unsorted_long = random_list(0, 20, 12)

## Lists of every length from 2 to 12, a few seeds each, to catch length-dependent bugs.
random_cases = [2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12].join_map(
	|len| [1, 2, 3].map(|seed| random_list(seed * 100 + len, len, 20)),
)

check = |name, input, func| {
	result = func(input)
	passed = result == input.sort()
	if !passed {
		dbg name
		dbg ("input:", input)
		dbg ("result:", result)
	}
	passed
}
