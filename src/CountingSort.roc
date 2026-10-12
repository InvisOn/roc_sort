import Utils exposing [get, max, replace]
import Tests

CountingSort :: {}.{
	counting_sort : List(U64) -> List(U64)
	counting_sort = |array| {
		len = array.len()
		if len < 2 {
			return array
		}

		k = max(array)
		positions = tally(List.repeat(0, k + 1), array)
			|> prefix_sums(1)

		place(List.repeat(0, len), array.rev(), positions)
	}

	counting_sort2 : List(U64) -> List(U64)
	counting_sort2 = |array| {
		len = array.len()
		if len < 2 {
			return array
		}

		k = max(array)
		var $count = List.repeat(0, k + 1)
		var $output = List.repeat(0, len)

		for i in array {
			$count = replace($count, i, get($count, i) + 1)
		}

		for i in 1..=k {
			sum = get($count, i) + get($count, i - 1)
			$count = replace($count, i, sum)
		}

		for i in array.rev() {
			$count = replace($count, i, get($count, i) - 1)
			$output = replace($output, get($count, i), i)
		}

		$output
	}
}

## Count how many times each value occurs.
tally = |acc, array| {
	match array {
		[] => acc
		[head, .. as tail] => {
			replace(acc, head, get(acc, head) + 1)
				|> tally(tail)
		}
	}
}

## Turn counts into end positions: each slot becomes the number of values <= its index.
prefix_sums = |acc, idx| {
	if idx == acc.len() {
		acc
	} else {
		sum = get(acc, idx) + get(acc, idx - 1)
		replace(acc, idx, sum)
			|> prefix_sums(idx + 1)
	}
}

## Walk the input backwards (keeping the sort stable), dropping each value into its slot.
place = |output, array_rev, positions| {
	match array_rev {
		[] => output
		[i, .. as tail] => {
			new_positions = replace(positions, i, get(positions, i) - 1)
			replace(output, get(new_positions, i), i)
				|> place(tail, new_positions)
		}
	}
}

expect Tests.run(CountingSort.counting_sort)
expect Tests.run(CountingSort.counting_sort2)
