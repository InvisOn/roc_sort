import Utils exposing [swap, get, replace, test_already_sorted_even, test_permutation_even, test_already_sorted_odd, test_permutation_odd, test_empty]

CountingSort :: {}.{
	counting_sort = |array| {
		len = array.len()
		if len == 0 {
			return array
		}

		k = match array.max() {
			Ok(i) => i
			Err(_) => crash "unreachable"
		}

		var $count = List.repeat(0, k + 1)
		var $output = List.repeat(0, len)

		$count = count($count, array)
		$count = count2($count, k + 1, 1)

		for i in array.rev() {
			$count = (get($count, i) - 1) |> (|j| replace($count, i, j))
			$output = get($count, i) |> (|j| replace($output, j, i))
		}

		$output
	}

	counting_sort2 = |array| {
		len = array.len()
		if len == 0 {
			return array
		}

		max = match array.max() {
			Ok(i) => i
			Err(_) => crash "unreachable"
		}

		var $count = List.repeat(0, max + 1)
		var $output = List.repeat(0, len)

		for i in array {
			$count = get($count, i)
				|> (|j| replace($count, i, j + 1))
		}

		for i in 1..<max + 1 {
			$count = (get($count, i) + get($count, i - 1))
				|> (|j| replace($count, i, j))
		}

		for i in array.rev() {
			$count = (get($count, i) - 1)
				|> (|j| replace($count, i, j))
			$output = get($count, i)
				|> (|j| replace($output, j, i))
		}

		$output
	}
}

count = |acc, array| {
	match array {
		[] => acc
		[head, .. as tail] => {
			get(acc, head)
				|> (|j| replace(acc, head, j + 1))
				|> count(tail)
		}
	}
}

count2 = |acc, max, idx| {
	if idx == max {
		return acc
	}

	return (get(acc, idx) + get(acc, idx - 1))
		|> (|j| replace(acc, idx, j))
		|> count2(max, idx + 1)
}

sort = |acc, var $count, array| {
	match array {
		[] => acc
		[head, .. as tail] => {
			$count = (get($count, head) - 1)
				|> (|j| replace($count, head, j))

			get($count, head)
				|> (|j| replace(acc, j, head))
				|> sort($count, tail)
		}
	}
}

# expect test_already_sorted_even(CountingSort.counting_sort)
expect test_permutation_even(CountingSort.counting_sort)
# expect test_already_sorted_odd(CountingSort.counting_sort)
# expect test_permutation_odd(CountingSort.counting_sort)
# expect test_empty(CountingSort.counting_sort)

expect test_already_sorted_even(CountingSort.counting_sort2)
expect test_permutation_even(CountingSort.counting_sort2)
expect test_already_sorted_odd(CountingSort.counting_sort2)
expect test_permutation_odd(CountingSort.counting_sort2)
expect test_empty(CountingSort.counting_sort2)
