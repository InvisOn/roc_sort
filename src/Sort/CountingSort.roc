import Utils exposing [swap, get, replace, test1, test2, test3, test4, test5]

CountingSort :: {}.{
	counting_sort : List(U64) -> List(U64)
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

		for i in array {
			$count = (get($count, i) + 1) |> (|j| replace($count, i, j))
		}

		for i in 1..<k + 1 {
			$count = (get($count, i) + get($count, i - 1))
				|> (|j| replace($count, i, j))
		}

		for i in array.rev() {
			$count = (get($count, i) - 1) |> (|j| replace($count, i, j))
			$output = get($count, i) |> (|j| replace($output, j, i))
		}

		$output
	}

}

expect test1(CountingSort.counting_sort)
expect test2(CountingSort.counting_sort)
expect test3(CountingSort.counting_sort)
expect test4(CountingSort.counting_sort)
expect test5(CountingSort.counting_sort)
