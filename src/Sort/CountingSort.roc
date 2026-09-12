import Utils exposing [swap, get, test, replace]

CountingSort :: {}.{
	sort : List(U64) -> List(U64)
	sort = |array| {
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

expect test(CountingSort.sort)
