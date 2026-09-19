import Utils exposing [replace, get, insert, test_already_sorted_even, test_permutation_even, test_already_sorted_odd, test_permutation_odd, test_empty]

InsertionSort :: {}.{
	insertion_sort = |array| {
		match array {
			[] | [_] => array
			[head, .. as tail] => return insert_head(insertion_sort(tail), head)
		}
	}

	insertion_sort2 = |var $array| {
		var $idx = 1
		while $idx < $array.len() {
			x = get($array, $idx)
			var $j = $idx
			while $j > 0 and get($array, $j - 1) > x {
				$array = replace($array, $j, get($array, $j - 1))
				$j = $j - 1
			}
			$array = replace($array, $j, x)
			$idx = $idx + 1
		}

		$array
	}
}

insert_head = |array, e| {
	len = array.len()
	if len == 0 or e <= get(array, 0) {
		return insert(array, 0, e)
	}

	first = get(array, 0)
	return array.sublist({ start: 1, len })
		|> insert_head(e)
		|> insert(0, first)
}

expect test_already_sorted_even(InsertionSort.insertion_sort)
expect test_permutation_even(InsertionSort.insertion_sort)
expect test_already_sorted_odd(InsertionSort.insertion_sort)
expect test_permutation_odd(InsertionSort.insertion_sort)
expect test_empty(InsertionSort.insertion_sort)

expect test_already_sorted_even(InsertionSort.insertion_sort2)
expect test_permutation_even(InsertionSort.insertion_sort2)
expect test_already_sorted_odd(InsertionSort.insertion_sort2)
expect test_permutation_odd(InsertionSort.insertion_sort2)
expect test_empty(InsertionSort.insertion_sort2)
