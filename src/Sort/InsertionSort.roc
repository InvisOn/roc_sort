import Utils exposing [replace, get, insert, test1, test2, test3, test4, test5]

InsertionSort :: {}.{
	insertion_sort : List(U64) -> List(U64)
	insertion_sort = |array| {
		match array {
			[] | [_] => array
			[head, .. as tail] => return insert_head(insertion_sort(tail), head)
		}
	}

	insertion_sort2 : List(U64) -> List(U64)
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

expect test1(InsertionSort.insertion_sort)
expect test2(InsertionSort.insertion_sort)
expect test3(InsertionSort.insertion_sort)
expect test4(InsertionSort.insertion_sort)
expect test5(InsertionSort.insertion_sort)

expect test1(InsertionSort.insertion_sort2)
expect test2(InsertionSort.insertion_sort2)
expect test3(InsertionSort.insertion_sort2)
expect test4(InsertionSort.insertion_sort2)
expect test5(InsertionSort.insertion_sort2)
