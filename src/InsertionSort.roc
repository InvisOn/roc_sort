import Utils exposing [replace, get, insert]
import Tests

InsertionSort :: {}.{
	insertion_sort : List(U64) -> List(U64)
	insertion_sort = |array| {
		match array {
			[] | [_] => array
			[head, .. as tail] => return insertion_sort(tail)
				|> insert_head(head)
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

	return array.sublist({ start: 1, len })
		|> insert_head(e)
		|> insert(0, get(array, 0))
}

expect Tests.test_duo(InsertionSort.insertion_sort)
expect Tests.test_trio(InsertionSort.insertion_sort)
expect Tests.test_already_sorted_even(InsertionSort.insertion_sort)
expect Tests.test_permutation_even(InsertionSort.insertion_sort)
expect Tests.test_already_sorted_odd(InsertionSort.insertion_sort)
expect Tests.test_permutation_odd(InsertionSort.insertion_sort)
expect Tests.test_empty(InsertionSort.insertion_sort)
expect Tests.test_singleton(InsertionSort.insertion_sort)
expect Tests.test_duplicates(InsertionSort.insertion_sort)
expect Tests.test_spare(InsertionSort.insertion_sort)
expect Tests.test_reversed(InsertionSort.insertion_sort)
expect Tests.test_all_equal(InsertionSort.insertion_sort)

expect Tests.test_duo(InsertionSort.insertion_sort2)
expect Tests.test_trio(InsertionSort.insertion_sort2)
expect Tests.test_already_sorted_even(InsertionSort.insertion_sort2)
expect Tests.test_permutation_even(InsertionSort.insertion_sort2)
expect Tests.test_already_sorted_odd(InsertionSort.insertion_sort2)
expect Tests.test_permutation_odd(InsertionSort.insertion_sort2)
expect Tests.test_empty(InsertionSort.insertion_sort2)
expect Tests.test_singleton(InsertionSort.insertion_sort2)
expect Tests.test_duplicates(InsertionSort.insertion_sort2)
expect Tests.test_spare(InsertionSort.insertion_sort2)
expect Tests.test_reversed(InsertionSort.insertion_sort2)
expect Tests.test_all_equal(InsertionSort.insertion_sort2)
