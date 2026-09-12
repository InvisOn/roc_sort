import Utils exposing [replace, get, insert, test]

InsertionSort :: {}.{
	sort : List(U64) -> List(U64)
	sort = |array| {
		match array {
			[] | [_] => array
			[head, .. as tail] => return insert_head(sort(tail), head)
		}
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

expect test(InsertionSort.sort)
