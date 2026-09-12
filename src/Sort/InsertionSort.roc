import Utils exposing [replace, get, insert, test]

InsertionSort :: {}.{
	sort : List(U64) -> List(U64)
	sort = |array| {
		match array {
			[] | [_] => array
			[head, .. as tail] => return insertion(sort(tail), head)
		}
	}
}

insertion = |array, e| {
	len = array.len()
	if len == 0 or e <= get(array, 0) {
		return insert(array, 0, e)
	} else {
		first = get(array, 0)
		return array.sublist({ start: 1, len: len }) |> insertion(e) |> insert(0, first)
	}
}

expect test(InsertionSort.sort)
