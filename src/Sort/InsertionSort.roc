import Utils

# shift : List(U64), U64, Bool -> List(U64)
# shift = |array, idx, done| {
# 	array
# }

InsertionSort :: {}.{
	sort : List(U64) -> List(U64)
	sort = |var $array| {
		for i in 1..<$array.len() {
			key = Utils.get($array, i)
			var $j = i - 1

			e = Utils.get($array, $j)
			while $j >= 0 and e > key {
				$array = Utils.replace($array, $j, e)
			}

		}

		$array
	}
}

expect {
	array = [2, 4, 1, 3]
	expected = [1, 2, 3, 4]
	dbg expected == InsertionSort.sort(array)
	True

}
# expect Utils.test(InsertionSort.sort)
