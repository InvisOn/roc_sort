import sort/Bubble_sort

main! = |_args| {
	arr = [5, 2, 4, 1, 3]

	sorted = bubble_sort(arr)

	dbg sorted

	Ok({})
}
