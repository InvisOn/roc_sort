import Sort/BubbleSort
import Sort/GnomeSort

main! = |_args| {
	dbg [5, 2, 4, 1, 3] |> GnomeSort.gnome_sort
	Ok({})
}
