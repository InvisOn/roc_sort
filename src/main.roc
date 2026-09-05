import Sort/BubbleSort
import Sort/GnomeSort
import Sort/OddEvenSort

main! = |_args| {
	_ = BubbleSort.bubble_sort([1])
	_ = GnomeSort.gnome_sort([2])
	_ = OddEvenSort.odd_even_sort([3])
	Ok({})
}
