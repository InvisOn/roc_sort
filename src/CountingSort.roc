import Utils exposing [get, max, replace]
import Tests

CountingSort :: {}.{
	counting_sort = |array| {
		len = array.len()
		if len < 2 {
			return array
		}

		k = max(array)
		count = count1(List.repeat(0, k + 1), array)
			|> count2(k + 1, 1)

		count3(List.repeat(0, len), array.rev(), count)
	}

	counting_sort2 = |array| {
		len = array.len()
		if len < 2 {
			return array
		}

		k = max(array)
		var $count = List.repeat(0, k + 1)
		var $output = List.repeat(0, len)

		for i in array {
			$count = replace($count, i, get($count, i) + 1)
		}

		for i in 1..<k + 1 {
			sum = get($count, i) + get($count, i - 1)
			$count = replace($count, i, sum)
		}

		for i in array.rev() {
			$count = replace($count, i, get($count, i) - 1)
			$output = replace($output, get($count, i), i)
		}

		$output
	}
}

count1 = |acc, array| {
	match array {
		[] => acc
		[head, .. as tail] => {
			replace(acc, head, get(acc, head) + 1)
				|> count1(tail)
		}
	}
}

count2 = |acc, max, idx| {
	if idx == max {
		acc
	} else {
		sum = get(acc, idx) + get(acc, idx - 1)
		replace(acc, idx, sum)
			|> count2(max, idx + 1)
	}
}

count3 = |output, array_rev, count| {
	match array_rev {
		[] => output
		[i, .. as tail] => {
			new_count = replace(count, i, get(count, i) - 1)
			replace(output, get(new_count, i), i)
				|> count3(tail, new_count)
		}
	}
}

expect Tests.run(CountingSort.counting_sort)
expect Tests.run(CountingSort.counting_sort2)
