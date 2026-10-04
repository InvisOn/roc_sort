Utils :: {}.{
	get = |array, idx| {
		expect idx < array.len()
		ok(|| array.get(idx), "get")
	}

	swap = |array, idx, idx2| {
		expect {
			len = array.len()
			idx < len and idx2 < len
		}
		ok(|| array.swap(idx, idx2), "swap")
	}

	replace = |array, idx, elem| {
		expect idx < array.len()
		ok(|| array.replace(idx, elem), "replace").list
	}

	insert = |array, idx, elem| {
		expect idx <= array.len()
		ok(|| array.insert(idx, elem), "insert")
	}

	min = |array| {
		expect array.len() > 0
		ok(|| array.min(), "min")
	}

	max = |array| {
		expect array.len() > 0
		ok(|| array.max(), "max")
	}

	pop = |array| {
		match array {
			[head, .. as tail] => (head, tail)
			[] => crash "pop unreachable"
		}
	}

	## Deterministic pseudo-random list of `len` values in `0..=upper`, using the
	## Park–Miller (MINSTD) generator. The same seed always yields the same list.
	random_list : U64, U64, U64 -> List(U64)
	random_list = |seed, len, upper| {
		generate = |list, state| {
			if list.len() == len {
				return list
			}

			next = (state * 48271) % 2147483647
			generate(list.append(next % (upper + 1)), next)
		}

		generate(List.with_capacity(len), (seed % 2147483646) + 1)
	}
}

ok = |func, name| {
	match func() {
		Ok(v) => v
		Err(_) => crash "${name} unreachable"
	}
}

expect Utils.random_list(42, 5, 100) == Utils.random_list(42, 5, 100)
expect Utils.random_list(42, 5, 100).len() == 5
expect Utils.random_list(42, 50, 10).all(|x| x <= 10)
expect Utils.random_list(1, 3, 100) != Utils.random_list(2, 3, 100)
expect Utils.random_list(7, 0, 100) == []
