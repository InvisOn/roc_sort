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
}

ok = |func, name| {
	match func() {
		Ok(v) => v
		Err(_) => crash "${name} unreachable"
	}
}
