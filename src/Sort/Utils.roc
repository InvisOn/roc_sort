Utils :: {}.{
	get = |array, idx| {
		expect idx < array.len()
		match array.get(idx) {
			Ok(arr) => arr
			_ => crash "get unreachable"
		}
	}

	swap = |array, idx, idx2| {
		expect {
			len = array.len()
			idx < len and idx2 < len
		}
		match array.swap(idx, idx2) {
			Ok(arr) => arr
			_ => crash "swap unreachable"
		}
	}

	replace = |array, idx, elem| {
		expect idx < array.len()
		match array.replace(idx, elem) {
			Ok({ list: arr, prev: _ }) => arr
			_ => crash "replace unreachable"
		}
	}

	insert = |array, idx, elem| {
		expect idx <= array.len()
		match array.insert(idx, elem) {
			Ok(arr) => arr
			_ => crash "insert unreachable"
		}
	}

	min = |array| {
		expect array.len() > 0
		match array.min() {
			Ok(m) => m
			Err(_) => crash "unreachable"
		}
	}

	max = |array| {
		expect array.len() > 0
		match array.max() {
			Ok(m) => m
			Err(_) => crash "unreachable"
		}
	}
}
