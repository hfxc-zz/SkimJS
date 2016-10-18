// Implementação de len e mergesort

function len(list) {
		aux = tail(list);
		if (aux == []) {
			return 1;
		}
	return 1 + len(aux);
}

function merge(l, r) {
	var result = [];

	while ((l != []) && (r != [])) {
		if (head(l) <= head(r)) {
			result = concat(result,[head(l)])
			l = tail(l)
		}
		else {
			result = concat(result,[head(r)])
			r = tail(r)
		}
	}

	if (l != []) {
		result = concat(result,l)
	}
	else {
		result = concat(result,r)
	}

	return result;
}

function mergesort(list) {
	var length = len(list);

	if (length<=1) {
		return list;
	}

	var left = [];
	var right = [];

	for (var i=0; i<length; i++) {
		if (i < length/2) {
			left = concat(left, [list[i]]);
		}
		else {
			right = concat(right, [list[i]]);
		}
	}

	left = mergesort(left);
	right = mergesort(right);

	return merge(left, right);
}

var list = [13, 3, 8, 1, -4, -9, 6, 7];
var ord = mergesort(list);
ord;