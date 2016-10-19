// Implementação de len

function len(l) {
	if (l == []) {
		return 0;
	}

	return 1 + len(tail(l));
}

var list = [13, 3, 8, 1, -4, -9, 6, 7];
var length = len(list);

length;