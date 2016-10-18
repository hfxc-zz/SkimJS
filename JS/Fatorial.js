// Testando -> Função recursiva

function fat(n) {
	if (n == 0) {
		return 1;
	} else {
		return (n*fat(n-1));
	}
}

var x = fat(3);

x;