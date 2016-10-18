// Testando -> Variáveis automaticamente globais e locais.

function exp(n, m) { // n^m
	g = 10; // Variável global

	var aux = n;
	for(var i = m; i > 0; i--) {
		aux = aux*n;
	}
	return aux;
}

var x = exp(3, 2);

x;