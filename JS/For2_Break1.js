// Testando -> For (sem inicialização) e break

var i = 0;
var x = 1;

for(; i < 100; ++i) {
	if (i == 10) {
		break;
	}
	++x;
}

x;