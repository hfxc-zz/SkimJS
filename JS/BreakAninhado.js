// Testando -> Break dentro de escopo aninhado

var x = 3;

for(var i=1; i <= 4;  i=i+1) {
	if(i > 2) {
		if (i > 3) {
			break;
		}
	}
	x++;
}

x;