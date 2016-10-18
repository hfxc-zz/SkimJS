// Testando -> Variável global incrementada por uma função

function inc() {
	x++;
}
var x = 0;

for(var i=1; ; ++i) {
	if (i > 3) {
		break;
	} else {
		inc();
	}
}

x;