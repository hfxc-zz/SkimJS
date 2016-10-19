// Testando -> Declaração de listas e funções pré-carregadas head, tail e concat.

var list1 = [4, 5, 9, 2, 8];
var list2 = [2*1, 4-1, 1+3, 9+9]; // [2, 3, 4]

var a = head(list1); // 4
var b = tail(list1); // [5, 9]
var c = tail(list2); // [3, 4]

var d = concat(b, c); // [5, 9, 3, 4]

d;