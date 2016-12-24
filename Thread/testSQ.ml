open SQueue;;

let q = SQueue.create();;

SQueue.add 4 q;;

print_int (SQueue.take q);;
print_newline();;
SQueue.add 1 q;;
SQueue.add 2 q;;
SQueue.add 3 q;;
print_int (SQueue.take q);;
print_newline();;
print_int (SQueue.take q);;
print_newline();;
print_int (SQueue.take q);;
print_newline();;
print_int (SQueue.take q);;
print_newline();;
