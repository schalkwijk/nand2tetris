load Add.hack,
output-file Add.out,
compare-to Add.cmp,
output-list RAM[0]%D2.6.2

set RAM[0] 0,   // Set test arguments
repeat 20 {
  ticktock;
}
output;