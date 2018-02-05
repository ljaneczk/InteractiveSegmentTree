# InteractiveSegmentTree
This project implements segment tree which supports operations:
- build tree of given size
- initialize tree with some values from beginning
- change all values on interval into new value v
- add v to all values on interval
- get maximum value on interval
- get minimum value on interval
- get sum of values on interval
- print values from interval in list format

Please note that this program is interactive. If you want to exit, you have to type appropriate command.
At the beginning you need to give n - the size of segment tree.
Then you may initialize some part of tree from beginning (left) by typing numbers separated by space (you may type nothing).
After that you type below commands as long as you type them in correct format.
Legal commands:
set b e v   - change values on interval [b,e) into v
add b e v   - add value v to all numbers on interval [b,e)
sum b e     - return sum of numbers on interval [b,e)
min b e     - return minimum number on interval [b,e)
max b e     - return maximum number on interval [b,e)
tre b e     - print numbers on interval [b,e) in list format
q           - exit
:q          - exit
quit        - exit
exit        - exit
