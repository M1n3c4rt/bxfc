# Boxfuscate
Boxfuscate is an esoteric programming language where all of the code (even the comments) are written with box drawing characters.

This repository contains the source code for the interpreter written in haskell, as well as documentation for the language in this README.

## Documentation

### Components
A program consists of the following components:
- Memory: An infinite array of bits that extends in both directions.
- Memory pointer: A number that points to a location in memory. Always starts at position 0.
- Grid: The grid of box drawing characters supplied by the user. This is the visible part of the program.
- Instruction pointer: A point on the grid at which the program is currently being executed, along with the direction it is facing.

### Input
Input is read and stored in memory only at the start of the program. The interpreter can choose how to do this.

This interpreter has three modes of input, none of which store any information in negative positions in memory:
- Bytestrings (read left to right)
- Binary files (read left to right)
- Numbers (read in decimal and converted to binary, then stored right to left with the least significant bit at position 0)

### Grid Characters
Only characters in the unicode block U+25XX (box drawing characters), and the whitespace and newline (U+20 and U+A) are allowed. Characters are distinguished by their line style in each of the four cardinal directions (for example, ╧ has a thin line pointing north and double lines pointing east and west). There are some additional rules as to how these characters can border each other:

- Empty space can only border other empty spaces or dotted lines.
- Thin lines can only border other thin lines or dotted thin lines.
- Thick lines can only border other thick lines or dotted thick lines.
- Double lines can only border other double lines.
- Dotted thin lines can only border other dotted thin lines, thin lines, or empty space.
- Dotted thick lines can only border other dotted thick lines, thick lines, or empty space.
- Curved characters (╭ ╮ ╯╰) count as having thin lines pointing in two directions and empty space in the other two.
- Diagonals (╲ ╲ ╳) count as having empty space in all directions.

### Shape behaviour
- Characters having empty space in four directions will never be reached by the instruction pointer.
- Characters having empty space in three directions are treated as the starting point of the program. A valid program has exactly one of these characters.
- Characters having empty space in two directions are entered by the instruction pointer through one of the non-empty sides and exited through the other.
- Characters having empty space in one direction are called "branches". When an instruction pointer lands on a branch, it has two possible directions to proceed in (backtracking is never allowed). The direction is decided by these rules:
    - If the bit at the memory pointer is 0, prioritize east, then north, then west, then south.
    - If the bit at the memory pointer is 1, prioritize directions in the reverse order.
    - The instruction pointer then follows the direction with the highest priority.
- Characters having no empty spaces (┼ shapes) are always treated as two straight lines overlapping each other. For example, if the instruction pointer enters from the west, it will always leave to the east.

### Line style behaviour
A character's behaviour is determined by the line style at the direction the instruction pointer _exits_ from.
For example, if the pointer enters from the west, ╖ it is treated as a double line (because it exits to the south).
Each line style has a different behaviour:

- Thin lines move the memory pointer _back_ by 1.
- Thick lines move the memory pointer _forward_ by 1.
- Double lines _flip the bit_ at the memory pointer.
- Curved lines (╭ ╮ ╯╰) do nothing, they serve as a way to redirect the instruction pointer.
- Diagonal lines (╱ ╲ ╳) do nothing, they serve as a way to write comments since they can border empty space.
- _Double_ dotted lines act as bridges. (described in the section below)
- _Triple_ dotted lines will either:
    - Terminate the program if all the bits to the _right_ of the memory pointer (not including the position at the pointer itself) are zero.
    - Act as a bridge otherwise.
- _Quadruple_ dotted lines will unconditionally terminate the program.

### Bridges
Double and triple dotted lines can act as bridges (as described above).
A _bridge_ "launches" the instruction pointer into empty space. The pointer keeps travelling in the same direction until a character is encountered that _has a non-empty line facing in the same direction as the pointer._ Once such a character is encountered, the instruction pointer lands on the start of the character _after_ it and resumes execution.

WARNING: Any character outside the grid is assumed to be empty space. If the instruction pointer never lands on a character, the program will not terminate!

### Output
Similar to input, output is given only after the program finishes running, and the interpreter can choose how to do this.

This interpreter has three modes of output, all of which ignore bits at negative positions:
- Bytetring (read left to right)
- Number (read right to left with least significant bit at position 0, and displayed in decimal)
- Bits (displays the raw sequence of bits until the infinitely many trailing zeroes are reached)