# assembly-interpreter

This is a project that interprets an assembly language.

## Specification
- 16 32-bit registers, indexed 0 through 15
- There are no memory instructions
- Clock cycles are counted - every instruction has takes one clock cycle, apart from `mul`, `jp` and `jz`, which take 3
- `jz` and `jp` both do a relative jump; positive is forward, negative is backwards
- Any overflow will crash the system

The instruction set is listed below.

| instruction | description |
| --- | --- |
| add d a b | r[d] = r[a] + r[b] |
| sub d a b |  r[d] = r[a] - r[b] |
| mul d a b | r[d] = r[a] * r[b] |
| shl d a | left shift r[d] by r[a] bits |
| shr d a | right shift r[d] by r[a] bits |
| bor d a b | r[d] = r[a] | r[b] |
| band d a b | r[d] = r[a] & r[b] |
| li d i | load immediate value to a register: r[d] = i | 
| jz a i | do a relative jump by i instructions if r[a]==0 |
| jp a i | do a relative jump by i instructions if r[a] > 0 |

## Usage

1. Clone the repository, using `git clone`
2. Navigate to the directory the repository is located
3. Run `$ cabal run . -- <code> <register initialisation>`

Code denotes the assembly file you want to interpret, e.g., `test.asm`.
The register initialisation is a comma separated list of the initial register values. You can have __zero__ to __sixteen__ of these.

### Example command
```
$ cabal run . -- test.asm 0,100,20
# r[0] = 0
# r[1] = 100
# r[2] = 20
```
