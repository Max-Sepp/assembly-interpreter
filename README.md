# assembly-interpreter

this project roughly interprets this assembly language:

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

# how to use
1. navigate to the directory the project is located in
2. `$ cabal run . -- <code.asm> 0,0,0`

Second command line argument is comma seperated initial values of registers does not have to be all 16 if not specified will be set to 0. E.g. 5,2 will set register 0 to 5 and register 1 to 2 and the reset 0