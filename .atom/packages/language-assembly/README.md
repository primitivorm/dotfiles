# language-assembly
## An assembly grammar for Atom.
------------

* Supports Intel syntax and x86 instruction sets.
* Fully supports the 8051 family. (_NEW_) (Yay!)

![Yay, pretty colours!](https://raw.githubusercontent.com/oliverkeeble/atom-language-assembly/master/screenshot.png)

------------

## Example snippets
Right now, the package does not include any snippets, but you can add them manually.

Check out all the snippets we have on our [Wiki](https://github.com/oliverkeeble/atom-language-assembly/wiki)!

```
'.source.assembly.asm.x86.intel':
    'Move instruction':
        'prefix': 'MOV'
        'body': 'MOV $1, $2'
    'Move to Accumulator':
        'prefix': 'MOVA'
        'body': 'MOV A, $1'
    'Add':
        'prefix': 'ADD'
        'body': 'ADD A, $1'
```

------------
### To Do List
- [ ] Other syntaxes
	- [ ] AT&T
	- [ ] Others?
- [ ] Other instruction sets
	- [ ] ARM
	- [ ] MIPS
	- [x] ATMEL
	- [ ] Others?
- [ ] Add snippets to package.
- [ ] Lots of testing.
- [ ] More instructions.
- [ ] Even more instructions.
- [ ] Fix all the bugs.
- [ ] Fix even more bugs.
