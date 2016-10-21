# language-assembly
## An assembly grammar for Atom.
------------

* Supports Intel syntax and x86 instruction sets.
* Fully supports the 8051 family. (NEW) (Yay!)

![Yay, pretty colours!](https://raw.githubusercontent.com/oliverkeeble/atom-language-assembly/master/screenshot.png)

------------
### To Do List
* Other syntaxes
	* AT&T
	* Others?
* Other instruction sets
	* ARM
	* MIPS
	* Others?
* Lots of testing.
* More instructions.
* Even more instructions.
* Fix all the bugs.
* Fix even more bugs.

### Done
* Other instruction sets:
	* ATMEL

------------

## Example snippets

Feel free to add your own here!

```
'.source.assembly.asm.x86.intel':
    'Assign code origin':
        'prefix': 'ORG'
        'body': 'ORG ${1:40}H'
    'Assign locations':
        'prefix': 'EQU'
        'body': '$1 EQU $2'
    'Move instruction':
        'prefix': 'MOV'
        'body': 'MOV $1, $2'
    'Move to Accumulator':
        'prefix': 'MOVA'
        'body': 'MOV A, $1'
    'Add':
        'prefix': 'ADD'
        'body': 'ADD A, $1'
    'Subtract':
        'prefix': 'SUBB'
        'body': 'SUBB A, $1'
```
