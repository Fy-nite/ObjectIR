define a spec:
- for ObjectIR itself
 	- this will a description of the core concepts to represent programs !!**ABSTRACTLY**!! in ObjectIR.
- a spec for the binary IR (normative)
- a text ir spec for how it compiles to binary IR
- a runtime

and then a spec for how runtimes EXECUTE Binary IR.

We will define a specification of the following things (in seperate documents):
- ObjectIR Abstract Virtual Machine
	- Defines an abstract repesentation of the data used by the virtual machine (AST equvilant)
- ObjectIR Binary Format
	- Defines the real implementation of the abstract repesentation as a real binary format
- ObjectIR Text Language
	- Describes how language concepts associate with those in the the abstract virtual machine
	- Describes how high level features of the language are implemented in the abstract virtual machine
	- Describes how it compiles to the abstract repesentation
- ObjectIR Runtime
	- Describes how to implement a real virtual machine the is equivilent the abstract virtual machine
	- Describes how the real virutal machine should implement interperate the abstract repesentation
	- Additional note on how to implement the Binary Representation into the structure of the your program and how to associate them with the abstract repesentation


brtrue

inputs: stack value, label to jump on true

outputs: IP modification to jump to specificed label