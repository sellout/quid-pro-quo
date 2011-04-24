# Quid Pro Quo

A contract programming library for Common Lisp in the style of Eiffel’s Design by Contract ™.

## What is it all about?

One main goals of every program is reliability, that is, correctness and robustness. A program is correct if it performs according to its specification, it is robust if it handles situations that were not covered in the specification in a graceful manner. One way to prove the correctness of a program with respect to a (formal) specification is the Hoare calculus. Based on this formal method Bertrand Meyer developed a method of software engineering called Design by Contract ™.

The principal idea of contract programming is that a class and its clients have a contract with each other: The client must guarantee certain conditions before calling a method specialized on the class (the preconditions), the class guarantees certain properties after the call (the postconditions).  If the pre- and postconditions are included in a form that the compiler can check, then any violation of the contract between caller and class can be detected immediately.

## Support for Contract Programming in Programming Languages

The language that offers the best support for contract programming is [Eiffel](http://www.eiffel.com), designed by Bertrand Meyer. It is rather difficult to add support for contract programming to most other languages, but not so for Common Lisp: I [Matthias Hölzl] have written a package for Common Lisp that provides support for contract programming. It is still very new and not too well tested so you should expect some rough edges and changes in its future design. There is no larger program depending on this package available, only some silly test cases. Since I intend to use the Quid Pro Quo package for my own programs this should change in the not so distant future.

## Contract Programming in Common Lisp.

One of the outstanding features of the Eiffel language is that it supports a concept called contract programming. A comprehensive description is given in the following books

> Object Oriented Software Construction, 2nd ed.
> Bertrand Meyer
> Prentice Hall PTR, 1997
> ISBN 0-13-629155-4

> Eiffel: The Language, 2nd ed.
> Bertrand Meyer
> Prentice Hall PTR, 1992
> ISBN ???

but the key point of contract programming is that the relationship between a class and its clients is specified by a contract: There are certain conditions that the caller of a method specialized on a class has to fulfill so that the method can do its job (the preconditions) and the method guarantees certain things after its completion (the postconditions). Furthermore a class may have certain properties that are always true about that class; these properties are called invariants.

This file contains an implementation of contract programming for CLOS. Pre- and postconditions as well as invariants are specified by qualified methods of type `contract`; the usual before, after and around method combinations are available for these methods as well.

## Implementation Support

* ABCL – NO, because it’s not supported by Closer-MOP
* CLISP – ?
* ACL (both ansi & modern) – **YES**
* CCL – **YES**
* CMUCL – currently errors
* ECL – NO, `DEFINE-METHOD-COMBINATION` is broken
* LispWorks – currently errors
* SBCL – **YES**
* SCL – ?
