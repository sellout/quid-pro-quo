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
* Allegro (both ansi & modern) – **YES**
* CLISP – it compiles, but the tests fail to compile
* Clozure – **YES**
* CMUCL – currently errors (22 pass, 7 fail)
* ECL – NO, `DEFINE-METHOD-COMBINATION` is broken
* LispWorks – currently errors
* SBCL – **YES**
* SCL – ?

## Usage

Preconditions (`:require`) and postconditions (`:ensure`) are added to functions similarly to `:before` and `:after` methods.

```common lisp
(defmethod put :require "the stack is not full" (item (stack stack))
  (declare (ignore item))
  (not (full stack)))

(defmethod put :ensure (item (stack stack))
  (and (not (empty stack))
       (eq (top-item stack) item)
       (= (count stack) (1+ (old (count stack))))))
```

This simple example illustrates a few things: an optional description of what is being required or ensured can be included between the method qualifier and the lambda list (this is because the docstring is not necessarily available), and the macro `old` is available in postconditions so that state from before the call can be compared to the state after the call.

Invariants are placed on classes.

```common lisp
(defclass stack ()
  ((capacity :initarg :capacity :reader capacity :type integer)
   (count :initform 0 :reader count :type integer)
   (top-item :reader top-item))
  (:metaclass contracted-class)
  (:invariants (lambda (instance)
                 "the count must be between 0 and the capacity"
                 (<= 0 (count instance) (capacity instance)))))
```

In order to have invariants on a class, the metaclass must be specified as `contracted-class`.

Invariants are added to classes explicitly with the `:invariants` option, which allows you to specify any number of predicates that take the instance as their only argument. When available (depending on the Lisp implementation), the documentation string for the function is used. If no documentation is available, we fall back to the body (in the case of a lambda) or the function name as the description.

Types are also checked as invariants. Most implementations check slot types little enough that it's possible for a bad value to end up there in some cases.

The description (for `:require` and `:ensure`, as well as invariants) is included in the report if a `contract-violation-error` is raised. The description is also added to the documentation for the class, function, or method as appropriate. Slot type declarations are also added to the class documentation.
