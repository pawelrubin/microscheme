# Micro Scheme Compiler

Micro Scheme language is a subset of the Scheme programming language. It compiles to the LLVM intermediate representation.

# Features
- arithmetic operations (`+`, `-`, `*`, `/`, `%`)
- boolean operations (`&&`, `||`, `>`, `<`, `>=`, `<=`, `=`, `!=`)
- conditional expressions
- variables
- user-defined functions
- simple input/output operations (`read`, `display`)

# Example

The code below is a recursive implementation of the factorial function written in the Micro Scheme language.

```lisp
(define (fact n)
    (if (= n 0)
        1
        (* (fact (- n 1)) n)))
```

It will be compiled to the LLVM IR below:

```llvm
define i32 @fact(i32 %n) {
entry:
  %0 = alloca i32 
  store i32 %n, i32* %0 
  %1 = load i32, i32* %0 
  %2 = icmp eq i32 %1, 0 
  br i1 %2, label %then, label %else
then:
  br label %merge 
else:
  %3 = load i32, i32* %0 
  %4 = sub i32 %3, 1 
  %5 = call i32 @fact(i32  %4)  
  %6 = load i32, i32* %0 
  %7 = mul i32  %5, %6 
  br label %merge 
merge:
  %8 = phi i32 [1, %then], [%7, %else] 
  ret i32 %8 
}
```

# Installation

## Requirements

- [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
- [LLVM 9](https://releases.llvm.org/9.0.0/docs/index.html)

### Linux

Install the haskell tool stack
```shell
$ curl -sSL https://get.haskellstack.org/ | sh
```
or:
```shell
$ wget -qO- https://get.haskellstack.org/ | sh
```

Install llvm
```shell
$ apt-get install llvm-9-dev
```

Alternatively, run the automatic installer
```shell
$ wget https://apt.llvm.org/llvm.sh
$ chmod +x llvm.sh
$ ./llvm.sh 9 # might require sudo
```

In case of failure install dependencies:

```shell
$ apt install wget lsb-release software-properties-common
```


### Windows

WSL setup with any Linux distribution is a prefered way of development, however, this project can be build directly in the Windows environment.

Install the haskell tool stack: on Windows, you can download and install the [Windows 64-bit Installer](https://get.haskellstack.org/stable/windows-x86_64-installer.exe).

