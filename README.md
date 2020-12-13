# haschemell

# Install

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

### Windows

WSL setup with any Linux distribution is a prefered way of development, however, this project can be build directly in the Windows environment.

Install the haskell tool stack: on Windows, you can download and install the [Windows 64-bit Installer](https://get.haskellstack.org/stable/windows-x86_64-installer.exe).

