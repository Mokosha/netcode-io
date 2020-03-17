# netcode-io
Haskell bindings to the
[netcode.io](https://github.com/networkprotocol/netcode.io) library

## Development

These bindings were developed using `stack` in the "usual" way. We expect
that anyone building from source to use the same workflow.

### Windows

The cabal file specifies that the `sodium` library is required for building
this package. Unfortunately, this is a bit tricky on Windows since there's no
native package manager. To use this with Windows, we recommend using
[`MSYS2`](https://www.msys2.org/). `msys2.exe` has a built-in shell for using
the `gcc` toolchain on Windows. This toolchain is compatible with the one `ghc`
uses. Within this environment, you can install `sodium` using the `pacman`
package manager:

```
$ pacman -S mingw64/mingw-w64-x86_64-libsodium
```

Once installed, you can add the following lines to your global `config.yaml`:

```
extra-include-dirs:
  - D:\msys64\mingw64\include

extra-lib-dirs:
  - D:\msys64\mingw64\lib
```

Your `config.yaml` is located at your `%STACK_ROOT%` directory:

```
C:\> stack path --stack-root
C:\path\to\stack\root
```

### OS X

On MacOS, we can install `sodium` simply by using homebrew:

```
$ brew install libsodium
```