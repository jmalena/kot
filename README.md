# kot

Kot is a minimalist imperative language and a compiler of the same name.

## Usage

To display available command-line options of the Kot compiler:

```bash
kotc --help
```

To compile a Kot program from a source file:

```bash
kotc path/to/file.kot
```

This will outputs [object file](https://en.wikipedia.org/wiki/Object_file) `a.o` and [LLVM IR](https://llvm.org/docs/LangRef.html) `a.ll` of the compiled source file.

<!--
## Documentation

- [Documentation (English)](docs/en/index.md)
-->

## Building From Source

**Requirements:** GHC 8 or later, LLVM 9

### Using Nix

> [!NOTE]
> On `aarch64-darwin` (Apple Silicon), a lot of libraries must be built from source because prebuilt binary for `LLVM 9` is not available for this platform.

Build with:

```bash
nix build
```

### Using Homebrew

Install LLVM 9:

```bash
brew install llvm-hs/llvm/llvm-9
```

Ensure you are using GHC 8 or later and build the project:

```bash
cabal build
```

## Contributing

Contributions, ideas, and bug reports are welcome! Feel free to open issues or submit pull requests.

## License

Kot language and compiler are licensed under the MIT License.
