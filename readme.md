
# Huffman (de)compression implemented in OCaml in a single file

You are very much advised to read the
[Wikipedia](https://en.wikipedia.org/wiki/Huffman_coding) article
about Huffman coding.

## Instalation and usage

This program can only be compiled on UNIX-like systems, although you
are free to modify it to run on other, possibly non-free, operating
systems. The dependencies are:

- `ocaml >= 5.2` (because `huffman.ml` uses the `Dynarray` module),
- `dune` (I use `3.15.2`),
- `unix` (is a dependency of the OCaml compiler, as far as I know),
- optionally `dot`, one of [Graphviz](https://graphviz.org/)'s
  renderers, to see the Huffman tree as an SVG (see
  <examples/romeo-and-juliet.txt.svg> for an example).

If you have never used OPAM or OCaml, start by [downloading
OPAM](https://opam.ocaml.org/doc/Install.html), then run

```sh
$ opam init
...
$ opam switch create 5.2.0   # or a newer version, see `opam switch list-available`
...
$ opam install dune
...
```

When you have a functional environment, you can compile and add to
your path with

```sh
$ dune build --profile=release
$ dune install
$ huffman
Not enough arguments.

Usage: huffman <option> <file>

Synopsis: Compress and decompress mainly text files through Huffman encoding.

Options:
  - g, graph       Generate a Graphviz dot SVG out of the Huffman tree of the given file
  - w, codewords   Display the corresponding codewords of a file and exit
  - c, compress    Compress the given file
  - d, decompress  Decompress the given file
```

## Examples

We are going to compress and decompress . From the root of this project:

```sh
$ cd examples/

$ huffman compress romeo-and-juliet.txt
Compression ratio: 0.626529. Good.

$ mv romeo-and-juliet.txt romeo-and-juliet.txt.bak

$ huffman decompress romeo-and-juliet.txt.huff
Decompressed to romeo-and-juliet.txt

$ cmp romeo-and-juliet.txt romeo-and-juliet.txt.bak
$ echo $?
0
$ # zero means that the files have the exact same content (ie. lossless compression)
```
