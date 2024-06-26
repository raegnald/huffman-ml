
(* Copyright (c) 2024 Ronaldo Gligan.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, version 3.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>. *)

open Printf

type sense = Left | Right    (* Invariant: { Left |-> 0, Right |-> 1} *)

type huffman_tree
  = Leaf of char
  | Node of huffman_tree * huffman_tree

type compressed_data =
  { tree       : huffman_tree
  ; dirs_count : int
  ; data       : Buffer.t }

let frequencies = Array.make 256 0

let rec huff_weight =
  function Leaf c -> let n = int_of_char c
                     in frequencies.(n) * n (* This isn't really a
                                               Huffman weight per se,
                                               but it works *)
         | Node (s, t) -> huff_weight s + huff_weight t

(* Priority queue for Huffman trees *)
module PQueue =
struct
  include
    Set.Make(struct
        type t = huffman_tree
        let compare s t =
          let intcmp = Int.compare (huff_weight s) (huff_weight t) in
          if intcmp != 0 then intcmp else
          if s = t then 0 else 1
      end)

  type order = Least | Greatest

  (** [take pq ord] returns the [ord]est ([Least] or [Greatest])
      element and the queue with that element removed *)
  let take pq ord =
    let f = match ord with Least -> min_elt | Greatest -> max_elt in
    let elt = f pq in
    elt, remove elt pq
end

let rec make_huff_tree pq =
  let card = PQueue.cardinal pq in
  if card < 1 then invalid_arg "make_huff_tree: queue is empty" else
  if card = 1 then
    PQueue.elements pq |> List.hd
  else
    (* Get the two subtrees with the least weights, then combine those
       toghether and add them back to the priority queue as one, then
       repeat *)
    let a, pq'  = PQueue.(take pq Least) in
    let b, pq'' = PQueue.(take pq' Least) in
    let elt = Node (a, b) in
    let pq''' = PQueue.add elt pq'' in
    make_huff_tree pq'''

let show_dir dir =
  let show_sense = function Left -> "0" | Right -> "1" in
  List.map show_sense dir |> List.fold_left (^) ""

let get_frequencies text =
begin
  assert (text != "");
  let leaves = ref PQueue.empty in

  let count_char c =
    let n = int_of_char c
    in frequencies.(n) <- succ frequencies.(n)
  and add_leaf i count =
    if count > 0 then
      leaves := PQueue.add (Leaf (char_of_int i)) !leaves
  in

  String.iter count_char text;
  Array.iteri add_leaf frequencies;
  !leaves
end

(** Converts a Huffman tree into a hash table of { char |-> bits } *)
let rec codewords_of_tree ?(curr_dir=[]) ~table =
  function Leaf c -> Hashtbl.add table c curr_dir
         | Node (s, t) -> codewords_of_tree ~curr_dir:(curr_dir@[Left]) ~table s;
                          codewords_of_tree ~curr_dir:(curr_dir@[Right]) ~table t

(** Converts a Huffman tree into a hash table of { bits |-> char } *)
let rec codewords_of_tree_rev ?(curr_dir=[]) ~table =
  function Leaf c -> Hashtbl.add table curr_dir c
         | Node (s, t) -> codewords_of_tree_rev ~curr_dir:(curr_dir@[Left]) ~table s;
                          codewords_of_tree_rev ~curr_dir:(curr_dir@[Right]) ~table t

(** Uses Graphviz notation *)
let rec print_huff_tree oc ?(curr_dir=[]) ?(sense=None) =
  let d = show_dir curr_dir in
  function Leaf c ->
            let c = if c = ' ' then "space" else Char.escaped c in
            fprintf oc "\"*%s*\" [color=green, label=\"%s\"]; %S [shape=box,color=blue];\n\"*%s*\" -> %S;\n"
              d d c d c
         | Node (s, t) ->
            fprintf oc "\"*%s*\" [label=\"%s\"]; \n\"*%s*\" -> \"*%s0*\";\n\"*%s*\" -> \"*%s1*\";\n"
              d (match dir with Some d -> show_dir [d] | None -> "root") d d d d;
            print_huff_tree oc ~curr_dir:(curr_dir@[Left]) ~sense:(Some Left) s;
            print_huff_tree oc ~curr_dir:(curr_dir@[Right]) ~sense:(Some Right) t

(* Compressing *)

let data_to_dirs codewords data =
  let dirs = Dynarray.create () in
  for i = 0 to String.length data - 1 do
    let dir = try Hashtbl.find codewords data.[i]
              with Not_found -> ksprintf failwith
                                  "data_to_dirs: dir not found character %C"
                                  data.[i]
    in
    Dynarray.append_list dirs dir
  done;
  dirs

let dirs_to_encoded_chars dirs =
  let encoded_chars = Dynarray.create ()
  and i = ref 0 in

  let padding_count = 8 - Int.rem (Dynarray.length dirs) 8 in
  (* Add padding `Rights` to make encoded_chars work *)
  Dynarray.append_array dirs (Array.make padding_count Right);

  while !i < Dynarray.length dirs do
    let n = ref 0 in
    for off = 7 downto 0 do
      let s = match Dynarray.get dirs !i with Left -> 0 | Right -> 1 in
      n := !n lor (s lsl off);
      incr i
    done;
    Dynarray.add_last encoded_chars (char_of_int !n)
  done;

  encoded_chars

let compress_data huff_tree data =
begin
  let codewords = Hashtbl.create 256 in
  codewords_of_tree huff_tree ~table:codewords;

  let dirs = data_to_dirs codewords data in
  let dirs_count = Dynarray.length dirs in

  let encoded_chars = dirs_to_encoded_chars dirs in (* NOTE: This mutates `dirs` adding padding [Right]s *)
  let encoded_chars_len = Dynarray.length encoded_chars in

  let buf = Buffer.create encoded_chars_len in
  for i = 0 to encoded_chars_len - 1 do
    Buffer.add_char buf (Dynarray.get encoded_chars i)
  done;

  { tree = huff_tree
  ; dirs_count
  ; data = buf }
end

(* End of compressing *)

(* Decompression *)

let encoded_chars_to_dirs encoded_chars dirs_count =
  let dirs = Dynarray.create ()
  and curr_dir = ref 0 in
  for i = 0 to Dynarray.length encoded_chars do
    let b = ref 7 in
    while !b >= 0 && !curr_dir < dirs_count do
      let sense =
        if ((int_of_char (Dynarray.get encoded_chars i) lsr !b) land 1) = 0
        then Left
        else Right
      in
      Dynarray.append_list dirs [sense];
      incr curr_dir;
      decr b
    done
  done;
  dirs

let decompress compressed_data =
  let buf = Buffer.create 1024 in (* Heuristic *)

  let encoded_chars = Dynarray.of_seq @@ Bytes.to_seq @@ Buffer.to_bytes compressed_data.data in
  let dirs = encoded_chars_to_dirs encoded_chars compressed_data.dirs_count in

  let codewords = Hashtbl.create 1024 in (* Heuristic *)
  codewords_of_tree_rev compressed_data.tree ~table:codewords;

  let curr_dir = ref [] in
  for i = 0 to compressed_data.dirs_count - 1 do
    curr_dir := !curr_dir@[Dynarray.get dirs i];
    match Hashtbl.find_opt codewords !curr_dir
    with Some c -> curr_dir := [];
                   Buffer.add_char buf c
       | None -> ()
  done;
  buf

(* End of decompression *)

(* Interface to the program *)

let usage () =
  eprintf {|
Usage: %s <option> <file>

Synopsis: Compress and decompress mainly text files through Huffman encoding.

Options:
  - g, graph       Generate a Graphviz dot SVG out of the Huffman tree of the given file
  - w, codewords   Display the corresponding codewords of a file and exit
  - c, compress    Compress the given file
  - d, decompress  Decompress the given file
|}
  Sys.argv.(0);
  exit 1

let () =
  if Array.length Sys.argv < 3 then (eprintf "Not enough arguments.\n"; usage ()) else
  let file = Sys.argv.(2) in
  match Sys.argv.(1)
  with "g" | "graph" ->
        begin
          let text = In_channel.(with_open_bin file input_all) in
          let freqs = get_frequencies text in
          let huff_tree = make_huff_tree freqs in
          let dot_file = file ^ ".dot" in
          let svg_file = file ^ ".svg" in
          let oc = Out_channel.open_bin dot_file in
          fprintf oc "digraph G {\n";
          print_huff_tree oc huff_tree;
          fprintf oc "}\n";
          flush oc;
          close_out oc;
          match ksprintf Unix.system "dot %s -Tsvg > %s && rm %s" dot_file svg_file dot_file
          with WEXITED 0 -> printf "Wrote SVG to %s\n" svg_file
             | _ -> failwith "Could not convert dot file into an svg. Perhaps you don't have 'dot' installed"
        end
     | "w" | "codewords" ->
        begin
          let text = In_channel.(with_open_bin file input_all) in
          let freqs = get_frequencies text in
          let huff_tree = make_huff_tree freqs in
          let codewords = Hashtbl.create 256 in
          codewords_of_tree huff_tree ~table:codewords;
          let codewords = Hashtbl.to_seq codewords in
          Seq.iter (fun (c, dir) -> printf " %5s : %s\n" (Char.escaped c) (show_dir dir)) codewords
        end
     | "c" | "compress" ->
        begin
          let text = In_channel.(with_open_bin file input_all) in
          let freqs = get_frequencies text in
          let huff_tree = make_huff_tree freqs in
          let compressed_filename = file ^ ".huff" in
          let compressed_data = compress_data huff_tree text in
          let oc = open_out compressed_filename in
          Marshal.to_channel oc compressed_data [];
          flush oc;
          let marshaled_len = Int64.to_float (Out_channel.length oc)
          and original_len = float (String.length text) in
          close_out oc;

          let saved_percentage = 100. *. (1. -. marshaled_len /. original_len) in
          printf "Compressed file saved to %s\nCompressed file is %f%% %s than the original.\n"
            compressed_filename
            saved_percentage
            (if saved_percentage > 0. then "smaller" else "larger")
        end
     | "d" | "decompress" ->
        begin
          let compressed_ic = open_in file in
          let compressed_data : compressed_data = Marshal.from_channel compressed_ic in
          let decompressed = decompress compressed_data in
          let decompressed_file = Filename.remove_extension file in
          let oc = open_out decompressed_file in
          Buffer.output_buffer oc decompressed;
          flush oc;
          close_out oc;
          printf "Decompressed to %s\n" decompressed_file
        end
     | _ -> eprintf "Unknown command.\n";
            usage ()
