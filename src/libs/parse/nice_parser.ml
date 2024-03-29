(* Taken from https://github.com/smolkaj/nice-parser *)
(* 
MIT License

Copyright (c) 2017-2019 Steffen Smolka

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

let reraise exn =
  Printexc.(raise_with_backtrace exn (get_raw_backtrace ()))

module type RAW_PARSER = sig
  type token
  type result
  exception LexError of string
  exception ParseError
  val next_token : Lexing.lexbuf -> token
  val indent : Lexing.lexbuf -> token
  val parse : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> result
end

module type NICE_PARSER = sig
  type token
  type result
  exception LexError of { msg: string; loc: Location.t }
  exception ParseError of { token: token; loc: Location.t }
  val pp_exceptions : unit -> unit
  val parse_string : ?pos:Lexing.position -> string -> result
  val parse_chan : ?pos:Lexing.position -> in_channel -> result
  val parse_file : string -> result
end

module Make (P : RAW_PARSER) : NICE_PARSER with 
  type token = P.token and
  type result = P.result
= struct
  type token = P.token
  type result = P.result
  exception LexError of { msg: string; loc: Location.t }
  exception ParseError of { token: token; loc:Location.t }

  let pp_exceptions () = begin
    Location.register_error_of_exn (function
      | LexError {msg; loc} ->
        Some (Location.error ~loc msg)
      | ParseError {loc; _} ->
        Some (Location.error ~loc "[parser] unexpected token")
      | _ ->
        None
    );
    Printexc.register_printer (function exn ->
      try
        ignore (Format.flush_str_formatter ());
        Location.report_exception Format.str_formatter exn;
        Some (Format.flush_str_formatter ());
      with _ ->
        None
    );
  end

  let curr_token : token option ref =
    ref None

  let next_token lexbuf =
    let token = P.indent lexbuf in
    curr_token := Some token;
    token

  let parse ?(file="") lexbuf =
    Location.input_name := file;
    Location.input_lexbuf := Some lexbuf;
    try 
      P.parse next_token lexbuf
    with
    | P.LexError msg ->
      reraise (LexError { msg; loc = Location.curr lexbuf })
    | P.ParseError ->
      let[@warning "-8"] (Some token) = !curr_token in
      reraise (ParseError { token; loc = Location.curr lexbuf })

  let parse_string ?(pos : Lexing.position option) s =
    match pos with
    | None -> parse (Lexing.from_string s)
    | Some ({pos_fname=file; _} as p) ->
      parse ~file Lexing.{(from_string s) with lex_start_p=p; lex_curr_p=p}

  let parse_chan ?(pos : Lexing.position option) chan =
    match pos with
    | None -> parse (Lexing.from_channel chan)
    | Some ({pos_fname=file; _} as p) ->
      parse ~file Lexing.{(from_channel chan) with lex_start_p=p; lex_curr_p=p}

  let parse_file file =
    Stdio.In_channel.with_file file ~f:(fun chan ->
      let lexbuf = Lexing.from_channel chan in
      Location.init lexbuf file;
      parse ~file lexbuf
    )

end