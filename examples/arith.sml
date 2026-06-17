(* E ::= E '+' E | E '*' E | '(' E ')' | number *)

structure CharParcom = Parcom (
  type token = char
  val table_size = 256
  structure Stream = Stream
)

structure Arith =
struct
  open CharParcom

  fun char c = remove (fn c' => c = c')

  val digit : int t =
    terminal (fn c =>
      if Char.isDigit c
      then SOME (Char.ord c - Char.ord #"0")
      else NONE)

  val number : int t =
    map (List.foldl (fn (d, acc) => acc * 10 + d) 0) (plus digit)

  val expr : int t_memo = fix (fn expr => either [
    bind expr (fn l =>
    bind (char #"+") (fn () =>
    bind expr (fn r =>
    return (l + r)))),

    bind expr (fn l =>
    bind (char #"*") (fn () =>
    bind expr (fn r =>
    return (l * r)))),

    bind (char #"(") (fn () =>
    bind expr (fn e =>
    bind (char #")") (fn () =>
    return e))),

    number
  ])

  fun eval s =
    let
      val results = parser expr (Stream.fromString s)
    in
      List.mapPartial (fn (v, s) =>
        case Stream.front s of Stream.Nil => SOME v | _ => NONE) results
    end

  val () = let
    fun run input =
      let
        val results = eval input
        val strs = List.map Int.toString results
      in
        print (String.concat
          ["  \"", input, "\" => [",
           String.concatWith ", " strs, "]\n"])
      end
  in
    print "arith.sml:\n";
    (* Two groupings: 2+(3*4)=14, (2+3)*4=20 *)
    run "2+3*4";
    (* Two parse trees, same value: (10+22)+34=66, 10+(22+34)=66 *)
    run "10+22+34";
    (* Unambiguous due to parens *)
    run "(1+2)*3"
  end
end
