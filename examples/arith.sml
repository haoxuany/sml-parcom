(* E ::= E '+' E | E '*' E | '(' E ')' | number *)

structure CharParcom = Parcom (
  structure TokenStream = struct
    type token = char
    type stream = char Stream.stream

    datatype front = Nil | Cons of token * stream
    val front : stream -> front =
      fn s => case Stream.front s of
        Stream.Nil => Nil
      | Stream.Cons (h, t) => Cons (h, t)
  end
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

  (* star digit gives all possible splits of a digit sequence;
   * starLongest digit greedily takes the longest match. *)
  val starDigit : int list t_memo = memoize (star digit)
  val starDigitLongest : int list t_memo = memoize (starLongest digit)

  val () = let
    fun show xs =
      "[" ^ String.concatWith ", " (List.map Int.toString xs) ^ "]"
    fun run label p input =
      let
        val results = parser p (Stream.fromString input)
        val strs = List.map (fn (v, _) => show v) results
      in
        print (String.concat
          ["  ", label, " \"", input, "\" => [",
           String.concatWith ", " strs, "]\n"])
      end
  in
    print "\ngreedy digit parsing:\n";
    (* star digit returns all prefixes: [], [1], [1,2], [1,2,3] *)
    run "star digit" starDigit "123";
    (* starLongest digit returns only the longest: [1,2,3] *)
    run "starLongest digit" starDigitLongest "123"
  end
end
