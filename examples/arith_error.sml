(* E ::= E '+' E | E '*' E | '(' E ')' | number *)
(* Same as arith.sml, but uses ParcomError for error reporting. *)

structure ArithParseError = struct
  datatype t =
    UnexpectedEof
  | UnexpectedChar of char
  | ExpectedChar of char

  val unexpected_eof = UnexpectedEof
end

structure CharParcomError = ParcomError (
  structure TokenStream = struct
    type token = char
    type stream = char Stream.stream

    datatype front = Nil | Cons of token * stream
    val front : stream -> front =
      fn s => case Stream.front s of
        Stream.Nil => Nil
      | Stream.Cons (h, t) => Cons (h, t)
  end

  structure ParseError = ArithParseError
)

structure ArithError =
struct
  open CharParcomError

  fun errorToString e =
    case e of
      ArithParseError.UnexpectedEof => "unexpected end of input"
    | ArithParseError.UnexpectedChar c =>
        "unexpected '" ^ String.str c ^ "'"
    | ArithParseError.ExpectedChar c =>
        "expected '" ^ String.str c ^ "'"

  fun char c : unit t =
    terminal (fn c' =>
      if c = c' then ParseSuccess ()
      else ParseFailure (ArithParseError.ExpectedChar c))

  val digit : int t =
    terminal (fn c =>
      if Char.isDigit c
      then ParseSuccess (Char.ord c - Char.ord #"0")
      else ParseFailure (ArithParseError.UnexpectedChar c))

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
    case parser expr (Stream.fromString s) of
      ResultSuccess results =>
        List.mapPartial (fn (v, s) =>
          case Stream.front s of Stream.Nil => SOME v | _ => NONE) results
    | ResultFailure _ => nil

  fun showResult label input =
    case parser expr (Stream.fromString input) of
      ResultSuccess results =>
        print (String.concat
          ["  ", label, " \"", input, "\" => success (",
           Int.toString (List.length results), "): [",
           String.concatWith ", "
             (List.map (fn (v, _) => Int.toString v) results),
           "]\n"])
    | ResultFailure errors =>
        print (String.concat
          ["  ", label, " \"", input, "\" => errors: [",
           String.concatWith ", "
             (List.map (fn (e, _) => errorToString e) errors),
           "]\n"])

  val () = let
    fun run input =
      let
        val strs = List.map Int.toString (eval input)
      in
        print (String.concat
          ["  \"", input, "\" => [",
           String.concatWith ", " strs, "]\n"])
      end
  in
    print "arith_error.sml:\n";
    run "2+3*4";
    run "10+22+34";
    run "(1+2)*3"
  end

  val () = (
    print "\nerror reporting:\n";
    showResult "either" "2+3";
    showResult "either" "2+";
    showResult "either" "+3";
    showResult "either" "(2+3";
    showResult "either" ""
  )

  (* Non-recursive: either with no left recursion *)
  val nonrec : int t_memo = memoize (either [
    bind (char #"(") (fn () =>
    bind number (fn n =>
    bind (char #")") (fn () =>
    return n))),

    number
  ])

  fun showNonrec input =
    case parser nonrec (Stream.fromString input) of
      ResultSuccess results =>
        print (String.concat
          ["  nonrec \"", input, "\" => success (",
           Int.toString (List.length results), "): [",
           String.concatWith ", "
             (List.map (fn (v, _) => Int.toString v) results),
           "]\n"])
    | ResultFailure errors =>
        print (String.concat
          ["  nonrec \"", input, "\" => errors: [",
           String.concatWith ", "
             (List.map (fn (e, _) => errorToString e) errors),
           "]\n"])

  val () = (
    print "\nnon-recursive either:\n";
    showNonrec "42";
    showNonrec "(42)";
    showNonrec "(42";
    showNonrec "abc";
    showNonrec ""
  )
end
