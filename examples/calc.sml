
structure Calc = struct

structure M = MixFix (
  structure Id = StringOrdered
  structure Exp = struct
    type t = real
    fun id s =
      case Real.fromString s of
        SOME r => r
      | NONE => raise Fail (String.concat [ "Unknown value: " , s ])
  end
  val table_size = 256
  structure Stream = Stream
)

open M

val S = SyntaxId
val H = SyntaxHole

val ops : rule list =
  [ { syntax = [ H , S "+" , H ] , precedence = 6 , assoc = AssocLeft
    , construct = fn [ a , b ] => a + b | _ => raise Fail "+" }
  , { syntax = [ H , S "-" , H ] , precedence = 6 , assoc = AssocLeft
    , construct = fn [ a , b ] => a - b | _ => raise Fail "-" }
  , { syntax = [ H , S "*" , H ] , precedence = 7 , assoc = AssocLeft
    , construct = fn [ a , b ] => a * b | _ => raise Fail "*" }
  , { syntax = [ H , S "/" , H ] , precedence = 7 , assoc = AssocLeft
    , construct = fn [ a , b ] => a / b | _ => raise Fail "/" }
  , { syntax = [ S "-" , H ]     , precedence = 9 , assoc = AssocNone
    , construct = fn [ a ] => ~a | _ => raise Fail "neg" }
  , { syntax = [ S "(" , H , S ")" ] , precedence = 0 , assoc = AssocNone
    , construct = fn [ a ] => a | _ => raise Fail "()" }
  ]

val parser = build ops

fun showReal v =
  let val n = Real.floor v
  in if Real.== (Real.fromInt n , v)
     then Int.toString n
     else Real.toString v
  end

fun eval s =
  let
    val toks = String.tokens Char.isSpace s
    val results =
      List.mapPartial (fn ( v , s ) =>
        case Stream.front s of Stream.Nil => SOME v | _ => NONE)
      (Parser.parser parser (Stream.fromList (List.map TokenId toks)))
  in
    case results of
      nil => print "  parse error\n"
    | [ v ] => print (String.concat [ "  " , showReal v , "\n" ])
    | vs =>
        ( print (String.concat
            [ "  ambiguous (" , Int.toString (List.length vs) , " parses)\n" ])
        ; List.app (fn v => print (String.concat [ "    " , showReal v , "\n" ])) vs )
  end

fun repl () =
  ( print "> "
  ; case TextIO.inputLine TextIO.stdIn of
      NONE => print "\n"
    | SOME line =>
        let val s = String.substring ( line , 0 , String.size line - 1 )
        in if s = "" then repl ()
           else ( eval s handle Fail msg =>
             print (String.concat [ "  error: " , msg , "\n" ])
           ; repl () )
        end
  )

val () = ( print "calculator repl:\n" ; repl () )

end
