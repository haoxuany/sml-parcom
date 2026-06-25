
structure MixfixExample = struct

datatype expr =
  Atom of string
| App of expr * expr
| OpApp of string * expr list

structure M = MixFix (
  structure Id = StringOrdered
  structure Exp = struct
    type t = expr
    val id = Atom
  end
  structure Stream = Stream
)

open M

fun rule ( name , parts , prec , assoc ) : rule =
  { syntax = parts
  , precedence = prec
  , assoc = assoc
  , construct = fn args => OpApp ( name , args )
  }

fun exprToString e =
  case e of
    Atom s => s
  | App ( f , x ) =>
      String.concat [ "(" , exprToString f , " " , exprToString x , ")" ]
  | OpApp ( name , args ) =>
      String.concat [ "(" , name , " " ,
        String.concatWith " " (List.map exprToString args) , ")" ]

fun run ops s =
  let
    val parser = build ops
    val toks = String.tokens Char.isSpace s
    val results =
      List.mapPartial (fn ( e , s ) =>
        case Stream.front s of Stream.Nil => SOME e | _ => NONE)
      (Parser.parser parser (Stream.fromList (List.map TokenId toks)))
  in
    case results of
      nil  => print (String.concat [ "  No parse for: " , s , "\n" ])
    | [ e ] => print (String.concat [ "  " , s , "  ==>  " , exprToString e , "\n" ])
    | es  => (print (String.concat [ "  AMBIGUOUS (" , Int.toString (List.length es) ,
                     " parses) for: " , s , "\n" ]);
              List.app (fn e => print (String.concat [ "    " , exprToString e , "\n" ])) es)
  end

val S = SyntaxId
val H = SyntaxHole

fun appRule prec : rule =
  { syntax = [ H , H ]
  , precedence = prec
  , assoc = AssocLeft
  , construct = fn [ f , x ] => App ( f , x ) | _ => raise Fail "App"
  }

val () =
let
  val arith = List.map rule [
    ( "_+_" , [ H , S "+" , H ] , 6 , AssocLeft ) ,
    ( "_*_" , [ H , S "*" , H ] , 7 , AssocLeft ) ,
    ( "-_"  , [ S "-" , H ]     , 8 , AssocNone ) ]
  val exp' = List.map rule [
    ( "_+_" , [ H , S "+" , H ] , 6 , AssocLeft ) ,
    ( "_^_" , [ H , S "^" , H ] , 8 , AssocRight ) ]
  val ite = List.map rule [
    ( "_+_" , [ H , S "+" , H ] , 6 , AssocLeft ) ,
    ( "if_then_else_" ,
      [ S "if" , H , S "then" , H , S "else" , H ] , 1 , AssocRight ) ]
  val post = List.map rule [
    ( "_+_" , [ H , S "+" , H ] , 6 , AssocLeft ) ,
    ( "_!"  , [ H , S "!" ]     , 9 , AssocNone ) ]
  val closed = List.map rule [
    ( "_+_" , [ H , S "+" , H ]     , 6 , AssocLeft ) ,
    ( "[_]" , [ S "[" , H , S "]" ] , 0 , AssocNone ) ]
  val app = appRule 0 :: List.map rule [
    ( "_+_" , [ H , S "+" , H ] , 6 , AssocLeft ) ,
    ( "_*_" , [ H , S "*" , H ] , 7 , AssocLeft ) ,
    ( "-_"  , [ S "-" , H ]     , 8 , AssocNone ) ]
  val eq = List.map rule [
    ( "_==_" , [ H , S "==" , H ] , 4 , AssocNone ) ,
    ( "_+_"  , [ H , S "+"  , H ] , 6 , AssocLeft ) ]
  val tern = List.map rule [
    ( "_+_"   , [ H , S "+" , H ]             , 6 , AssocLeft ) ,
    ( "_?_:_" , [ H , S "?" , H , S ":" , H ] , 2 , AssocRight ) ]
  val multi = List.map rule [
    ( "_+_+_" , [ H , S "+" , H , S "+" , H ] , 6 , AssocLeft ) ,
    ( "_*_*_" , [ H , S "*" , H , S "*" , H ] , 7 , AssocLeft ) ]
  val ambig = List.map rule [
    ( "_+_"   , [ H , S "+" , H ]             , 6 , AssocLeft ) ,
    ( "_+_+_" , [ H , S "+" , H , S "+" , H ] , 6 , AssocLeft ) ]
in
  print "mixfix.sml:\n";

  run arith "a + b"; run arith "a + b + c";
  run arith "a + b * c"; run arith "a * b + c";
  run arith "- a + b"; run arith "- - a";

  run exp' "a ^ b ^ c"; run exp' "a + b ^ c";

  run ite "if a then b else c";
  run ite "if a then b + c else d";
  run ite "if a then b else if c then d else e";

  run post "a !"; run post "a ! + b"; run post "a + b !";

  run closed "[ a ]"; run closed "[ a + b ]"; run closed "[ a ] + [ b ]";

  run app "f x"; run app "f x y";
  run app "f x + a y"; run app "f x * a y";
  run app "- f x"; run app "f - x";

  run eq "a == b"; run eq "a + b == c"; run eq "a == b == c";

  run tern "a ? b : c"; run tern "a ? b + c : d";
  run tern "a ? b : c ? d : e";

  run multi "a + b + c"; run multi "a * b * c";
  run multi "a + b + c * d * e";
  run multi "a * b * c + d + e";

  run ambig "a + b";
  run ambig "a + b + c";
  run ambig "a + b + c + d"
end

end
