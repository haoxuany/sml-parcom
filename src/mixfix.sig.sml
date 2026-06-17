
signature MIXFIX = sig
  type id
  type exp

  datatype token =
    TokenId of id
  | TokenExp of exp

  structure Parser : PARCOM
    where type token = token

  datatype syntax =
    SyntaxId of id
  | SyntaxHole

  (* Following Agda conventions: higher precedence binds more tightly *)
  type precedence = int

  datatype assoc =
    AssocLeft
  | AssocRight
  | AssocNone

  type rule =
  { syntax : syntax list 
  , precedence : precedence
  , assoc : assoc
  , construct : exp list -> exp
  }

  exception RuleError of rule * string

  val build : rule list -> exp Parser.t_memo
end
