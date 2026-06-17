
functor MixFix (
  structure Id : ORDERED
  structure Exp : sig
    type t
    val id : Id.t -> t
  end

  val table_size : int
  structure Stream : sig
    type 'a stream
    datatype 'a front = Nil | Cons of 'a * 'a stream
    val front : 'a stream -> 'a front
  end
) : MIXFIX = struct

  datatype token =
    TokenId of Id.t
  | TokenExp of Exp.t
  
  structure Parser = Parcom (
    type token = token
    val table_size = table_size
    structure Stream = Stream
  )

  structure P = Parser

  datatype syntax =
    SyntaxId of Id.t
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
  , construct : Exp.t list -> Exp.t
  }

  datatype fixity =
    FixityInfix
  | FixityPrefix
  | FixityPostfix
  | FixityNonfix

  datatype form =
    (* infix , non associative, ex. (a == b) == c should not parse *)
    FormNonAssocInfix 
    (* right associative infix or prefix, ex. a :: (b :: tail), test (test x) *)
  | FormRightRecursive 
    (* left associative infix or postfix, ex. (1 + 2) + 3, int list list *)
  | FormLeftRecursive
    (* neither infix, prefix, nor postfix. Basically wrapped. ex. [ 1 ] *)
  | FormNonfix

  exception RuleError of rule * string

  structure DictPrec = SplayDict(structure Key = IntOrdered)
  structure Set = SplaySet(structure Elem = Id)

  type rule_compute =
  { syntax : syntax list 
  , precedence : precedence
  , assoc : assoc
  , construct : Exp.t list -> Exp.t
  , fixity : fixity
  , inner : syntax list
  , form : form
  }
  
  fun build (rules : rule list) : Exp.t P.t_memo =
    let
      val rules =
        List.map
        (fn (rule as { syntax , precedence , assoc , construct }) =>
          let
            val ( form , inner , fixity ) =
              case syntax of
                nil  => raise RuleError ( rule , "Empty syntax" )
              | SyntaxId _ :: _ =>
                  (case List.rev syntax of
                     SyntaxId _ :: _ => ( FormNonfix , syntax , FixityNonfix )
                   | SyntaxHole :: tail => ( FormRightRecursive , List.rev tail ,
                   FixityPrefix )
                   | nil => raise Fail "Impossible"
                  )
              | SyntaxHole :: tail =>
                  (case List.rev tail of
                     ( SyntaxId _ ) :: _ => ( FormLeftRecursive , tail , FixityPostfix )
                   | SyntaxHole :: tail =>
                       (* Infix case, we need to check *)
                       let
                         val tail = List.rev tail
                       in
                         ( case assoc of
                           AssocLeft => FormLeftRecursive
                         | AssocRight => FormRightRecursive
                         | AssocNone => FormNonAssocInfix
                         , tail , FixityInfix )
                       end
                  | nil => raise RuleError ( rule , "Single hole" )
                  )
          in
            { syntax = syntax , precedence = precedence
            , assoc = assoc , construct = construct
            , form = form , inner = inner , fixity = fixity }
          end
        )
        rules

      val ( nonfixRules , fixityRules ) =
        List.partition
        (fn { fixity , ... } => fixity = FixityNonfix) 
        rules

      (* We only care about precedences for fixity (infix/prefix/postfix),
      * because nonfix are always enclosed in two ids, so associativity issues
      * never arise. *)
      val precedences : rule_compute list DictPrec.dict =
        List.foldl
        (fn ( rule as { precedence , ... } , dict ) =>
          DictPrec.insertMerge dict precedence [ rule ] (fn l => rule :: l))
        DictPrec.empty fixityRules

      fun parseInner (expParse : Exp.t P.t)
        (syntax : syntax list) : Exp.t list P.t =
        case syntax of
          nil => P.return nil
        | SyntaxId id :: tail =>
            P.bind (P.remove (fn (TokenId id') => Id.eq (id', id)
                               | _ => false))
              (fn () => parseInner expParse tail)
        | SyntaxHole :: tail =>
            P.bind expParse
              (fn e => P.map (fn tail => e :: tail) (parseInner expParse tail))

      val keywords : Set.set =
        List.foldl (fn ({ inner , ... } , set) =>
          List.foldl (fn ( v , set ) =>
            case v of
              SyntaxId id => Set.insert set id
            | SyntaxHole => set
          ) set inner
        ) Set.empty rules


      val ( parseTop , parseAtom ) = P.fix2 (fn ( parseTop , parseAtom ) =>
        let
          (* For parsing atom (stuff that don't have associativity issues) ,
          * it is either a single (already abstract) expression, or
          * a nonfix expression (which is already enclosed, and hence do not
          * need to be parenthesized.) *)

          val parseAbstract = P.terminal
            (fn s =>
              case s of
                TokenId id => 
                  (* If we see an Id, there are 2 cases:
                  * - if it is not a keyword/id appearing in the syntax of rules,
                  * then it is basically an id/variable name.
                  * - if it is a keyword/id appearing in the syntax of rules,
                  * we take the Agda decision here, and treat it as a keyword,
                  * so it cannot be standalone. *)
                  (case Set.member keywords id of
                    true => NONE
                  | false => SOME (Exp.id id)
                  )
              | TokenExp exp => SOME exp
            )

          val parseNonfix = 
            List.map (fn { construct , syntax , ... } =>
              P.map construct (parseInner parseTop syntax)
            )
            nonfixRules

          (* Due to fixed point backpatching, this gets returned but not used
          * directly. *)
          val parseAtom' = P.either ( parseAbstract :: parseNonfix )

          (* For parsing fixity, we need to be more involved. *)

          (* Everything else left is complicated since it involves
          * associativity, and we have to construct this by precedence
          * level by level. Any expression at a lower precedence, can
          * involve parsing subexpressions at a higher precedence, but not
          * vice versa. *)
          
          val parseTop' = 
            let
              fun parseLevel 
                (parseTop : Exp.t Parser.t) 
                (parseHigher : Exp.t Parser.t)
                (rule : rule_compute list) : Exp.t Parser.t =
                let
                  (* We already handled the nonfix case in atom, so this
                  * boils down to 3 cases: NonAssocInfix, RightRecursive,
                  * LeftRecursive. *)

                  val ( nonassocInfix , rightRecursive , leftRecursive ) =
                    List.foldr
                    (fn ( rule as { form , ... } , 
                      ( nonassocInfix , rightRecursive , leftRecursive ) ) =>
                      case form of
                        FormNonfix => raise Fail "Impossible"
                      | FormNonAssocInfix => 
                          ( rule :: nonassocInfix 
                          , rightRecursive 
                          , leftRecursive 
                          )
                      | FormRightRecursive =>
                          ( nonassocInfix 
                          , rule :: rightRecursive 
                          , leftRecursive 
                          )
                      | FormLeftRecursive =>
                          ( nonassocInfix 
                          , rightRecursive 
                          , rule :: leftRecursive 
                          )
                    ) ( nil , nil , nil ) rule

                  (* NonAssocInfix can be parsed as "_inner_", so we basically
                  * just construct all inner alternatives. *)
                  val parseNonAssocInfix =
                    case nonassocInfix of
                      nil => NONE
                    | _ => 
                        let
                          val parseInner : 
                            (Exp.t list * (Exp.t list -> Exp.t)) Parser.t = 
                            P.either 
                            (List.map (fn { inner , construct , ... } => 
                              P.bind (parseInner parseTop inner) (fn v =>
                              P.return ( v , construct )))
                            nonassocInfix)
                        in
                          SOME (
                          P.bind parseHigher (fn left =>
                          P.bind parseInner (fn ( middle , construct ) =>
                          P.bind parseHigher (fn right =>
                          P.return (construct (left :: (middle @ [ right ])))))))
                        end

                  (* RightRecursive can be parsed as "prefix_" *)
                  val parseRightRecursive =
                    case rightRecursive of
                      nil => NONE
                    | _ =>
                        let
                          val parsePrefix =
                            List.map
                            (fn { inner , fixity , construct , ... } =>
                              case fixity of
                                FixityPrefix =>
                                  P.bind (parseInner parseTop inner) (fn v =>
                                  P.return ( v , construct ))
                              | FixityInfix => 
                                  P.bind parseHigher (fn left =>
                                  P.bind (parseInner parseTop inner) (fn middle =>
                                  P.return ( left :: middle , construct ))
                                  )
                              | _ => raise Fail "Impossible"
                            )
                            rightRecursive

                          val parsePrefix = P.either parsePrefix
                        in
                          SOME (P.fix (fn parseRightRecursive =>
                            P.bind parsePrefix (fn ( prefix , construct ) =>
                            P.bind 
                            (P.either [ parseHigher , parseRightRecursive ])
                            (fn exp => 
                              P.return (construct (prefix @ [ exp ]) )
                            )
                          )))
                        end

                  (* LeftRecursive can be parsed as "_postfix" *)
                  val parseLeftRecursive =
                    case leftRecursive of
                      nil => NONE
                    | _ =>
                        let
                          val parsePostfix =
                            List.map
                            (fn { inner , fixity , construct , ... } =>
                              case fixity of
                                FixityPostfix =>
                                  P.map 
                                  (fn v => ( v , construct ))
                                  (parseInner parseTop inner)
                              | FixityInfix => 
                                  P.bind 
                                  (parseInner parseTop inner) (fn middle =>
                                  
                                  P.bind parseHigher (fn right =>

                                  P.return ( middle @ [ right ] , construct )
                                  ))
                              | _ => raise Fail "Impossible"
                            )
                            leftRecursive

                          val parsePostfix = P.either parsePostfix
                        in
                          SOME (P.fix (fn parseLeftRecursive =>
                            P.bind 
                            (P.either [ parseLeftRecursive , parseHigher ])
                            (fn left =>
                            
                            P.bind parsePostfix (fn ( right , construct ) =>
                            
                            P.return (construct (left :: right))
                            ))
                          ))
                        end
                      
                  val parseFixity = 
                    List.mapPartial (fn x => x)
                    [ parseNonAssocInfix 
                    , parseRightRecursive 
                    , parseLeftRecursive ]
                in
                  P.either (parseHigher :: parseFixity)
                end
            in 
              DictPrec.foldr 
              (fn ( _ , rules , parseHigher ) =>
                parseLevel parseTop parseHigher rules)
              parseAtom precedences
            end
            

        in
          ( parseTop' , parseAtom' )
        end)
    in
      parseTop
    end

    type id = Id.t
    type exp = Exp.t
end

