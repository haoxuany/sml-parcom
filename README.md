# sml-parcom : Johnson's Memoized CPS parser combinator and Mixfix Parser

This is a parser combinator library that, in essence, implements (Johnson 1995).

> Johnson, M. (1995). *Memoization in Top-Down Parsing*. Computational Linguistics, 21(3), 405–417.
> [arXiv:cmp-lg/9504016](https://arxiv.org/abs/cmp-lg/9504016)

A direct result of (Johnson 1995) is that left factoring is no longer necessary.
One consequence is that all recursive definitions of the grammar must be memoized
for the algorithm to terminate. As such, in this implementation, the signature 
is modified to ensure that memoization must be run before running.

This library supports SML/NJ and mlton/MLKit. There are two libraries provided.

## Usage

### Parser Combinator

The compilation file parcom.{cm/mlb} provides a functor that satisfies this signature:

```
functor Parcom (
  type token
  val table_size : int
  structure Stream : sig
    type 'a stream
    datatype 'a front = Nil | Cons of 'a * 'a stream
    val front : 'a stream -> 'a front
  end
) :> PARCOM 
  where type token = token
  and type 'a stream = 'a Stream.stream
```

where the `PARCOM` signature is:
```
signature PARCOM = sig
  type token
  type 'a stream
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

  val terminal : (token -> 'a option) -> 'a t
  (* When multiple tokens need to be consumed (lookahead), you need to provide 
  * an accurate number of how many tokens are consumed. This is necessary
  * because of memoization in Johnson. *)
  val terminals : (token stream ->  ('a * int * token stream) option) -> 'a t
  (* Provides () if true, no parse if false *)
  val remove : (token -> bool) -> unit t

  val either : ('a t list) -> 'a t 
  val epsilon : 'a -> 'a t
  val optional : 'a t -> 'a option t
  val star : 'a t -> 'a list t
  val plus : 'a t -> 'a list t

  (* prefer is similar to either, but it short circuits to the first combinator
  * that produces any parse, and ignores the rest. *)
  val prefer : ('a t list) -> 'a t
  (* These use prefer over either, aka they will always give you only the
  * longest parse and ignore the rest. *)
  val optionalLongest : 'a t -> 'a option t
  val starLongest : 'a t -> 'a list t
  val plusLongest : 'a t -> 'a list t

  type 'a t_memo

  val memoize : 'a t -> 'a t_memo

  (* These are for manual backpatching *)
  (* This doesn't really forget, it just relaxes the type for backpatching. *)
  val forget : 'a t_memo -> 'a t

  type 'a t_dummy
  val dummy : unit -> 'a t_dummy
  val set : 'a t_dummy -> 'a t_memo -> unit
  val deref : 'a t_dummy -> 'a t

  val fix : ('a t -> 'a t) -> 'a t_memo
  val fix2 : ('a t * 'b t -> 'a t * 'b t) -> 'a t_memo * 'b t_memo
  val fix3 : ('a t * 'b t * 'c t -> 'a t * 'b t * 'c t)
    -> 'a t_memo * 'b t_memo * 'c t_memo
  val fix4 : ('a t * 'b t * 'c t * 'd t -> 'a t * 'b t * 'c t * 'd t)
    -> 'a t_memo * 'b t_memo * 'c t_memo * 'd t_memo
  val fix5 : ('a t * 'b t * 'c t * 'd t * 'e t
    -> 'a t * 'b t * 'c t * 'd t * 'e t)
    -> 'a t_memo * 'b t_memo * 'c t_memo * 'd t_memo * 'e t_memo
  (* If you need more than 5 fixed points, you will have to backpatch with
  * memoize yourself. *)

  val parser : 'a t_memo -> token stream -> ('a * token stream) list
end
```

The signature is mostly self-explanatory. Given `structure Parser : PARCOM`, a `'a Parser.t` is 
a parser that parses and returns results of type `'a`.
`bind` basically parses in sequence (aka, `seq` in the original paper).

The functor takes in a `table_size`, which is the initial size of a hash table used internally for memoization.
An appropriate size here is dependent on how large you expect the average stream to be.

Note that as suggested in the function signature of `parser`, it returns all possible parses,
along with the rest of the stream, for ambiguous grammars. The exception is with the usage of
`prefer` (which does not exist in the Johnson paper): only the first combinator that produces any
number of parses will be used. 
This is useful for, say, pulling the longest list of items 
(ex. all digits of an integer rather than all prefixes of an integer),
to cut down the number of ambiguous results.

An example of usage can be found in [examples/arith.sml](examples/arith.sml).

### Backpatching

A consequence of (Johnson 1995) is that all recursive definitions must be memoized.
The helpers `fix` help you perform this form of backpatching for most cases. For instance, for parsing:
```
E = "1" | E "+" E
```
Since the rules of `E` depend on parsing `E` itself, this involves using the backpatching `fix` function (which by itself already memoizes for you), ex:
```
  val parseExp : int t_memo = fix (fn parseExp => either [
   bind (char #"1") (fn () =>
   return 1),

    bind parseExp (fn l =>
    bind (char #"+") (fn () =>
    bind parseExp (fn r =>
    return (l + r))))
  ])
```

This applies to mutual fixed points as well, and hence the functions `fix2`, `fix3`, etc. In the case where you have more (or unknown number of) mutual fixed points, you can implement backpatching through, for example:
```
  fun fix2 (f : 'a t * 'b t -> 'a t * 'b t) : 'a t_memo * 'b t_memo =
    let
      val v1 = dummy ()
      val v2 = dummy ()
      val (r1, r2) = f (deref v1, deref v2)
      val r1 = memoize r1
      val r2 = memoize r2
      val () = set v1 r1
      val () = set v2 r2
    in
      (r1, r2)
    end
```

### Mixfix Parser

The library also includes a mixfix parser similar to Agda's mixfix parser (with some modifications) provided by 
mixfix.{cm/mlb}. It provides a functor of this signature:
```
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
) : MIXFIX 
  where type id = Id.t
  and type exp = Exp.t
  and type 'a Parser.stream = 'a Stream.stream
```
```
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
```

An example usage of this can be found in [examples/mixfix.sml](examples/mixfix.sml). A few notes:
- precedence follows Agda conventions, higher = binds more tightly.
- As in the signature of `PARCOM`, all parses are returned in case of ambiguous parses.
- This is single sorted. The type id can be thought of as keywords in the syntax, whereas
  exp is everything else. These two types are functorized, but one example for the syntax
  of a rule could be
  ```
  [ SyntaxId "if" , SyntaxHole , SyntaxId "then" , SyntaxHole , SyntaxId "else" , SyntaxHole]
  ```
  which matches a producing stream of
  ```
    TokenId "if" , TokenExp "true" , TokenId "then" , TokenId "1" , TokenId "else" , TokenExp "2", ...
  ```
- There are two kinds of syntax rules that are not allowed, and calling `build` will throw `RuleError` in these cases,
  since they are not meaningful:
  + empty syntax
  + rule with only a single hole (`[ SyntaxHole ]`).
- As in the above example (`TokenId "1"`), in the case where a TokenId is *not* a keyword in any of the syntax,
it gets treated like an expression through `Exp.id` (aka, "variable name", similar to Agda).
  + For this reason, `Id` needs to implement `ORDERED` for a fast enough lookup to see if something is a keyword or not.
- Unlike Agda, function application is not encoded as a default fallback rule. You can mimic the behavior
  by having a rule of `[ SyntaxHole , SyntaxHole ]` with a suitable precedence and left associativity.

## Miscellaneous

The implementation is a straightforward generalization of (Johnson 1995) and is not too difficult to do,
if you are interested in implementing this on your own, I have a walkthrough on [my blog](https://www.haoxuany.com/blog/johnson/).

## License

This library is under MIT license. See [LICENSE](LICENSE).

The implementation makes extensive use of data structures within cmlib, most
notably HashTable and SplaySet. cmlib is licensed under MIT license.
See [lib/cmlib/LICENSE](lib/cmlib/LICENSE).
