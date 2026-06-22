
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
