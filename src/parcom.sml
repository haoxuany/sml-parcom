
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
  = struct

  open Stream

  type token = token
  type stream = token stream * int
  type 'a t = stream -> (('a * stream) -> unit) -> unit
  type 'a t_memo = 'a t

  fun map (f : 'a -> 'b) (a : 'a t) : 'b t =
    fn s => fn k =>
      a s (fn ( b , s ) => k ( f b , s ))

  fun terminal (f : token -> 'a option) : 'a t =
    fn ( s , pos ) => fn k =>
      case front s of
        Nil => ()
      | Cons ( h , t ) =>
          (case f h of
            NONE => ()
          | SOME v => k ( v , ( t , pos + 1 ) ))

  fun remove (f : token -> bool) : unit t =
    terminal (fn tok => if f tok then SOME () else NONE)

  fun bind (a : 'a t) (f : 'a -> 'b t) : 'b t =
    fn s => fn k =>
      a s (fn ( result , s ) => f result s k)

  fun return (a : 'a) : 'a t =
    fn ( s , pos ) => fn k =>
      k ( a , ( s , pos ) )

  fun either (l : 'a t list) : 'a t =
    fn s => fn k =>
      List.app (fn f => f s k) l

  val epsilon = return

  fun optional (a : 'a t) : 'a option t =
    either [ map SOME a , epsilon NONE ]

  fun star (a : 'a t) : 'a list t =
    either [ plus a , epsilon nil ]
  and plus (a : 'a t) : 'a list t =
    bind a (fn hd => fn s => fn k =>
      star a s (fn ( tail , s ) => k ( hd :: tail , s )))

  structure HashTable = HashTable (structure Key = IntHashable)

  structure Lookup = struct
    type 'a mem =
      { results : ('a * stream) list ref
      , continuations : (('a * stream) -> unit) list ref
      }

    type 'a table = 'a mem HashTable.table

    (* Positions in streams need to be unique. To enforce this,
     * we bump largest whenever we perform a lookup. *)
    val largest = ref 0

    fun init () : 'a table = HashTable.table table_size

    fun find (table : 'a table) (pos : int) : 'a mem =
      case HashTable.find table pos of
        SOME v => v
        (* We create the hash entry, since for memoization the lookup means
         * we will need to push a continuation later. *)
      | NONE =>
          let
            val mem = { results = ref [] , continuations = ref [] }
            val () = HashTable.insert table pos mem
            val () = if pos > (!largest) then largest := pos
                     else ()
          in
            mem
          end

    fun push (l : 'a list ref) (v : 'a) =
      l := v :: (!l)
  end

  fun memoize (parser : 'a t) : 'a t =
    let
      val table = Lookup.init ()
      fun f (s as ( _ , pos )) k =
        let
          val { results , continuations } = Lookup.find table pos
        in
          case !continuations of
            nil =>
              (* First parse *)
              ( Lookup.push continuations k
              ; let
                  val k = fn result =>
                    ( Lookup.push results result
                    ; List.app (fn f => f result) (!continuations)
                    )
                in
                  parser s k
                end
              )
          | _ =>
              (* We already know the result, call continuation on all
               * results directly *)
              ( Lookup.push continuations k
              ; List.app (fn result => k result) (!results)
              )
        end
    in
      f
    end

  fun forget (parser : 'a t) : 'a t = parser

  fun parser (p : 'a t) :
    token Stream.stream -> ('a * token Stream.stream) list =
    fn s =>
    let
      val results = ref []
      val () = p ( s , !Lookup.largest + 1 )
        (fn ( parse , ( s , _ ) ) => Lookup.push results ( parse , s ))
    in
      !results
    end

  val dummy = fn _ => fn _ => ()
  fun eta (v : ('a -> 'b) ref) = fn x => !v x

  fun fix (f : 'a t -> 'a t) : 'a t =
    let
      val v = ref dummy
      val result = memoize (f (eta v))
      val () = v := result
    in
      result
    end

  fun fix2 (f : 'a t * 'b t -> 'a t * 'b t) : 'a t * 'b t =
    let
      val v1 = ref dummy
      val v2 = ref dummy
      val (r1, r2) = f (eta v1, eta v2)
      val r1 = memoize r1
      val r2 = memoize r2
      val () = v1 := r1
      val () = v2 := r2
    in
      (r1, r2)
    end

  fun fix3 (f : 'a t * 'b t * 'c t -> 'a t * 'b t * 'c t)
    : 'a t * 'b t * 'c t =
    let
      val v1 = ref dummy
      val v2 = ref dummy
      val v3 = ref dummy
      val (r1, r2, r3) = f (eta v1, eta v2, eta v3)
      val r1 = memoize r1
      val r2 = memoize r2
      val r3 = memoize r3
      val () = v1 := r1
      val () = v2 := r2
      val () = v3 := r3
    in
      (r1, r2, r3)
    end

  fun fix4 (f : 'a t * 'b t * 'c t * 'd t
    -> 'a t * 'b t * 'c t * 'd t)
    : 'a t * 'b t * 'c t * 'd t =
    let
      val v1 = ref dummy
      val v2 = ref dummy
      val v3 = ref dummy
      val v4 = ref dummy
      val (r1, r2, r3, r4) = f (eta v1, eta v2, eta v3, eta v4)
      val r1 = memoize r1
      val r2 = memoize r2
      val r3 = memoize r3
      val r4 = memoize r4
      val () = v1 := r1
      val () = v2 := r2
      val () = v3 := r3
      val () = v4 := r4
    in
      (r1, r2, r3, r4)
    end

  fun fix5 (f : 'a t * 'b t * 'c t * 'd t * 'e t
    -> 'a t * 'b t * 'c t * 'd t * 'e t)
    : 'a t * 'b t * 'c t * 'd t * 'e t =
    let
      val v1 = ref dummy
      val v2 = ref dummy
      val v3 = ref dummy
      val v4 = ref dummy
      val v5 = ref dummy
      val (r1, r2, r3, r4, r5) = f (eta v1, eta v2, eta v3, eta v4, eta v5)
      val r1 = memoize r1
      val r2 = memoize r2
      val r3 = memoize r3
      val r4 = memoize r4
      val r5 = memoize r5
      val () = v1 := r1
      val () = v2 := r2
      val () = v3 := r3
      val () = v4 := r4
      val () = v5 := r5
    in
      (r1, r2, r3, r4, r5)
    end

  type 'a stream = 'a Stream.stream
end
