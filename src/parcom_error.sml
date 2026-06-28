
functor ParcomError (
  structure TokenStream : sig
    type token
    type stream

    datatype front = Nil | Cons of token * stream
    val front : stream -> front
  end

  structure ParseError : sig
    type t

    val unexpected_eof : t
  end
) :> PARCOM_ERROR 
  where type token = TokenStream.token
  and type stream = TokenStream.stream
  and type error = ParseError.t
= struct

  open TokenStream

  type token = token
  type pos = int * int (* request id * stream position *)
  type stream = TokenStream.stream * pos
  type error = ParseError.t

  datatype 'a parse_result =
    ParseSuccess of 'a
  | ParseFailure of error

  type 'a t = 
    stream -> (('a parse_result * stream) -> unit) -> unit
  type 'a t_memo = 'a t

  fun map (f : 'a -> 'b) (a : 'a t) : 'b t =
    fn s => fn k =>
      a s (fn ( v , s ) => k 
        ( case v of
            ParseSuccess v => ParseSuccess ( f v )
          | ParseFailure e => ParseFailure e
        , s ))

  fun mapError (f : error -> error) (a : 'a t) : 'a t =
    fn s => fn k =>
      a s (fn ( v , s ) => k 
        ( case v of
            ParseSuccess _ => v
          | ParseFailure e => ParseFailure ( f e )
        , s ))

  fun terminal (f : token -> 'a parse_result) : 'a t =
    fn ( s , ( reqid , pos ) ) => fn k =>
      case front s of
        Nil => k 
          ( ParseFailure ParseError.unexpected_eof 
          , ( s , ( reqid , pos + 1 ) )
          )
      | Cons ( h , t ) =>
          (case f h of
            ParseFailure e => k
              ( ParseFailure e
              , ( s , ( reqid , pos + 1 ) )
              )
          | ParseSuccess v => k 
              ( ParseSuccess v , ( t , ( reqid , pos + 1 )) ))

  fun terminals f : 'a t =
    fn ( s , ( reqid , pos ) ) => fn k =>
      case f s of
        ParseFailure e => k
          ( ParseFailure e
          , ( s , ( reqid , pos + 1 ) )
          )
      | ParseSuccess ( v , len , t ) =>
          k ( ParseSuccess v , ( t , ( reqid , pos + len ) ) )

  fun bind (a : 'a t) (f : 'a -> 'b t) : 'b t =
    fn s => fn k =>
      a s (fn ( result , s ) =>
        case result of
          ParseFailure e => k ( ParseFailure e , s )
        | ParseSuccess v => f v s k)

  fun return (a : 'a) : 'a t =
    fn ( s , pos ) => fn k =>
      k ( ParseSuccess a , ( s , pos ) )

  fun returnError (e : error) : 'a t =
    fn ( s , pos ) => fn k =>
      k ( ParseFailure e , ( s , pos ) )

  fun either (l : 'a t list) : 'a t =
    fn s => fn k =>
    let
      val hasSuccess = ref false
      val failures = ref nil
      val () = List.app (fn f => f s
        (fn ( result , s ) =>
          case result of
            ParseSuccess _ => ( hasSuccess := true ; k ( result , s ) )
          | ParseFailure _ => failures := ( result , s ) :: (!failures)
        )) l
    in
      if !hasSuccess then ()
      else List.app k (List.rev (!failures))
    end

  fun all (l : 'a t list) : 'a list t =
    case l of
      nil => return nil
    | f :: rest =>
        bind f (fn hd => fn s => fn k =>
          all rest s (fn ( result , s ) =>
            case result of
              ParseFailure e => k ( ParseFailure e , s )
            | ParseSuccess tail => k ( ParseSuccess ( hd :: tail ) , s )))

  val epsilon = return

  fun optional (a : 'a t) : 'a option t =
    either [ map SOME a , epsilon NONE ]

  fun star (a : 'a t) : 'a list t =
    either [ plus a , epsilon nil ]
  and plus (a : 'a t) : 'a list t =
    bind a (fn hd => fn s => fn k =>
      star a s (fn ( result , s ) =>
        case result of
          ParseFailure e => k ( ParseFailure e , s )
        | ParseSuccess tail => k ( ParseSuccess ( hd :: tail ) , s )))

  fun partition_results (l : ('a parse_result * stream) list) =
    List.foldr
    (fn ( ( v , s ) , ( success , fail ) ) =>
      (case v of
        ParseFailure e => ( success , ( e , s ) :: fail )
      | ParseSuccess v => ( ( v , s ) :: success , fail )))
    ( nil , nil ) l

  fun prefer (l : 'a t list) : 'a t =
    fn s => fn k =>
    let
      fun prefer l failures =
        case l of
          nil =>
            List.app 
            (List.app 
              (fn ( e , s ) => k ( ParseFailure e , s )))
            (List.rev failures)
        | f :: rest =>
          let
            val results = ref nil
            val () = f s (fn v => 
              results := v :: (!results)
            )
            val results = List.rev (!results)
            val ( success , fail ) = 
              partition_results results
          in
            case success of
              nil => prefer rest ( fail :: failures )
            | _ => List.app
                (fn ( v , s ) => k ( ParseSuccess v , s ) )
                success
          end
    in
      prefer l nil
    end

  (* I don't have a good straightforward proof that this works,
  * this is a little sketchy, but the reasoning is that:
  * - Continuations are always called in the order in which functions are
  * called, even (and especially) in the case of memoize. 
  * - All continuations are called with the same result in each position.
  * - As such, simply testing for continuation output is sufficient. *)
  (* Also removes failures if there are any successes. *)
  fun join (f : ('a * stream) list -> ('a * stream) list) (p : 'a t) : 'a t =
    fn s => fn k =>
    let
      val results = ref nil
      val () = p s
        (fn result => results := result :: (!results))
      val results = List.rev (!results)
      val ( success , fail ) = partition_results results
      val success = f success
    in
      List.app k
      ( case success of
          nil => 
            ( List.map (fn ( e , s ) => ( ParseFailure e , s ) )
              fail
            )
        | _ =>
          ( List.map (fn ( v , s ) => ( ParseSuccess v , s ) )
            success
          )
      )
    end

  fun optionalLongest (a : 'a t) : 'a option t =
    prefer [ map SOME a , epsilon NONE ]

  fun starLongest (a : 'a t) : 'a list t =
    prefer [ plusLongest a , epsilon nil ]
  and plusLongest (a : 'a t) : 'a list t =
    bind a (fn hd => fn s => fn k =>
      starLongest a s (fn ( result , s ) =>
        case result of
          ParseFailure e => k ( ParseFailure e , s )
        | ParseSuccess tail => k ( ParseSuccess ( hd :: tail ) , s )))

  structure Lookup = struct

    structure Dict = SplayDict (structure Key = IntOrdered)

    type 'a mem =
      { results : ('a * stream) list ref
      , continuations : (('a * stream) -> unit) list ref
      }

    type 'a table = ('a mem Dict.dict ref) * (int ref)

    fun init () : 'a table = ( ref Dict.empty , ref 0 )

    fun find ( ( table , tableid ) : 'a table) (( reqid , pos ) : pos) : 'a mem =
      let
        (* We create the hash entry, since for memoization the lookup means
         * we will need to push a continuation later. *)
        fun create_empty () =
          let
            val mem = { results = ref [] , continuations = ref [] }
            val () = table := Dict.insert (!table) pos mem
          in
            mem
          end
      in
        if reqid <> (!tableid) then
          ( table := Dict.empty
          ; tableid := reqid
          ; create_empty ()
          )
        else
          case Dict.find (!table) pos of
            SOME v => v
          | NONE => create_empty ()
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

  local
    val currentid = ref ~1
  in
    fun newid () =
      let 
        val current = (!currentid) + 1
      in
        ( currentid := current
        ; current
        )
      end
  end

  datatype 'a result =
    ResultSuccess of ( 'a * TokenStream.stream ) list
  | ResultFailure of ( error * TokenStream.stream ) list

  fun parser (p : 'a t) :
    TokenStream.stream -> 'a result =
    fn s =>
    let
      val results = ref []
      val () = p ( s , ( newid () , 0 ) )
        (fn v => Lookup.push results v)

      val results = List.rev (!results)
      val ( success , fail ) = partition_results results
    in
      case success of
        nil =>
          let
            val fail =
              List.map
              (fn ( v , ( s , ( _ , pos ) ) ) =>
                ( pos , ( v , s ) ) )
              fail

            val fail = Quicksort.sort
              (fn ( ( p1 , _ ) , ( p2 , _ ) ) =>
                Int.compare ( p2 , p1 ))
              fail

            val fail = List.map (fn ( _ , v ) => v) fail
          in
            ResultFailure fail
          end
      | _ =>
          ResultSuccess
          ( List.map 
            (fn ( v , ( s , _ ) )  => ( v , s ) )
            success
          )
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

  type stream = TokenStream.stream

  (* Backpatching stuff *)
  type 'a t_dummy = 'a t ref
  fun dummy () = ref (fn _ => fn _ => ())
  fun set dummy v = dummy := v
  fun deref dummy = fn x => (!dummy) x
end
