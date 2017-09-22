(* This file is part of FuSe.                                           *)
(*                                                                      *)
(* FuSe is free software: you can redistribute it and/or modify         *)
(* it under the terms of the GNU General Public License as published by *)
(* the Free Software Foundation, either version 3 of the License, or    *)
(* (at your option) any later version.                                  *)
(*                                                                      *)
(* FuSe is distributed in the hope that it will be useful,              *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of       *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *)
(* GNU General Public License for more details.                         *)
(*                                                                      *)
(* You should have received a copy of the GNU General Public License    *)
(* along with FuSe.  If not, see <http://www.gnu.org/licenses/>.        *)
(*                                                                      *)
(* Copyright 2015-2017 Luca Padovani                                    *)

exception InvalidEndpoint

module type UNSAFE_CHANNEL =
sig
  type t
  val create     : unit -> t
  val send       : 'a -> t -> unit
  val receive    : t -> 'a
  val flip       : t -> t
end

module UnsafeChannel : UNSAFE_CHANNEL
= struct
  type t         = unit Event.channel
  let create     = Event.new_channel
  let send x ch  = Event.sync (Event.send ch (Obj.magic x))
  let receive ch = Obj.magic (Event.sync (Event.receive ch))
  let flip ch    = ch
end

module BufferedUnsafeChannel : UNSAFE_CHANNEL
= struct
  module Aux = struct
    module M = Mutex
    module C = Condition
    module Q = Queue
  
    type t = unit Q.t * M.t * C.t
  
    let create () : t = Q.create (), M.create (), C.create ()
  
    let send v (q,m,c) =
      M.lock m;
      Q.add (Obj.magic v) q;
      C.signal c;
      M.unlock m
      
  
    let receive (q,m,c) =
      M.lock m;
      let rec loop () =
        if Q.is_empty q then begin
          C.wait c m;
          loop ()
        end else begin
          Q.take q
        end
      in
      let v = loop () in
      M.unlock m;
      Obj.magic v
  end
  type t = Aux.t * Aux.t
  let create () = Aux.create (), Aux.create ()
  let send v (_, oc) = Aux.send v oc
  let receive (ic, _) = Aux.receive ic
  let flip (ic,oc) = (oc, ic)
end

module PipeUnsafeChannel : UNSAFE_CHANNEL
= struct
  module Aux = struct
    type t = in_channel * out_channel
  
    let create () : t = let fi, fo = Unix.pipe () in (Unix.in_channel_of_descr fi, Unix.out_channel_of_descr fo)
  
    let send v (_,oc) = output_value oc v; flush oc
    let receive (ic,_) = input_value ic
  end
  type t = Aux.t * Aux.t
  let create () = Aux.create (), Aux.create ()
  let send v (_, oc) = Aux.send v oc
  let receive (ic, _) = Aux.receive ic
  let flip (ic,oc) = (oc, ic)
end


module type FLAG = sig
  type t
  val create     : unit -> t
  val use        : t -> unit
  val try_use    : t -> bool
end

module NoFlag : FLAG =
struct
  type t         = unit
  let create ()  = ()
  let use _      = ()
  let try_use f  = true
end
module UnsafeFlag : FLAG =
struct
  type t         = bool ref
  let create ()  = ref true
  let use f      =
    (* BEGIN ATOMIC *)
    if !f then f := false else raise InvalidEndpoint
    (* END ATOMIC *)
  let try_use f  = let valid = !f in f := false; valid
end
module StdMutexFlag : FLAG =
struct
  type t         = Mutex.t
  let create ()  = Mutex.create ()
  let use f      =
    if not (Mutex.try_lock f) then raise InvalidEndpoint
  let try_use f  = Mutex.try_lock f
end    
module NanoMutexFlag : FLAG =
struct
  type t         = Core.Nano_mutex.t
  let create ()  = Core.Nano_mutex.create ()
  let try_lock_nano f = match Core.Nano_mutex.try_lock f with Ok `Acquired -> true | Ok `Not_acquired | Error _ -> false
  let use f      =
    if not (try_lock_nano f) then raise InvalidEndpoint
  let try_use f  = try_lock_nano f
end

module type S = sig
  (** Empty type. *)
  type _0
  
  (** The type of endpoints for {e receiving} messages of type
  ['a] and {e sending} messages of type ['b]. *)
  type (+'a, -'b) st
  
  (** The type of endpoints that can only be closed. *)
  type et = (_0, _0) st
  
  (** The type of endpoints for {e receiving} messages of type
  ['a]. *)
  type +'a it = ('a, _0) st
  
  (** The type of endpoints for {e sending} messages of type
  ['a]. *)
  type -'a ot = (_0, 'a) st
  
  (** The type of a sequentially composed protocol, where ['a] is the
  protocol to be performed first, followed by ['b]. *)
  type (+'a, +'b) seq
  
  (** The type of a binary choice, where ['a] is the protocol to be
  performed if [`True] is selected and ['b] is the protocol to be
  performed if [`False] is selected. *)
  type (+'a, +'b) choice = [ `True of 'a | `False of 'b ]
  
  module Bare : sig
    (** {2 Session initiation and termination} *)
  
    (** [create ()] creates a new session.  @return a pair with two
  valid endpoints and dual types. *)
    val create : ?name:string -> unit -> ('a, 'b) st * ('b, 'a) st
  
    (** [close ep] closes endpoint [ep].  @raise InvalidEndpoint if the
   endpoint [ep] is invalid. *)
    val close : et -> unit
  
    (** {2 Basic message passing} *)
  
    (** [send e ep] sends [e] on the endpoint [ep] with output
   capability.  @return the endpoint [ep].  @raise InvalidEndpoint if
   [ep] is invalid. *)
    val send : 'm -> ('m * ('a, 'b) st) ot -> ('b, 'a) st
  
    (** [receive ep] receives a message from the endpoint [ep] with
   input capability.  @return a pair [(v, ep)] with the received message
   [v] and the endpoint [ep].  @raise InvalidEndpoint if the endpoint
   [ep] is invalid.  *)
    val receive : ('m * ('a, 'b) st) it -> 'm * ('a, 'b) st
  
    (** {2 Choices} *)
  
    (** [select f ep] sends [f] to the peer endpoint of [ep], where it
  is used to compute the received message.  @return the endpoint [ep].
  @raise InvalidEndpoint if the endpoint [ep] is invalid. *)
    (* val select : (('a, 'b) st -> 'm) -> 'm ot -> ('b, 'a) st *)
  
    (** [select_true ep] selects the [True] branch of a choice.  @return
   the endpoint [ep] after the selection.  @raise InvalidEndpoint if the
   endpoint [ep] is invalid. *)
    val select_true : [> `True of ('a, 'b) st] ot -> ('b, 'a) st
  
    (** [select_false ep] selects the [False] branch of a choice.
   @return the endpoint [ep] after the selection.  @raise
   InvalidEndpoint if the endpoint [ep] is invalid. *)
    val select_false : [> `False of ('a, 'b) st] ot -> ('b, 'a) st
  
    (** [branch ep] receives a selection from the endpoint [ep] with
   input capability.  @return the endpoint [ep] injected through the
   selected tag.  @raise InvalidEndpoint if the endpoint [ep] is
   invalid.  *)
    (* val branch : ([>] as 'm) it -> 'm *)
    val branch : ([>`True of ('a, 'b) st | `False of ('c, 'd) st] as 'm) it -> 'm
  
    (** {2 Endpoint validity and identity} *)
  
    (** [is_valid ep] determines whether [ep] is a valid endpoint or not.
   @return [true] if [ep] is valid, [false] otherwise. *)
    (* val is_valid : ('a, 'b) st -> bool *)
  
    (** [acquire ep] acquires the endpoint [ep], if it is valid.  @return
   the unique valid reference to the endpoint [ep].  @raise
   InvalidEndpoint if [ep] invalid. *)
    val acquire : ('a, 'b) st -> ('a, 'b) st
  
    (** [try_acquire ep] attempts to acquire the endpoint [ep].  @return
   [Some ep] where [ep] is the unique valid reference to the endpoint,
   if [ep] is valid, and [None] otherwise. *)
    val try_acquire : ('a, 'b) st -> ('a, 'b) st option
  
    (** [same_session ep ep'] checks whether [ep] and [ep'] are endpoints
   of the same session (but not necessarily peer endpoints).  @return
   [true] if [ep] and [ep'] are (possibly peer) endpoints pertaining the
   same session, [false] otherwise. *)
    val same_session : ('a, 'b) st -> ('c, 'd) st -> bool
  
    (** [string_of_endpoint ep] returns a textual representation of the
  endpoint [ep]. *)
    val string_of_endpoint : ('a, 'b) st -> string
  
    (** {2 Resumption combinators} *)
  
    (** [f @= ep] evaluates [f ep].  @return the pair to which [f ep]
   evaluates.  @raise InvalidEndpoint if the second component of [f ep]
   is an endpoint other than [ep]. *)
    val (@=) :
      (('a, 'b) st -> 'm * et) ->
      ((('a, 'b) st, ('c, 'd) st) seq, (('b, 'a) st, ('d, 'c) st) seq) st ->
      'm * ('c, 'd) st
  
    (** [f @> ep] evaluates [f ep].  @return the endpoint [ep].  @raise
   InvalidEndpoint if [f ep] evaluates to an endpoint different from
   [ep]. *)
    val (@>) :
      (('a, 'b) st -> et) ->
      ((('a, 'b) st, ('c, 'd) st) seq, (('b, 'a) st, ('d, 'c) st) seq) st ->
      ('c, 'd) st
  end
  
  module Monadic : sig
    (** The type of a computation that returns a result of type ['a]
    while using a session endpoint and changing its type from ['t0] to
    ['t1]. *)
    type ('t0, 't1, 'a) t
  
    (** [return e] is the trivial monadic computation that does not
    perform communications and returns the value of [e]. *)
    val return  : 'm -> ('t0, 't0, 'm) t
  
    (** The monadic composition operator. *)
    val (>>=) : ('t0, 't1, 'a) t -> ('a -> ('t1, 't2, 'b) t) -> ('t0, 't2, 'b) t
  
    (** [m1 >>> m2] is a shortcut for [m1 >>= fun _ -> m2]. *)
    val (>>>) : ('t0, 't1, 'a) t -> ('t1, 't2, 'b) t -> ('t0, 't2, 'b) t
  
    (** Fixpoint operator for monadic computations. [fix (fun x -> m)]
    represents the same computation as [m] in which [x] is bound to [m]
    itself. *)
    val fix     : (('t0, 't1, 'a) t -> ('t0, 't1, 'a) t) -> ('t0, 't1, 'a) t
  
    (** [connect ms mc] creates a new session that connects the server
    [ms], spawned into a new thread, and the client [mc]. The result is
    that returned by the client. *)
    val connect : (('a, 'b) st, et, unit) t -> (('b, 'a) st, et, 'm) t -> 'm
  
    (** [receive] waits for a message from the session endpoint and
    returns its value. *)
    val receive : (('m * ('a, 'b) st) it, ('a, 'b) st, 'm) t
  
    (** [send e] sends the message [e] on the session endpoint. *)
    val send    : 'm -> (('m * ('b, 'a) st) ot, ('a, 'b) st, unit) t
  
    (** [branch mtrue mfalse] accepts a boolean selection from the
    session endpoint and executes either [mtrue] or [mfalse]
    accordingly. *)
    (* val branch  : ('t0, 't2, 'a) t -> ('t1, 't2, 'a) t -> (('t0, 't1) choice it, 't2, 'a) t *)
    val branch  : (('b,'c) st, 't2, 'a) t -> (('d,'e)st, 't2, 'a) t -> ((('b,'c) st, ('d,'e) st) choice it, 't2, 'a) t
  
    (** [select_true] selects the [True] branch of a choice. *)
    val select_true : ((('b, 'a) st, ('d, 'c) st) choice ot, ('a, 'b) st, unit) t
  
    (** [select_false] selects the [False] branch of a choice. *)
    val select_false : ((('b, 'a) st, ('d, 'c) st) choice ot, ('c, 'd) st, unit) t
  end
end  

module Make(UnsafeChannel:UNSAFE_CHANNEL)(Flag:FLAG) : S = struct
  type _0
  type (+'a, -'b) st = { name     : string;
  		       channel  : UnsafeChannel.t;
  		       polarity : int;
  		       once     : Flag.t }
  type et            = (_0, _0) st
  type +'a it        = ('a, _0) st
  type -'a ot        = (_0, 'a) st
  type (+'a, +'b) seq
  type (+'a, +'b) choice = [ `True of 'a | `False of 'b ]
  
  module Bare = struct
    let fresh ep = { ep with once = Flag.create () }
  
    (**********************************)
    (*** INITIATION AND TERMINATION ***)
    (**********************************)
  
    let create ?(name = "channel") () =
      let ch = UnsafeChannel.create () in
      let ep1 = { name = name ^ "⁺";
                  channel = ch;
                  polarity = +1;
                  once = Flag.create () }
      and ep2 = { name = name ^ "⁻";
                  channel = UnsafeChannel.flip ch;
                  polarity = -1;
                  once = Flag.create () }
      in (ep1, ep2)
  
    let close ep = Flag.use ep.once
  
    (****************)
    (*** IDENTITY ***)
    (****************)
  
    let same_session ep ep' = ep.channel == ep'.channel
    let same_endpoint ep ep' = same_session ep ep' && ep.polarity == ep'.polarity
    let string_of_endpoint ep = ep.name
  
    (*****************)
    (*** LINEARITY ***)
    (*****************)
  
    (* let is_valid ep = Flag.is_valid ep.once *)
    let acquire ep = Flag.use ep.once; fresh ep
    let try_acquire ep = if Flag.try_use ep.once then Some (fresh ep) else None
  
    (***********************)
    (*** MESSAGE PASSING ***)
    (***********************)
  
    let send x ep  = Flag.use ep.once; UnsafeChannel.send x ep.channel; fresh ep
    let receive ep = Flag.use ep.once; (UnsafeChannel.receive ep.channel, fresh ep)
  
    (***************)
    (*** CHOICES ***)
    (***************)
  
    (* let select f ep     = Flag.use ep.once; UnsafeChannel.send f ep.channel; fresh ep *)
    (* let select_true ep  = select (fun x -> `True x) ep *)
    (* let select_false ep = select (fun x -> `False x) ep *)
    (* let branch ep       = Flag.use ep.once; (UnsafeChannel.receive ep.channel) (fresh ep) *)
    let select_true ep  = send `True ep
    let select_false ep = send `False ep
    let branch ep       =
      Flag.use ep.once;
      match UnsafeChannel.receive ep.channel with
      | `True -> `True(fresh ep)
      | `False -> `False(fresh ep)
  
    (******************************)
    (*** SEQUENTIAL COMPOSITION ***)
    (******************************)
  
    let (@=) scope ep =
      let result, ep' = scope (Obj.magic ep) in
      if same_endpoint ep ep' then (result, Obj.magic ep')
      else raise InvalidEndpoint
  
    let (@>) scope ep =
      snd ((fun ep -> ((), scope ep)) @= ep)
  end
  
  module Monadic = struct
    type ('t0, 't1, 'a) t = 't0 -> ('a * 't1)
  
    let (>>=) m f ep = let x, ep = m ep in f x ep
    let (>>>) m1 m2 = m1 >>= fun _ -> m2
    let return m ep = (m, ep)
  
    let rec fix f = f (fun ep -> fix f ep)
  
    let connect ms mc =
      let eps, epc = Bare.create () in
      let _ = Thread.create (fun ep -> let (), ep = ms ep in Bare.close ep) eps in
      let x, epc = mc epc in
      Bare.close epc;
      x
  
    let receive = Bare.receive
    let send x ep = let ep = Bare.send x ep in ((), ep)
    let select_true ep = let ep = Bare.select_true ep in ((), ep)
    let select_false ep = let ep = Bare.select_false ep in ((), ep)
    let branch m1 m2 ep =
      match Bare.branch ep with
      | `True ep -> m1 ep
      | `False ep -> m2 ep
  end
end

