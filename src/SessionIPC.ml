exception InvalidEndpoint

module IPCChannel : sig
  type t
  val fork     : (t -> unit) -> t
  val send       : 'a -> t -> unit
  val receive    : t -> 'a
end = struct
  type t         = in_channel * out_channel
  let fork f     =
    let c_in, s_out = Unix.pipe () in
    let s_in, c_out = Unix.pipe () in
    if Unix.fork () = 0 then begin
        let (ic,oc) = (Unix.in_channel_of_descr c_in, Unix.out_channel_of_descr c_out) in
        (input_value ic : unit);
        (ic,oc)
      end
    else begin
        let (ic,oc) = (Unix.in_channel_of_descr s_in, Unix.out_channel_of_descr s_out) in
        output_value oc (); flush oc;
        ignore (f (ic,oc));
        exit 0;
      end
  let send x (_, oc)  = output_value oc x; flush oc
  let receive (ic, _) = input_value ic
end

module type FLAG = sig
  type t
  val create     : unit -> t
  val use        : t -> unit
  val try_use    : t -> bool
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
    val fork : ?name:string -> (('a, 'b) st -> unit) -> ('b, 'a) st
    val close : et -> unit
    val send : 'm -> ('m * ('a, 'b) st) ot -> ('b, 'a) st
    val receive : ('m * ('a, 'b) st) it -> 'm * ('a, 'b) st
    val select_true : [> `True of ('a, 'b) st] ot -> ('b, 'a) st
    val select_false : [> `False of ('a, 'b) st] ot -> ('b, 'a) st
    val branch : ([>`True of ('a, 'b) st | `False of ('c, 'd) st] as 'm) it -> 'm
  end
  module Monadic : sig
    type ('t0, 't1, 'a) t
    val return  : 'm -> ('t0, 't0, 'm) t
    val (>>=) : ('t0, 't1, 'a) t -> ('a -> ('t1, 't2, 'b) t) -> ('t0, 't2, 'b) t
    val (>>>) : ('t0, 't1, 'a) t -> ('t1, 't2, 'b) t -> ('t0, 't2, 'b) t
    val fix     : (('t0, 't1, 'a) t -> ('t0, 't1, 'a) t) -> ('t0, 't1, 'a) t
    val fork    : (('a, 'b) st, et, unit) t -> (('b, 'a) st, et, 'm) t -> 'm
    val receive : (('m * ('a, 'b) st) it, ('a, 'b) st, 'm) t
    val send    : 'm -> (('m * ('b, 'a) st) ot, ('a, 'b) st, unit) t
    val branch  : (('b,'c) st, 't2, 'a) t -> (('d,'e)st, 't2, 'a) t -> ((('b,'c) st, ('d,'e) st) choice it, 't2, 'a) t
    val select_true : ((('b, 'a) st, ('d, 'c) st) choice ot, ('a, 'b) st, unit) t
    val select_false : ((('b, 'a) st, ('d, 'c) st) choice ot, ('c, 'd) st, unit) t
  end
end  

module Make(Flag:FLAG) : S = struct
  type _0
  type (+'a, -'b) st = { name     : string;
  		       channel  : IPCChannel.t;
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
  
    let fork ?(name = "channel") f =
      let ch = IPCChannel.fork
                (fun ch ->
                  let ep1 = { name = name ^ "⁺";
                              channel = ch;
                              polarity = +1;
                              once = Flag.create () }
                  in
                  f ep1
        )
      in
      let ep2 = { name = name ^ "⁻";
                  channel = ch;
                  polarity = -1;
                  once = Flag.create () }
      in ep2
  
    let close ep = Flag.use ep.once
  
    let send x ep  = Flag.use ep.once; (* Printf.printf "%s send\n" ep.name; flush_all();  *)IPCChannel.send x ep.channel; fresh ep
    let receive ep = Flag.use ep.once; (* Printf.printf "%s receive\n" ep.name; flush_all();  *)(IPCChannel.receive ep.channel, fresh ep)
  
    let select_true ep  = send `True ep
    let select_false ep = send `False ep
    let branch ep       =
      Flag.use ep.once;
      (* Printf.printf "%s branch\n" ep.name; flush_all(); *)
      match IPCChannel.receive ep.channel with
      | `True -> `True(fresh ep)
      | `False -> `False(fresh ep)

  end
  
  module Monadic = struct
    type ('t0, 't1, 'a) t = 't0 -> ('a * 't1)
  
    let (>>=) m f ep = let x, ep = m ep in f x ep
    let (>>>) m1 m2 = m1 >>= fun _ -> m2
    let return m ep = (m, ep)
  
    let rec fix f = f (fun ep -> fix f ep)
  
    let fork ms mc =
      let epc = Bare.fork (fun eps -> let (), ep = ms eps in Bare.close ep) in
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

