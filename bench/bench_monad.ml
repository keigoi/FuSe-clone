let start = ref @@ Core.Int63.of_int 0
let now () = (Core.Time_stamp_counter.now () :> Core.Int63.t)

module Make(S : SessionBase.S) = struct
  open S.Monadic
    
  let cOUNT = 8000
  
  let rec server () =
    branch begin
        receive >>= fun n ->
        receive >>= fun m ->
        send (n + m) >>=
        server
      end
    (return ())
  
  
  let client cnt =
    start := now ();
    let rec aux acc n =
      if n = 0 then begin
        select_false >>> return acc
      end else begin
        select_true >>>
        send acc >>>
        send n >>>
        receive >>= fun res ->
        aux res (n - 1)
      end
    in aux 0 cnt
  		      
  let run () =
    ignore (connect (server ()) (client cOUNT));
    let countpersec =
      let c = (cOUNT * 4 + 1) * 1000000000 in
      Core.Int63.(of_int c / (now () - !start))
    in
    print_endline (Core.Int63.to_string countpersec)
end

module M = Make(SessionBase.Make(SessionBase.UnsafeChannel)(SessionBase.NoFlag))
module M_ = Make(SessionBase.Make(SessionBase.BufferedUnsafeChannel)(SessionBase.NoFlag))
module MP = Make(SessionBase.Make(SessionBase.PipeUnsafeChannel)(SessionBase.NoFlag))

let _ =
  if Array.length Sys.argv = 1 then
    M.run ()
  else
    MP.run ()


