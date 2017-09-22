let start = ref @@ Core.Int63.of_int 0
let now () = (Core.Time_stamp_counter.now () :> Core.Int63.t)
      
module Make(S : SessionIPC.S) = struct
  open S.Bare
  
  let cOUNT = 8000
  
  let rec server x =
    match branch x with                  (* wait for a request       *)
    | `False x -> close x                     (* close session            *)
    | `True x  -> let n, x = receive x in     (* receive first operand    *)
  	       let m, x = receive x in     (* receive second operand   *)
  	       let x = send (n + m) x in   (* send result              *)
  	       server x                    (* serve more requests      *)
  
  let client cnt =
    start := now ();
    let rec aux acc y n =                (* add n naturals           *)
      if n = 0 then begin
        close (select_false y); acc      (* close session and return *)
      end else
        let y = select_true y in         (* select plus operation    *)
        let y = send acc y in            (* send first operand       *)
        let y = send n y in              (* send second operand      *)
        let res, y = receive y in        (* receive result           *)
        aux res y (n - 1)                (* possibly add more        *)
    in aux 0 cnt
  		      
  let run () =
    let c = fork server in
    ignore (client c cOUNT);               (* run the client           *)
    let countpersec =
      let c = (cOUNT * 4 + 1) * 1000000000 in
      Core.Int63.(of_int c / (now () - !start))
    in
    print_endline (Core.Int63.to_string countpersec)
end

module Z = Make(SessionIPC.Make(SessionBase.NoFlag))
module U = Make(SessionIPC.Make(SessionBase.UnsafeFlag))
module S = Make(SessionIPC.Make(SessionBase.StdMutexFlag))
module N = Make(SessionIPC.Make(SessionBase.NanoMutexFlag))

let _ =
  match Sys.argv.(1).[0] with
  | 'Z' -> Z.run ()
  | 'U' -> U.run ()
  | 'S' -> S.run ()
  | 'N' -> N.run ()
  | _ -> ()
