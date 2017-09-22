let start = ref @@ Core.Int63.of_int 0
let now () = (Core.Time_stamp_counter.now () :> Core.Int63.t)
      
module Make(S : SessionBase.S) = struct
  open S.Bare
  
  let cOUNT = 8000
  
  let rec server x =
    match branch x with                  (* wait for a request       *)
    | `False x -> close x                     (* close session            *)
    | `True x  -> let n, x = receive x in     (* receive first operand    *)
               let y, x = receive x in
  	       let m, y = receive y in     (* receive second operand   *)
  	       let y = send (n + m) y in   (* send result              *)
               close y;
  	       server x                    (* serve more requests      *)
  
  let client cnt =
    start := now ();
    let rec aux acc y n =                (* add n naturals           *)
      if n = 0 then begin
        close (select_false y); acc      (* close session and return *)
      end else
        let y = select_true y in         (* select plus operation    *)
        let y = send acc y in            (* send first operand       *)
        let z, w = create () in
        let y = send w y in
        let z = send n z in              (* send second operand      *)
        let res, z = receive z in        (* receive result           *)
        close z;
        aux res y (n - 1)                (* possibly add more        *)
    in aux 0 cnt
  		      
  let run () =
    let a, b = create () in                (* create the session       *)
    let _ = Thread.create server a in      (* spawn the server         *)
    ignore (client b cOUNT);               (* run the client           *)
    let countpersec =
      let c = (cOUNT * 5 + 1) * 1000000000 in
      Core.Int63.(of_int c / (now () - !start))
    in
    print_endline (Core.Int63.to_string countpersec)
end

module N = Make(SessionBase.Make(SessionBase.UnsafeChannel)(SessionBase.NanoMutexFlag))
module NA = Make(SessionBase.Make(SessionBase.BufferedUnsafeChannel)(SessionBase.NanoMutexFlag))
module S = Make(SessionBase.Make(SessionBase.UnsafeChannel)(SessionBase.StdMutexFlag))
module SA = Make(SessionBase.Make(SessionBase.BufferedUnsafeChannel)(SessionBase.StdMutexFlag))

let _ =
  match Sys.argv.(1) with
  | "N" -> N.run ()
  | "NA" -> NA.run ()
  | "S" -> S.run ()
  | "SA" -> SA.run ()
  | _ -> ()
