open Lwt.Syntax
open Functional
open Choreography
open Freer
open Location

module MaybeLwt : sig
  include MonadS with type 'a t = 'a option Lwt.t

  val none : 'a t
end = struct
  type 'a t = 'a option Lwt.t

  let map f x =
    let* x = x in
    Option.map f x |> Lwt.return

  let return x = Some x |> Lwt.return
  let none = Lwt.return None

  let ( >>= ) x f =
    let* x = x in
    match x with None -> none | Some x -> f x

  let ( <*> ) f x =
    let* f = f and* x = x in
    match f with None -> none | Some f -> Option.map f x |> Lwt.return
end

module type TcpHandlerS =
  Freer.HandlerS
    with type 'a boxt = 'a BaseChoreo.t
     and type 'a mont = 'a MaybeLwt.t

let make_handler (type a) (loc : a locMod) : (module TcpHandlerS) =
  let module TcpHandler :
    Freer.HandlerS
      with type 'a boxt = 'a BaseChoreo.t
       and type 'a mont = 'a MaybeLwt.t = struct
    type 'a boxt = 'a BaseChoreo.t
    type 'a mont = 'a MaybeLwt.t

    include MaybeLwt

    let uuid (type a) (l : a locMod) =
      let module L = (val l) in
      L.uuid

    let other_location (type b) (l : b locMod) : bool = uuid l != uuid loc

    let handler : type a. a boxt -> a mont =
     fun box ->
      match box with
      | BaseChoreo.Var (l, v) -> return (wrap v l)
      | BaseChoreo.App (f, v) ->
          let l = unwrap_loc f in
          if other_location l then none
          else return (wrap ((unwrap_val f) (unwrap_val v)) l)
      | BaseChoreo.Comm (l, v) ->
          if other_location l then
            let* () = Utils.create_sender (unwrap_val v) (uuid loc) (uuid l) in
            MaybeLwt.none
          else
            let* res = Utils.create_receiver (uuid loc) (uuid (unwrap_loc v)) in
            wrap res l |> return
  end in
  (module TcpHandler)

let epp chor loc =
  let (module TcpHandler) = make_handler loc in
  let module TcpInterpreter = Freer.MakeInterp (Choreo) (TcpHandler) in
  TcpInterpreter.interpFreer chor
