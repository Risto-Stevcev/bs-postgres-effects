module Client = BsPostgres.Client
module Monad_Affect = BsAbstract.Functions.Monad (BsEffects.Affect.Monad)
module Apply_Fn = struct
  module A = BsAbstract.Function.Apply (struct type t = Client.t end)
  include BsAbstract.Infix.Apply (A)
end
module Apply_Affect =
  BsAbstract.Functions.Apply' (BsEffects.Affect.Apply) (struct type t = BsPostgres.Client.t end)

let (const, (>.)) = BsAbstract.Function.(const, Infix.(>.))
let from_promise = BsEffects.Affect.from_promise
let (<*>) = Apply_Fn.(<*>)
type 'a affect = 'a BsEffects.Affect.affect

let connect = Client.Promise.connect >. from_promise
and query   = fun sql ?values -> Client.Promise.query sql ?values >. from_promise
and end_    = Client.Promise.end_ >. from_promise

module Chain = struct
  let connect: Client.t -> Client.t affect = const >. Monad_Affect.liftM1 <*> connect

  and query sql ?values : Client.t -> Client.t affect =
    const >. Monad_Affect.liftM1 <*> query sql ?values

  let query_and_end sql ?values =
    Apply_Affect.apply_second end_ @@ query sql ?values
end
