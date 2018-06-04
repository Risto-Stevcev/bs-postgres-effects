module Pool = BsPostgres.Pool
module Monad_Affect = BsAbstract.Functions.Monad (BsEffects.Affect.Monad)
module Apply_Fn = struct
  module A = BsAbstract.Function.Apply (struct type t = Pool.t end)
  include BsAbstract.Infix.Apply (A)
end
module Apply_Affect =
  BsAbstract.Functions.Apply' (BsEffects.Affect.Apply) (struct type t = BsPostgres.Client.t end)
module Apply_Affect' =
  BsAbstract.Functions.Apply' (BsEffects.Affect.Apply) (struct type t = BsPostgres.Pool.t end)

let (const, (>.)) = BsAbstract.Function.(const, Infix.(>.))
let from_promise = BsEffects.Affect.from_promise
let (<*>) = Apply_Fn.(<*>)
type 'a affect = 'a BsEffects.Affect.affect

let connect = Pool.Promise.connect >. from_promise
and query   = fun sql ?values -> Pool.Promise.query sql ?values >. from_promise
and release = Pool.Pool_Client.release >. from_promise
and end_    = Pool.Promise.end_ >. from_promise

module Chain = struct
  let connect: Pool.t -> Pool.t affect = const >. Monad_Affect.liftM1 <*> connect

  and query sql ?values : Pool.t -> Pool.t affect =
    const >. Monad_Affect.liftM1 <*> query sql ?values

  and query_and_release sql ?values =
    Apply_Affect.apply_second release @@ Client.query sql ?values

  and query_and_end sql ?values =
    Apply_Affect'.apply_second end_ @@ query sql ?values
end
