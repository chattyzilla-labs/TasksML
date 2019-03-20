open Task
let encaseP = (promiseFn, param) => 
  Task(
    (rej, res) => {
      promiseFn(param) |> Js.Promise.then_(
        pResp => res(pResp) |> Js.Promise.resolve
      ) |> Js.Promise.catch(error => rej(error) |> Js.Promise.resolve) |> ignore
      NoCancel
    }
  )
let encaseP2 = (promiseFn, param1, param2) => 
  Task(
    (rej, res) => {
      promiseFn(param1, param2) |> Js.Promise.then_(
        pResp => res(pResp) |> Js.Promise.resolve
      ) |> Js.Promise.catch(error => rej(error) |> Js.Promise.resolve) |> ignore
      NoCancel
    }
  )

let encaseP3 = (promiseFn, param1, param2, param3) => 
  Task(
    (rej, res) => {
      promiseFn(param1, param2, param3) |> Js.Promise.then_(
        pResp => res(pResp) |> Js.Promise.resolve
      ) |> Js.Promise.catch(error => rej(error) |> Js.Promise.resolve) |> ignore
      NoCancel
    }
  )

let toPromise = task => 
  Js.Promise.make(
    (~resolve, ~reject as _) => 
      task |> run(value => resolve(. value)) |> ignore
  )

let after = (wait, value) =>
  Task(
    (_, res) => {
      let timer = Js.Global.setTimeout(() => res(value), wait)
      Cancel(() => Js.Global.clearTimeout(timer))
    }
  )

let rejectAfter = (wait, value) =>
  Task(
    (rej, _) => {
      let timer = Js.Global.setTimeout(() => rej(value), wait)
      Cancel(() => Js.Global.clearTimeout(timer))
    }
  )