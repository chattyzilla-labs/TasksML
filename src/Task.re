type cancel =
  | NoCancel
  | Cancel(unit => unit);

type status('r, 's) =
  | Rejection('r)
  | Success('s)

type computation('error, 'value) = ('error => unit, 'value => unit) => cancel;

type task('rej, 'res) =
    | Task(computation('rej, 'res));

type taskState('r, 's) =
  | Todo
  | Active
  | Resolved(status('r, 's))

let run = (onResponse, Task(task)) => {
    let openend = ref(true);
    let rejection = err => if(openend^){
      openend := false;
      onResponse(Rejection(err))
    };
    let success = res => if(openend^){
      openend := false;
      onResponse(Success(res))
    };
    let cancelFn = task(rejection, success);
    () => {
        if(openend^) {
          openend := false;
          switch cancelFn {
          | Cancel(fn) => fn()
          | NoCancel => ()
          };
        }
    }
}

let chain = (task, fn) => Task(
    (rej, res) => {
        let cancelFn = ref(() => ())
        let onResponse = status =>
            switch status {
            | Rejection(err) => rej(err)
            | Success(value) => {
                cancelFn := fn(value) |> run(
                  status =>
                    switch status {
                    | Rejection(err) => rej(err)
                    | Success(value) => res(value)
                    }
                )
              }
            };
        cancelFn := task |> run(onResponse)
        Cancel(() => {
          cancelFn^()
        })
    }
)

let chainRej = (task, fn) => Task(
    (rej, res) => {
        let cancelFn = ref(() => ())
        let onResponse = status =>
            switch status {
            | Rejection(err) => {
                cancelFn := fn(err) |> run(
                  status =>
                    switch status {
                    | Rejection(err) => rej(err)
                    | Success(value) => res(value)
                    }
                )
              }
            | Success(value) => res(value)
            };
        cancelFn := task |> run(onResponse)
        Cancel(() => {
          cancelFn^()
        })
    }
)

let map = (task, fn) =>  Task(
  (rej, res) => {
        let onResponse = status =>
            switch status {
            | Rejection(err) => rej(err)
            | Success(value) => fn(value) |> res
            };
        let cancel = task |> run(onResponse)
        Cancel(cancel)
    }
)

let mapRej = (task, fn) =>  Task(
  (rej, res) => {
        let onResponse = status =>
            switch status {
            | Rejection(err) => fn(err) |> rej
            | Success(value) => res(value)
            };
        let cancel = task |> run(onResponse)
        Cancel(cancel)
    }
)

let identity = value => Task((_, res) => { res(value); NoCancel})

module Operators = {
  let (>==<) = chain
  let (<@>) = map
  let (<!==!>) = chainRej
  let (<!@!>) = mapRej
}

open Operators

let timeout = value => Task((rej, res) => {
  let timer = Js.Global.setTimeout(() => rej(value), 1000)
  Cancel(() => Js.Global.clearTimeout(timer))
})

let parallel = concurrentTasks => Task((rej, res) => {
  let count = ref(0)
  let tasksLength = concurrentTasks |> Array.length
  let responses = Array.make(tasksLength, Todo)
  let cancelFns = Array.make(tasksLength, NoCancel)
  concurrentTasks |> Array.iteri(
    (i, Task(task)) => {
      let reject = err => {
        Resolved(Rejection(err)) |> Array.set(responses, i)
        responses |> Array.iteri(
          (j, state) =>
            switch state {
            | Active =>
              switch (Array.get(cancelFns, j)) {
              | Cancel(c) => c()
              | NoCancel => ()
              }
            | Todo => ()
            | Resolved(_) => ()
            }
        );
        rej(Rejection(err));
      }
      let resolved = value => {
        Resolved(Success(value)) |> Array.set(responses, i)
        count := count^ + 1
        if(count^ === tasksLength) {

        }
      }

      task(
        reject,
        resolved
      )
    }
  )

  let cancel = () => ()
  Cancel(cancel)
})
let unsubscribe = identity(5000)
  >==< (x => identity(x * 5000))
  <@> (x => x + 300)
  >==< timeout
  <!@!> string_of_int
  // <!!> identity
  |> run(fun | Rejection(v) => Js.log(v ++ " there was an error") | Success(s) => Js.log(s))
//Js.Global.setTimeout(unsubscribe, 1500)
// todo
// encaseP, encaseN, parrallel,after, race, stack saftey

//
  //The eagerness also gives rise to “promise factories” AKA “tasks” (parameter-less function returning Promise). E.g. you can’t simply map an array of values to an array of promises, since they will execute immediately. So if you actually intend to execute them in series or in some more complex way, you end up having to create an array of tasks.

//Fluture looks great! Although I used to just go with “thunks”. Basically just functions taking a Nodeback as the sole parameter and returning a cancellation function. Have even written the node-monad library to be as close as possible to async/await. There are some problems with this approach, like being unable to distinguish between a thunk and a regular function.
 //*/
