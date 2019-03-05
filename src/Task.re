type cancel =
  | NoCancel
  | Cancel(unit => unit);

type status('r, 's) =
  | Rejection('r)
  | Success('s)

type computation('error, 'value) = ('error => unit, 'value => unit) => cancel;


type task('rej, 'res) =
    | Task(computation('rej, 'res));

type taskState = {
    cancel: unit => unit,
    index: int
}

type interator('a, 'b) = | Next('a) | Done('b)

type recursiveTask('a, 'b, 'rej) = (('a) => task('rej, interator('a, 'b)), 'a) => task('rej, 'b)

//let defineTask = fn =>
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

let chainRec = (recTask, init) => Task(
  (rej, res) => {
    let cancelFn = ref(() => ())
    let currentValue = ref(init)
    let async = ref(false)
    let settled = ref(false)

    let rec drain = () => {
      let break = ref(false);
      async := false
      while(!break^){
        settled := false
        cancelFn := recTask(currentValue^) |> run(onResponse)
        if(!settled^){
          async := true
          break := true
        }
      }
    }
    and onResponse = status =>
            switch status {
            | Rejection(err) => rej(err)
            | Success(value) => {
                switch value {
                | Next(a) => {
                  currentValue := a
                  if(async^){
                    drain()
                  } else {
                    settled := true
                  }
                }
                | Done(b) => res(b)
                };
              }
            };
    drain()
    Cancel(cancelFn^)
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
let reject = value => Task((rej, _) => { rej(value); NoCancel})

module Operators = {
  let (>==<) = chain
  let (<@>) = map
  let (<!==!>) = chainRej
  let (<!@!>) = mapRej
}

open Operators

// Todo update to control the amount of task run in parrallel
let parallel = concurrentTasks => Task(
    (rej, res) => {
        let taskSize = List.length(concurrentTasks)
        let responses = ref([||])
        let hotTask = ref([||])
        let rejected = ref(false)
        let syncQueue = Queue.create()
        let async = ref(false)
        let onResponse = fun
        | Success(value) => {
                responses :=  Array.append(responses^, [|value|])
                if(Array.length(responses^) === taskSize){
                    responses^ |> Array.sort((a, b) => fst(a) - fst(b))
                    let response = responses^ |>  Array.map(snd) |> Array.to_list
                    hotTask := [||]
                    res(response)
                }
            }
        | Rejection(err) => {
                hotTask^ |> Array.iter(task => task.cancel())
                hotTask := [||]
                rejected := true
                rej(err)
            }

        hotTask := concurrentTasks |> Array.of_list |> Array.mapi(
            (index, task) => {
                cancel: task <@> (value => (index, value)) |> run(value => async^ ? onResponse(value) : Queue.add(value, syncQueue)),
                index: index,
              }
        )

        async := true

        while (!Queue.is_empty(syncQueue) && !rejected^){
            let value = Queue.take(syncQueue)
            onResponse(value)
        }

        Cancel(() => hotTask^ |> Array.iter(task => task.cancel()))
    }
)


//// test
let timeout = value => Task((rej, res) => {
  let timer = Js.Global.setTimeout(() => {res(value)}, 1000)
  Cancel(() => Js.Global.clearTimeout(timer))
})
let notTimeout = value => Task((rej, res) => {
  // Js.log(value)
  res(value)
  NoCancel
})
// let j = ([1, 2, 3, 4, 5, 6, 7, 8, 9] |> List.map(timeout)) @ [notTimeout(10) >==< reject, timeout(11), timeout(12), timeout(13)]
//         |> parallel
//         |> run(fun | Rejection(v) => Js.log(v) | Success(s) => Js.log(s))
let p = ([1, 2, 3, 4, 5, 6, 7, 8, 9] |> List.map(timeout)) @ (Array.make(10000, 1) |> Array.mapi((index, _) => index + 10 ) |> Array.map(notTimeout) |> Array.to_list)
        |> parallel <@> (List.fold_left((a, b) => a + b, 0))
        // |> run(fun | Rejection(v) => Js.log(v) | Success(s) => Js.log(s))

let makeTask = (i) =>
  switch i {
  | i when (i >= 100000000) => identity(Done(i + 1))
  | i when i < 0 => reject("i must be positive")
  | i => identity(Next(i + 1))
  };

let t = p >==< chainRec(makeTask) |> run(fun | Rejection(v) => Js.log(v) | Success(s) => Js.log(s))

// let unsubscribe = identity(5000)
//   >==< (x => identity(x * 5000))
//   <@> (x => x + 300)
//   >==< timeout
//   <!@!> string_of_int
//   // <!!> identity
//   |> run(fun | Rejection(v) => Js.log(v ++ " there was an error") | Success(s) => Js.log(s))
// Js.Global.setTimeout(p, 500)
// todo
// encaseP, encaseN, after, race,

//
  //The eagerness also gives rise to “promise factories” AKA “tasks” (parameter-less function returning Promise). E.g. you can’t simply map an array of values to an array of promises, since they will execute immediately. So if you actually intend to execute them in series or in some more complex way, you end up having to create an array of tasks.

//Fluture looks great! Although I used to just go with “thunks”. Basically just functions taking a Nodeback as the sole parameter and returning a cancellation function. Have even written the node-monad library to be as close as possible to async/await. There are some problems with this approach, like being unable to distinguish between a thunk and a regular function.
 //*/
