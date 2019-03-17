type cancel =
  | NoCancel
  | Cancel(unit => unit);

type status('r, 's) =
  | Rejection('r)
  | Success('s);

type computation('error, 'value) = ('error => unit, 'value => unit) => cancel;

type task('rej, 'res) =
  | Task(computation('rej, 'res));

type taskState = {
  cancel: unit => unit,
  index: int,
};

type interator('a, 'b) =
  | Next('a)
  | Done('b);


let run = (onResponse, Task(task)) => {
  let openend = ref(true);
  let rejection = err =>
    if (openend^) {
      openend := false;
      onResponse(Rejection(err));
    };
  let success = res =>
    if (openend^) {
      openend := false;
      onResponse(Success(res));
    };
  let cancelFn = task(rejection, success);
  () =>
    if (openend^) {
      openend := false;
      switch (cancelFn) {
      | Cancel(fn) => fn()
      | NoCancel => ()
      };
    };
};

let chain = (task, fn) =>
  Task(
    (rej, res) => {
      let cancelFn = ref(() => ());
      let onResponse = status =>
        switch (status) {
        | Rejection(err) => rej(err)
        | Success(value) =>
          cancelFn :=
            fn(value)
            |> run(status =>
                 switch (status) {
                 | Rejection(err) => rej(err)
                 | Success(value) => res(value)
                 }
               )
        };
      cancelFn := task |> run(onResponse);
      Cancel(() => cancelFn^());
    },
  );

let bind = chain

let chainRec = (recTask, init) =>
  Task(
    (rej, res) => {
      let cancelFn = ref(() => ());
      let currentValue = ref(init);
      let async = ref(false);
      let settled = ref(false);

      let rec drain = () => {
        let break = ref(false);
        async := false;
        while (! break^) {
          settled := false;
          cancelFn := recTask(currentValue^) |> run(onResponse);
          if (! settled^) {
            async := true;
            break := true;
          };
        };
      }
      and onResponse = status =>
        switch (status) {
        | Rejection(err) => rej(err)
        | Success(value) =>
          switch (value) {
          | Next(a) =>
            currentValue := a;
            if (async^) {
              drain();
            } else {
              settled := true;
            };
          | Done(b) => res(b)
          }
        };
      drain();
      Cancel(() => cancelFn^());
    },
  );

let chainRej = (task, fn) =>
  Task(
    (rej, res) => {
      let cancelFn = ref(() => ());
      let onResponse = status =>
        switch (status) {
        | Rejection(err) =>
          cancelFn :=
            fn(err)
            |> run(status =>
                 switch (status) {
                 | Rejection(err) => rej(err)
                 | Success(value) => res(value)
                 }
               )
        | Success(value) => res(value)
        };
      cancelFn := task |> run(onResponse);
      Cancel(() => cancelFn^());
    },
  );

let map = (task, fn) =>
  Task(
    (rej, res) => {
      let onResponse = status =>
        switch (status) {
        | Rejection(err) => rej(err)
        | Success(value) => fn(value) |> res
        };
      let cancel = task |> run(onResponse);
      Cancel(cancel);
    },
  );

let mapRej = (task, fn) =>
  Task(
    (rej, res) => {
      let onResponse = status =>
        switch (status) {
        | Rejection(err) => fn(err) |> rej
        | Success(value) => res(value)
        };
      let cancel = task |> run(onResponse);
      Cancel(cancel);
    },
  );

let bimap = (task, rejMap, resMap) =>
  Task(
    (rej, res) => {
      let onResponse = status =>
        switch (status) {
        | Rejection(err) => rejMap(err) -> rej
        | Success(value) => resMap(value) -> res
        };
      let cancel = task |> run(onResponse);
      Cancel(cancel);
    },
  );

let fold = (task, rejMap, resMap) =>
  Task(
    (_, res) => {
      let onResponse = status =>
        switch (status) {
        | Rejection(err) => rejMap(err) -> res
        | Success(value) => resMap(value) -> res
        };
      let cancel = task |> run(onResponse);
      Cancel(cancel);
    },
  );

let also = (task1, task2) =>
  Task(
    (rej, res) => {
      let cancelFn = ref(() => ());
      let onResponse = status =>
        switch (status) {
        | Rejection(err) => rej(err)
        | Success(_) =>
          cancelFn :=
            task2
            |> run(status =>
                 switch (status) {
                 | Rejection(err) => rej(err)
                 | Success(value) => res(value)
                 }
               )
        };
      cancelFn := task1 |> run(onResponse);
      Cancel(() => cancelFn^());
    },
  );

let alt = (task1, task2) =>
  Task(
    (rej, res) => {
      let cancelFn = ref(() => ());
      let onResponse = status =>
        switch (status) {
        | Success(value) => res(value)
        | Rejection(_) =>
          cancelFn :=
            task2
            |> run(status =>
                 switch (status) {
                 | Rejection(err) => rej(err)
                 | Success(value) => res(value)
                 }
               )
        };
      cancelFn := task1 |> run(onResponse);
      Cancel(() => cancelFn^());
    },
  );

let finally = (task1, task2) =>
  Task(
    (rej, res) => {
      let cancelFn = ref(() => ());
      let onResponse = status1 => {
        cancelFn :=
          task2
          |> run(status =>
                switch (status) {
                | Rejection(err) => rej(err)
                | Success(_) =>
                  switch (status1) {
                  | Rejection(err) => rej(err)
                  | Success(value) => res(value)
                  }
                }
              )
      }
      cancelFn := task1 |> run(onResponse);
      Cancel(() => cancelFn^());
    },
  );

let pure = value =>
  Task(
    (_, res) => {
      res(value);
      NoCancel;
    },
  );

let resolve = pure

let reject = value =>
  Task(
    (rej, _) => {
      rej(value);
      NoCancel;
    },
  );

module Operators = {
  let (>==<) = chain;
  let (<@>) = map;
  let (<!==!>) = chainRej;
  let (<!@!>) = mapRej;
};

open Operators;

let parallel = concurrentTasks =>
  Task(
    (rej, res) => {
      let taskSize = List.length(concurrentTasks);
      let responses = ref([||]);
      let hotTask = ref([||]);
      let rejected = ref(false);
      let syncQueue = Queue.create();
      let async = ref(false);
      let onResponse =
        fun
        | Success(value) => {
            responses := Array.append(responses^, [|value|]);
            if (Array.length(responses^) === taskSize) {
              responses^ |> Array.sort((a, b) => fst(a) - fst(b));
              let response = responses^ |> Array.map(snd) |> Array.to_list;
              hotTask := [||];
              res(response);
            };
          }
        | Rejection(err) => {
            hotTask^ |> Array.iter(task => task.cancel());
            hotTask := [||];
            rejected := true;
            rej(err);
          };

      hotTask :=
        concurrentTasks
        |> Array.of_list
        |> Array.mapi((index, task) =>
             {
               cancel:
                 task
                 <@> (value => (index, value))
                 |> run(value =>
                      async^ ?
                        onResponse(value) : Queue.add(value, syncQueue)
                    ),
               index,
             }
           );

      async := true;

      while (!Queue.is_empty(syncQueue) && ! rejected^) {
        let value = Queue.take(syncQueue);
        onResponse(value);
      };

      Cancel(() => hotTask^ |> Array.iter(task => task.cancel()));
    },
  );

let timeout = value =>
  Task(
    (_, res) => {
      let timer = Js.Global.setTimeout(() => res(value), 1000);
      Cancel(() => Js.Global.clearTimeout(timer));
    },
  );
let notTimeout = value =>
  Task(
    (_, res) => {
      res(value);
      NoCancel;
    },
  );
let p =
  ([1, 2, 3, 4, 5, 6, 7, 8, 9] |> List.map(timeout))
  @ (
    Array.make(10000, 1)
    |> Array.mapi((index, _) => index + 10)
    |> Array.map(notTimeout)
    |> Array.to_list
  )
  |> parallel
  <@> List.fold_left((a, b) => a + b, 0)

let makeTask = i =>
  switch (i) {
  | i when i >= 100000 => (i + 1) -> Done |> pure
  | i when i < 0 => reject("i must be positive")
  | i => pure(Next(i + 1))
  };

let t =
  p
  >==< chainRec(makeTask)
  |> run(
       fun
       | Rejection(v) => Js.log(v)
       | Success(s) => Js.log(s),
     );

// TODO: add hook method, both, triple, quadruple, quintuple, sextuple,
