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


let run = (Task(task), onResponse) => {
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

let noop = () => ()

let chain = (task, fn) =>
  Task(
    (rej, res) => {
      let cancelFn = ref(noop);
      let onResponse = status =>
        switch (status) {
        | Rejection(err) => rej(err)
        | Success(value) =>
          cancelFn :=
            fn(value)
            -> run(status =>
                 switch (status) {
                 | Rejection(err) => rej(err)
                 | Success(value) => res(value)
                 }
               )
        };
      cancelFn := task -> run(onResponse);
      Cancel(() => cancelFn^());
    },
  );

let bind = chain

let chainRec = (recTask, init) =>
  Task(
    (rej, res) => {
      let cancelFn = ref(noop);
      let currentValue = ref(init);
      let async = ref(false);
      let settled = ref(false);

      let rec drain = () => {
        let break = ref(false);
        async := false;
        while (! break^) {
          settled := false;
          cancelFn := recTask(currentValue^) -> run(onResponse);
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
      let cancelFn = ref(noop);
      let onResponse = status =>
        switch (status) {
        | Rejection(err) =>
          cancelFn :=
            fn(err)
            -> run(status =>
                 switch (status) {
                 | Rejection(err) => rej(err)
                 | Success(value) => res(value)
                 }
               )
        | Success(value) => res(value)
        };
      cancelFn := task -> run(onResponse);
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
      let cancel = task -> run(onResponse);
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
      let cancel = task -> run(onResponse);
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
      let cancel = task -> run(onResponse);
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
      let cancel = task -> run(onResponse);
      Cancel(cancel);
    },
  );

let also = (task1, task2) =>
  Task(
    (rej, res) => {
      let cancelFn = ref(noop);
      let onResponse = status =>
        switch (status) {
        | Rejection(err) => rej(err)
        | Success(_) =>
          cancelFn :=
            task2
            -> run(status =>
                 switch (status) {
                 | Rejection(err) => rej(err)
                 | Success(value) => res(value)
                 }
               )
        };
      cancelFn := task1 -> run(onResponse);
      Cancel(() => cancelFn^());
    },
  );

let alt = (task1, task2) =>
  Task(
    (rej, res) => {
      let cancelFn = ref(noop);
      let onResponse = status =>
        switch (status) {
        | Success(value) => res(value)
        | Rejection(_) =>
          cancelFn :=
            task2
            -> run(status =>
                 switch (status) {
                 | Rejection(err) => rej(err)
                 | Success(value) => res(value)
                 }
               )
        };
      cancelFn := task1 -> run(onResponse);
      Cancel(() => cancelFn^());
    },
  );

let finally = (task1, task2) =>
  Task(
    (rej, res) => {
      let cancelFn = ref(noop);
      let onResponse = status1 => {
        cancelFn :=
          task2
          -> run(status =>
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
      cancelFn := task1 -> run(onResponse);
      Cancel(() => cancelFn^());
    },
  );

let hook = (acquire, dispose, consume) =>
  Task(
    (rej, res) => {
      let cancelFn = ref(noop)
      let onAquire = status =>
        switch status {
        | Rejection(err) => rej(err)
        | Success(resource) => {
            let runDispose = run(_, fun | Rejection(err) => raise(err) | Success(_) => ())
            let onResponse = status =>
              switch status {
              | Rejection(err) => {
                  rej(err)
                  dispose(resource) -> runDispose |> ignore
                }
              | Success(value) => {
                  res(value)
                  dispose(resource) -> runDispose |> ignore
                }
              };
            let cancelConsumer = consume(resource) -> run(onResponse)
            cancelFn := () => {
              cancelConsumer()
              dispose(resource) -> runDispose |> ignore
            }
          }
        };
      cancelFn := acquire -> run(onAquire)
      Cancel(() => cancelFn^())
    }
  )

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

let race = (task1, task2) =>
  Task(
    (rej, res) => {
      let cancelTask1 = ref(noop)
      let cancelTask2 = ref(noop)
      cancelTask1 := task1 -> run(
        fun
        | Rejection(err) => { rej(err); cancelTask2^();}
        | Success(value) => { res(value); cancelTask2^();}
      )
      cancelTask2 := task2 -> run(
        fun
        | Rejection(err) => { rej(err); cancelTask1^();}
        | Success(value) => { res(value); cancelTask1^();}
      )
      Cancel(() => { cancelTask1^(); cancelTask2^(); })
    }
  )

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
                 -> map(value => (index, value))
                 -> run(value =>
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

let both = ((task1, task2)) =>
  Task(
    (rej, res) => {
      let task1Res = ref(None)
      let task2Res = ref(None)
      let task1Cancel = ref(noop)
      let task2Cancel = ref(noop)
      let onResponse1 = (response) => {
        switch response {
        | Success(value) =>
          switch task2Res^ {
          | None => {
              task1Res := Some(value);
              ()
            }
          | Some(task2Value) => res((value, task2Value))
          }
        | Rejection(err) => {
            task2Cancel^()
            rej(err)
          }
        }
      }
      let onResponse2 = (response) => {
        switch response {
        | Success(value) =>
          switch task1Res^ {
          | None => {
              task2Res := Some(value)
            }
          | Some(task1Value) => res((task1Value, value))
          }
        | Rejection(err) => {
            task1Cancel^()
            rej(err)
          }
        }
      }

      task1Cancel := task1 -> run(onResponse1)
      task2Cancel := task2 -> run(onResponse2)

      Cancel(() => { task1Cancel^(); task2Cancel^(); })
    }
  )

let triple = ((task1, task2, task3)) =>
  Task(
    (rej, res) => {
      let task1Res = ref(None)
      let task2Res = ref(None)
      let task1Cancel = ref(noop)
      let task2Cancel = ref(noop)
      let onResponse1 = (response) => {
        switch response {
        | Success(value) =>
          switch task2Res^ {
          | None => {
              task1Res := Some(value);
              ()
            }
          | Some(task2Value) => res((fst(value), snd(value), task2Value))
          }
        | Rejection(err) => {
            task2Cancel^()
            rej(err)
          }
        }
      }
      let onResponse2 = (response) => {
        switch response {
        | Success(value) =>
          switch task1Res^ {
          | None => {
              task2Res := Some(value)
            }
          | Some(task1Value) => res((fst(task1Value), snd(task1Value), value))
          }
        | Rejection(err) => {
            task1Cancel^()
            rej(err)
          }
        }
      }

      task1Cancel := both((task1, task2)) -> run(onResponse1)
      task2Cancel := task3 -> run(onResponse2)

      Cancel(() => { task1Cancel^(); task2Cancel^(); })
    }
  )

let quadruple = ((task1, task2, task3, task4)) =>
  Task(
    (rej, res) => {
      let task1Res = ref(None)
      let task2Res = ref(None)
      let task1Cancel = ref(noop)
      let task2Cancel = ref(noop)
      let onResponse1 = (response) => {
        switch response {
        | Success(value) =>
          switch task2Res^ {
          | None => {
              task1Res := Some(value);
              ()
            }
          | Some(task2Value) => res((fst(value), snd(value), fst(task2Value), snd(task2Value)))
          }
        | Rejection(err) => {
            task2Cancel^()
            rej(err)
          }
        }
      }
      let onResponse2 = (response) => {
        switch response {
        | Success(value) =>
          switch task1Res^ {
          | None => {
              task2Res := Some(value)
            }
          | Some(task1Value) => res((fst(task1Value), snd(task1Value), fst(value), snd(value)))
          }
        | Rejection(err) => {
            task1Cancel^()
            rej(err)
          }
        }
      }

      task1Cancel := both((task1, task2)) -> run(onResponse1)
      task2Cancel := both((task3, task4)) -> run(onResponse2)

      Cancel(() => { task1Cancel^(); task2Cancel^(); })
    }
  )

let quintuple = ((task1, task2, task3, task4, task5)) =>
  Task(
    (rej, res) => {
      let task1Res = ref(None)
      let task2Res = ref(None)
      let task1Cancel = ref(noop)
      let task2Cancel = ref(noop)
      let onResponse1 = (response) => {
        switch response {
        | Success(value) =>
          switch task2Res^ {
          | None => {
              task1Res := Some(value);
              ()
            }
          | Some(task2Value) => {
            let (a,b,c,d) = value
            res((a, b, c, d, task2Value))
          }
          }
        | Rejection(err) => {
            task2Cancel^()
            rej(err)
          }
        }
      }
      let onResponse2 = (response) => {
        switch response {
        | Success(value) =>
          switch task1Res^ {
          | None => {
              task2Res := Some(value)
            }
          | Some(task1Value) => {
            let (a,b,c,d) = task1Value
            res((a, b, c, d, value))
          }
          }
        | Rejection(err) => {
            task1Cancel^()
            rej(err)
          }
        }
      }

      task1Cancel := quadruple((task1, task2, task3, task4)) -> run(onResponse1)
      task2Cancel := task5 -> run(onResponse2)

      Cancel(() => { task1Cancel^(); task2Cancel^(); })
    }
  )

let sextuple = ((task1, task2, task3, task4, task5, task6)) =>
  Task(
    (rej, res) => {
      let task1Res = ref(None)
      let task2Res = ref(None)
      let task1Cancel = ref(noop)
      let task2Cancel = ref(noop)
      let onResponse1 = (response) => {
        switch response {
        | Success(value) =>
          switch task2Res^ {
          | None => {
              task1Res := Some(value);
              ()
            }
          | Some(task2Value) => {
            let (a,b,c,d, e) = value
            res((a, b, c, d, e, task2Value))
          }
          }
        | Rejection(err) => {
            task2Cancel^()
            rej(err)
          }
        }
      }
      let onResponse2 = (response) => {
        switch response {
        | Success(value) =>
          switch task1Res^ {
          | None => {
              task2Res := Some(value)
            }
          | Some(task1Value) => {
            let (a,b,c,d, e) = task1Value
            res((a, b, c, d, e, value))
          }
          }
        | Rejection(err) => {
            task1Cancel^()
            rej(err)
          }
        }
      }

      task1Cancel := quintuple((task1, task2, task3, task4, task5)) -> run(onResponse1)
      task2Cancel := task6 -> run(onResponse2)

      Cancel(() => { task1Cancel^(); task2Cancel^(); })
    }
  )

module Operators = {
  let (>>=) = chain;
  let (>>@) = map;
  let (>>!) = chainRej;
  let (>>@!) = mapRej;
  let (>>>) = run;
};

