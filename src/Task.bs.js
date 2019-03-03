// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Queue = require("bs-platform/lib/js/queue.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");

function run(onResponse, param) {
  var openend = /* record */[/* contents */true];
  var rejection = function (err) {
    if (openend[0]) {
      openend[0] = false;
      return Curry._1(onResponse, /* Rejection */Block.__(0, [err]));
    } else {
      return 0;
    }
  };
  var success = function (res) {
    if (openend[0]) {
      openend[0] = false;
      return Curry._1(onResponse, /* Success */Block.__(1, [res]));
    } else {
      return 0;
    }
  };
  var cancelFn = Curry._2(param[0], rejection, success);
  return (function (param) {
      if (openend[0]) {
        openend[0] = false;
        if (cancelFn) {
          return Curry._1(cancelFn[0], /* () */0);
        } else {
          return /* () */0;
        }
      } else {
        return 0;
      }
    });
}

function chain(task, fn) {
  return /* Task */[(function (rej, res) {
              var cancelFn = /* record */[/* contents */(function (param) {
                    return /* () */0;
                  })];
              var onResponse = function (status) {
                if (status.tag) {
                  cancelFn[0] = run((function (status) {
                          if (status.tag) {
                            return Curry._1(res, status[0]);
                          } else {
                            return Curry._1(rej, status[0]);
                          }
                        }), Curry._1(fn, status[0]));
                  return /* () */0;
                } else {
                  return Curry._1(rej, status[0]);
                }
              };
              cancelFn[0] = run(onResponse, task);
              return /* Cancel */[(function (param) {
                          return Curry._1(cancelFn[0], /* () */0);
                        })];
            })];
}

function chainRej(task, fn) {
  return /* Task */[(function (rej, res) {
              var cancelFn = /* record */[/* contents */(function (param) {
                    return /* () */0;
                  })];
              var onResponse = function (status) {
                if (status.tag) {
                  return Curry._1(res, status[0]);
                } else {
                  cancelFn[0] = run((function (status) {
                          if (status.tag) {
                            return Curry._1(res, status[0]);
                          } else {
                            return Curry._1(rej, status[0]);
                          }
                        }), Curry._1(fn, status[0]));
                  return /* () */0;
                }
              };
              cancelFn[0] = run(onResponse, task);
              return /* Cancel */[(function (param) {
                          return Curry._1(cancelFn[0], /* () */0);
                        })];
            })];
}

function map(task, fn) {
  return /* Task */[(function (rej, res) {
              var onResponse = function (status) {
                if (status.tag) {
                  return Curry._1(res, Curry._1(fn, status[0]));
                } else {
                  return Curry._1(rej, status[0]);
                }
              };
              var cancel = run(onResponse, task);
              return /* Cancel */[cancel];
            })];
}

function mapRej(task, fn) {
  return /* Task */[(function (rej, res) {
              var onResponse = function (status) {
                if (status.tag) {
                  return Curry._1(res, status[0]);
                } else {
                  return Curry._1(rej, Curry._1(fn, status[0]));
                }
              };
              var cancel = run(onResponse, task);
              return /* Cancel */[cancel];
            })];
}

function identity(value) {
  return /* Task */[(function (param, res) {
              Curry._1(res, value);
              return /* NoCancel */0;
            })];
}

var Operators = /* module */[
  /* >==< */chain,
  /* <@> */map,
  /* <!==!> */chainRej,
  /* <!@!> */mapRej
];

function parallel(concurrentTasks) {
  return /* Task */[(function (rej, res) {
              var responses = /* record */[/* contents : [] */0];
              var hotTask = /* record */[/* contents : [] */0];
              var rejected = /* record */[/* contents */false];
              var syncQueue = Queue.create(/* () */0);
              var async = /* record */[/* contents */false];
              var onResponse = function (param) {
                if (param.tag) {
                  var value = param[0];
                  responses[0] = /* :: */[
                    value,
                    responses[0]
                  ];
                  hotTask[0] = List.map((function (task) {
                          var match = task[/* index */2] === value[0];
                          if (match) {
                            return /* record */[
                                    /* cancel */task[/* cancel */0],
                                    /* resolved */true,
                                    /* index */task[/* index */2]
                                  ];
                          } else {
                            return task;
                          }
                        }), hotTask[0]);
                  if (List.for_all((function (task) {
                            return task[/* resolved */1];
                          }), hotTask[0])) {
                    var response = List.map((function (prim) {
                            return prim[1];
                          }), List.sort((function (a, b) {
                                return a[0] - b[0] | 0;
                              }), responses[0]));
                    hotTask[0] = /* [] */0;
                    return Curry._1(res, response);
                  } else {
                    return 0;
                  }
                } else {
                  List.iter((function (task) {
                          return Curry._1(task[/* cancel */0], /* () */0);
                        }), hotTask[0]);
                  hotTask[0] = /* [] */0;
                  rejected[0] = true;
                  return Curry._1(rej, param[0]);
                }
              };
              hotTask[0] = List.mapi((function (index, task) {
                      return /* record */[
                              /* cancel */run((function (value) {
                                      var match = async[0];
                                      if (match) {
                                        return onResponse(value);
                                      } else {
                                        return Queue.add(/* tuple */[
                                                    value,
                                                    onResponse
                                                  ], syncQueue);
                                      }
                                    }), map(task, (function (value) {
                                          return /* tuple */[
                                                  index,
                                                  value
                                                ];
                                        }))),
                              /* resolved */false,
                              /* index */index
                            ];
                    }), concurrentTasks);
              async[0] = true;
              while(!Queue.is_empty(syncQueue) && !rejected[0]) {
                var match = Queue.take(syncQueue);
                Curry._1(match[1], match[0]);
              };
              return /* Cancel */[(function (param) {
                          return List.iter((function (task) {
                                        return Curry._1(task[/* cancel */0], /* () */0);
                                      }), hotTask[0]);
                        })];
            })];
}

function timeout(value) {
  return /* Task */[(function (rej, res) {
              var timer = setTimeout((function (param) {
                      return Curry._1(rej, value);
                    }), 1000);
              return /* Cancel */[(function (param) {
                          clearTimeout(timer);
                          return /* () */0;
                        })];
            })];
}

var unsubscribe = run((function (param) {
        if (param.tag) {
          console.log(param[0]);
          return /* () */0;
        } else {
          console.log(param[0] + " there was an error");
          return /* () */0;
        }
      }), mapRej(chain(map(chain(/* Task */[(function (param, res) {
                          Curry._1(res, 5000);
                          return /* NoCancel */0;
                        })], (function (x) {
                        var value = Caml_int32.imul(x, 5000);
                        return /* Task */[(function (param, res) {
                                    Curry._1(res, value);
                                    return /* NoCancel */0;
                                  })];
                      })), (function (x) {
                    return x + 300 | 0;
                  })), timeout), (function (prim) {
            return String(prim);
          })));

exports.run = run;
exports.chain = chain;
exports.chainRej = chainRej;
exports.map = map;
exports.mapRej = mapRej;
exports.identity = identity;
exports.Operators = Operators;
exports.parallel = parallel;
exports.timeout = timeout;
exports.unsubscribe = unsubscribe;
/* unsubscribe Not a pure module */
