// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Queue = require("bs-platform/lib/js/queue.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");

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

function chainRec(recTask, init) {
  return /* Task */[(function (rej, res) {
              var cancelFn = /* record */[/* contents */(function (param) {
                    return /* () */0;
                  })];
              var currentValue = /* record */[/* contents */init];
              var async = /* record */[/* contents */false];
              var settled = /* record */[/* contents */false];
              var drain = function (param) {
                var $$break = false;
                async[0] = false;
                while(!$$break) {
                  settled[0] = false;
                  cancelFn[0] = run(onResponse, Curry._1(recTask, currentValue[0]));
                  if (!settled[0]) {
                    async[0] = true;
                    $$break = true;
                  }
                  
                };
                return /* () */0;
              };
              var onResponse = function (status) {
                if (status.tag) {
                  var value = status[0];
                  if (value.tag) {
                    return Curry._1(res, value[0]);
                  } else {
                    currentValue[0] = value[0];
                    if (async[0]) {
                      return drain(/* () */0);
                    } else {
                      settled[0] = true;
                      return /* () */0;
                    }
                  }
                } else {
                  return Curry._1(rej, status[0]);
                }
              };
              drain(/* () */0);
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

function bimap(task, rejMap, resMap) {
  return /* Task */[(function (rej, res) {
              var onResponse = function (status) {
                if (status.tag) {
                  return Curry._1(res, Curry._1(resMap, status[0]));
                } else {
                  return Curry._1(rej, Curry._1(rejMap, status[0]));
                }
              };
              var cancel = run(onResponse, task);
              return /* Cancel */[cancel];
            })];
}

function fold(task, rejMap, resMap) {
  return /* Task */[(function (param, res) {
              var onResponse = function (status) {
                if (status.tag) {
                  return Curry._1(res, Curry._1(resMap, status[0]));
                } else {
                  return Curry._1(res, Curry._1(rejMap, status[0]));
                }
              };
              var cancel = run(onResponse, task);
              return /* Cancel */[cancel];
            })];
}

function also(task1, task2) {
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
                        }), task2);
                  return /* () */0;
                } else {
                  return Curry._1(rej, status[0]);
                }
              };
              cancelFn[0] = run(onResponse, task1);
              return /* Cancel */[(function (param) {
                          return Curry._1(cancelFn[0], /* () */0);
                        })];
            })];
}

function alt(task1, task2) {
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
                        }), task2);
                  return /* () */0;
                }
              };
              cancelFn[0] = run(onResponse, task1);
              return /* Cancel */[(function (param) {
                          return Curry._1(cancelFn[0], /* () */0);
                        })];
            })];
}

function $$finally(task1, task2) {
  return /* Task */[(function (rej, res) {
              var cancelFn = /* record */[/* contents */(function (param) {
                    return /* () */0;
                  })];
              var onResponse = function (status1) {
                cancelFn[0] = run((function (status) {
                        if (status.tag) {
                          if (status1.tag) {
                            return Curry._1(res, status1[0]);
                          } else {
                            return Curry._1(rej, status1[0]);
                          }
                        } else {
                          return Curry._1(rej, status[0]);
                        }
                      }), task2);
                return /* () */0;
              };
              cancelFn[0] = run(onResponse, task1);
              return /* Cancel */[(function (param) {
                          return Curry._1(cancelFn[0], /* () */0);
                        })];
            })];
}

function pure(value) {
  return /* Task */[(function (param, res) {
              Curry._1(res, value);
              return /* NoCancel */0;
            })];
}

function reject(value) {
  return /* Task */[(function (rej, param) {
              Curry._1(rej, value);
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
              var taskSize = List.length(concurrentTasks);
              var responses = /* record */[/* contents : array */[]];
              var hotTask = /* record */[/* contents : array */[]];
              var rejected = /* record */[/* contents */false];
              var syncQueue = Queue.create(/* () */0);
              var async = /* record */[/* contents */false];
              var onResponse = function (param) {
                if (param.tag) {
                  responses[0] = $$Array.append(responses[0], /* array */[param[0]]);
                  if (responses[0].length === taskSize) {
                    $$Array.sort((function (a, b) {
                            return a[0] - b[0] | 0;
                          }), responses[0]);
                    var response = $$Array.to_list($$Array.map((function (prim) {
                                return prim[1];
                              }), responses[0]));
                    hotTask[0] = /* array */[];
                    return Curry._1(res, response);
                  } else {
                    return 0;
                  }
                } else {
                  $$Array.iter((function (task) {
                          return Curry._1(task[/* cancel */0], /* () */0);
                        }), hotTask[0]);
                  hotTask[0] = /* array */[];
                  rejected[0] = true;
                  return Curry._1(rej, param[0]);
                }
              };
              hotTask[0] = $$Array.mapi((function (index, task) {
                      return /* record */[
                              /* cancel */run((function (value) {
                                      var match = async[0];
                                      if (match) {
                                        return onResponse(value);
                                      } else {
                                        return Queue.add(value, syncQueue);
                                      }
                                    }), map(task, (function (value) {
                                          return /* tuple */[
                                                  index,
                                                  value
                                                ];
                                        }))),
                              /* index */index
                            ];
                    }), $$Array.of_list(concurrentTasks));
              async[0] = true;
              while(!Queue.is_empty(syncQueue) && !rejected[0]) {
                onResponse(Queue.take(syncQueue));
              };
              return /* Cancel */[(function (param) {
                          return $$Array.iter((function (task) {
                                        return Curry._1(task[/* cancel */0], /* () */0);
                                      }), hotTask[0]);
                        })];
            })];
}

function both(param) {
  var task2 = param[1];
  var task1 = param[0];
  return /* Task */[(function (rej, res) {
              var task1Res = /* record */[/* contents */undefined];
              var task2Res = /* record */[/* contents */undefined];
              var task1Cancel = /* record */[/* contents */(function (param) {
                    return /* () */0;
                  })];
              var task2Cancel = /* record */[/* contents */(function (param) {
                    return /* () */0;
                  })];
              var onResponse1 = function (response) {
                if (response.tag) {
                  var value = response[0];
                  var match = task2Res[0];
                  if (match !== undefined) {
                    return Curry._1(res, /* tuple */[
                                value,
                                Caml_option.valFromOption(match)
                              ]);
                  } else {
                    task1Res[0] = Caml_option.some(value);
                    return /* () */0;
                  }
                } else {
                  Curry._1(task2Cancel[0], /* () */0);
                  return Curry._1(rej, response[0]);
                }
              };
              var onResponse2 = function (response) {
                if (response.tag) {
                  var value = response[0];
                  var match = task1Res[0];
                  if (match !== undefined) {
                    return Curry._1(res, /* tuple */[
                                Caml_option.valFromOption(match),
                                value
                              ]);
                  } else {
                    task2Res[0] = Caml_option.some(value);
                    return /* () */0;
                  }
                } else {
                  Curry._1(task1Cancel[0], /* () */0);
                  return Curry._1(rej, response[0]);
                }
              };
              task1Cancel[0] = run(onResponse1, task1);
              task2Cancel[0] = run(onResponse2, task2);
              return /* Cancel */[(function (param) {
                          Curry._1(task1Cancel[0], /* () */0);
                          return Curry._1(task2Cancel[0], /* () */0);
                        })];
            })];
}

function triple(param) {
  var task3 = param[2];
  var task2 = param[1];
  var task1 = param[0];
  return /* Task */[(function (rej, res) {
              var task1Res = /* record */[/* contents */undefined];
              var task2Res = /* record */[/* contents */undefined];
              var task1Cancel = /* record */[/* contents */(function (param) {
                    return /* () */0;
                  })];
              var task2Cancel = /* record */[/* contents */(function (param) {
                    return /* () */0;
                  })];
              var onResponse1 = function (response) {
                if (response.tag) {
                  var value = response[0];
                  var match = task2Res[0];
                  if (match !== undefined) {
                    return Curry._1(res, /* tuple */[
                                value[0],
                                value[1],
                                Caml_option.valFromOption(match)
                              ]);
                  } else {
                    task1Res[0] = value;
                    return /* () */0;
                  }
                } else {
                  Curry._1(task2Cancel[0], /* () */0);
                  return Curry._1(rej, response[0]);
                }
              };
              var onResponse2 = function (response) {
                if (response.tag) {
                  var value = response[0];
                  var match = task1Res[0];
                  if (match !== undefined) {
                    var task1Value = match;
                    return Curry._1(res, /* tuple */[
                                task1Value[0],
                                task1Value[1],
                                value
                              ]);
                  } else {
                    task2Res[0] = Caml_option.some(value);
                    return /* () */0;
                  }
                } else {
                  Curry._1(task1Cancel[0], /* () */0);
                  return Curry._1(rej, response[0]);
                }
              };
              task1Cancel[0] = run(onResponse1, both(/* tuple */[
                        task1,
                        task2
                      ]));
              task2Cancel[0] = run(onResponse2, task3);
              return /* Cancel */[(function (param) {
                          Curry._1(task1Cancel[0], /* () */0);
                          return Curry._1(task2Cancel[0], /* () */0);
                        })];
            })];
}

function quadruple(param) {
  var task4 = param[3];
  var task3 = param[2];
  var task2 = param[1];
  var task1 = param[0];
  return /* Task */[(function (rej, res) {
              var task1Res = /* record */[/* contents */undefined];
              var task2Res = /* record */[/* contents */undefined];
              var task1Cancel = /* record */[/* contents */(function (param) {
                    return /* () */0;
                  })];
              var task2Cancel = /* record */[/* contents */(function (param) {
                    return /* () */0;
                  })];
              var onResponse1 = function (response) {
                if (response.tag) {
                  var value = response[0];
                  var match = task2Res[0];
                  if (match !== undefined) {
                    var task2Value = match;
                    return Curry._1(res, /* tuple */[
                                value[0],
                                value[1],
                                task2Value[0],
                                task2Value[1]
                              ]);
                  } else {
                    task1Res[0] = value;
                    return /* () */0;
                  }
                } else {
                  Curry._1(task2Cancel[0], /* () */0);
                  return Curry._1(rej, response[0]);
                }
              };
              var onResponse2 = function (response) {
                if (response.tag) {
                  var value = response[0];
                  var match = task1Res[0];
                  if (match !== undefined) {
                    var task1Value = match;
                    return Curry._1(res, /* tuple */[
                                task1Value[0],
                                task1Value[1],
                                value[0],
                                value[1]
                              ]);
                  } else {
                    task2Res[0] = value;
                    return /* () */0;
                  }
                } else {
                  Curry._1(task1Cancel[0], /* () */0);
                  return Curry._1(rej, response[0]);
                }
              };
              task1Cancel[0] = run(onResponse1, both(/* tuple */[
                        task1,
                        task2
                      ]));
              task2Cancel[0] = run(onResponse2, both(/* tuple */[
                        task3,
                        task4
                      ]));
              return /* Cancel */[(function (param) {
                          Curry._1(task1Cancel[0], /* () */0);
                          return Curry._1(task2Cancel[0], /* () */0);
                        })];
            })];
}

function quintuple(param) {
  var task5 = param[4];
  var task4 = param[3];
  var task3 = param[2];
  var task2 = param[1];
  var task1 = param[0];
  return /* Task */[(function (rej, res) {
              var task1Res = /* record */[/* contents */undefined];
              var task2Res = /* record */[/* contents */undefined];
              var task1Cancel = /* record */[/* contents */(function (param) {
                    return /* () */0;
                  })];
              var task2Cancel = /* record */[/* contents */(function (param) {
                    return /* () */0;
                  })];
              var onResponse1 = function (response) {
                if (response.tag) {
                  var value = response[0];
                  var match = task2Res[0];
                  if (match !== undefined) {
                    return Curry._1(res, /* tuple */[
                                value[0],
                                value[1],
                                value[2],
                                value[3],
                                Caml_option.valFromOption(match)
                              ]);
                  } else {
                    task1Res[0] = value;
                    return /* () */0;
                  }
                } else {
                  Curry._1(task2Cancel[0], /* () */0);
                  return Curry._1(rej, response[0]);
                }
              };
              var onResponse2 = function (response) {
                if (response.tag) {
                  var value = response[0];
                  var match = task1Res[0];
                  if (match !== undefined) {
                    var task1Value = match;
                    return Curry._1(res, /* tuple */[
                                task1Value[0],
                                task1Value[1],
                                task1Value[2],
                                task1Value[3],
                                value
                              ]);
                  } else {
                    task2Res[0] = Caml_option.some(value);
                    return /* () */0;
                  }
                } else {
                  Curry._1(task1Cancel[0], /* () */0);
                  return Curry._1(rej, response[0]);
                }
              };
              task1Cancel[0] = run(onResponse1, quadruple(/* tuple */[
                        task1,
                        task2,
                        task3,
                        task4
                      ]));
              task2Cancel[0] = run(onResponse2, task5);
              return /* Cancel */[(function (param) {
                          Curry._1(task1Cancel[0], /* () */0);
                          return Curry._1(task2Cancel[0], /* () */0);
                        })];
            })];
}

function sextuple(param) {
  var task6 = param[5];
  var task5 = param[4];
  var task4 = param[3];
  var task3 = param[2];
  var task2 = param[1];
  var task1 = param[0];
  return /* Task */[(function (rej, res) {
              var task1Res = /* record */[/* contents */undefined];
              var task2Res = /* record */[/* contents */undefined];
              var task1Cancel = /* record */[/* contents */(function (param) {
                    return /* () */0;
                  })];
              var task2Cancel = /* record */[/* contents */(function (param) {
                    return /* () */0;
                  })];
              var onResponse1 = function (response) {
                if (response.tag) {
                  var value = response[0];
                  var match = task2Res[0];
                  if (match !== undefined) {
                    return Curry._1(res, /* tuple */[
                                value[0],
                                value[1],
                                value[2],
                                value[3],
                                value[4],
                                Caml_option.valFromOption(match)
                              ]);
                  } else {
                    task1Res[0] = value;
                    return /* () */0;
                  }
                } else {
                  Curry._1(task2Cancel[0], /* () */0);
                  return Curry._1(rej, response[0]);
                }
              };
              var onResponse2 = function (response) {
                if (response.tag) {
                  var value = response[0];
                  var match = task1Res[0];
                  if (match !== undefined) {
                    var task1Value = match;
                    return Curry._1(res, /* tuple */[
                                task1Value[0],
                                task1Value[1],
                                task1Value[2],
                                task1Value[3],
                                task1Value[4],
                                value
                              ]);
                  } else {
                    task2Res[0] = Caml_option.some(value);
                    return /* () */0;
                  }
                } else {
                  Curry._1(task1Cancel[0], /* () */0);
                  return Curry._1(rej, response[0]);
                }
              };
              task1Cancel[0] = run(onResponse1, quintuple(/* tuple */[
                        task1,
                        task2,
                        task3,
                        task4,
                        task5
                      ]));
              task2Cancel[0] = run(onResponse2, task6);
              return /* Cancel */[(function (param) {
                          Curry._1(task1Cancel[0], /* () */0);
                          return Curry._1(task2Cancel[0], /* () */0);
                        })];
            })];
}

function timeout(value) {
  return /* Task */[(function (param, res) {
              var timer = setTimeout((function (param) {
                      return Curry._1(res, value);
                    }), 1000);
              return /* Cancel */[(function (param) {
                          clearTimeout(timer);
                          return /* () */0;
                        })];
            })];
}

function notTimeout(value) {
  return /* Task */[(function (param, res) {
              Curry._1(res, value);
              return /* NoCancel */0;
            })];
}

var p = map(parallel(Pervasives.$at(List.map(timeout, /* :: */[
                  1,
                  /* :: */[
                    2,
                    /* :: */[
                      3,
                      /* :: */[
                        4,
                        /* :: */[
                          5,
                          /* :: */[
                            6,
                            /* :: */[
                              7,
                              /* :: */[
                                8,
                                /* :: */[
                                  9,
                                  /* [] */0
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]), $$Array.to_list($$Array.map(notTimeout, $$Array.mapi((function (index, param) {
                            return index + 10 | 0;
                          }), Caml_array.caml_make_vect(10000, 1)))))), (function (param) {
        return List.fold_left((function (a, b) {
                      return a + b | 0;
                    }), 0, param);
      }));

function makeTask(i) {
  if (i >= 100000) {
    var value = /* Done */Block.__(1, [i + 1 | 0]);
    return /* Task */[(function (param, res) {
                Curry._1(res, value);
                return /* NoCancel */0;
              })];
  } else if (i < 0) {
    return /* Task */[(function (rej, param) {
                Curry._1(rej, "i must be positive");
                return /* NoCancel */0;
              })];
  } else {
    var value$1 = /* Next */Block.__(0, [i + 1 | 0]);
    return /* Task */[(function (param, res) {
                Curry._1(res, value$1);
                return /* NoCancel */0;
              })];
  }
}

var t = run((function (param) {
        console.log(param[0]);
        return /* () */0;
      }), chain(p, (function (param) {
            return chainRec(makeTask, param);
          })));

var bind = chain;

var resolve = pure;

exports.run = run;
exports.chain = chain;
exports.bind = bind;
exports.chainRec = chainRec;
exports.chainRej = chainRej;
exports.map = map;
exports.mapRej = mapRej;
exports.bimap = bimap;
exports.fold = fold;
exports.also = also;
exports.alt = alt;
exports.$$finally = $$finally;
exports.pure = pure;
exports.resolve = resolve;
exports.reject = reject;
exports.Operators = Operators;
exports.parallel = parallel;
exports.both = both;
exports.triple = triple;
exports.quadruple = quadruple;
exports.quintuple = quintuple;
exports.sextuple = sextuple;
exports.timeout = timeout;
exports.notTimeout = notTimeout;
exports.p = p;
exports.makeTask = makeTask;
exports.t = t;
/* p Not a pure module */
