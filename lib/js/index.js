var curry = require('ramda').curry
var Task = require('./src/Task.bs')
var TaskJs = require('./src/TaskJs.bs')

var obj = {}
obj.encaseP = TaskJs.encaseP;
obj.encaseP2 = TaskJs.encaseP2;
obj.encaseP3 = TaskJs.encaseP3;
obj.toPromise = TaskJs.toPromise;
obj.after = TaskJs.after;
obj.encaseCB = TaskJs.encaseCB;
obj.encaseRevokableCB = TaskJs.encaseRevokableCB;
obj.rejectAfter = TaskJs.rejectAfter;
obj.run = Task.run;
obj.noop = Task.noop;
obj.chain = Task.chain;
obj.bind = Task.bind;
obj.chainRec = Task.chainRec;
obj.chainRej = Task.chainRej;
obj.map = Task.map;
obj.mapRej = Task.mapRej;
obj.bimap = Task.bimap;
obj.fold = Task.fold;
obj.also = Task.also;
obj.alt = Task.alt;
obj.concludeWith = Task.$$finally;
obj.hook = Task.hook;
obj.pure = Task.pure;
obj.resolve = Task.resolve;
obj.reject = Task.reject;
obj.race = Task.race;
obj.parallel = Task.parallel;
obj.both = Task.both;
obj.triple = Task.triple;
obj.quadruple = Task.quadruple;
obj.quintuple = Task.quintuple;
obj.sextuple = Task.sextuple;

// make a nice easy chainer method for the js side
var _ = fn =>  {

  const obj = {
    fns: [fn],
    P : (fn) => {
      obj.fns.push(fn)
      let f = (...args) => {
        let val = obj.fns.shift()(...args)
        while (!R.isEmpty(obj.fns)) {
          val = obj.fns.shift()(val)
        }
        return val
      }
      f.P = (_fn) => {
        f = null;
        return obj.P(_fn)
      }
      return f
    }
  }
  return obj
}

exports._ = _

Object.keys(obj).forEach(key => {
  exports[key] = curry(obj[key])
})