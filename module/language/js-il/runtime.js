var scheme = {
    obarray : {},
    primitives : {},
    env : {},
    cache: [],
    builtins: [],
    // TODO: placeholders
    FALSE : false,
    TRUE : true,
    NIL  : false,
    EMPTY : [],
    UNSPECIFIED : []
};

function not_implemented_yet() {
    throw "not implemented yet";
};

// Numbers
scheme.primitives.add = function (x, y) {
    return x + y;
};

scheme.primitives.add1 = function (x) {
    return x + 1;
};

scheme.primitives.sub = function (x, y) {
    return x - y;
};

scheme.primitives.sub1 = function (x) {
    return x - 1;
};

scheme.primitives.mul = function (x, y) {
    return x * y;
};

scheme.primitives.div = function (x, y) {
    return x / y;
};

scheme.primitives["="] = function (x, y) {
    return x == y;
};

scheme.primitives["<"] = function (x, y) {
    return x < y;
};

scheme.primitives.quo = not_implemented_yet;
scheme.primitives.rem = not_implemented_yet;
scheme.primitives.mod = not_implemented_yet;

// Boxes
scheme.Box = function (x) {
    this.x = x;
    return this;
};

scheme.primitives["box-ref"] = function (box) {
    return box.x;
};

scheme.primitives["box-set!"] = function (box, val) {
    box.x = val;
};

// Lists
scheme.Pair = function (car, cdr) {
    this.car = car;
    this.cdr = cdr;
    return this;
};

scheme.primitives.cons = function (car, cdr) {
    return new scheme.Pair(car,cdr);
};

scheme.primitives.car = function (obj) {
    return obj.car;
};

scheme.primitives.cdr = function (obj) {
    return obj.cdr;
};

scheme.list = function () {
    var l = scheme.EMPTY;
    for (var i = arguments.length - 1; i >= 0; i--){
        l = scheme.primitives.cons(arguments[i],l);
    };
    return l;
};

scheme.primitives["null?"] = function(obj) {
    return scheme.EMPTY == obj;
};

// Symbols
scheme.Symbol = function(s) {
    if (scheme.obarray[s]) {
        return scheme.obarray[s];
    } else {
        this.name = s;
        scheme.obarray[s] = this;
        return this;
    };
};

// Vectors

// Bytevectors

// Booleans

// Chars

// Strings

// Closures
scheme.Closure = function(f, size) {
    this.fun = f;
    this.freevars = new Array(size);
    return this;
};

scheme.primitives["free-set!"] = function (closure, idx, obj) {
    closure.freevars[idx] = obj;
};

scheme.primitives["free-ref"] = function (closure, idx) {
    return closure.freevars[idx];
};

scheme.primitives["builtin-ref"] = function (idx) {
    return scheme.builtins[idx];
};

// Modules
scheme.primitives["define!"] = function(sym, obj) {
    scheme.env[sym.name] = new scheme.Box(obj);
};

scheme.primitives["cache-current-module!"] = function (module, scope) {
    scheme.cache[scope] = module;
};

scheme.primitives["cached-toplevel-box"] = function (scope, sym, is_bound) {
    return scheme.cache[scope][sym.name];
};

scheme.primitives["current-module"] = function () {
    return scheme.env;
};

scheme.primitives["resolve"] = function (sym, is_bound) {
    return scheme.env[sym.name];
};

// values
scheme.Values = function () {
    this.values = arguments;
    return this;
};

// bleh
scheme.initial_cont = function (x) { return x; };
scheme.primitives.return = function (x) { return x; };
scheme.is_true = function (obj) {
    return !(obj == scheme.FALSE || obj == scheme.NIL);
};

var callcc = function (k,vals) {
    var closure = vals.values[0];
    var f = function (k2, val) {
        // TODO: multivalue continuations
        return k(val);
    };
    return closure.fun(k, new scheme.Closure(f, 0));
};
scheme.builtins[4] = new scheme.Closure(callcc, 0);
// #define FOR_EACH_VM_BUILTIN(M) \
//   M(apply, APPLY, 2, 0, 1) \
//   M(values, VALUES, 0, 0, 1) \
//   M(abort_to_prompt, ABORT_TO_PROMPT, 1, 0, 1) \
//   M(call_with_values, CALL_WITH_VALUES, 2, 0, 0) \
//   M(call_with_current_continuation, CALL_WITH_CURRENT_CONTINUATION, 1, 0, 0)

// ---
