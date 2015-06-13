var scheme = {
    obarray : {},
    primitives : {},
    utils : {},
    env : {},
    cache: [],
    builtins: [],
    // TODO: placeholders
    FALSE : false,
    TRUE : true,
    NIL  : false,
    EMPTY : [],
    UNSPECIFIED : [],
    // FIXME: wingo says not to leak undefined to users
    UNDEFINED: undefined
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

// Keywords
scheme.Keyword = function(s) {
    this.name = s;
    return this;
};

scheme.utils.keyword_ref = function(kw, args, start, dflt) {
    var l = args.length;

    if ((l - start) % 2 == 1) {
        // FIXME: should error
        return undefined;
    }
    // Need to loop in reverse because last matching keyword wins
    for (var i = l - 2; i >= start; i -= 2) {
        if (!(args[i] instanceof scheme.Keyword)) {
            return undefined;
        }
        if (args[i].name === kw.name) {
            return args[i + 1];
        }
    }
    return dflt;
};

// Vectors
scheme.Vector = function () {
    this.array = Array.prototype.slice.call(arguments);
    return this;
};

scheme.primitives["vector-ref"] = function (vec, idx) {
    return vec.array[idx];
};

scheme.primitives["vector-set!"] = function (vec, idx, obj) {
    return vec.array[idx] = obj;
};

scheme.primitives["vector-length"] = function (vec) {
    return vec.array.length;
};

// Bytevectors

// Booleans

// Chars
scheme.Char = function(c) {
    this.c = c;
    return this;
};

// Strings
scheme.String = function(s) {
    this.s = s;
    return this;
};

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

// bleh
scheme.initial_cont = function (x) { return x; };
scheme.primitives.return = function (x) { return x; };
scheme.is_true = function (obj) {
    return !(obj == scheme.FALSE || obj == scheme.NIL);
};

var callcc = function (self, k, closure) {
    var f = function (self, k2, val) {
        return k(val);
    };
    return closure.fun(closure, k, new scheme.Closure(f, 0));
};
scheme.builtins[4] = new scheme.Closure(callcc, 0);
// #define FOR_EACH_VM_BUILTIN(M) \
//   M(apply, APPLY, 2, 0, 1) \
//   M(values, VALUES, 0, 0, 1) \
//   M(abort_to_prompt, ABORT_TO_PROMPT, 1, 0, 1) \
//   M(call_with_values, CALL_WITH_VALUES, 2, 0, 0) \
//   M(call_with_current_continuation, CALL_WITH_CURRENT_CONTINUATION, 1, 0, 0)

// ---
