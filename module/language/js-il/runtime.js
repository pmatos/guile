var scheme = {
    obarray : {},
    primitives : {},
    utils : {},
    env : {},
    cache: [],
    builtins: [],
    dynstack : [],
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

function coerce_bool(obj) {
    return obj ? scheme.TRUE : scheme.FALSE;
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
    return coerce_bool(x == y);
};

scheme.primitives["<"] = function (x, y) {
    return coerce_bool(x < y);
};

scheme.primitives["<="] = function (x, y) {
    return coerce_bool(x <= y);
};

scheme.primitives[">"] = function (x, y) {
    return coerce_bool(x > y);
};

scheme.primitives[">="] = function (x, y) {
    return coerce_bool(x >= y);
};

scheme.primitives.quo = not_implemented_yet;
scheme.primitives.rem = not_implemented_yet;
scheme.primitives.mod = not_implemented_yet;

// Boxes
scheme.Box = function (x) {
    this.x = x;
    return this;
};

scheme.primitives["box"] = function(x) {
    return new scheme.Box(x);
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

scheme.primitives["pair?"] = function (obj) {
    return coerce_bool(obj instanceof scheme.Pair);
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

scheme.primitives["set-car!"] = function (pair, obj) {
    obj.car = obj;
};

scheme.primitives["set-cdr!"] = function (pair, obj) {
    obj.cdr = obj;
};

scheme.list = function () {
    var l = scheme.EMPTY;
    for (var i = arguments.length - 1; i >= 0; i--){
        l = scheme.primitives.cons(arguments[i],l);
    };
    return l;
};

scheme.primitives["null?"] = function(obj) {
    return coerce_bool(scheme.EMPTY == obj);
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

scheme.primitives["symbol?"] = function (obj) {
    return coerce_bool(obj instanceof scheme.Symbol);
};

// Keywords
scheme.Keyword = function(s) {
    this.name = s;
    return this;
};

scheme.primitives["keyword?"] = function (obj) {
    return coerce_bool(obj instanceof scheme.Keyword);
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

scheme.primitives["vector?"] = function (obj) {
    return coerce_bool(obj instanceof scheme.Vector);
};

scheme.primitives["make-vector/immediate"] = not_implemented_yet;
scheme.primitives["vector-set!/immediate"] = not_implemented_yet;
scheme.primitives["vector-ref/immediate"] = not_implemented_yet;

// Bytevectors

// Booleans

scheme.primitives["boolean?"] = not_implemented_yet;

// Chars
scheme.Char = function(c) {
    this.c = c;
    return this;
};

scheme.primitives["char?"] = function (obj) {
    return coerce_bool(obj instanceof scheme.Char);
};

// Strings
scheme.String = function(s) {
    this.s = s;
    return this;
};

scheme.primitives["string?"] = function (obj) {
    return coerce_bool(obj instanceof scheme.String);
};

scheme.primitives["string-length"] = function (str) {
    return str.s.length;
};

scheme.primitives["string-ref"] = function (str, idx) {
    return new scheme.Char(str.s[idx]);
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

scheme.primitives["cached-module-box"] = not_implemented_yet;

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
    return !(obj === scheme.FALSE || obj === scheme.NIL);
};

// Builtins
var apply = function(self, k, f, arg) {
    return f.fun(f.freevars, k, arg);
};

var values = function(self, k, arg) {

    return k(arg);
};

var abort_to_prompt = function(self, k, prompt, arg) {

    var idx = find_prompt(prompt);
    var spec = scheme.dynstack[idx];

    var kont = undefined; // actual value doesn't matter

    if (!scheme.is_true(spec[1])) {
        var f = function (self, k2) {
            var args = Array.prototype.slice.call(arguments, 2);
            return k.apply(k,args);
        };
        kont = new scheme.Closure(f, 0);
    };

    unwind(idx);

    var handler = spec[2];

    return handler(kont, arg);
};

var call_with_values = not_implemented_yet;

var callcc = function (self, k, closure) {
    var f = function (self, k2) {
        var args = Array.prototype.slice.call(arguments, 2);
        return k.apply(k,args);
    };
    return closure.fun(closure, k, new scheme.Closure(f, 0));
};
scheme.builtins[0] = new scheme.Closure(apply, 0);
scheme.builtins[1] = new scheme.Closure(values, 0);
scheme.builtins[2] = new scheme.Closure(abort_to_prompt, 0);
scheme.builtins[3] = new scheme.Closure(call_with_values, 0);
scheme.builtins[4] = new scheme.Closure(callcc, 0);

// Structs
scheme.primitives["struct?"] = not_implemented_yet;
scheme.primitives["struct-set!/immediate"] = not_implemented_yet;
scheme.primitives["struct-vtable"] = not_implemented_yet;
scheme.primitives["struct-ref/immediate"] = not_implemented_yet;
scheme.primitives["struct-ref"] = not_implemented_yet;
scheme.primitives["struct-set!"] = not_implemented_yet;
scheme.primitives["allocate-struct/immediate"] = not_implemented_yet;

// Equality
scheme.primitives["eq?"] = function(x, y) {
    return coerce_bool(x === y);
};

scheme.primitives["eqv?"] = function(x, y) {
    return coerce_bool(x === y);
};

scheme.primitives["equal?"] = not_implemented_yet;

// Fluids
scheme.primitives["pop-fluid"] = not_implemented_yet;
scheme.primitives["push-fluid"] = not_implemented_yet;
scheme.primitives["fluid-ref"] = not_implemented_yet;

// Variables
scheme.primitives["variable?"] = not_implemented_yet;

// Dynamic Wind
scheme.primitives["wind"] = not_implemented_yet;
scheme.primitives["unwind"] = not_implemented_yet;

// Misc
scheme.primitives["prompt"] = function(escape_only, tag, handler){
    scheme.dynstack.unshift([tag, escape_only, handler]);
};

var unwind = function (idx) {
    // TODO: call winders
    scheme.dynstack = scheme.dynstack.slice(idx+1);
};

var find_prompt = function(prompt) {
    var eq = scheme.primitives["eq?"];
    function test(x){
        return scheme.is_true(eq(x,prompt)) || scheme.is_true(eq(x,scheme.TRUE));
    };
    for (idx in scheme.dynstack) {
        if (test(scheme.dynstack[idx][0])) {
            return idx;
        };
    };
    // FIXME: should error
    return undefined;
};
