var scheme = {
    obarray : {},
    primitives : {},
    utils : {},
    env : {},
    cache: [],
    module_cache: {},
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

scheme.primitives["add/immediate"] = function (x, y) {
    return x + y;
};

scheme.primitives.sub = function (x, y) {
    return x - y;
};

scheme.primitives.sub1 = function (x) {
    return x - 1;
};

scheme.primitives["sub/immediate"] = function (x, y) {
    return x - y;
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

// Unboxed Numbers
scheme.primitives["load-u64"] = function(x) {
    return x;
};

scheme.primitives["u64-=-scm"] = function(x, y) {
    // i.e. br-if-u64-=-scm
    return coerce_bool(x === y);
};

scheme.primitives["u64-<=-scm"] = function(x, y) {
    return coerce_bool(x <= y);
};

scheme.primitives["u64-<-scm"] = function(x, y) {
    return coerce_bool(x < y);
};

scheme.primitives["u64->-scm"] = function(x, y) {
    return coerce_bool(x > y);
};

scheme.primitives["u64->=-scm"] = function(x, y) {
    return coerce_bool(x >= y);
};

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

scheme.primitives["make-vector/immediate"] = function(length, init) {
    var v = new scheme.Vector();

    var temp = []
    for (var i=0; i < length; i++) {
        temp[i] = init;
    }

    v.array = temp;

    return v;
};

scheme.primitives["vector-set!/immediate"] = scheme.primitives["vector-set!"];
scheme.primitives["vector-ref/immediate"] = scheme.primitives["vector-ref"];

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

// Syntax Objects
scheme.Syntax = function (expr, wrap, module) {
    this.expr = expr;
    this.wrap = wrap;
    this.module = module;
    return this;
};

// Modules
scheme.primitives["define!"] = function(sym) {
    var b = new scheme.Box(scheme.UNDEFINED);
    scheme.env[sym.name] = b;
    return b;
};

scheme.primitives["cache-current-module!"] = function (module, scope) {
    scheme.cache[scope] = module;
};

scheme.primitives["cached-toplevel-box"] = function (scope, sym, is_bound) {
    return scheme.cache[scope][sym.name];
};

scheme.primitives["cached-module-box"] = function (module_name, sym, is_public, is_bound)  {
    var cache = scheme.module_cache;

    while (scheme.EMPTY != module_name.cdr) {
        cache = cache[module_name.car.name];
    }

    cache = cache[module_name.car.name];
    var r = cache[sym.name];
    if (typeof r === 'undefined') {
        throw {r : "cached-module-box", s : sym, m : module_name};
    } else {
        return r;
    }
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
    return !(obj === scheme.FALSE || obj === scheme.NIL);
};

// Builtins
var apply = function(self, k, f) {
    var args = Array.prototype.slice.call(arguments, 3);
    var tail = args.pop();

    while (scheme.is_true(scheme.primitives["pair?"](tail))) {
        args.push(tail.car);
        tail = tail.cdr;
    };

    return f.fun.apply(f.fun, [f,k].concat(args));
};

var values = function(self, k) {
    var args = Array.prototype.slice.call(arguments, 2);
    return k.apply(k,args);
};

var abort_to_prompt = function(self, k, prompt) {

    var args = Array.prototype.slice.call(arguments, 3);
    var idx = find_prompt(prompt);
    var frame = scheme.dynstack[idx];

    var kont = undefined; // actual value doesn't matter

    if (!scheme.is_true(frame.escape_only)) {
        var f = function (self, k2) {
            var args = Array.prototype.slice.call(arguments, 2);
            return k.apply(k,args);
        };
        kont = new scheme.Closure(f, 0);
    };

    unwind(scheme.dynstack, idx); // FIXME:

    var handler = frame.handler;
    args.unshift(kont);
    return handler.apply(handler, args);
};

var call_with_values = function (self, k, producer, consumer) {
    var k2 = function () {
        var args = Array.prototype.slice.call(arguments);
        return consumer.fun.apply(consumer.fun, [consumer, k].concat(args));
    };
    return producer.fun(producer, k2);
};

var callcc = function (self, k, closure) {
    var dynstack = scheme.dynstack.slice();

    var f = function (self, k2) {
        var args = Array.prototype.slice.call(arguments, 2);

        var i = shared_stack_length(dynstack, scheme.dynstack);

        unwind(scheme.dynstack, i);
        wind(dynstack, i);
        scheme.dynstack = dynstack;

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
scheme.Struct = function (vtable, nfields) {
    this.is_vtable = false;
    this.vtable = vtable;
    this.fields = [];

    // FIXME: worth doing?
    for(var i = 0; i < nfields; i++){
        this.fields[i]=scheme.UNDEFINED;
    }

    return this;
};

scheme.primitives["struct?"] = function (obj) {
    return coerce_bool(obj instanceof scheme.Struct);
};

scheme.primitives["allocate-struct/immediate"] = function (vtable, nfields) {
    return new scheme.Struct(vtable, nfields);
};

scheme.primitives["struct-vtable"] = function(struct) {
    return struct.vtable;
};

scheme.primitives["struct-set!"] = function (struct, idx, obj) {
    return struct.fields[idx] = obj;
};

scheme.primitives["struct-ref"] = function (struct, idx) {
    return struct.fields[idx];
};

scheme.primitives["struct-set!/immediate"] = scheme.primitives["struct-set!"];
scheme.primitives["struct-ref/immediate"] = scheme.primitives["struct-ref"];

// Equality
scheme.primitives["eq?"] = function(x, y) {
    return coerce_bool(x === y);
};

scheme.primitives["eqv?"] = function(x, y) {
    return coerce_bool(x === y);
};

scheme.primitives["equal?"] = not_implemented_yet;

// Fluids
scheme.Fluid = function (x) {
    this.value = x;
    return this;
};

scheme.primitives["pop-fluid"] = function () {
    var frame = scheme.dynstack.shift();
    if (frame instanceof scheme.frame.Fluid) {
        frame.fluid.value = frame.fluid.old_value;
        return;
    } else {
        throw "not a frame";
    };
};

scheme.primitives["push-fluid"] = function (fluid, new_value) {
    var old_value = fluid.value;
    fluid.value = new_value;
    var frame = new scheme.frame.Fluid(fluid, old_value);
    scheme.dynstack.unshift(frame);
    return;
};

scheme.primitives["fluid-ref"] = function (fluid) {
    // TODO: check fluid type
    return fluid.value;
};

// Variables
scheme.primitives["variable?"] = not_implemented_yet;

// Dynamic Wind
scheme.primitives["wind"] = function(enter, leave) {
    var frame = new scheme.frame.DynWind(enter, leave);
    scheme.dynstack.unshift(frame);
};

scheme.primitives["unwind"] = function () {
    var frame = scheme.dynstack.shift();
    if (!(frame instanceof scheme.frame.DynWind)) {
        throw "not a dynamic wind frame";
    };
};

// Misc
scheme.primitives["prompt"] = function(escape_only, tag, handler){
    var frame = new scheme.frame.Prompt(tag, escape_only, handler);
    scheme.dynstack.unshift(frame);
};

var shared_stack_length = function (dynstack1, dynstack2) {
    // Assumes that if it matches at i then it matches for all x<i,
    // which will be fine given that we don't reuse frames
    var size1 = dynstack1.length;
    var size2 = dynstack2.length;
    var last = Math.min(size1, size2);

    var i = last-1;
    for (; i >= 0; i--) {
        if (dynstack1[i] === dynstack2[i]) {
            break;
        }
    };

    return i + 1;
};

var wind = function (dynstack, idx) {
    for (var i = idx; i < dynstack.length; i++) {
        var frame = dynstack[i];
        if (frame instanceof scheme.frame.DynWind) {
            // TODO: how to handle continuations and errors in this?
            frame.wind.fun(frame.wind, scheme.initial_cont);
        } else {
            throw "unsupported frame type -- wind";
        }
    }
};

var unwind = function (dynstack, idx) {
    for (var i = dynstack.length - 1; i >= idx; i--) {
        var frame = dynstack[i];
        if (frame instanceof scheme.frame.DynWind) {
            // TODO: how to handle continuations and errors in this?
            frame.unwind.fun(frame.unwind, scheme.initial_cont);
        } else {
            throw "unsupported frame type -- unwind";
        }
    }
};

var find_prompt = function(prompt) {
    var eq = scheme.primitives["eq?"];
    function test(x){
        return scheme.is_true(eq(x,prompt)) || scheme.is_true(eq(x,scheme.TRUE));
    };
    for (idx in scheme.dynstack) {
        var frame = scheme.dynstack[idx];
        if (frame instanceof scheme.frame.Prompt && test(frame.tag)) {
            return idx;
        };
    };
    // FIXME: should error
    return undefined;
};

scheme.primitives["handle-interrupts"] = function () {
    // TODO: implement
    return;
};

// Dynstack frames
scheme.frame = {};

scheme.frame.Prompt = function(tag, escape_only, handler){
    this.tag = tag;
    this.escape_only = escape_only;
    this.handler = handler;
};

scheme.frame.Fluid = function(fluid, old_value) {
    this.fluid = fluid;
    this.old_value = old_value;
};

scheme.frame.DynWind = function(wind, unwind) {
    this.wind = wind;
    this.unwind = unwind;
};

// Module Cache
scheme.module_cache["guile"] = scheme.env;

function def_guile0 (name, fn) {
    var sym = new scheme.Symbol(name); // put in obarray
    var clos = new scheme.Closure(fn, 0);
    var box = new scheme.Box(clos);
    scheme.module_cache["guile"][name] = box;
};

function def_guile_val (name, val) {
    var sym = new scheme.Symbol(name); // put in obarray
    var box = new scheme.Box(val);
    scheme.module_cache["guile"][name] = box;
};

function scm_list (self, cont) {
    var l = scheme.EMPTY;
    for (var i = arguments.length - 1; i >= 2; i--){
        l = scheme.primitives.cons(arguments[i],l);
    };
    return cont(l);
};
def_guile0("list", scm_list);

function scm_add(self, cont) {

    var total = 0;
    for (var i = arguments.length - 1; i >= 2; i--){
        total += arguments[i];
    };
    return cont(total);

};
def_guile0("+", scm_add);

function scm_mul(self, cont) {

    var total = 1;
    for (var i = arguments.length - 1; i >= 2; i--){
        total *= arguments[i];
    };
    return cont(total);

};
def_guile0("*", scm_mul);

// Lists
def_guile0("make-list", function (self, cont, n, obj) {
    var list = scheme.EMPTY;

    for (var i = 0; i <= n; i++) {
        list = new scheme.Pair(obj, list);
    }

    return cont(list);
});

def_guile0("length", function (self, cont, list) {
   var len = 0;

    while (!scheme.is_true(scheme.primitives["null?"](list))) {
        if (scheme.is_true(scheme.primitives["pair?"](list))) {
            list = list.cdr;
            len += 1;
        } else {
            console.log("length bad");
            not_implemented_yet();
        }
    }

    return cont(len);
});

def_guile0("list?", function (self, cont, list) {

    while (!scheme.is_true(scheme.primitives["null?"](list))) {
        if (scheme.is_true(scheme.primitives["pair?"](list))) {
            list = list.cdr;
        } else {
            return cont(scheme.FALSE);
        }
    }

    return cont(scheme.TRUE);
});

def_guile0("reverse", function (self, cont, lst) {
    var l = scheme.EMPTY;
    while (lst != scheme.EMPTY) {
        l = scheme.primitives.cons(lst.car, l);
        lst = lst.cdr;
    }
    return cont(l);
});

def_guile0("append", function (self, cont, l1, l2) {
    if (arguments.length != 4) {
        console.log("FIXAPPEND", arguments.length);
        throw "fail";
    }


    if (l1 === scheme.EMPTY) {
        return cont(l2);
    }

    var l = new scheme.Pair(l1.car, l2);

    var lp = l;
    while (scheme.is_true(scheme.primitives["pair?"](l1.cdr))) {

        var lo = new scheme.Pair(l1.cdr.car, l2);
        lp.cdr = l2;

        lp = lp.cdr;
        l1 = l1.cdr;
    }

    return cont(l);
});

def_guile0("memq", function (self, cont, val, args) {
    return cont(scheme.FALSE);
});

def_guile0("member", function (self, cont, elt, list) {
    // FIXME: needs equal? console.log("member", arguments);
    // throw "";
    return cont(scheme.FALSE);
});

def_guile0("delete!", function (self, cont, elt, list) {
    // FIXME:
    return cont(list);
});

// Macros
scheme.Macro = function (name, type, binding) {
    // TODO: prim field?
    this.name = name;
    this.type = type;
    this.binding = binding;
    return this;
};

def_guile0("make-syntax-transformer", function (self, cont, name, type, binding) {
    return cont(new scheme.Macro(name, type, binding));
});

function scm_is_syntax (self, cont, obj) {
    return cont(coerce_bool(obj instanceof scheme.Syntax));
};
def_guile0("syntax?", scm_is_syntax);

def_guile0("make-syntax", function (self, cont, expr, wrap, module) {
    return cont(new scheme.Syntax(expr, wrap, module));
});

def_guile0("syntax-expression", function (self, cont, obj) {
    return cont(obj.expr);
});

def_guile0("syntax-wrap", function (self, cont, obj) {
    return cont(obj.wrap);
});

def_guile0("syntax-module", function (self, cont, obj) {
    return cont(obj.module);
});

// Symbols
def_guile0("symbol->string", function (self, cont, sym) {
    return cont(new scheme.String(sym.name));
});

var gensym_counter = 0;
function scm_gensym (self, cont, prefix) {
    var name = prefix ? prefix.s : "gen ";
    name += gensym_counter;
    gensym_counter += 1;

    return cont(new scheme.Symbol(name));
};
def_guile0("gensym", scm_gensym);


// Strings
def_guile0("string=?", function (self, cont, s1, s2) {
    return cont(coerce_bool(s1.s === s2.s));
});

def_guile0("string-append", function (self, cont, s1, s2) {
    var s = "";

    for (var i = 2; i < arguments.length; i++) {
        s += arguments[i].s;
    }

    //console.log("sap", s1, s2, arguments.length);
    return cont(new scheme.String(s));
});

def_guile0("string-join", function (self, cont, strings) {
    var s = "";

    while (!scheme.is_true(scheme.primitives["null?"](strings))) {
        if (scheme.is_true(scheme.primitives["pair?"](strings))) {
            s += strings.car.s;
            strings = strings.cdr;
        } else {
            console.log("string-join bad");
            not_implemented_yet();
        }
    }

    return cont(new scheme.String(s));
});

});

// Structs
var vtable_base_layout = new scheme.String("pruhsruhpwphuhuh");
def_guile_val("standard-vtable-fields", vtable_base_layout);

var scm_vtable_index_layout = 0;
var scm_vtable_index_flags = 1;
var scm_vtable_index_self = 2;
var scm_vtable_index_instance_finalize = 3;
var scm_vtable_index_instance_printer = 4;
var scm_vtable_index_name = 5;
var scm_vtable_index_size = 6;
var scm_vtable_index_reserved_7 = 7;
var scm_vtable_offset_user = 8;

function scm_struct_init(struct, layout, args) {
    // FIXME: assumes there are no tail arrays
    var nfields = layout.length / 2; // assumes even
    var arg = 0;

    for (var i = 0; i < nfields; i++) {
        if (layout[2*i+1] == 'o') {
            continue;
        }
        switch (layout[2*i]) {
        case 'p' :
            struct.fields[i] = (arg < args.length) ? args[arg] : scheme.FALSE;
            arg += 1;
            break;
        case 'u' :
            struct.fields[i] = (arg < args.length) ? args[arg] : 0;
            arg += 1;
            break;
        case 's' :
            struct.fields[i] = struct;
        }
    }
};

// Set up <standard-vtable>
var scm_standard_vtable = new scheme.Struct(undefined, 0);
scm_standard_vtable.vtable = scm_standard_vtable;
scm_standard_vtable.is_vtable = true; // ?
scm_struct_init(scm_standard_vtable,
                vtable_base_layout.s,
                [new scheme.Symbol(vtable_base_layout.s)]);
//   scm_set_struct_vtable_name_x (scm_standard_vtable_vtable, name);

def_guile_val("<standard-vtable>", scm_standard_vtable);
def_guile_val("vtable-index-layout", scm_vtable_index_layout);
def_guile_val("vtable-index-printer", scm_vtable_index_instance_printer);
def_guile_val("vtable-offset-user", scm_vtable_offset_user);


function scm_make_struct (vtable, args) {
    var layout = vtable.fields[scm_vtable_index_layout].name;
    var s = new scheme.Struct(vtable, layout.length / 2);
    scm_struct_init(s, layout, args);
    return s;
}

def_guile0("make-struct/no-tail", function (self, cont, vtable) {
    var args = Array.prototype.slice.call(arguments, 3);
    return cont(scm_make_struct(vtable, args));
});

def_guile0("make-vtable", function(self, cont, fields, printer) {
    var layout = new scheme.Symbol(fields.s); // make-struct-layout
    var str = scm_make_struct(scm_standard_vtable, [layout, printer]);
    str.is_vtable = true;
    return cont(str);
});

def_guile0("make-struct-layout", function (self, cont, str) {
    var layout = new scheme.Symbol(str.s);
    return cont(layout);
});

def_guile0("struct-vtable?", function (self, cont, obj) {
    // We don't inherit flags, so =struct-vtable?= may give the wrong
    // answer where SCM_VTABLE_FLAG_VTABLE would have been set
    var bool = coerce_bool(obj instanceof scheme.Struct && obj.is_vtable);
    return cont(bool);
});

var applicable_vtable = scm_make_struct(scm_standard_vtable, [new scheme.Symbol(vtable_base_layout.s)]);
applicable_vtable.children_applicable_vtables = true;

def_guile_val("<applicable-struct-vtable>", applicable_vtable);

def_guile_val("record-type-vtable", scm_standard_vtable); // FIXME:

def_guile0("set-struct-vtable-name!", function (self, cont, val, args) {
    // FIXME:
    return cont(scheme.FALSE);
});

def_guile0("make-struct", function (self, cont, vtable, tailsize) {
    if (tailsize === 0) {
        // make-struct/no-tail
        var args = Array.prototype.slice.call(arguments, 4);
        return cont(scm_make_struct(vtable, args));
    } else {
        console.log("make-struct with tail", arguments);
        not_implemented_yet();
    }
});

