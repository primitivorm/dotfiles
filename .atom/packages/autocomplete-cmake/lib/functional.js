'use babel';

export function existy(value) {
    return value != null;
}

export function dispatch() {
    var funs = arguments;
    return function() {
        for (var f of funs) {
            var ret = f.apply(null, arguments);
            if (existy(ret)) {
                return ret;
            }
        }
    };
}
