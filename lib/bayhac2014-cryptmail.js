
var jsCatchJS = function (f, handler) {
    try {
        return  E(f())[1];
    } catch(e) {
        return E(handler([0,e.toString()]))[1];
    }
}

var jsCatchE = function (f) {
    try {
        return [1,E(f())[1]];
    } catch(e) {
        return [0,[0,e.toString()]];
    }
}

var catchEither = function (f) {
    try {
        return [1, [0, f()]];
    } catch (e) {
        return [0,[0,e.toString()]];
    }
}

/* ************************************************************************** */
/* ByteString */

var bsEmpty = function () { return []; }

var bsConcat = function (a, b) {
    return sjcl.bitArray.concat(a.slice(0), b);
}

var bsEqual = function (a, b) {
    return sjcl.bitArray.equal(a,b);
}

var bsSlice = function (a, l, u) {
    return sjcl.bitArray.bitSlice(a, Math.max(0,Math.min(l * 8, sjcl.bitArray.bitLength(a))), Math.max(0,Math.min(sjcl.bitArray.bitLength(a), u * 8)));
}

var bsExtract = function (a, o, l) {
    return sjcl.bitArray.extract(a, o * 8, l * 8);
}

var bsLength = function (a) {
    return sjcl.bitArray.bitLength(a) / 8
}

var bsReplicate = function(n, a) {
    var arr = [];
    var i = 0;
    for (i = 0; i < n; ++i) {
        arr.push(a);
    }
    return sjcl.codec.bytes.toBits(arr);
}

/* ************************************************************************** */
/* Aes */

if (sjcl.mode.cbc === undefined) {
    sjcl.beware['CBC mode is dangerous because it doesn\'t protect message integrity.']();
}

var aesEncrypt = function (key, data, iv) {
    return sjcl.mode.cbc.encrypt(new sjcl.cipher.aes(key), data, iv, []);
}

var aesDecrypt = function (key, data, iv) {
    return catchEither(function () { return sjcl.mode.cbc.decrypt(new sjcl.cipher.aes(key), data, iv, [])});
}

/* ************************************************************************** */
/* PBKDF2 */

var pbkdf2 = function (pwd, salt, rounds, size) {
    return sjcl.misc.pbkdf2(pwd, salt, rounds, size, function (a) { return new sjcl.misc.hmac(a,sjcl.hash.sha512); });
}

/* ************************************************************************** */
/* Codec */

var base64toBits = function (x) {
    return catchEither(function () { return sjcl.codec.base64.toBits(x) });
}

var base64fromBits = function (x) {
    return sjcl.codec.base64.fromBits(x);
}

var utf8toBits = function (x) {
    return sjcl.codec.utf8String.toBits(x);
}

var utf8fromBits = function (x) {
    return catchEither(function () { return sjcl.codec.utf8String.fromBits(x) });
}

/* ************************************************************************** */
/* RNG */

var randomWords = function (n, paranoia) {
    return sjcl.random.randomWords(n, paranoia);
}

/* ************************************************************************** */
/* JSON */

var Bayhac2014JSON = (function() {

    var parseJSON = function(str) {
        try {
            var js = JSON.parse(str);
            var hs = toHsValue(js);
        } catch(_) {
            return [0];
        }
        return [1,hs];
    }

    var toHsValue = function (obj) {
        switch(typeof obj) {
        case 'number':
            if (obj == Math.round(obj) && false) { /* Haste supports only 32bit arithmetic; we always use Double */
                return [0, [0, [0, Number(obj)]]];
            } else {
                return [0, [1, [0, Number(obj)]]];
            }
        case 'string':
            return [1, [0, obj]];
            break;
        case 'boolean':
            return [2, obj]; // Booleans are special wrt constructor tags!
            break;
        case 'object':
            if (obj === null) {
                return [3, obj];
            } else if (obj instanceof Array) {
                return [4, arr2lst(obj, 0)];
            } else {
                // Object type but not array - it's a dictionary.
                // The RFC doesn't say anything about the ordering of keys, but
                // considering that lots of people rely on keys being "in order" as
                // defined by "the same way someone put them in at the other end,"
                // it's probably a good idea to put some cycles into meeting their
                // misguided expectations.
                var ks = [];
                for(var k in obj) {
                    ks.unshift(k);
                }
                var xs = [0];

                for(var i = 0; i < ks.length; i++) {
                    xs = [1, [0, [0,ks[i]], toHsValue(obj[ks[i]])], xs];
                }
                return [5, xs];
            }
        }
    }

    /* unfold the array into a list */
    var arr2lst = function (arr, elem) {
        if(elem >= arr.length) {
            return [0]; /* constructur for [] */
        }
        return [1, toHsValue(arr[elem]), new T(function() {return arr2lst(arr,elem+1);})]
    }

    var fromHsValue = function (jsVal) {
        var obj = E(jsVal);
        switch(obj[0]) {
        case 0: /* Number */
            return obj[1][1][1];
            break;
        case 1: /* String */
            return obj[1][1];
            break;
        case 2: /* Boolean */
            return obj[1];
            break;
        case 3: /* Null */
            return null;
            break;
        case 4: /* Array */
            return lst2arr(obj[1],[]);
            break;
        case 5: /* Object */
            var r = {};
            var o = obj[1];
            while (o[0] === 1) {
                var tuple = E(o[1]);
                var prop = E(tuple[1])
                if (tuple[0] === 0 && prop[0] === 0) {
                    r[prop[1]] = fromHsValue(tuple[2]);
                    o = E(o[2]);
                } else {
                    console.log("fromHsValue failed with prop[0] === " + tuple[0] + " and prop[0] === " + prop[0]);
                    return null;
                }
            }
            if (o[0] === 0) {
                return r;
            } else {
                console.log("fromHsValue failed with o[0] === " + o[0]);
                return null;
            }
            break;
        }
    }

    var lst2arr = function(l,a) {
        var l1 = E(l);
        if (l1[0] === 0) {
            return a;
        } else {
            a[a.length] = fromHsValue(l1[1]);
            return lst2arr(l1[2],a);
        }
    }

    return {
        "fromHsValue" : fromHsValue,
        "toHsValue" : toHsValue,
        "parseJSON" : parseJSON,
    };

})();

var jsonParseUri = function (x) {
    return Bayhac2014JSON.parseUri(x);
}

var parseJson = function (x) {
    return Bayhac2014JSON.parseJSON(x);
}

var jsonStringify = function (x) {
    return JSON.stringify(x);
}

/* ************************************************************************** */
/* export */

function API () {
    return this;
}

API.prototype.addMethod = function (name, method) {
    API.prototype[name] = function (arg, log, fail, succ) {
        var successCB = function (hsResult) {

            /* convert result from the Haste JSON representation into a Javascript Object */
            var result = Bayhac2014JSON.fromHsValue(hsResult);

            /* ensure that the result is an object */
            if (typeof result === 'object') {
                succ(result);
            } else {
                var err = "WARNING: results of API methods are expected to be objects";
                log(err + "Result: " + JSON.stringify(result));
                fail(err + "Failure: " + JSON.stringify(result));
            }
        }

        /* ensure that the argument is an object */
        if (typeof arg === 'object') {

            /* parse the argument object into the Haste JSON representation */
            var hsArg = Bayhac2014JSON.toHsValue(arg);

            /* call the method callback */
            var mapply = function () { return A(method,[[0,hsArg],[0,log],[0,fail],[0,successCB],0]); };
            setTimeout(mapply,0);

        } else {
            fail("WARNING: arguments to API methods are expected to be objects: " + JSON.stringify(arg));
        }
    }
    return 0;
}

/* Global function wrapper for Haste FFI
 */
function addApiMethod(name, method) {
    API.prototype.addMethod(name, method);
}

function calls(f,s) { f(E(s)[1]); }
function callv(f,v) { f(v[1]); }

