h$SystemziGlibziMainLoop_df30 = function () {}
h$LanguageziJavascriptziJSCziObject_dbwE = function () {}
h$LanguageziJavascriptziJSCziObject_dnMr = function () {};
h$LanguageziJavascriptziJSCziObject_dnMu = function () {};

function h$JSContextGetGlobalObject(ctx) {
    return ctx;
};
function h$JSObjectGetProperty(ctx, ctx_2, this_, this_2, name, name_2, pexception, pexception_2) {
    try {
        return this_[name];
    }
    catch(e) {
        pexception.arr[pexception_2] = [e, 0];
    }
};
function h$JSObjectGetPropertyAtIndex(ctx, ctx_2, this_, this_2, n, pexception, pexception_2) {
    try {
        return this_[n];
    }
    catch(e) {
        pexception.arr[pexception_2] = [e, 0];
    }
};
function h$JSObjectSetProperty(ctx, ctx_2, this_, this_2, name, name_2, value, value_2, attrs, attrs_2, pexception, pexception_2) {
    try {
        this_[name] = value;
    }
    catch(e) {
        pexception.arr[pexception_2] = [e, 0];
    }
};
function h$JSObjectSetPropertyAtIndex(ctx, ctx_2, this_, this_2, n, value, value_2, attrs, attrs_2, pexception, pexception_2) {
    try {
        this_[n] = value;
    }
    catch(e) {
        pexception.arr[pexception_2] = [e, 0];
    }
};
function h$JSObjectMakeFunctionWithCallback(ctx, ctx_2, name, name_2, callback, callback_2) {
    var f = function() {
        var argv = h$malloc(arguments.length<<2);
        argv.arr = [];
        for(var i = 0; i != arguments.length; ++i)
            argv.arr[i<<2] = [arguments[i],0];
        var ex = h$malloc(4);
        ex.arr = [[null,0]];
        h$runSync(h$c3(h$ap2_e,
            h$c6(h$pap_4,
                callback.arr[callback_2],
                2,
                h$mkPtr(ctx, ctx_2),
                h$mkPtr(f, 0),
                h$mkPtr(this, 0),
                arguments.length),
            h$mkPtr(argv,0),
            h$mkPtr(ex,0)), true);
        var e = ex.arr[0][0];
        if(e !== null) throw e;
    }

    return f;
};
function h$JSObjectCallAsFunction(ctx, ctx_2, f, f_2, this_, this_2, argc, argv, argv_2, pexception, pexception_2) {
    try {
        var a = [];
        for(var i = 0; i != argc; i++) {
            a[i] = argv.arr[argv_2+(i<<2)][0];
        }
        return f.apply(this_, a);
    }
    catch(e) {
        pexception.arr[pexception_2] = [e, 0];
    }
};
function h$JSObjectCallAsConstructor(ctx, ctx_2, f, f_2, argc, argv, argv_2, pexception, pexception_2) {
    try {
        var a = [];
        for(var i = 0; i != argc; i++) {
            a[i] = argv.arr[argv_2+(i<<2)][0];
        }
        switch(argc) {
            case 0 : return new f();break;
            case 1 : return new f(a[0]);break;
            case 2 : return new f(a[0],a[1]);break;
            case 3 : return new f(a[0],a[1],a[2]);break;
            case 4 : return new f(a[0],a[1],a[2],a[3]);break;
            case 5 : return new f(a[0],a[1],a[2],a[3],a[4]);break;
            case 6 : return new f(a[0],a[1],a[2],a[3],a[4],a[5]);break;
            case 7 : return new f(a[0],a[1],a[2],a[3],a[4],a[5],a[6]);break;
            default:
                var ret;
                var temp = function() {
                    ret = f.apply(this, a.slice(0, argc));
                };
                temp.prototype = f.prototype;
                var instance = new temp();
                if (ret instanceof Object)
                    return ret;
                instance.constructor = f;
                return instance;
        }
    }
    catch(e) {
        pexception.arr[pexception_2] = [e, 0];
    }
};
function h$JSObjectMakeArray(ctx, ctx_2, l, p, p_2, pexception, pexception_2) {
    try {
        var a = [];
        for(var i = 0; i != l; i++) {
            a[i] = p.arr[p_2+(i<<2)][0];
        }
        return a;
    }
    catch(e) {
        pexception.arr[pexception_2] = [e, 0];
    }
};
function h$JSValueGetType(ctx, ctx_2, v, v_2) {
  if(v === undefined)        return 0;
  if(v === null)             return 1;
  if(typeof v === "boolean") return 2;
  if(typeof v === "number")  return 3;
  if(typeof v === "string")  return 4;
  if(typeof v === "object")  return 5;
};
function h$JSStringCreateWithCharacters(p, p_2, len) {
    return h$decodeUtf16l(p, len << 1, p_2);
};
function h$JSStringCreateWithUTF8CString(p) {
  return h$decodeUtf8z(p, p_2);
};
function h$JSValueMakeBoolean(ctx, b) {
  return b !== 0;
};
function h$JSValueMakeFromJSONString(ctx, ctx_2, string, string_2) {
  try {
    return ctx.JSON.parse(string);
  }
  catch(e) {
    return null;
  }
};
function h$JSValueMakeNull(ctx, ctx_2) {
  return null;
};
function h$JSValueMakeNumber(ctx, ctx_2, n) {
  return n;
};
function h$JSValueMakeString(ctx, ctx_2, s, s_2) {
  return s;
};
function h$JSValueMakeUndefined(ctx, ctx_2) {
  return undefined;
};
function h$JSValueProtect(ctx, ctx_2, v, v_2) {
};
function h$JSValueUnprotect(ctx, ctx_2, v, v_2) {
};
function h$JSValueToBoolean(ctx, ctx_2, v, v_2) {
  return v?1:0;
};
function h$JSValueToBoolean(ctx, ctx_2, v, v_2) {
  return v?1:0;
};
function h$JSValueToNumber(ctx, ctx_2, v, v_2, e, e_2) {
  return Number(v);
};
function h$JSValueToObject(ctx, ctx_2, v, v_2, e, e_2) {
  return v;
};
function h$JSValueToStringCopy(ctx, ctx_2, v, v_2, e, e_2) {
  return v.toString();
};
function h$JSStringIsEqual(a, a_2, b, b_2) {
  return a == b?1:0;
};
function h$JSStringIsStrictEqual(a, a_2, b, b_2) {
  return a === b?1:0;
};
function h$JSStringGetLength(s, s_2) {
    return s.length;
};
function h$JSStringGetCharactersPtr(s, s_2) {
    return h$encodeUtf16(s);
};
function h$JSEvaluateScript(ctx, ctx_2, script, script_2, thisObject, thisObject_2, sourceURL, sourceURL_2, startingLineNumber, exception, exception_2) {
  return eval(script);
};
function h$JSValueIsUndefined(ctx, v) {
  return v === undefined?1:0;
};
function h$JSValueIsNull(ctx, v) {
  return v === null?1:0;
};
function h$JSValueIsBoolean(ctx, v) {
  return typeof v === "boolean"?1:0;
};
function h$JSValueIsNumber(ctx, v) {
  return typeof v === "number"?1:0;
};
function h$JSValueIsString(ctx, v) {
  return typeof v === "string"?1:0;
};
function h$JSValueIsObject(ctx, v) {
  return typeof v === "object"?1:0;
};

