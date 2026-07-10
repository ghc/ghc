
type key = string | number;

type seconds = number;

type color = string;

type MapString<T> = { [key: string]: T };
type MapNumber<T> = { [key: number]: T };

type int = number;
type MapInt<T> = MapNumber<T>;


/////////////////////////////////////////////////////////////////////
// JQUERY EXTENSIONS

// tslint:disable-next-line: interface-name
interface JQuery {
    enable(x: boolean): JQuery;
}

jQuery.fn.enable = function(x: boolean) {
    // Set the values to enabled/disabled
    return this.each(function() {
        if (x)
            $(this).removeAttr("disabled");
        else
            $(this).attr("disabled", "disabled");
    });
};


/////////////////////////////////////////////////////////////////////
// BROWSER HELPER METHODS

// Given "?foo=bar&baz=1" returns {foo:"bar",baz:"1"}
function uriQueryParameters(s: string): MapString<string> {
    // From https://stackoverflow.com/questions/901115/get-querystring-values-with-jquery/3867610#3867610
    const params: MapString<string> = {};
    const a = /\+/g;  // Regex for replacing addition symbol with a space
    const r = /([^&=]+)=?([^&]*)/g;
    const d = (x: string) => decodeURIComponent(x.replace(a, " "));
    const q = s.substring(1);

    while (true) {
        const e = r.exec(q);
        if (!e) break;
        params[d(e[1])] = d(e[2]);
    }
    return params;
}


/////////////////////////////////////////////////////////////////////
// STRING FORMATTING

function showTime(x: seconds): string {
    function digits(x: seconds) {const s = String(x); return s.length === 1 ? "0" + s : s; }

    if (x >= 3600) {
        x = Math.round(x / 60);
        return Math.floor(x / 60) + "h" + digits(x % 60) + "m";
    } else if (x >= 60) {
        x = Math.round(x);
        return Math.floor(x / 60) + "m" + digits(x % 60) + "s";
    } else
        return x.toFixed(2) + "s";
}

function showPerc(x: number): string {
    return (x * 100).toFixed(2) + "%";
}

function showInt(x: int): string {
    // From https://stackoverflow.com/questions/2901102/how-to-print-a-number-with-commas-as-thousands-separators-in-javascript
    // Show, with commas
    return x.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
}

function showRun(run: timestamp): string {
    return run === 0 ? "Latest run" : run + " run" + plural(run) + " ago";
}

function plural(n: int, not1 = "s", is1 = ""): string {
    return n === 1 ? is1 : not1;
}


/////////////////////////////////////////////////////////////////////
// MISC

function compareFst<A>(a: [number, A], b: [number, A]): number {
    return a[0] - b[0];
}

function compareSnd<A>(a: [A, number], b: [A, number]): number {
    return a[1] - b[1];
}

function compareSndRev<A>(a: [A, number], b: [A, number]): number {
    return b[1] - a[1];
}

function pair<A, B>(a: A, b: B): [A, B] {
    return [a, b];
}

function triple<A, B, C>(a: A, b: B, c: C): [A, B, C] {
    return [a, b, c];
}

function fst<A, B>([x, _]: [A, B]): A {
    return x;
}

function snd<A, B>([_, x]: [A, B]): B {
    return x;
}

function execRegExp(r: string | RegExp, s: string): string[] {
    if (typeof r === "string")
        return s.indexOf(r) === -1 ? null : [];
    else
        return r.exec(s);
}

function cache<K, V>(key: (k: K) => string, op: (k: K) => V): (k: K) => V {
    const store: MapString<V> = {};
    return k => {
        const s = key(k);
        if (!(s in store))
            store[s] = op(k);
        return store[s];
    };
}

function lazy<V>(thunk: () => V): () => V {
    let store: V = null;
    let done = false;
    return () => {
        if (!done) {
            store = thunk();
            done = true;
        }
        return store;
    };
}

interface Array<T> {
    insertSorted(x: T, compare: (a: T, b: T) => number): T[];
    concatLength<A, T extends A[]>(): int;
    sortOn(f: (x: T) => number): T[];
    last(): T;
    sum<T extends number>(): number;
    maximum<T extends number>(def?: number): number;
    minimum<T extends number>(def?: number): number;
}

Array.prototype.sum = function<T>(this: number[]): number {
    let res = 0;
    for (const x of this)
        res += x;
    return res;
};

Array.prototype.insertSorted = function<T>(this: T[], x: T, compare: (a: T, b: T) => number): T[] {
    let start = 0;
    let stop = this.length - 1;
    let middle = 0;
    while (start <= stop) {
        middle = Math.floor((start + stop) / 2);
        if (compare(this[middle], x) > 0)
            stop = middle - 1;
        else
            start = middle + 1;
    }
    this.splice(start, 0, x);
    return this;
};

Array.prototype.concatLength = function<A>(this: A[][]): int {
    let res = 0;
    for (const x of this)
        res += x.length;
    return res;
};

Array.prototype.sortOn = function<T>(this: T[], f: (x: T) => number): T[] {
    return this.map(x => pair(f(x), x)).sort(compareFst).map(snd);
};

Array.prototype.last = function<T>(this: T[]): T {
    return this[this.length - 1];
};

Array.prototype.maximum = function<T>(this: number[], def?: number): number {
    if (this.length === 0) return def;
    let res: number = this[0];
    for (let i = 1; i < this.length; i++)
        res = Math.max(res, this[i]);
    return res;
};

Array.prototype.minimum = function<T>(this: number[], def?: number): number {
    if (this.length === 0) return def;
    let res: number = this[0];
    for (let i = 1; i < this.length; i++)
        res = Math.min(res, this[i]);
    return res;
};


// Use JSX with el instead of React.createElement
// Originally from https://gist.github.com/sergiodxa/a493c98b7884128081bb9a281952ef33

// our element factory
function createElement(type: string, props?: MapString<any>, ...children: any[]) {
    const element = document.createElement(type);

    for (const name in props || {}) {
        if (name.substr(0, 2) === "on")
            element.addEventListener(name.substr(2), props[name]);
        else
            element.setAttribute(name, props[name]);
    }
    for (const child of children.flat(10)) {
        const c = typeof child === "object" ? child : document.createTextNode(child.toString());
        element.appendChild(c);
    }
    return element;
}

// How .tsx gets desugared
const React = {createElement};
