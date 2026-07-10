"use strict";
function bindPlot(element, data, options) {
    const redraw = () => {
        if ($(element).is(":visible"))
            $.plot($(element), data.get(), options);
    };
    window.setTimeout(redraw, 1);
    $(window).on("resize", redraw);
    data.event(redraw);
}
function varLink(name) {
    return React.createElement("a", { href: "https://hackage.haskell.org/package/shake/docs/Development-Shake.html#v:" + name },
        React.createElement("tt", null, name));
}
function newTable(columns, data, sortColumn, sortDescend) {
    const f = (x) => ({ name: x.field, label: x.label, width: x.width, cellClasses: x.alignRight ? "right" : "" });
    const formatters = {};
    for (const c of columns)
        formatters[c.field] = c.show || ((x) => x);
    const table = new DGTable({
        adjustColumnWidthForSortArrow: false,
        cellFormatter: (val, colname) => formatters[colname](val),
        columns: columns.map(f),
        width: DGTable.Width.SCROLL
    });
    $(table.el).css("height", "100%");
    window.setTimeout(() => {
        table.render();
        table.tableHeightChanged();
        if (sortColumn)
            table.sort(sortColumn, sortDescend);
        table.setRows(data.get(), true);
    }, 1);
    let toRender = false;
    data.event(xs => {
        table.setRows(xs, true);
        if ($(table.el).is(":visible"))
            table.render();
        else
            toRender = true;
    });
    $(window).on("resize", () => {
        if ($(table.el).is(":visible")) {
            table.tableHeightChanged();
            if (toRender) {
                table.render();
                toRender = false;
            }
        }
    });
    return React.createElement("div", { style: "height:100%;width:100%;" }, table.el);
}
// These are global variables mutated/queried by query execution
let environmentAll; // All the profiles
let environmentThis; // The specific profile under test
let environmentGroup; // The group produced as a result
function group(x) {
    environmentGroup.push(x);
    return true;
}
function leaf() {
    return environmentThis.depends.length === 0;
}
function run(i) {
    if (i === undefined)
        return environmentThis.built;
    else
        return environmentThis.built === i;
}
function changed() {
    return environmentThis.changed === environmentThis.built;
}
function unchanged() {
    return !unchanged();
}
function named(r, groupName) {
    if (r === undefined)
        return environmentThis.name;
    const res = execRegExp(r, environmentThis.name);
    if (res === null) {
        if (groupName === undefined)
            return false;
        else {
            group(groupName);
            return true;
        }
    }
    if (res.length !== 1) {
        for (let i = 1; i < res.length; i++)
            group(res[i]);
    }
    return true;
}
function command(r, groupName) {
    const n = (environmentThis.traces || []).length;
    if (r === undefined)
        return n === 0 ? "" : environmentThis.traces[0].command;
    for (const t of environmentThis.traces) {
        const res = execRegExp(r, t.command);
        if (res === null)
            continue;
        if (res.length !== 1) {
            for (let j = 1; j < res.length; j++)
                group(res[j]);
        }
        return true;
    }
    if (groupName === undefined)
        return false;
    else {
        group(groupName);
        return true;
    }
}
function profileLoaded(profileRaw) {
    $(document.body).empty().append(profileRoot(unraw(profileRaw)));
}
function unraw(xs) {
    const ans = xs.map((x, i) => ({
        index: i,
        name: x[0],
        execution: x[1],
        built: x[2],
        changed: x[3],
        depends: x.length > 4 ? x[4] : [],
        rdepends: [],
        traces: x.length > 5 ? x[5].map(y => ({ command: y[0], start: y[1], stop: y[2] })) : []
    }));
    for (const p of ans)
        for (const ds of p.depends)
            for (const d of ds)
                ans[d].rdepends.push(p.index);
    return ans;
}
function profileRoot(profile) {
    const [s, search] = createSearch(profile);
    const t = createTabs([["Summary", () => reportSummary(profile)],
        ["Command plot", () => reportCmdPlot(profile)],
        ["Commands", () => reportCmdTable(profile, search)],
        ["Rules", () => reportRuleTable(profile, search)],
        ["Parallelizability", () => reportParallelism(profile)],
        ["Details", () => reportDetails(profile, search)]
        // , ["Why rebuild", () => reportRebuild(profile, search)]
    ]);
    return React.createElement("table", { class: "fill" },
        React.createElement("tr", null,
            React.createElement("td", { style: "padding-top: 8px; padding-bottom: 8px;" },
                React.createElement("a", { href: "https://shakebuild.com/", style: "font-size: 20px; text-decoration: none; color: #3131a7; font-weight: bold;" }, "Shake profile report"),
                React.createElement("span", { style: "color:gray;white-space:pre;" },
                    "   - generated at ",
                    generated,
                    " by Shake v",
                    version))),
        React.createElement("tr", null,
            React.createElement("td", null, s)),
        React.createElement("tr", null,
            React.createElement("td", { height: "100%" }, t)));
}
function createTabs(xs) {
    const bodies = xs.map(x => {
        const el = React.createElement("div", { style: "padding:5px;width:100%;height:100%;min-width:150px;min-height:150px;overflow:auto;display:none;" });
        const upd = lazy(() => $(el).append(x[1]()));
        return pair(el, upd);
    });
    let lbls = [];
    const f = (i) => () => {
        bodies[i][1]();
        lbls.map((x, j) => $(x).toggleClass("active", i === j));
        bodies.map((x, j) => $(x[0]).toggle(i === j));
        $(window).trigger("resize");
    };
    lbls = xs.map((x, i) => React.createElement("a", { onclick: f(i) }, x[0]));
    f(0)();
    return React.createElement("table", { class: "fill" },
        React.createElement("tr", null,
            React.createElement("td", null,
                React.createElement("table", { width: "100%", style: "border-spacing:0px;" },
                    React.createElement("tr", { class: "tabstrip" },
                        React.createElement("td", { width: "20", class: "bottom" }, "\u00A0"),
                        React.createElement("td", { style: "padding:0px;" }, lbls),
                        React.createElement("td", { width: "100%", class: "bottom" }, "\u00A0"))))),
        React.createElement("tr", { height: "100%" },
            React.createElement("td", { style: "background-color:white;" }, bodies.map(fst))));
}
// A mapping from names (rule names or those matched from rule parts)
// to the indicies in profiles.
class Search {
    constructor(profile, mapping) {
        this.profile = profile;
        if (mapping !== undefined)
            this.mapping = mapping;
        else {
            this.mapping = {};
            for (const p of profile)
                this.mapping[p.name] = [p.index];
        }
    }
    forEachProfiles(f) {
        for (const s in this.mapping)
            f(this.mapping[s].map(i => this.profile[i]), s);
    }
    forEachProfile(f) {
        this.forEachProfiles((ps, group) => ps.forEach(p => f(p, group)));
    }
    mapProfiles(f) {
        const res = [];
        this.forEachProfiles((ps, group) => res.push(f(ps, group)));
        return res;
    }
    mapProfile(f) {
        const res = [];
        this.forEachProfile((p, group) => res.push(f(p, group)));
        return res;
    }
}
function createSearch(profile) {
    const caption = React.createElement("div", null,
        "Found ",
        profile.length,
        " entries, not filtered or grouped.");
    const input = React.createElement("input", { id: "search", type: "text", value: "", placeholder: "Filter and group", style: "width: 100%; font-size: 16px; border-radius: 8px; padding: 5px 10px; border: 2px solid #999;" });
    const res = new Prop(new Search(profile));
    $(input).on("change keyup paste", () => {
        const s = $(input).val();
        if (s === "") {
            res.set(new Search(profile));
            $(caption).text("Found " + profile.length + " entries, not filtered or grouped.");
        }
        else if (s.indexOf("(") === -1) {
            const mapping = {};
            let found = 0;
            for (const p of profile) {
                if (p.name.indexOf(s) !== -1) {
                    found++;
                    mapping[p.name] = [p.index];
                }
            }
            res.set(new Search(profile, mapping));
            $(caption).text("Substring filtered to " + found + " / " + profile.length + " entries, not grouped.");
        }
        else {
            let f;
            try {
                f = new Function("return " + s);
            }
            catch (e) {
                $(caption).text("Error compiling function, " + e);
                return;
            }
            const mapping = {};
            let groups = 0;
            let found = 0;
            environmentAll = profile;
            for (const p of profile) {
                environmentThis = p;
                environmentGroup = [];
                let bool;
                try {
                    bool = f();
                }
                catch (e) {
                    $(caption).text("Error running function, " + e);
                    return;
                }
                if (bool) {
                    found++;
                    const name = environmentGroup.length === 0 ? p.name : environmentGroup.join(" ");
                    if (name in mapping)
                        mapping[name].push(p.index);
                    else {
                        groups++;
                        mapping[name] = [p.index];
                    }
                }
            }
            res.set(new Search(profile, mapping));
            $(caption).text("Function filtered to " + found + " / " + profile.length + " entries, " +
                (groups === found ? "not grouped." : groups + " groups."));
        }
    });
    const body = React.createElement("table", { width: "100%", style: "padding-bottom: 17px;" },
        React.createElement("tr", null,
            React.createElement("td", { width: "100%" }, input),
            React.createElement("td", { style: "padding-left:6px;padding-right: 6px;" }, searchHelp(input))),
        React.createElement("tr", null,
            React.createElement("td", null, caption)));
    return [body, res];
}
function searchHelp(input) {
    const examples = [["Only the last run", "run(0)"],
        ["Named 'Main'", "named(\"Main\")"],
        ["Group by file extension", "named(/(\\.[_0-9a-z]+)$/)"],
        ["No dependencies (an input)", "leaf()"],
        ["Didn't change when it last rebuilt", "unchanged()"],
        ["Ran 'gcc'", "command(\"gcc\")"]
    ];
    const f = (code) => () => {
        $(input).val((i, x) => x + (x === "" ? "" : " && ") + code);
        $(input).trigger("change");
    };
    const dropdown = React.createElement("div", { class: "dropdown", style: "display:none;" },
        React.createElement("ul", { style: "padding-left:30px;" }, examples.map(([desc, code]) => React.createElement("li", null,
            React.createElement("a", { onclick: f(code) },
                React.createElement("tt", null, code)),
            " ",
            React.createElement("span", { class: "note" }, desc)))));
    const arrow_down = React.createElement("span", { style: "vertical-align:middle;font-size:80%;" }, "\u25BC");
    const arrow_up = React.createElement("span", { style: "vertical-align:middle;font-size:80%;display:none;" }, "\u25B2");
    const show_inner = () => { $(dropdown).toggle(); $(arrow_up).toggle(); $(arrow_down).toggle(); };
    return React.createElement("div", null,
        React.createElement("button", { style: "white-space:nowrap;padding-top:5px;padding-bottom:5px;", onclick: show_inner },
            React.createElement("b", { style: "font-size:150%;vertical-align:middle;" }, "+"),
            "\u00A0 Filter and Group \u00A0",
            arrow_down,
            arrow_up),
        dropdown);
}
function initProgress() {
    $(function () {
        $(".version").html("Generated by <a href='https://shakebuild.com'>Shake " + version + "</a>.");
        $("#output").html("");
        for (const x of progress) {
            var actual = [];
            var ideal = [];
            // Start at t = 5 seconds, since the early progress jumps a lot
            for (var t = 5; t < x.values.length; t++) {
                var y = x.values[t];
                actual.push([y.idealSecs, y.actualSecs]);
                ideal.push([y.idealSecs, y.idealSecs]);
            }
            var ys = [{ data: ideal, color: "gray" }, { label: x.name, data: actual, color: "red" }];
            var div = $("<div class='plot'>");
            $("#output").append(div);
            $.plot(div, ys, {
                xaxis: {
                    transform: function (v) { return -v; },
                    inverseTransform: function (v) { return -v; }
                }
            });
        }
    });
}
// Stuff that Shake generates and injects in
function untraced(p) {
    return Math.max(0, p.execution - p.traces.map(t => t.stop - t.start).sum());
}
/////////////////////////////////////////////////////////////////////
// BASIC UI TOOLKIT
class Prop {
    constructor(val) { this.val = val; this.callback = () => { return; }; }
    get() { return this.val; }
    set(val) {
        this.val = val;
        this.callback(val);
    }
    event(next) {
        const old = this.callback;
        this.callback = val => { old(val); next(val); };
        next(this.val);
    }
    map(f) {
        const res = new Prop(f(this.get()));
        this.event(a => res.set(f(a)));
        return res;
    }
}
jQuery.fn.enable = function (x) {
    // Set the values to enabled/disabled
    return this.each(function () {
        if (x)
            $(this).removeAttr("disabled");
        else
            $(this).attr("disabled", "disabled");
    });
};
/////////////////////////////////////////////////////////////////////
// BROWSER HELPER METHODS
// Given "?foo=bar&baz=1" returns {foo:"bar",baz:"1"}
function uriQueryParameters(s) {
    // From https://stackoverflow.com/questions/901115/get-querystring-values-with-jquery/3867610#3867610
    const params = {};
    const a = /\+/g; // Regex for replacing addition symbol with a space
    const r = /([^&=]+)=?([^&]*)/g;
    const d = (x) => decodeURIComponent(x.replace(a, " "));
    const q = s.substring(1);
    while (true) {
        const e = r.exec(q);
        if (!e)
            break;
        params[d(e[1])] = d(e[2]);
    }
    return params;
}
/////////////////////////////////////////////////////////////////////
// STRING FORMATTING
function showTime(x) {
    function digits(x) { const s = String(x); return s.length === 1 ? "0" + s : s; }
    if (x >= 3600) {
        x = Math.round(x / 60);
        return Math.floor(x / 60) + "h" + digits(x % 60) + "m";
    }
    else if (x >= 60) {
        x = Math.round(x);
        return Math.floor(x / 60) + "m" + digits(x % 60) + "s";
    }
    else
        return x.toFixed(2) + "s";
}
function showPerc(x) {
    return (x * 100).toFixed(2) + "%";
}
function showInt(x) {
    // From https://stackoverflow.com/questions/2901102/how-to-print-a-number-with-commas-as-thousands-separators-in-javascript
    // Show, with commas
    return x.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
}
function showRun(run) {
    return run === 0 ? "Latest run" : run + " run" + plural(run) + " ago";
}
function plural(n, not1 = "s", is1 = "") {
    return n === 1 ? is1 : not1;
}
/////////////////////////////////////////////////////////////////////
// MISC
function compareFst(a, b) {
    return a[0] - b[0];
}
function compareSnd(a, b) {
    return a[1] - b[1];
}
function compareSndRev(a, b) {
    return b[1] - a[1];
}
function pair(a, b) {
    return [a, b];
}
function triple(a, b, c) {
    return [a, b, c];
}
function fst([x, _]) {
    return x;
}
function snd([_, x]) {
    return x;
}
function execRegExp(r, s) {
    if (typeof r === "string")
        return s.indexOf(r) === -1 ? null : [];
    else
        return r.exec(s);
}
function cache(key, op) {
    const store = {};
    return k => {
        const s = key(k);
        if (!(s in store))
            store[s] = op(k);
        return store[s];
    };
}
function lazy(thunk) {
    let store = null;
    let done = false;
    return () => {
        if (!done) {
            store = thunk();
            done = true;
        }
        return store;
    };
}
Array.prototype.sum = function () {
    let res = 0;
    for (const x of this)
        res += x;
    return res;
};
Array.prototype.insertSorted = function (x, compare) {
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
Array.prototype.concatLength = function () {
    let res = 0;
    for (const x of this)
        res += x.length;
    return res;
};
Array.prototype.sortOn = function (f) {
    return this.map(x => pair(f(x), x)).sort(compareFst).map(snd);
};
Array.prototype.last = function () {
    return this[this.length - 1];
};
Array.prototype.maximum = function (def) {
    if (this.length === 0)
        return def;
    let res = this[0];
    for (let i = 1; i < this.length; i++)
        res = Math.max(res, this[i]);
    return res;
};
Array.prototype.minimum = function (def) {
    if (this.length === 0)
        return def;
    let res = this[0];
    for (let i = 1; i < this.length; i++)
        res = Math.min(res, this[i]);
    return res;
};
// Use JSX with el instead of React.createElement
// Originally from https://gist.github.com/sergiodxa/a493c98b7884128081bb9a281952ef33
// our element factory
function createElement(type, props, ...children) {
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
const React = { createElement };
function reportCmdPlot(profile) {
    // first find the end point
    const runs = findRuns(profile);
    if (runs.length === 0) {
        return React.createElement("div", null,
            React.createElement("h2", null, "No data found"),
            React.createElement("p", null, "The Shake database contains no rules which ran traced commands."),
            React.createElement("p", null,
                "You can populate this information by using ",
                varLink("cmd"),
                " or wrapping your ",
                React.createElement("tt", null, "IO"),
                " actions in ",
                varLink("traced"),
                "."));
    }
    const combo = React.createElement("select", null,
        runs.map(([run, time], i) => React.createElement("option", null,
            showRun(run) + " (" + showTime(time) + ") ",
            i === 0 ? "" : " - may be incomplete")),
        ";");
    const warning = React.createElement("i", null);
    const plot = React.createElement("div", { style: "width:100%; height:100%;" });
    const plotData = new Prop([]);
    bindPlot(plot, plotData, {
        legend: { show: true, position: "nw", sorted: "reverse" },
        series: { stack: true, lines: { fill: 1, lineWidth: 0 } },
        yaxis: { min: 0 },
        xaxis: { tickFormatter: showTime }
    });
    function setPlotData(runsIndex) {
        const [run, end] = runs[runsIndex];
        const profileRun = profile.filter(p => p.built === run);
        // Make sure we max(0,) every step in the process, in case one does parallelism of threads
        const missing = profileRun.map(untraced).sum();
        $(warning).text(missing < 1 ? "" : "Warning: " + showTime(missing) + " of execution was not traced.");
        const series = calcPlotData(end, profileRun, 100);
        const res = [];
        for (const s in series)
            res.push({ label: s, data: series[s].map((x, i) => pair(end * i / 100, x)) });
        plotData.set(res);
    }
    setPlotData(0);
    $(combo).change(() => setPlotData(combo.selectedIndex));
    return React.createElement("table", { class: "fill" },
        React.createElement("tr", null,
            React.createElement("td", { width: "100%", style: "text-align:center;" },
                React.createElement("h2", null, "Number of commands executing over time")),
            React.createElement("td", null, combo)),
        React.createElement("tr", null,
            React.createElement("td", { height: "100%", colspan: "2" }, plot)),
        React.createElement("tr", null,
            React.createElement("td", { colspan: "2", style: "text-align:center;" },
                "Time since the start of building. ",
                warning)));
}
// Find which runs had traced commands and when the last stopped, sort so most recent first
function findRuns(profile) {
    const runs = {};
    for (const p of profile) {
        if (p.traces.length > 0) {
            if (p.traces.length === 1 && p.traces[0].command === "")
                continue; // the fake end command
            const old = runs[p.built];
            const end = p.traces.last().stop;
            runs[p.built] = old === undefined ? end : Math.max(old, end);
        }
    }
    const runsList = [];
    for (const i in runs)
        runsList.push(pair(Number(i), runs[i]));
    runsList.sort(compareFst);
    return runsList;
}
function calcPlotData(end, profile, buckets) {
    const ans = {};
    for (const p of profile) {
        for (const t of p.traces) {
            let xs;
            if (t.command in ans)
                xs = ans[t.command];
            else {
                xs = [];
                for (let i = 0; i < buckets; i++)
                    xs.push(0); // fill with 1 more element, but the last bucket will always be 0
                ans[t.command] = xs;
            }
            const start = t.start * buckets / end;
            const stop = t.stop * buckets / end;
            if (Math.floor(start) === Math.floor(stop))
                xs[Math.floor(start)] += stop - start;
            else {
                for (let j = Math.ceil(start); j < Math.floor(stop); j++)
                    xs[j]++;
                xs[Math.floor(start)] += Math.ceil(start) - start;
                xs[Math.floor(stop)] += stop - Math.floor(stop);
            }
        }
    }
    return ans;
}
function reportCmdTable(profile, search) {
    const columns = [{ field: "name", label: "Name", width: 200 },
        { field: "count", label: "Count", width: 65, alignRight: true, show: showInt },
        { field: "total", label: "Total", width: 75, alignRight: true, show: showTime },
        { field: "average", label: "Average", width: 75, alignRight: true, show: showTime },
        { field: "max", label: "Max", width: 75, alignRight: true, show: showTime }
    ];
    return newTable(columns, search.map(cmdData), "total", true);
}
function cmdData(search) {
    const res = {};
    search.forEachProfile(p => {
        for (const t of p.traces) {
            const time = t.stop - t.start;
            if (t.command === "")
                continue; // do nothing
            else if (!(t.command in res))
                res[t.command] = { count: 1, total: time, max: time };
            else {
                const ans = res[t.command];
                ans.count++;
                ans.total += time;
                ans.max = Math.max(ans.max, time);
            }
        }
    });
    const res2 = [];
    for (const i in res)
        res2.push({ name: i, average: res[i].total / res[i].count, ...res[i] });
    return res2;
}
function reportDetails(profile, search) {
    const result = React.createElement("div", { class: "details" });
    const self = new Prop(0);
    search.event(xs => self.set(xs.mapProfile((p, _) => p.index).maximum()));
    const f = (i) => React.createElement("a", { onclick: () => self.set(i) }, profile[i].name);
    self.event(i => {
        const p = profile[i];
        const content = React.createElement("ul", null,
            React.createElement("li", null,
                React.createElement("b", null, "Name:"),
                " ",
                p.name),
            React.createElement("li", null,
                React.createElement("b", null, "Built:"),
                " ",
                showRun(p.built)),
            React.createElement("li", null,
                React.createElement("b", null, "Changed:"),
                " ",
                showRun(p.changed)),
            React.createElement("li", null,
                React.createElement("b", null, "Execution time:"),
                showTime(p.execution)),
            React.createElement("li", null,
                React.createElement("b", null, "Traced commands:"),
                React.createElement("ol", null, p.traces.map(t => React.createElement("li", null,
                    t.command,
                    " took ",
                    showTime(t.stop - t.start))))),
            React.createElement("li", null,
                React.createElement("b", null, "Dependencies:"),
                React.createElement("ol", null, p.depends.map(ds => React.createElement("li", null,
                    React.createElement("ul", null, ds.map(d => React.createElement("li", null, f(d)))))))),
            React.createElement("li", null,
                React.createElement("b", null, "Things that depend on me:"),
                React.createElement("ul", null, p.rdepends.map(d => React.createElement("li", null, f(d))))));
        $(result).empty().append(content);
    });
    return result;
}
function reportParallelism(profile) {
    // now simulate for -j1 .. -j24
    const plotData = [{ label: "Realistic (based on current dependencies)", data: [], color: "#3131a7" },
        { label: "Ideal (if no dependencies and perfect speedup)", data: [], color: "green" },
        { label: "Gap", data: [], color: "orange" }
    ];
    let threads1;
    for (let threads = 1; threads <= 24; threads++) {
        const taken = simulateThreads(profile, threads)[0];
        if (threads === 1)
            threads1 = taken;
        plotData[0].data.push([threads, taken]);
        plotData[1].data.push([threads, threads1 / threads]);
        plotData[2].data.push([threads, Math.max(0, taken - (threads1 / threads))]);
    }
    const plot = React.createElement("div", { style: "width:100%; height:100%;" });
    bindPlot(plot, new Prop(plotData), {
        xaxis: { tickDecimals: 0 },
        yaxis: { min: 0, tickFormatter: showTime }
    });
    return React.createElement("table", { class: "fill" },
        React.createElement("tr", null,
            React.createElement("td", { style: "text-align:center;" },
                React.createElement("h2", null, "Time to build at different number of threads"))),
        React.createElement("tr", null,
            React.createElement("td", { height: "100%" }, plot)),
        React.createElement("tr", null,
            React.createElement("td", { style: "text-align:center;" }, "Number of threads available.")));
}
// Simulate running N threads over the profile, return:
// [total time take, point at which each entry kicked off]
function simulateThreads(profile, threads) {
    // How far are we through this simulation
    let timestamp = 0;
    // Who is currently running, with the highest seconds FIRST
    const running = [];
    const started = [];
    // Things that are done
    const ready = profile.filter(x => x.depends.length === 0);
    const waiting = profile.map(x => x.depends.concatLength()); // number I am waiting on before I am done
    function runningWait() {
        const [ind, time] = running.pop();
        timestamp = time;
        for (const d of profile[ind].rdepends) {
            waiting[d]--;
            if (waiting[d] === 0)
                ready.push(profile[d]);
        }
    }
    while (true) {
        // Queue up as many people as we can
        while (running.length < threads && ready.length > 0) {
            const p = ready.pop();
            started[p.index] = timestamp;
            running.insertSorted([p.index, timestamp + p.execution], compareSndRev);
        }
        if (running.length === 0) {
            if (waiting.maximum(0) > 0)
                throw new Error("Failed to run all tasks");
            return [timestamp, started];
        }
        runningWait();
    }
}
function reportRebuild(profile, search) {
    const depth = [];
    for (const p of profile) {
        depth[p.index] = p.depends.flat().map(d => depth[d] + 1).maximum(0);
    }
    const ind = search.get().mapProfile((p, _) => p.index).sortOn(i => -depth[i])[0];
    const p = profile[ind];
    function f(p) {
        const res = [];
        while (p.depends.length !== 0) {
            const ds = p.depends.flat().sortOn(i => -depth[i]);
            res.push(React.createElement("li", null,
                React.createElement("select", { style: "width:400px;" }, ds.slice(0, 1).map(x => React.createElement("option", null, profile[x].name)))));
            p = profile[ds[0]];
        }
        return res;
    }
    return React.createElement("div", null,
        React.createElement("h2", null, "Why did it rebuild?"),
        React.createElement("p", null,
            "Rule ",
            p.name + " " + (p.built === 0 ? "rebuild in the last run" : "did not rebuild")),
        React.createElement("ul", null, f(p)));
}
function reportRuleTable(profile, search) {
    const [etimes, wtimes] = calcEWTimes(profile, 24);
    const columns = [{ field: "name", label: "Name", width: 400 },
        { field: "count", label: "Count", width: 65, alignRight: true, show: showInt },
        { field: "leaf", label: "Leaf", width: 60, alignRight: true },
        { field: "run", label: "Run", width: 50, alignRight: true },
        { field: "changed", label: "Change", width: 60, alignRight: true },
        { field: "time", label: "Time", width: 75, alignRight: true, show: showTime },
        { field: "etime", label: "ETime", width: 75, alignRight: true, show: showTime },
        { field: "wtime", label: "WTime", width: 75, alignRight: true, show: showTime },
        { field: "untraced", label: "Untraced", width: 100, alignRight: true, show: showTime }
    ];
    return newTable(columns, search.map(s => ruleData(etimes, wtimes, s)), "time", true);
}
// Calculate the exclusive time of each rule at some number of threads
function calcEWTimes(profile, threads) {
    const [_, started] = simulateThreads(profile, threads);
    const starts = started.map((s, i) => pair(i, s)).sort(compareSnd);
    const costs = starts.map(([ind, start], i) => {
        // find out who else runs before I finish
        const execution = profile[ind].execution;
        const end = start + execution;
        let overlap = 0; // how much time I am overlapped for
        let exclusive = 0; // how much time I am the only runner
        let finisher = start; // the first overlapping person to finish
        for (let j = i + 1; j < starts.length; j++) {
            const [jInd, jStarts] = starts[j];
            if (jStarts > end)
                break;
            overlap += Math.min(end - jStarts, profile[jInd].execution);
            exclusive += Math.max(0, Math.min(jStarts, end) - finisher);
            finisher = Math.max(finisher, jStarts + profile[jInd].execution);
        }
        exclusive += Math.max(0, end - finisher);
        return triple(ind, execution === 0 ? 0 : execution * (execution / (execution + overlap)), exclusive);
    });
    const etimes = [];
    const wtimes = [];
    for (const [ind, etime, wtime] of costs) {
        etimes[ind] = etime;
        wtimes[ind] = wtime;
    }
    return [etimes, wtimes];
}
function ruleData(etimes, wtimes, search) {
    return search.mapProfiles((ps, name) => ({
        name,
        count: ps.length,
        leaf: ps.every(p => p.depends.length === 0),
        run: ps.map(p => p.built).minimum(),
        changed: ps.some(p => p.built === p.changed),
        time: ps.map(p => p.execution).sum(),
        etime: ps.map(p => etimes[p.index]).sum(),
        wtime: ps.map(p => wtimes[p.index]).sum(),
        untraced: ps.map(untraced).sum()
    }));
}
function reportSummary(profile) {
    let countLast = 0; // number of rules run in the last run
    let highestRun = 0; // highest run you have seen (add 1 to get the count of runs)
    let sumExecution = 0; // build time in total
    let sumExecutionLast = 0; // build time in total
    let countTrace = -1;
    let countTraceLast = -1; // traced commands run
    // start both are -1 because the end command will have run in the previous step
    let maxTraceStopLast = 0; // time the last traced command stopped
    for (const p of profile) {
        sumExecution += p.execution;
        highestRun = Math.max(highestRun, p.changed); // changed is always greater or equal to built
        countTrace += p.traces.length;
        if (p.built === 0) {
            sumExecutionLast += p.execution;
            countLast++;
            countTraceLast += p.traces.length;
            if (p.traces.length > 0)
                maxTraceStopLast = Math.max(maxTraceStopLast, p.traces.last().stop);
        }
    }
    return React.createElement("div", null,
        React.createElement("h2", null, "Totals"),
        React.createElement("ul", null,
            React.createElement("li", null,
                React.createElement("b", null, "Runs:"),
                " ",
                showInt(highestRun + 1),
                " ",
                React.createElement("span", { class: "note" }, "number of times Shake has been run.")),
            React.createElement("li", null,
                React.createElement("b", null, "Rules:"),
                " ",
                showInt(profile.length),
                " (",
                showInt(countLast),
                " in last run) ",
                React.createElement("span", { class: "note" }, "number of defined rules, e.g. individual files.")),
            React.createElement("li", null,
                React.createElement("b", null, "Traced:"),
                " ",
                showInt(countTrace),
                " (",
                showInt(countTraceLast),
                " in last run)",
                React.createElement("span", { class: "note" },
                    "number of calls to ",
                    varLink("cmd"),
                    " or ",
                    varLink("traced"),
                    "."))),
        React.createElement("h2", null, "Performance"),
        React.createElement("ul", null,
            React.createElement("li", null,
                React.createElement("b", null, "Build time:"),
                " ",
                showTime(sumExecution),
                " ",
                React.createElement("span", { class: "note" }, "how long a complete build would take single threaded.")),
            React.createElement("li", null,
                React.createElement("b", null, "Last build time:"),
                " ",
                showTime(maxTraceStopLast),
                " ",
                React.createElement("span", { class: "note" }, "how long the last build take.")),
            React.createElement("li", null,
                React.createElement("b", null, "Parallelism:"),
                " ",
                (maxTraceStopLast === 0 ? 0 : sumExecutionLast / maxTraceStopLast).toFixed(2),
                " ",
                React.createElement("span", { class: "note" }, "average number of commands executing simultaneously in the last build.")),
            React.createElement("li", null,
                React.createElement("b", null, "Speculative critical path:"),
                " ",
                showTime(speculativeCriticalPath(profile)),
                " ",
                React.createElement("span", { class: "note" }, "how long it would take on infinite CPUs.")),
            React.createElement("li", null,
                React.createElement("b", null, "Precise critical path:"),
                " ",
                showTime(preciseCriticalPath(profile)),
                " ",
                React.createElement("span", { class: "note" }, "critical path not speculatively executing."))));
}
function speculativeCriticalPath(profile) {
    const criticalPath = []; // the critical path to any element
    let maxCriticalPath = 0;
    for (const p of profile) {
        let cost = 0;
        for (const ds of p.depends)
            for (const d of ds)
                cost = Math.max(cost, criticalPath[d]);
        cost += p.execution;
        maxCriticalPath = Math.max(cost, maxCriticalPath);
        criticalPath[p.index] = cost;
    }
    return maxCriticalPath;
}
/*
Calculating a precise critical path, taking into account the deep dependeny structure, is non-obvious.
Dependencies have the type [{X}], e.g:

    X = [{a,b},{c,d}]

That is r builds a and b, then after those both complete (assuming they don't change), it builds c and d,
then it is finished. Importantly, r doesn't start building c/d until after a and b have finished. This
detail extends the critical path.

To calculate the precise critical path, we simulate with the notion of demand and waiting.
*/
function preciseCriticalPath(profile) {
    const waiting = profile.map(x => x.depends.concatLength()); // number I am waiting on before I am done
    const demanded = []; // I have been demanded by someone
    const oncomplete = []; // Completion functions
    const complete = []; // Who is complete already
    const running = [];
    let timestamp = 0;
    // demand dependency set N of a rule
    function demandN(p, round) {
        for (; round < p.depends.length; round++) {
            let todo = p.depends[round].length; // Number before we continue
            const step = () => {
                todo--;
                if (todo === 0)
                    demandN(p, round + 1);
            };
            for (const d of p.depends[round]) {
                if (complete[d])
                    todo--;
                else {
                    const old = oncomplete[d];
                    oncomplete[d] = !old ? step : () => { old(); step(); };
                    demand(profile[d]);
                }
            }
            if (todo !== 0)
                break;
            // todo === 0, so continue (equivalent to calling step but tail recursive)
        }
    }
    // demand a particular rule
    function demand(p) {
        if (demanded[p.index])
            return;
        demanded[p.index] = true;
        if (waiting[p.index] === 0)
            running.insertSorted([p.index, timestamp + p.execution], compareSndRev);
        else
            demandN(p, 0);
    }
    // We don't know the targets we ask for, so we approximate by saying the ones which nothing depends on
    for (const p of profile) {
        if (p.rdepends.length === 0)
            demand(p);
    }
    while (running.length > 0) {
        const [ind, time] = running.pop();
        timestamp = time;
        complete[ind] = true;
        if (oncomplete[ind]) {
            oncomplete[ind]();
            delete oncomplete[ind];
        }
        for (const d of profile[ind].rdepends) {
            waiting[d]--;
            if (waiting[d] === 0 && demanded[d])
                running.insertSorted([d, timestamp + profile[d].execution], compareSndRev);
        }
    }
    for (let i = 0; i < profile.length; i++)
        if (!complete[i])
            throw new Error("Failed to run all tasks");
    return timestamp;
}
