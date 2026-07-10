
function reportRuleTable(profile: Profile[], search: Prop<Search>): HTMLElement {
    const [etimes, wtimes] = calcEWTimes(profile, 24);
    const columns: Column[] =
        [ {field: "name", label: "Name", width: 400}
        , {field: "count", label: "Count", width: 65, alignRight: true, show: showInt}
        , {field: "leaf", label: "Leaf", width: 60, alignRight: true}
        , {field: "run", label: "Run", width: 50, alignRight: true}
        , {field: "changed", label: "Change", width: 60, alignRight: true}
        , {field: "time", label: "Time", width: 75, alignRight: true, show: showTime}
        , {field: "etime", label: "ETime", width: 75, alignRight: true, show: showTime}
        , {field: "wtime", label: "WTime", width: 75, alignRight: true, show: showTime}
        , {field: "untraced", label: "Untraced", width: 100, alignRight: true, show: showTime}
        ];
    return newTable(columns, search.map(s => ruleData(etimes, wtimes, s)), "time", true);
}

// Calculate the exclusive time of each rule at some number of threads
function calcEWTimes(profile: Profile[], threads: int): [seconds[], seconds[]] {
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
            if (jStarts > end) break;
            overlap += Math.min(end - jStarts, profile[jInd].execution);
            exclusive += Math.max(0, Math.min(jStarts, end) - finisher);
            finisher = Math.max(finisher, jStarts + profile[jInd].execution);
        }
        exclusive += Math.max(0, end - finisher);
        return triple(ind, execution === 0 ? 0 : execution * (execution / (execution + overlap)), exclusive);
    });
    const etimes: seconds[] = [];
    const wtimes: seconds[] = [];
    for (const [ind, etime, wtime] of costs) {
        etimes[ind] = etime;
        wtimes[ind] = wtime;
    }
    return [etimes, wtimes];
}

function ruleData(etimes: seconds[], wtimes: seconds[], search: Search): object[] {
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
