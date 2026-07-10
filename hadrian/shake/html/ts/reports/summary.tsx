
function reportSummary(profile: Profile[]): HTMLElement {
    let countLast: int = 0; // number of rules run in the last run
    let highestRun: timestamp = 0; // highest run you have seen (add 1 to get the count of runs)
    let sumExecution: seconds = 0; // build time in total
    let sumExecutionLast: seconds = 0; // build time in total
    let countTrace: int = -1; let countTraceLast: int = -1; // traced commands run
        // start both are -1 because the end command will have run in the previous step
    let maxTraceStopLast: seconds = 0; // time the last traced command stopped

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

    return <div>
        <h2>Totals</h2>
        <ul>
            <li><b>Runs:</b> {showInt(highestRun + 1)} <span class="note">number of times Shake has been run.</span></li>
            <li><b>Rules:</b> {showInt(profile.length)} ({showInt(countLast)} in last run) <span class="note">number of defined rules, e.g. individual files.</span></li>
            <li><b>Traced:</b> {showInt(countTrace)} ({showInt(countTraceLast)} in last run)
                <span class="note">number of calls to {varLink("cmd")} or {varLink("traced")}.</span>
            </li>
        </ul>
        <h2>Performance</h2>
        <ul>
            <li><b>Build time:</b> {showTime(sumExecution)} <span class="note">how long a complete build would take single threaded.</span></li>
            <li><b>Last build time:</b> {showTime(maxTraceStopLast)} <span class="note">how long the last build take.</span></li>
            <li><b>Parallelism:</b> {(maxTraceStopLast === 0 ? 0 : sumExecutionLast / maxTraceStopLast).toFixed(2)} <span class="note">average number of commands executing simultaneously in the last build.</span></li>
            <li><b>Speculative critical path:</b> {showTime(speculativeCriticalPath(profile))} <span class="note">how long it would take on infinite CPUs.</span></li>
            <li><b>Precise critical path:</b> {showTime(preciseCriticalPath(profile))} <span class="note">critical path not speculatively executing.</span></li>
        </ul>
    </div>;
}

function speculativeCriticalPath(profile: Profile[]): seconds {
    const criticalPath: seconds[] = []; // the critical path to any element
    let maxCriticalPath: seconds = 0;
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
function preciseCriticalPath(profile: Profile[]): seconds {
    const waiting: int[] = profile.map(x => x.depends.concatLength()) ; // number I am waiting on before I am done
    const demanded: boolean[] = []; // I have been demanded by someone
    const oncomplete: Array<() => void> = []; // Completion functions
    const complete: boolean[] = []; // Who is complete already
    const running: Array<[pindex, seconds]> = [];
    let timestamp: seconds = 0;

    // demand dependency set N of a rule
    function demandN(p: Profile, round: int): void {
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
            if (todo !== 0) break;
            // todo === 0, so continue (equivalent to calling step but tail recursive)
        }
    }

    // demand a particular rule
    function demand(p: Profile): void {
        if (demanded[p.index]) return;
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
