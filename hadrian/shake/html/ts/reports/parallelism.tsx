
function reportParallelism(profile: Profile[]): HTMLElement {
    // now simulate for -j1 .. -j24
    const plotData: jquery.flot.dataSeries[] =
        [ {label: "Realistic (based on current dependencies)", data: [], color: "#3131a7"}
        , {label: "Ideal (if no dependencies and perfect speedup)", data: [], color: "green"}
        , {label: "Gap", data: [], color: "orange"}
        ];
    let threads1: seconds;
    for (let threads = 1; threads <= 24; threads++) {
        const taken = simulateThreads(profile, threads)[0];
        if (threads === 1) threads1 = taken;
        plotData[0].data.push([threads, taken]);
        plotData[1].data.push([threads, threads1 / threads]);
        plotData[2].data.push([threads, Math.max(0, taken - (threads1 / threads))]);
    }

    const plot = <div style="width:100%; height:100%;"></div>;
    bindPlot(plot, new Prop(plotData), {
        xaxis: { tickDecimals: 0 },
        yaxis: { min: 0, tickFormatter: showTime }
    });
    return <table class="fill">
        <tr>
            <td style="text-align:center;"><h2>Time to build at different number of threads</h2></td>
        </tr>
        <tr>
            <td height="100%">{plot}</td>
        </tr>
        <tr>
            <td style="text-align:center;">Number of threads available.</td>
        </tr>
    </table>;
}

// Simulate running N threads over the profile, return:
// [total time take, point at which each entry kicked off]
function simulateThreads(profile: Profile[], threads: int): [seconds, seconds[]] {
    // How far are we through this simulation
    let timestamp: seconds = 0;

    // Who is currently running, with the highest seconds FIRST
    const running: Array<[pindex, seconds]> = [];
    const started: seconds[] = [];

    // Things that are done
    const ready: Profile[] = profile.filter(x => x.depends.length === 0);
    const waiting: int[] = profile.map(x => x.depends.concatLength()) ; // number I am waiting on before I am done

    function runningWait(): void {
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
