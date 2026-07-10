
function profileLoaded(profileRaw: ProfileRaw[]): void {
    $(document.body).empty().append(profileRoot(unraw(profileRaw)));
}

function unraw(xs: ProfileRaw[]): Profile[] {
    const ans = xs.map((x, i) => ({
        index: i,
        name: x[0],
        execution: x[1],
        built: x[2],
        changed: x[3],
        depends: x.length > 4 ? x[4] : [],
        rdepends: [],
        traces: x.length > 5 ? x[5].map(y => ({command: y[0], start: y[1], stop: y[2]})) : []
    } as Profile));
    for (const p of ans)
        for (const ds of p.depends)
            for (const d of ds)
                ans[d].rdepends.push(p.index);
    return ans;
}

function profileRoot(profile: Profile[]): HTMLElement {
    const [s, search] = createSearch(profile);
    const t = createTabs(
        [ ["Summary", () => reportSummary(profile)]
        , ["Command plot", () => reportCmdPlot(profile)]
        , ["Commands", () => reportCmdTable(profile, search)]
        , ["Rules", () => reportRuleTable(profile, search)]
        , ["Parallelizability", () => reportParallelism(profile)]
        , ["Details", () => reportDetails(profile, search)]
        // , ["Why rebuild", () => reportRebuild(profile, search)]
        ]);
    return <table class="fill">
        <tr><td style="padding-top: 8px; padding-bottom: 8px;">
            <a href="https://shakebuild.com/" style="font-size: 20px; text-decoration: none; color: #3131a7; font-weight: bold;">
                Shake profile report
            </a>
            <span style="color:gray;white-space:pre;">   - generated at {generated} by Shake v{version}</span>
        </td></tr>
        <tr><td>{s}</td></tr>
        <tr><td height="100%">{t}</td></tr>
    </table>;
}


function createTabs(xs: Array<[string, () => HTMLElement]>): HTMLElement {
    const bodies: Array< [HTMLElement, () => void] > = xs.map(x => {
        const el = <div style="padding:5px;width:100%;height:100%;min-width:150px;min-height:150px;overflow:auto;display:none;"></div>;
        const upd = lazy(() => $(el).append(x[1]()));
        return pair(el, upd);
    });
    let lbls = [];
    const f = (i: int) => () => {
        bodies[i][1]();
        lbls.map((x, j) => $(x).toggleClass("active", i === j));
        bodies.map((x, j) => $(x[0]).toggle(i === j));
        $(window).trigger("resize");
    };
    lbls = xs.map((x, i) => <a onclick={f(i)}>{x[0]}</a>);
    f(0)();
    return <table class="fill">
        <tr><td>
            <table width="100%" style="border-spacing:0px;"><tr class="tabstrip">
                <td width="20" class="bottom">&nbsp;</td>
                <td style="padding:0px;">{lbls}</td>
                <td width="100%" class="bottom">&nbsp;</td>
            </tr></table>
        </td></tr>
        <tr height="100%">
            <td style="background-color:white;">
                {bodies.map(fst)}
            </td>
        </tr>
    </table>;
}
