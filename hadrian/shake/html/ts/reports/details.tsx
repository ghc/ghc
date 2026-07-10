
function reportDetails(profile: Profile[], search: Prop<Search>): HTMLElement {
    const result = <div class="details"></div>;
    const self: Prop<pindex> = new Prop(0);
    search.event(xs => self.set(xs.mapProfile((p, _) => p.index).maximum()));
    const f = (i: pindex) => <a onclick={() => self.set(i)}>{profile[i].name}</a>;
    self.event(i => {
        const p = profile[i];
        const content = <ul>
            <li><b>Name:</b> {p.name}</li>
            <li><b>Built:</b> {showRun(p.built)}</li>
            <li><b>Changed:</b> {showRun(p.changed)}</li>
            <li><b>Execution time:</b>{showTime(p.execution)}</li>
            <li><b>Traced commands:</b>
                <ol>
                    {p.traces.map(t => <li>{t.command} took {showTime(t.stop - t.start)}</li>)}
                </ol>
            </li>
            <li><b>Dependencies:</b>
                <ol>
                    {p.depends.map(ds => <li><ul>{ds.map(d => <li>{f(d)}</li>)}</ul></li>)}
                </ol>
            </li>
            <li><b>Things that depend on me:</b>
                <ul>
                    {p.rdepends.map(d => <li>{f(d)}</li>)}
                </ul>
            </li>
        </ul>;
        $(result).empty().append(content);
    });
    return result;
}
