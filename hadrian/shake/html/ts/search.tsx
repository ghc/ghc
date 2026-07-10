
// A mapping from names (rule names or those matched from rule parts)
// to the indicies in profiles.
class Search {
    private profile: Profile[];
    private mapping: MapString<int[]>;

    constructor(profile: Profile[], mapping?: MapString<int[]>) {
        this.profile = profile;
        if (mapping !== undefined)
            this.mapping = mapping;
        else {
            this.mapping = {};
            for (const p of profile)
                this.mapping[p.name] = [p.index];
        }
    }
    public forEachProfiles(f: (ps: Profile[], group: string) => void): void {
        for (const s in this.mapping)
            f(this.mapping[s].map(i => this.profile[i]), s);
    }
    public forEachProfile(f: (p: Profile, group: string) => void): void {
        this.forEachProfiles((ps, group) => ps.forEach(p => f(p, group)));
    }
    public mapProfiles<A>(f: (ps: Profile[], group: string) => A): A[] {
        const res: A[] = [];
        this.forEachProfiles((ps, group) => res.push(f(ps, group)));
        return res;
    }
    public mapProfile<A>(f: (p: Profile, group: string) => A): A[] {
        const res: A[] = [];
        this.forEachProfile((p, group) => res.push(f(p, group)));
        return res;
    }
}


function createSearch(profile: Profile[]): [HTMLElement, Prop<Search>] {
    const caption = <div>Found {profile.length} entries, not filtered or grouped.</div>;
    const input = <input id="search" type="text" value="" placeholder="Filter and group"
                    style="width: 100%; font-size: 16px; border-radius: 8px; padding: 5px 10px; border: 2px solid #999;" />;
    const res = new Prop(new Search(profile));
    $(input).on("change keyup paste", () => {
        const s: string = $(input).val();
        if (s === "") {
            res.set(new Search(profile));
            $(caption).text("Found " + profile.length + " entries, not filtered or grouped.");
        } else if (s.indexOf("(") === -1) {
            const mapping = {};
            let found = 0 ;
            for (const p of profile) {
                if (p.name.indexOf(s) !== -1) {
                    found++;
                    mapping[p.name] = [p.index];
                }
            }
            res.set(new Search(profile, mapping));
            $(caption).text("Substring filtered to " + found + " / " + profile.length + " entries, not grouped.");
        } else {
            let f;
            try {
                f = new Function("return " + s);
            } catch (e) {
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
                let bool: boolean;
                try {
                    bool = f();
                } catch (e) {
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

    const body =
        <table width="100%" style="padding-bottom: 17px;">
            <tr>
                <td width="100%">{input}</td>
                <td style="padding-left:6px;padding-right: 6px;">{searchHelp(input)}</td>
            </tr>
            <tr>
                <td>{caption}</td>
            </tr>
        </table>;
    return [body, res];
}

function searchHelp(input: HTMLElement): HTMLElement {
    const examples: Array<[string, string]> =
        [ ["Only the last run", "run(0)"]
        , ["Named 'Main'", "named(\"Main\")"]
        , ["Group by file extension", "named(/(\\.[_0-9a-z]+)$/)"]
        , ["No dependencies (an input)", "leaf()"]
        , ["Didn't change when it last rebuilt", "unchanged()"]
        , ["Ran 'gcc'", "command(\"gcc\")"]
        ];
    const f = (code: string) => () => {
        $(input).val((i, x) => x + (x === "" ? "" : " && ") + code);
        $(input).trigger("change");
    };
    const dropdown = <div class="dropdown" style="display:none;">
        <ul style="padding-left:30px;">
            {examples.map(([desc, code]) => <li><a onclick={f(code)}><tt>{code}</tt></a> <span class="note">{desc}</span></li>)}
        </ul>
    </div>;
    const arrow_down = <span style="vertical-align:middle;font-size:80%;">&#9660;</span>;
    const arrow_up   = <span style="vertical-align:middle;font-size:80%;display:none;">&#9650;</span>;
    const show_inner = () => { $(dropdown).toggle(); $(arrow_up).toggle(); $(arrow_down).toggle(); };
    return <div>
        <button style="white-space:nowrap;padding-top:5px;padding-bottom:5px;" onclick={show_inner}>
            <b style="font-size:150%;vertical-align:middle;">+</b>&nbsp; Filter and Group &nbsp;
            {arrow_down}{arrow_up}
        </button>{dropdown}
    </div>;
}
