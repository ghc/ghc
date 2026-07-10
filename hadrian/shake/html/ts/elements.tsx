

function bindPlot(element: HTMLElement, data: Prop<jquery.flot.dataSeries[]>, options: jquery.flot.plotOptions): void {
    const redraw = () => {
        if ($(element).is(":visible"))
            $.plot($(element), data.get(), options);
    };
    window.setTimeout(redraw, 1);
    $(window).on("resize", redraw);
    data.event(redraw);
}


function varLink(name: string): HTMLElement {
    return <a href={"https://hackage.haskell.org/package/shake/docs/Development-Shake.html#v:" + name}><tt>{name}</tt></a>;
}


interface Column {
    field: string;
    label: string;
    width: int;
    alignRight?: boolean;
    show?: (x: any) => string;
}

// A simple approximation of what DGTable provides
declare class DGTable {
    public static Width: {SCROLL: void};
    public el: HTMLElement;
    constructor(options: any);
    public setRows(rows: object[], resort: boolean): void;
    public render(): void;
    public tableHeightChanged(): void;
    public sort(x: string, descending: boolean): void;
}

function newTable(columns: Column[], data: Prop<object[]>, sortColumn?: string, sortDescend?: boolean): HTMLElement {
    const f = (x: Column) => ({name: x.field, label: x.label, width: x.width, cellClasses: x.alignRight ? "right" : ""});
    const formatters = {};
    for (const c of columns)
        formatters[c.field] = c.show || ((x: any) => x);

    const table = new DGTable({
        adjustColumnWidthForSortArrow: false,
        cellFormatter: (val: any, colname: string) => formatters[colname](val),
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
    return <div style="height:100%;width:100%;">{table.el}</div>;
}
