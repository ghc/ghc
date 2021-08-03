function h$ghcjs_setLineDash(arr, ctx) {
    if (typeof ctx.setLineDash !== 'undefined' ) {
        ctx.setLineDash(arr);
    } else if (typeof ctx.mozDash !== 'undefined' ) {
        ctx.mozDash = arr;
    }
};

function h$ghcjs_lineDashOffset(off, ctx) {
    if (typeof ctx.setLineDash !== 'undefined' ) {
        ctx.lineDashOffset = off;
    } else if (typeof ctx.mozDash !== 'undefined' ) {
        ctx.mozDashOffset = off;
    }
};
