"use strict";
/*global setImmediate: false, specify: false, window: false */

function assert(condition) {
    if (!condition) {
        throw new Error("Assertion failed");
    }
}
assert.strictEqual = function (x, y) {
    if (x !== y) {
        throw new Error(x + " !== " + y);
    }
};

specify("Modal dialogs block handlers", function (done) {
    // Try to launch the less-annoying self-closing-window modal dialog; if that's not an option, fall back to alert.
    var showTheDialog = window.showModalDialog ?
        function () {
            window.showModalDialog("selfClose.htm");
        }
        : function () {
            window.alert("Please press OK to continue the test; we needed a modal dialog.");
        };

    var dialogClosed = false;
    setImmediate(function () {
        showTheDialog();
        dialogClosed = true;
    });

    setImmediate(function () {
        assert(dialogClosed);
        done();
    });
});

if (typeof window.Worker === "function") {
    specify("When inside a web worker context, setImmediate calls the passed handler", function (done) {
        var worker = new window.Worker("worker.js");
        worker.addEventListener("message", function (event) {
            assert.strictEqual(event.data, "TEST");
            done();
        }, false);
    });
}
