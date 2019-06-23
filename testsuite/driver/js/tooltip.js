/*
 * This is mostly copied from the example in https://www.chartjs.org/docs/latest/configuration/tooltip.html#external-custom-tooltips.
 */

setCustomTooltip = function(chartInput, extraTextByLabel) {
    chartInput.options = chartInput.options || {};
    chartInput.options.tooltips = chartInput.options.tooltips || {};
    chartInput.options.tooltips.enabled = false;
    chartInput.options.tooltips.custom = function (tooltipModel) {
        // `this` will be the overall tooltip
        var canvas = this._chart.canvas;
        return customTooltip(canvas, tooltipModel, extraTextByLabel);
    }

    return chartInput;
}
customTooltip = function(canvas, tooltipModel, extraTextByLabel) {
    // Tooltip Element
    var tooltipEl = document.getElementById('chartjs-tooltip');

    // Create element on first render
    if (!tooltipEl) {
        tooltipEl = document.createElement('div');
        tooltipEl.id = 'chartjs-tooltip';
        tooltipEl.innerHTML = '<table style="background: #cccd"></table>';
        document.body.appendChild(tooltipEl);
    }

    // Hide if no tooltip
    if (tooltipModel.opacity === 0) {
        tooltipEl.style.opacity = 0;
        return;
    }

    // Set caret Position
    tooltipEl.classList.remove('above', 'below', 'no-transform');
    if (tooltipModel.yAlign) {
        tooltipEl.classList.add(tooltipModel.yAlign);
    } else {
        tooltipEl.classList.add('no-transform');
    }

    function getBody(bodyItem) {
        return bodyItem.lines;
    }

    // Set Text
    if (tooltipModel.body) {
        var titleLines = tooltipModel.title || [];
        var bodyLines = tooltipModel.body.map(getBody);

        var innerHtml = '<thead>';

        titleLines.forEach(function(title) {
            innerHtml += '<tr><th>' + title + '</th></tr>';
        });
        innerHtml += '</thead><tbody>';

        bodyLines.forEach(function(body, i) {
            var colors = tooltipModel.labelColors[i];
            var style = 'background:' + colors.backgroundColor;
            style += '; border-color:' + colors.borderColor;
            style += '; border-width: 2px';
            var span = '<span style="' + style + '"></span>';
            innerHtml += '<tr><td>' + span + body + '</td></tr>';
        });

        // Set extra text.
        if (tooltipModel.dataPoints[0])
        {
            var tooltipItem = tooltipModel.dataPoints[0];
            var extra = extraTextByLabel[tooltipItem.label];
            innerHtml += '<tr><td><hr />' + escapeHtml(extra) + '</td></tr>';
        }

        innerHtml += '</tbody>';

        var tableRoot = tooltipEl.querySelector('table');
        tableRoot.innerHTML = innerHtml;
    }

    var position = canvas.getBoundingClientRect();

    // Display, position, and set styles for font
    tooltipEl.style.opacity = 1;
    tooltipEl.style.position = 'absolute';
    tooltipEl.style.left = '10px'
    tooltipEl.style.top = '10px'
    tooltipEl.style.fontFamily = tooltipModel._bodyFontFamily;
    tooltipEl.style.fontSize = tooltipModel.bodyFontSize + 'px';
    tooltipEl.style.fontStyle = tooltipModel._bodyFontStyle;
    tooltipEl.style.padding = tooltipModel.yPadding + 'px ' + tooltipModel.xPadding + 'px';
    tooltipEl.style.pointerEvents = 'none';
}

function escapeHtml(unsafe) {
    if(unsafe) {
        return unsafe
            .replace(/&/g, "&amp;")
            .replace(/</g, "&lt;")
            .replace(/>/g, "&gt;")
            .replace(/"/g, "&quot;")
            .replace(/'/g, "&#039;")
            .replace(/\n/g, "</br>");
    } else {
        return '';
    }
 }
