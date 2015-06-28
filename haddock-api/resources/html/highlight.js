
var styleForRule = function (rule) {
	var sheets = document.styleSheets;
	for (var s = 0; s < sheets.length; s++) {
		var rules = sheets[s].cssRules;
		if (rules === null) {
			return null;
		}

		for (var r = 0; r < rules.length; r++) {
			if (rules[r].selectorText == rule) {
				return rules[r].style;
			}
		}
	}
};

var highlight = function () {
	/*
	 * Chrome for security reasons disallows to read .cssRules property.
	 * So, we are forced to pick some color and set it as a highlight.
	 */
	var style = styleForRule("a:hover");
	var color = style !== null ? style["background-color"] : "#808080";

	var links = document.getElementsByTagName('a');
	for (var i = 0; i < links.length; i++) {
		var that = links[i];
		if (this.href == that.href) {
			that.style["background-color"] = color;
		}
	}
};

/*
 * I have no idea what is the proper antonym for "highlight" in this
 * context. "Diminish"? "Unhighlight"? "Lowlight" sounds ridiculously
 * so I like it.
 */
var lowlight = function () {
	var links = document.getElementsByTagName('a');
	for (var i = 0; i < links.length; i++) {
		var that = links[i];
		if (this.href == that.href) {
			that.style["background-color"] = "";
		}
	}
};

window.onload = function () {
	var links = document.getElementsByTagName('a');
	for (var i = 0; i < links.length; i++) {
		links[i].onmouseover = highlight;
		links[i].onmouseout = lowlight;
	}
};
