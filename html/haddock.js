// Haddock JavaScript utilities
function toggle(button,id) {
   var n = document.getElementById(id).style;
   if (n.display == "none") {
	button.childNodes[0].data = "-";
	n.display = "inline";
   } else {
	button.childNodes[0].data = "+";
	n.display = "none";
   }
}
