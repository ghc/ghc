// Haddock JavaScript utilities
function toggle(button,id) {
   var n = document.getElementById(id).style;
   if (n.display == "none") {
	button.src = "minus.jpg";
	n.display = "inline";
   } else {
	button.src = "plus.jpg";
	n.display = "none";
   }
}
