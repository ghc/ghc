// Haddock JavaScript utilities
function toggle(button,id)
{
   var n = document.getElementById(id).style;
   if (n.display == "none")
   {
	button.src = "minus.gif";
	n.display = "block";
   }
   else
   {
	button.src = "plus.gif";
	n.display = "none";
   }
}
