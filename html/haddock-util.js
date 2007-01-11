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


function quick_search()
{
    perform_search(false);
}

function full_search()
{
    perform_search(true);
}


function perform_search(full)
{
    var text = document.getElementById("searchbox").value.toLowerCase();
    var table = document.getElementById("indexlist");
    var children = table.firstChild.childNodes;
    for (var i = 0; i < children.length; i++)
    {
    	var c = children[i];
    	var s = c.firstChild.firstChild.data;
    	var show = s.toLowerCase().indexOf(text) != -1;
    
    	c.style.display = (show ? "" : "none");
    }
}
