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


var max_results = 75; // 50 is not enough to search for map in the base libraries
var shown_range = null;
var last_search = null;

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
    if (text == last_search && !full) return;
    last_search = text;
    
    var table = document.getElementById("indexlist");
    var status = document.getElementById("searchmsg");
    var children = table.firstChild.childNodes;
    
    // first figure out the first node with the prefix
    var first = bisect(-1);
    var last = (first == -1 ? -1 : bisect(1));

    if (first == -1)
    {
        table.className = "";
        status.innerHTML = "No results found, displaying all";
    }
    else if (first == 0 && last == children.length - 1)
    {
        table.className = "";
        status.innerHTML = "";
    }
    else if (last - first >= max_results && !full)
    {
        table.className = "";
        status.innerHTML = "More than " + max_results + ", press Search to display";
    }
    else
    {
        // decide what you need to clear/show
        if (shown_range)
            setclass(shown_range[0], shown_range[1], "indexrow");
        setclass(first, last, "indexshow");
        shown_range = [first, last];
        table.className = "indexsearch";
        status.innerHTML = "";
    }

    
    function setclass(first, last, status)
    {
        for (var i = first; i <= last; i++)
        {
            children[i].className = status;
        }
    }
    
    
    // do a binary search, treating 0 as ...
    // return either -1 (no 0's found) or location of most far match
    function bisect(dir)
    {
        var first = 0, finish = children.length - 1;
        var mid, success = false;

        while (finish - first > 3)
        {
            mid = Math.floor((finish + first) / 2);

            var i = checkitem(mid);
            if (i == 0) i = dir;
            if (i == -1)
                finish = mid;
            else
                first = mid;
        }
        var a = (dir == 1 ? first : finish);
        var b = (dir == 1 ? finish : first);
        for (var i = b; i != a - dir; i -= dir)
        {
            if (checkitem(i) == 0) return i;
        }
        return -1;
    }    
    
    
    // from an index, decide what the result is
    // 0 = match, -1 is lower, 1 is higher
    function checkitem(i)
    {
        var s = getitem(i).toLowerCase().substr(0, text.length);
        if (s == text) return 0;
        else return (s > text ? -1 : 1);
    }
    
    
    // from an index, get its string
    // this abstracts over alternates
    function getitem(i)
    {
        for ( ; i >= 0; i--)
        {
            var s = children[i].firstChild.firstChild.data;
            if (s.indexOf(' ') == -1)
                return s;
        }
        return ""; // should never be reached
    }
}

function setSynopsis(filename) {
    if (parent.window.synopsis) {
      parent.window.synopsis.location = filename;
    }
}
