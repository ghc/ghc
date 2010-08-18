// Haddock JavaScript utilities

var rspace = /\s\s+/g,
	  rtrim = /^\s+|\s+$/g;

function spaced(s) { return (" " + s + " ").replace(rspace, " "); }
function trim(s)   { return s.replace(rtrim, ""); }

function hasClass(elem, value) {
  var className = spaced(elem.className || "");
  return className.indexOf( " " + value + " " ) >= 0;
}

function addClass(elem, value) {
  var className = spaced(elem.className || "");
  if ( className.indexOf( " " + value + " " ) < 0 ) {
    elem.className = trim(className + " " + value);
  }
}

function removeClass(elem, value) {
  var className = spaced(elem.className || "");
  className = className.replace(" " + value + " ", " ");
  elem.className = trim(className);
}

function toggleClass(elem, valueOn, valueOff, bool) {
  if (bool == null) { bool = ! hasClass(elem, valueOn); }
  if (bool) {
    removeClass(elem, valueOff);
    addClass(elem, valueOn);
  }
  else {
    removeClass(elem, valueOn);
    addClass(elem, valueOff);
  }
  return bool;
}


function makeClassToggle(valueOn, valueOff)
{
  return function(elem, bool) {
    return toggleClass(elem, valueOn, valueOff, bool);
  }
}

toggleShow = makeClassToggle("show", "hide");
toggleCollapser = makeClassToggle("collapser", "expander");

function toggleSection(id)
{
  var b = toggleShow(document.getElementById("section." + id))
  toggleCollapser(document.getElementById("control." + id), b)
  return b;
}


function setCookie(name, value) {
  document.cookie = name + "=" + escape(value) + ";path=/;";
}

function clearCookie(name) {
  document.cookie = name + "=;path=/;expires=Thu, 01-Jan-1970 00:00:01 GMT;";
}

function getCookie(name) {
  var nameEQ = name + "=";
  var ca = document.cookie.split(';');
  for(var i=0;i < ca.length;i++) {
    var c = ca[i];
    while (c.charAt(0)==' ') c = c.substring(1,c.length);
    if (c.indexOf(nameEQ) == 0) {
      return unescape(c.substring(nameEQ.length,c.length));
    }
  }
  return null;
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

function addMenuItem(html) {
  var menu = document.getElementById("page-menu");
  if (menu) {
    var btn = menu.firstChild.cloneNode(false);
    btn.innerHTML = html;
    menu.appendChild(btn);
  }
}

function adjustForFrames() {
  if (parent.location.href == window.location.href) {
    // not in frames, so add Frames button
    addMenuItem("<a href='#' onclick='reframe();return true;'>Frames</a>");
  }
  else {
    // in frames, remove synopsis
    var syn = document.getElementById("synopsis");
    if (syn) { syn.parentNode.removeChild(syn); }
  }
}

function reframe() {
  setCookie("haddock-reframe", document.URL);
  window.location = "frames.html";
}

function postReframe() {
  var s = getCookie("haddock-reframe");
  if (s) {
    parent.window.main.location = s;
    clearCookie("haddock-reframe");
  }
}

function addStyleMenu() {
  var i, a, c = 0, btns = "";
  for(i=0; (a = document.getElementsByTagName("link")[i]); i++) {
    if(a.getAttribute("rel").indexOf("style") != -1
       && a.getAttribute("title")) {
      btns += "<li><a href='#' onclick=\"setActiveStyleSheet('"
        + a.getAttribute("href") + "'); return false;\">"
        + a.getAttribute("title") + "</a></li>"
      c += 1;
    }
  }
  if (c > 1) {
    var h = "<div id='style-menu-holder'>"
      + "<a href='#' onclick='styleMenu(); return false;'>Style &#9662;</a>"
      + "<ul id='style-menu' class='hide'>" + btns + "</ul>"
      + "</div>";
    addMenuItem(h);
  }
}

function setActiveStyleSheet(href) {
  var i, a, found = false;
  for(i=0; (a = document.getElementsByTagName("link")[i]); i++) {
    if(a.getAttribute("rel").indexOf("style") != -1
       && a.getAttribute("title")) {
      a.disabled = true;
            // need to do this always, some browsers are edge triggered
      if(a.getAttribute("href") == href) {
        a.disabled = false;
        found = true;
      }
    }
  }
  if (!found) href = "";
  setCookie("haddock-style", href);
  styleMenu(false);
}

function resetStyle() {
  var s = getCookie("haddock-style");
  if (s) setActiveStyleSheet(s);
}


function styleMenu(show) {
  var m = document.getElementById('style-menu');
  if (m) toggleShow(m, show);
}


function pageLoad() {
  addStyleMenu();
  adjustForFrames();
  resetStyle();
}

