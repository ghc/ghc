// Haddock JavaScript utilities

import {getCookie, setCookie, clearCookie} from "./cookies";
import preact = require("preact");

const { h, Component } = preact;
const rspace = /\s\s+/g,
    rtrim = /^\s+|\s+$/g;

function spaced(s: string) { return (" " + s + " ").replace(rspace, " "); }
function trim(s: string)   { return s.replace(rtrim, ""); }

function hasClass(elem: Element, value: string) {
  const className = spaced(elem.className || "");
  return className.indexOf( " " + value + " " ) >= 0;
}

function addClass(elem: Element, value: string) {
  const className = spaced(elem.className || "");
  if ( className.indexOf( " " + value + " " ) < 0 ) {
    elem.className = trim(className + " " + value);
  }
}

function removeClass(elem: Element, value: string) {
  let className = spaced(elem.className || "");
  className = className.replace(" " + value + " ", " ");
  elem.className = trim(className);
}

function toggleClass(elem: Element, valueOn: string, valueOff: string, bool?: boolean): boolean {
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

function makeClassToggle(valueOn: string, valueOff: string): (elem: Element, bool?: boolean) => boolean {
  return function(elem, bool) {
    return toggleClass(elem, valueOn, valueOff, bool);
  }
}

const toggleShow = makeClassToggle("show", "hide");

function styles(): HTMLLinkElement[] {
  const es = Array.prototype.slice.call(document.getElementsByTagName("link"));
  return es.filter((a: HTMLLinkElement) => a.rel.indexOf("style") != -1 && a.title);
}

class StyleMenuButton extends Component<any, any> {

  render(props: { stys: string[] }) {
    function action() {
      styleMenu();
      return false;
    };

    return <li><div id='style-menu-holder' onClick={action}>
      <a href='#'>Style &#9662;</a>
      <ul id='style-menu' class='hide'>
        {props.stys.map((sty) => {
              function action() {
                setActiveStyleSheet(sty);
                return false;
              };

              return <li><a href='#' onClick={action}>{sty}</a></li>;
        })}
      </ul>
    </div></li>;
  }

}

function addStyleMenu() {
  const stys = styles().map((s) => s.title);
  if (stys.length > 1) {
    const pageMenu = document.querySelector('#page-menu') as HTMLUListElement;
    const dummy = document.createElement('li');
    pageMenu.appendChild(dummy);
    preact.render(<StyleMenuButton stys={stys} title="Style"/>, pageMenu, dummy);
  }
}

function setActiveStyleSheet(title: string) {
  const as = styles();
  let found: null | HTMLLinkElement = null;
  for(let i = 0; i < as.length; i++) {
    const a = as[i];
    a.disabled = true;
          // need to do this always, some browsers are edge triggered
    if(a.title == title) {
      found = a;
    }
  }
  if (found) {
    found.disabled = false;
    setCookie("haddock-style", title);
  }
  else {
    as[0].disabled = false;
    clearCookie("haddock-style");
  }
}

function resetStyle() {
  const s = getCookie("haddock-style");
  if (s) setActiveStyleSheet(s);
}

function styleMenu(show?: boolean) {
  const m = document.getElementById('style-menu');
  if (m) toggleShow(m, show);
}

export function init() {
  addStyleMenu();
  resetStyle();
}
