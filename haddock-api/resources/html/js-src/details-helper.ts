import {getCookie} from "./cookies";

interface HTMLDetailsElement extends HTMLElement {
  open: boolean
}

interface DetailsInfo {
  element: HTMLDetailsElement
  openByDefault: boolean
  toggles: HTMLElement[]
}

// Global state
const detailsRegistry: { [id: string]: DetailsInfo } = {};
const toggled: { [id: string]: true } = {}; /* stores which <details> are not in their default state */

function lookupDetailsRegistry(id: string): DetailsInfo {
  const info = detailsRegistry[id];
  if (info == undefined) { throw new Error(`could not find <details> element with id '${id}'`); }
  return info;
}

function onDetailsToggle(ev: Event) {
  const element = ev.target as HTMLDetailsElement;
  const id = element.id;
  const info = lookupDetailsRegistry(id);
  const isOpen = info.element.open;
  for (const toggle of info.toggles) {
    if (toggle.classList.contains('details-toggle-control')) {
      toggle.classList.add(isOpen ? 'collapser' : 'expander');
      toggle.classList.remove(isOpen ? 'expander' : 'collapser');
    }
  }
  if (element.open == info.openByDefault) {
    delete toggled[id];
  } else {
    toggled[id] = true;
  }
  rememberToggled();
}

function gatherDetailsElements() {
  const els: HTMLDetailsElement[] = Array.prototype.slice.call(document.getElementsByTagName('details'));
  for (const el of els) {
    if (typeof el.id == "string" && el.id.length > 0) {
      detailsRegistry[el.id] = {
        element: el,
        openByDefault: !!el.open,
        toggles: [] // added later
      };
      el.addEventListener('toggle', onDetailsToggle);
    }
  }
}

function toggleDetails(id: string) {
  const {element} = lookupDetailsRegistry(id);
  element.open = !element.open;
}

function rememberToggled() {
  const sections: string[] = Object.keys(toggled);
  // cookie specific to this page; don't use setCookie which sets path=/
  document.cookie = "toggled=" + encodeURIComponent(sections.join('+'));
}

function restoreToggled() {
  const cookie = getCookie("toggled");
  if (!cookie) { return; }
  const ids = cookie.split('+');
  for (const id of ids) {
    const info = detailsRegistry[id];
    toggled[id] = true;
    if (info) {
      info.element.open = !info.element.open;
    }
  }
}

function onToggleClick(ev: MouseEvent) {
  ev.preventDefault();
  const toggle = ev.currentTarget as HTMLElement;
  const id = toggle.getAttribute('data-details-id');
  if (!id) { throw new Error("element with class 'details-toggle' has no 'data-details-id' attribute!"); }
  toggleDetails(id);
}

function initCollapseToggles() {
  const toggles: HTMLElement[] = Array.prototype.slice.call(document.getElementsByClassName('details-toggle'));
  toggles.forEach(toggle => {
    const id = toggle.getAttribute('data-details-id');
    if (!id) { throw new Error("element with class 'details-toggle' has no 'data-details-id' attribute!"); }
    const info = lookupDetailsRegistry(id);
    info.toggles.push(toggle);
    toggle.addEventListener('click', onToggleClick);
    if (toggle.classList.contains('details-toggle-control')) {
      toggle.classList.add(info.element.open ? 'collapser' : 'expander');
    }
  });
}

export function init() {
  gatherDetailsElements();
  restoreToggled();
  initCollapseToggles();
}