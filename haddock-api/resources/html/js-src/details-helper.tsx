// This file implements the UI and logic for collapsing and expanding
// instance lists ("details").
//
// A configuration ('GlobalConfig') controlled by the UI is persisted
// in local storage in the user's browser. The configuration includes:
//
// * a global default state ('defaultInstanceState') for all instance
//   lists. The possible values for the global default are "collapsed"
//   and "expanded".
//
// * a global boolean option ('rememberToggles') to remember which
//   specific instance lists are not in the default state (e.g. which
//   instance lists are expanded when the default is "collapsed").
//
// * a local / per-page record of which specific instance lists are
//   not in the default state, when the global option
//   ('rememberToggles') to remember this info is enabled.
//
// The UI consists of an Instances menu with buttons for expanding and
// collapsing all instance lists in the current module, a checkbox for
// setting the global default state, and a checkbox to enable
// remembering which instance lists are not in the global default
// state. Also, each instance list on each module page has buttons for
// collapsing and expanding.
//
// The logic of the UI is as follows:
//
// * setting the global default state erases any record of which
//   specific instances are in the non-default state, and collapses or
//   expands all instance lists on the current page to be in the
//   global default state.
//
// * changing boolean option for remembering which specific instance
//   lists are not in the default state erases any existing record of
//   which instances are not in the default state across all pages,
//   and updates the record for the current page when the option is
//   set to true. No collapsing or expanding is done.
//
// * toggling the collapse/expand state of a specific instance list
//   causes the state of that specific instance list to be recorded in
//   the persisted configuration iff the new state of that specific
//   instance list is different from the global default state, and the
//   option to remember instance list states is enabled. There are two
//   ways to toggle the collapse/expand state of a specific instance,
//   by clicking its collapse/expand button, and by clicking the
//   "collapse all" or "expand all" button in the Instances menu.
//
// This file also implements an association between elements (with
// class "details-toggle" and "details-toggle-control") that can be
// clicked to expand/collapse <details> elements, and the details
// elements themselves. Note that this covers both <details> elements
// that list instances -- what the above explained UI and logic is
// concerned with -- and details about individual instances themselves
// -- which the above is not concerend with. The association includes
// adding event listeners that change CSS classes back and forth
// between "expander" and "collapser"; these classes determine whether
// an element is adorned with a right arrow ("expander") or a down
// arrow ("collapser"). I don't understand why we don't directly use
// the the HTML <summary> element type to allow the <details> elements
// to be directly clickable.
import preact = require("preact");

const { h, Component } = preact;

enum DefaultState { Closed, Open }

interface GlobalConfig {
  defaultInstanceState: DefaultState
  rememberToggles: boolean
}

// Hackage domain-wide config
const globalConfig: GlobalConfig = {
  defaultInstanceState: DefaultState.Open,
  rememberToggles: true,
};

class PreferencesButton extends Component<any, any> {
  render(props: { title: string, onClick: () => void }) {
    function onClick(e: Event) {
      e.preventDefault();
      props.onClick();
    }
    return <li><a href="#" onClick={onClick}>{props.title}</a></li>;
  }
}

function addPreferencesButton(action: () => void) {
  const pageMenu = document.querySelector('#page-menu') as HTMLUListElement;
  const dummy = document.createElement('li');
  pageMenu.insertBefore(dummy, pageMenu.firstChild);
  preact.render(<PreferencesButton onClick={action} title="Instances" />, pageMenu, dummy);
}

type PreferencesProps = {
  showHideTrigger: (action: () => void) => void
}

type PreferencesState = {
  isVisible: boolean
}

class Preferences extends Component<PreferencesProps, PreferencesState> {
  componentWillMount() {
    document.addEventListener('mousedown', this.hide.bind(this));

    document.addEventListener('keydown', (e) => {
      if (this.state.isVisible) {
        if (e.key === 'Escape') {
          this.hide();
        }
      }
    })
  }

  hide() {
    this.setState({ isVisible: false });
  }

  show() {
    if (!this.state.isVisible) {
      this.setState({ isVisible: true });
    }
  }

  toggleVisibility() {
    if (this.state.isVisible) {
      this.hide();
    } else {
      this.show();
    }
  }

  componentDidMount() {
    this.props.showHideTrigger(this.toggleVisibility.bind(this));
  }

  render(props: PreferencesProps, state: PreferencesState) {
    const stopPropagation = (e: Event) => { e.stopPropagation(); };

    return <div id="preferences" class={state.isVisible ? '' : 'hidden'}>
        <div id="preferences-menu" class="dropdown-menu" onMouseDown={stopPropagation}>
          <PreferencesMenu />
        </div>
      </div>;
  }
}

function storeGlobalConfig() {
  const json = JSON.stringify(globalConfig);
  try {
    // https://developer.mozilla.org/en-US/docs/Web/API/Storage/setItem#Exceptions.
    localStorage.setItem('global', json);
  } catch (e) {}
}

var globalConfigLoaded: boolean = false;

function loadGlobalConfig() {
  if (globalConfigLoaded) { return; }
  globalConfigLoaded = true;
  const global = localStorage.getItem('global');
  if (!global) { return; }
  try {
    const globalConfig_ = JSON.parse(global);
    globalConfig.defaultInstanceState = globalConfig_.defaultInstanceState;
    globalConfig.rememberToggles = globalConfig_.rememberToggles;
  } catch(e) {
    // Gracefully handle errors related to changed config format.
    if (e instanceof SyntaxError || e instanceof TypeError) {
      localStorage.removeItem('global');
    } else {
      throw e;
    }
  }
}

function setDefaultInstanceState(s: DefaultState) {
  return (e: Event) => {
    globalConfig.defaultInstanceState = s;
    putInstanceListsInDefaultState();
    storeGlobalConfig();
    clearLocalStorage();
    storeLocalConfig();
  }
}

function setRememberToggles(e: Event) {
  const checked: boolean = (e as any).target.checked;
  globalConfig.rememberToggles = checked;
  storeGlobalConfig();
  clearLocalStorage();
  storeLocalConfig();
}

// Click event consumer for "default collapse" instance menu check box.
function defaultCollapseOnClick(e: Event) {
  const us = document.getElementById('default-collapse-instances') as HTMLInputElement;
  if (us !== null) {
    if (us.checked) {
      setDefaultInstanceState(DefaultState.Closed)(e);
    } else {
      setDefaultInstanceState(DefaultState.Open)(e);
    }
  }
}

// Instances menu.
function PreferencesMenu() {
  loadGlobalConfig();
  return <div>
      <div>
        <button type="button"
                onClick={expandAllInstances}>
        Expand All Instances
        </button>
        <button type="button"
                onClick={collapseAllInstances}>
        Collapse All Instances
        </button>
      </div>
      <div>
        <input type="checkbox"
               id="default-collapse-instances"
               name="default-instance-state"
               checked={globalConfig.defaultInstanceState===DefaultState.Closed}
               onClick={defaultCollapseOnClick}></input>

        <span>Collapse All Instances By Default</span>
      </div>
      <div>
        <input type="checkbox"
               id="remember-toggles"
               name="remember-toggles"
               checked={globalConfig.rememberToggles}
               onClick={setRememberToggles}></input>
        <label for="remember-toggles">Remember Manually Collapsed/Expanded Instances</label>
      </div>
    </div>;
}

interface HTMLDetailsElement extends HTMLElement {
  open: boolean
}

interface DetailsInfo {
  element: HTMLDetailsElement
  // Here 'toggles' is the list of all elements of class
  // 'details-toggle-control' that control toggling 'element'. I
  // believe this list is always length zero or one.
  toggles: HTMLElement[]
}

// Mapping from <details> elements to their info.
const detailsRegistry: { [id: string]: DetailsInfo } = {};

function lookupDetailsRegistry(id: string): DetailsInfo {
  const info = detailsRegistry[id];
  if (info == undefined) { throw new Error(`could not find <details> element with id '${id}'`); }
  return info;
}

// Return true iff instance lists are open by default.
function getDefaultOpenSetting(): boolean {
  return globalConfig.defaultInstanceState == DefaultState.Open;
}

// Event handler for "toggle" events, which are triggered when a
// <details> element's "open" property changes.  We don't deal with
// any config stuff here, because we only change configs in response
// to mouse clicks. In contrast, for example, this event is triggred
// automatically once for every <details> element when the user clicks
// the "collapse all elements" button.
function onToggleEvent(ev: Event) {
  const element = ev.target as HTMLDetailsElement;
  const id = element.id;
  const info = lookupDetailsRegistry(id);
  const isOpen = info.element.open;
  // Update the CSS of the toggle element users can click on to toggle
  // 'element'. The "collapser" and "expander" classes control what
  // kind of arrow appears next to the 'toggle' element.
  for (const toggle of info.toggles) {
    if (toggle.classList.contains('details-toggle-control')) {
      toggle.classList.add(isOpen ? 'collapser' : 'expander');
      toggle.classList.remove(isOpen ? 'expander' : 'collapser');
    }
  }
}

function gatherDetailsElements() {
  const els: HTMLDetailsElement[] = Array.prototype.slice.call(document.getElementsByTagName('details'));
  for (const el of els) {
    if (typeof el.id == "string" && el.id.length > 0) {
      detailsRegistry[el.id] = {
        element: el,
        toggles: [] // Populated later by 'initCollapseToggles'.
      };
      el.addEventListener('toggle', onToggleEvent);
    }
  }
}

// Return the id of the <details> element that the given 'toggle'
// element toggles.
function getDataDetailsId(toggle: Element): string {
  const id = toggle.getAttribute('data-details-id');
  if (!id) { throw new Error("element with class " + toggle + " has no 'data-details-id' attribute!"); }
  return id;
}

// Toggle the "open" state of a <details> element when that element's
// toggle element is clicked.
function toggleDetails(toggle: Element) {
  const id = getDataDetailsId(toggle);
  const {element} = lookupDetailsRegistry(id);
  element.open = !element.open;
}

// Prefix for local keys used with local storage. Idea is that other
// modules could also use local storage with a different prefix and we
// wouldn't step on each other's toes.
//
// NOTE: we're using the browser's "local storage" API via the
// 'localStorage' object to store both "local" (to the current Haddock
// page) and "global" (across all Haddock pages) configuration. Be
// aware of these two different uses of the term "local".
const localStoragePrefix: string = "local-details-config:";

// Local storage key for the current page.
function localStorageKey(): string {
  return localStoragePrefix + document.location.pathname;
}

// Clear all local storage related to instance list configs.
function clearLocalStorage() {
  const keysToDelete: string[] = [];
  for (var i = 0; i < localStorage.length; ++i) {
    const key = localStorage.key(i);
    if (key !== null && key.startsWith(localStoragePrefix)) {
      keysToDelete.push(key);
    }
  }
  keysToDelete.forEach(key => {
    localStorage.removeItem(key);
  });
}

// Compute and save the set of instance list ids that aren't in the
// default state.
function storeLocalConfig() {
  if (!globalConfig.rememberToggles) return;
  const instanceListToggles: HTMLElement[] =
    // Restrict to 'details-toggle' elements for "instances"
    // *plural*. These are the toggles that control instance lists and
    // not the list of methods for individual instances.
    Array.prototype.slice.call(document.getElementsByClassName(
      'instances details-toggle details-toggle-control'));
  const nonDefaultInstanceListIds: string[] = [];
  instanceListToggles.forEach(toggle => {
    const id = getDataDetailsId(toggle);
    const details = document.getElementById(id) as HTMLDetailsElement;
    if (details.open != getDefaultOpenSetting()) {
      nonDefaultInstanceListIds.push(id);
    }
  });

  const json = JSON.stringify(nonDefaultInstanceListIds);
  try {
    // https://developer.mozilla.org/en-US/docs/Web/API/Storage/setItem#Exceptions.
    localStorage.setItem(localStorageKey(), json);
  } catch (e) {}
}

function putInstanceListsInDefaultState() {
  switch (globalConfig.defaultInstanceState) {
    case DefaultState.Closed: _collapseAllInstances(true); break;
    case DefaultState.Open: _collapseAllInstances(false); break;
    default: break;
  }
}

// Expand and collapse instance lists according to global and local
// config.
function restoreToggled() {
  loadGlobalConfig();
  putInstanceListsInDefaultState();
  if (!globalConfig.rememberToggles) { return; }
  const local = localStorage.getItem(localStorageKey());
  if (!local) { return; }
  try {
    const nonDefaultInstanceListIds: string[] = JSON.parse(local);
    nonDefaultInstanceListIds.forEach(id => {
      const info = lookupDetailsRegistry(id);
      info.element.open = ! getDefaultOpenSetting();
    });
  } catch(e) {
    // Gracefully handle errors related to changed config format.
    if (e instanceof SyntaxError || e instanceof TypeError) {
      localStorage.removeItem(localStorageKey());
    } else {
      throw e;
    }
  }
}

// Handler for clicking on the "toggle" element that toggles the
// <details> element with id given by the 'data-details-id' property
// of the "toggle" element.
function onToggleClick(ev: MouseEvent) {
  const toggle = ev.currentTarget as HTMLElement;
  toggleDetails(toggle);
  storeLocalConfig();
}

// Set event handlers on elements responsible for expanding and
// collapsing <details> elements.
//
// This applies to all 'details-toggle's, not just to to top-level
// 'details-toggle's that control instance lists.
function initCollapseToggles() {
  const toggles: HTMLElement[] = Array.prototype.slice.call(document.getElementsByClassName('details-toggle'));
  toggles.forEach(toggle => {
    const id = getDataDetailsId(toggle);
    const info = lookupDetailsRegistry(id);
    info.toggles.push(toggle);
    toggle.addEventListener('click', onToggleClick);
    if (toggle.classList.contains('details-toggle-control')) {
      toggle.classList.add(info.element.open ? 'collapser' : 'expander');
    }
  });
}

// Collapse or expand all instances.
function _collapseAllInstances(collapse: boolean) {
  const ilists = document.getElementsByClassName('subs instances');
  [].forEach.call(ilists, function (ilist : Element) {
    const toggleType = collapse ? 'collapser' : 'expander';
    const toggle = ilist.getElementsByClassName('instances ' + toggleType)[0];
    if (toggle) {
      toggleDetails(toggle);
    }
  });
}

function collapseAllInstances() {
  _collapseAllInstances(true);
  storeLocalConfig();
}

function expandAllInstances() {
  _collapseAllInstances(false);
  storeLocalConfig();
}

export function init(showHide?: (action: () => void) => void) {
  gatherDetailsElements();
  initCollapseToggles();
  restoreToggled();
  preact.render(
    <Preferences showHideTrigger={showHide || addPreferencesButton} />,
    document.body
  );
}
