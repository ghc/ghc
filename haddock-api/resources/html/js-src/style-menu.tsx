// Haddock JavaScript utilities

import {getCookie, setCookie, clearCookie} from "./cookies";
import preact = require("preact");

const { h, Component } = preact;

// Get all of the styles that are available
function styles(): HTMLLinkElement[] {
  const es = Array.prototype.slice.call(document.getElementsByTagName("link"));
  return es.filter((a: HTMLLinkElement) => a.rel.indexOf("style") != -1 && a.title);
}

// Set a style (including setting the cookie)
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

// Reset the style based on the cookie
function resetStyle() {
  const s = getCookie("haddock-style");
  if (s) setActiveStyleSheet(s);
}

class StylesButton extends Component<any, any> {
  render(props: { title: string, onClick: () => void }) {
    function onClick(e: Event) {
      e.preventDefault();
      props.onClick();
    }
    return <li><a href="#" onClick={onClick}>{props.title}</a></li>;
  }
}

// Add the style menu button
function addStyleMenu(stys: string[], action: () => void) {
  if (stys.length > 1) {
    const pageMenu = document.querySelector('#page-menu') as HTMLUListElement;
    const dummy = document.createElement('li');
    pageMenu.appendChild(dummy);
    preact.render(<StylesButton onClick={action} title="Styles"/>, pageMenu, dummy);
  }
}

type StyleProps = {
  styles: string[]
  showHideTrigger: (action: () => void) => void
}

type StyleState = {
  isVisible: boolean
}

// Represents the full style dropdown
class Styles extends Component<StyleProps, StyleState> {

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

  render(props: StyleProps, state: StyleState) {
    const stopPropagation = (e: Event) => { e.stopPropagation(); };

    return <div id="style" class={state.isVisible ? '' : 'hidden'}>
        <div id="style-menu" class="dropdown-menu" onMouseDown={stopPropagation}>
          {
            props.styles.map((sty) =>
              <button type="button"
                      onClick={(e) => { this.hide(); setActiveStyleSheet(sty) }}>
                {sty}
              </button>
            )
          }
        </div>
      </div>;
  }
}


export function init(showHide?: (action: () => void) => void) {
  const stys = styles().map((s) => s.title);
  const addStylesButton = (action: () => void) => addStyleMenu(stys, action)
  resetStyle();
  preact.render(
    <Styles showHideTrigger={showHide || addStylesButton} styles={stys} />,
    document.body
  );
}
