import Fuse = require('fuse.js');
import preact = require("preact");

const { h, Component } = preact;

type DocItem = {
  display_html: string
  name: string
  module: string
  link: string
}

function loadJSON(path: string, success: (json: DocItem[]) => void, error: (xhr: XMLHttpRequest) => void) {
  const xhr = new XMLHttpRequest();
  xhr.onreadystatechange = () => {
    if (xhr.readyState === XMLHttpRequest.DONE) {
      if (xhr.status === 200) {
        if (success) {
          try {
            success(JSON.parse(xhr.responseText));
          } catch (exc) {
            error(xhr);
          }
        }
      } else {
        if (error) { error(xhr); }
      }
    }
  };
  xhr.open("GET", path, true);
  xhr.send();
}

// -------------------------------------------------------------------------- //

class PageMenuButton extends Component<any, any> {

  render(props: { title: string, onClick: () => void }) {
    function onClick(e: Event) {
      e.preventDefault();
      props.onClick();
    }
    return <li><a href="#" onClick={onClick}>{props.title}</a></li>;
  }

}

function addSearchPageMenuButton(action: () => void) {
  const pageMenu = document.querySelector('#page-menu') as HTMLUListElement;
  const dummy = document.createElement('li');
  pageMenu.insertBefore(dummy, pageMenu.firstChild);
  preact.render(<PageMenuButton onClick={action} title="Quick Jump" />, pageMenu, dummy);
}

// -------------------------------------------------------------------------- //

function take<T>(n: number, arr: T[]) {
  if (arr.length <= n) { return arr; }
  return arr.slice(0, n);
}

type FuseResult<T> = {
  score: number
  item: T
}

type ResultsInModule = { module: string, totalScore: number, items: FuseResult<DocItem>[] }

type QuickJumpProps = {
  baseUrl: string
  showHideTrigger: (action: () => void) => void
}

type QuickJumpState = {
  searchString: string
  isVisible: boolean
  expanded: { [moduleName: string]: true }
  activeLinkIndex: number
  moduleResults: ResultsInModule[]
  failedLoading?: boolean
  fuse: Fuse<DocItem>
}

class QuickJump extends Component<QuickJumpProps, QuickJumpState> {

  private linkIndex: number = 0;
  private focusPlease: boolean = false;
  private navigatedByKeyboard: boolean = false;
  private activeLink: undefined | HTMLAnchorElement;
  private activeLinkAction: undefined | (() => void);

  private input: undefined | HTMLInputElement;
  private searchResults: undefined | Element;

  componentWillMount() {
    this.setState({
      searchString: '',
      isVisible: false,
      expanded: {},
      activeLinkIndex: -1,
      moduleResults: []
    });
    loadJSON(this.props.baseUrl + "/doc-index.json", (data) => {
      this.setState({
        fuse: new Fuse(data, {
          threshold: 0.25,
          caseSensitive: true,
          includeScore: true,
          tokenize: true,
          keys: [ {
                    name: "name",
                    weight: 0.7
                  },
                  {
                    name: "module",
                    weight: 0.3
                  }
                ]
        }),
        moduleResults: []
      });
    }, (err) => {
      if (console) {
        console.error("could not load 'doc-index.json' for searching", err);
      }
      this.setState({ failedLoading: true });
    });

    document.addEventListener('mousedown', this.hide.bind(this));

    document.addEventListener('keydown', (e) => {
      if (this.state.isVisible) {
        if (e.key === 'Escape') {
          this.hide();
        } else if (e.key === 'ArrowUp' || (e.key === 'k' && e.ctrlKey)) {
          e.preventDefault();
          this.navigateLinks(-1);
        } else if (e.key === 'ArrowDown' || (e.key === 'j' && e.ctrlKey)) {
          e.preventDefault();
          this.navigateLinks(+1);
        } else if (e.key === 'Enter' && this.state.activeLinkIndex >= 0) {
          this.followActiveLink();
        }
      }

      if (e.key === 's' && (e.target as HTMLElement).tagName.toLowerCase() !== 'input') {
        e.preventDefault();
        this.show();
      }
    })
  }

  hide() {
    this.setState({ isVisible: false, searchString: '' });
  }

  show() {
    if (!this.state.isVisible) {
      this.focusPlease = true;
      this.setState({ isVisible: true, activeLinkIndex: -1 });
    }
  }

  toggleVisibility() {
    if (this.state.isVisible) {
      this.hide();
    } else {
      this.show();
    }
  }

  navigateLinks(change: number) {
    const newActiveLinkIndex = Math.max(-1, Math.min(this.linkIndex-1, this.state.activeLinkIndex + change));
    this.navigatedByKeyboard = true;
    this.setState({ activeLinkIndex: newActiveLinkIndex });
  }

  followActiveLink() {
    if (!this.activeLinkAction) { return; }
    this.activeLinkAction();
  }

  updateResults() {
    const searchString = (this.input && this.input.value) || '';
    const results: FuseResult<DocItem>[] = this.state.fuse.search(searchString) as any as FuseResult<DocItem>[];

    const resultsByModule: { [name: string]: FuseResult<DocItem>[] } = {};

    results.forEach((result) => {
      const moduleName = result.item.module;
      const resultsInModule = resultsByModule[moduleName] || (resultsByModule[moduleName] = []);
      resultsInModule.push(result);
    });

    const moduleResults: ResultsInModule[] = [];
    for (const moduleName in resultsByModule) {
      const items = resultsByModule[moduleName];
      let sumOfInverseScores = 0;
      items.forEach((item) => { sumOfInverseScores += 1/item.score; });
      moduleResults.push({ module: moduleName, totalScore: 1/sumOfInverseScores, items: items });
    }

    moduleResults.sort((a, b) => a.totalScore - b.totalScore);

    this.setState({ searchString: searchString, isVisible: true, moduleResults: moduleResults });
  }

  componentDidUpdate() {
    if (this.searchResults && this.activeLink && this.navigatedByKeyboard) {
      const rect = this.activeLink.getClientRects()[0];
      const searchResultsTop = this.searchResults.getClientRects()[0].top;
      if (rect.bottom > window.innerHeight) {
        this.searchResults.scrollTop += rect.bottom - window.innerHeight + 80;
      } else if (rect.top < searchResultsTop) {
        this.searchResults.scrollTop -= searchResultsTop - rect.top + 80;
      }
    }
    if (this.focusPlease && this.input) {
      this.input.focus();
    }
    this.navigatedByKeyboard = false;
    this.focusPlease = false;
  }

  componentDidMount() {
    this.props.showHideTrigger(this.toggleVisibility.bind(this));
  }

  render(props: any, state: QuickJumpState) {
    if (state.failedLoading) {
      const usingFileProtocol = window.location.protocol == 'file:';
      return <div id="search" class={state.isVisible ? '' : 'hidden'}>
        <div id="search-results">
          <p class="error">Failed to load file 'doc-index.json' containing definitions in this package.</p>
          {usingFileProtocol ? <p class="error">
              To use quick jump, load this page with HTTP (from a local static file web server) instead of using the <code>file://</code> protocol.
              (For security reasons, it is not possible to fetch auxiliary files using JS in a HTML page opened with <code>file://</code>.)
            </p> : []
          }
        </div>
      </div>;
    }

    this.linkIndex = 0;

    const stopPropagation = (e: Event) => { e.stopPropagation(); };

    const onMouseOver = (e: MouseEvent) => {
      let target: null | Element = e.target as Element;
      while (target && typeof target.getAttribute === 'function') {
        const linkIndexString = target.getAttribute('data-link-index');
        if (typeof linkIndexString == 'string') {
          const linkIndex = parseInt(linkIndexString, 10);
          this.setState({ activeLinkIndex: linkIndex });
          break;
        }
        target = target.parentNode as null | Element;
      }
    };

    const items = take(10, state.moduleResults).map((r) => this.renderResultsInModule(r));

    return <div id="search" class={state.isVisible ? '' : 'hidden'}>
      <div id="search-form" onMouseDown={stopPropagation}>
        <input
          placeholder="Search in package by name"
          ref={(input) => { this.input = input as undefined | HTMLInputElement; }}
          onFocus={this.show.bind(this)}
          onClick={this.show.bind(this)}
          onInput={this.updateResults.bind(this)}
        />
      </div>
      <div id="search-results" ref={(el) => { this.searchResults = el; }}
        onMouseDown={stopPropagation} onMouseOver={onMouseOver}>
        {state.searchString === ''
          ? [<IntroMsg />, <KeyboardShortcuts />]
          : items.length == 0
            ? <NoResultsMsg searchString={state.searchString} />
            : <ul>{items}</ul>}
      </div>
    </div>;
  }

  renderResultsInModule(resultsInModule: ResultsInModule): JSX.Element {
    const items = resultsInModule.items;
    const moduleName = resultsInModule.module;
    const showAll = this.state.expanded[moduleName] || items.length <= 10;
    const visibleItems = showAll ? items : take(8, items);

    const expand = () => {
      const newExpanded = Object.assign({}, this.state.expanded);
      newExpanded[moduleName] = true;
      this.setState({ expanded: newExpanded });
    };

    const renderItem = (item: DocItem) => {
      return <li class="search-result">
        {this.navigationLink(this.props.baseUrl + "/" + item.link, {},
          <DocHtml html={item.display_html} />
        )}
      </li>;
    };

    return <li class="search-module">
      <h4>{moduleName}</h4>
      <ul>
        {visibleItems.map((item) => renderItem(item.item))}
        {showAll
          ? []
          : <li class="more-results">
              {this.actionLink(expand, {}, "show " + (items.length - visibleItems.length) + " more results from this module")}
            </li>}
      </ul>
    </li>;
  }

  navigationLink(href: string, attrs: JSX.HTMLAttributes&JSX.SVGAttributes&{[propName: string]: any}, ...children: (JSX.Element|JSX.Element[]|string)[]) {
    const fullAttrs = Object.assign({ href: href, onClick: this.hide.bind(this) }, attrs);
    const action = () => { window.location.href = href; this.hide(); };
    return this.menuLink(fullAttrs, action, ...children);
  }

  actionLink(callback: () => void, attrs: JSX.HTMLAttributes&JSX.SVGAttributes&{[propName: string]: any}, ...children: (JSX.Element|JSX.Element[]|string)[]) {
    const onClick = (e: Event) => { e.preventDefault(); callback(); };
    const fullAttrs = Object.assign({ href: '#', onClick: onClick }, attrs);
    return this.menuLink(fullAttrs, callback, ...children);
  }

  menuLink(attrs: JSX.HTMLAttributes&JSX.SVGAttributes&{[propName: string]: any}, action: () => void, ...children: (JSX.Element|JSX.Element[]|string)[]) {
    const linkIndex = this.linkIndex;
    if (linkIndex === this.state.activeLinkIndex) {
      attrs['class'] = (attrs['class'] ? attrs['class'] + ' ' : '') + 'active-link';
      attrs.ref = (link?: Element) => { if (link) this.activeLink = link as HTMLAnchorElement; };
      this.activeLinkAction = action;
    }
    const newAttrs = Object.assign({ 'data-link-index': linkIndex }, attrs);
    this.linkIndex += 1;
    return h('a', newAttrs, ...children);
  }

}

class DocHtml extends Component<{ html: string }, {}> {

  shouldComponentUpdate(newProps: { html: string }) {
    return this.props.html !== newProps.html;
  }

  render(props: { html: string }) {
    return <div dangerouslySetInnerHTML={{__html: props.html}} />;
  }

};

function KeyboardShortcuts() {
  return <table class="keyboard-shortcuts">
    <tr>
      <th>Key</th>
      <th>Shortcut</th>
    </tr>
    <tr>
      <td><span class="key">s</span></td>
      <td>Open this search box</td>
    </tr>
    <tr>
      <td><span class="key">esc</span></td>
      <td>Close this search box</td>
    </tr>
    <tr>
      <td>
        <span class="key">↓</span>,
        <span class="key">ctrl</span> + <span class="key">j</span>
      </td>
      <td>Move down in search results</td>
    </tr>
    <tr>
      <td>
        <span class="key">↑</span>,
        <span class="key">ctrl</span> + <span class="key">k</span>
      </td>
      <td>Move up in search results</td>
    </tr>
    <tr>
      <td><span class="key">↵</span></td>
      <td>Go to active search result</td>
    </tr>
  </table>;
}

function IntroMsg() {
  return <p>You can find any exported type, constructor, class, function or pattern defined in this package by (approximate) name.</p>;
}

function NoResultsMsg(props: { searchString: string }) {
  const messages = [
    <p>
      Your search for '{props.searchString}' produced the following list of results: <code>[]</code>.
    </p>,
    <p>
      <code>Nothing</code> matches your query for '{props.searchString}'.
    </p>,
    <p>
      <code>
        Left "no matches for '{props.searchString}'" :: Either String (NonEmpty SearchResult)
      </code>
    </p>
  ];

  return messages[(props.searchString || 'a').charCodeAt(0) % messages.length];
}

export function init(docBaseUrl?: string, showHide?: (action: () => void) => void) {
  preact.render(
    <QuickJump baseUrl={docBaseUrl || "."} showHideTrigger={showHide || addSearchPageMenuButton} />,
    document.body
  );
}

// export to global object
(window as any).quickNav = { init: init };
