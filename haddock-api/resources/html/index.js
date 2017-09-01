quickNav = (function() {

var baseUrl;

// alias preact's hyperscript reviver since it's referenced a lot:
var h = preact.h;

function createClass(obj) {
  // sub-class Component:
  function F(){ preact.Component.call(this); }
  var p = F.prototype = new preact.Component;
  // copy our skeleton into the prototype:
  for (var i in obj) {
    if (i === 'getDefaultProps' && typeof obj.getDefaultProps === 'function') {
      F.defaultProps = obj.getDefaultProps() || {};
    } else {
      p[i] = obj[i];
    }
  }
  // restore constructor:
  return p.constructor = F;
}

function loadJSON(path, success, error) {
  var xhr = new XMLHttpRequest();
  xhr.onreadystatechange = function()
  {
    if (xhr.readyState === XMLHttpRequest.DONE) {
      if (xhr.status === 200) {
        if (success)
          success(JSON.parse(xhr.responseText));
      } else {
        if (error)
          error(xhr);
      }
    }
  };
  xhr.open("GET", path, true);
  xhr.send();
}

// -------------------------------------------------------------------------- //

var PageMenuButton = createClass({

  render: function(props) {
    function onClick(e) {
      e.preventDefault();
      props.onClick();
    }

    return h('li', null,
      h('a', { href: '#', onClick: onClick }, props.title)
    );
  }

});

function addSearchPageMenuButton(action) {
  var pageMenu = document.querySelector('#page-menu');
  var dummy = document.createElement('li');
  pageMenu.insertBefore(dummy, pageMenu.firstChild);
  preact.render(h(PageMenuButton, { onClick: action, title: "Quick Jump" }), pageMenu, dummy);
}

// -------------------------------------------------------------------------- //

function take(n, arr) {
  if (arr.length <= n) { return arr; }
  return arr.slice(0, n);
}

var App = createClass({
  componentWillMount: function() {
    var self = this;
    self.setState({
      searchString: '',
      isVisible: false,
      expanded: {},
      activeLinkIndex: -1,
      moduleResults: []
    });
    loadJSON(baseUrl + "/doc-index.json", function(data) {
      self.setState({
        fuse: new Fuse(data, {
          threshold: 0.4,
          caseSensitive: true,
          includeScore: true,
          tokenize: true,
          keys: ["name", "module"]
        }),
        moduleResults: []
      });
    }, function (err) {
      if (console) {
        console.error("could not load 'doc-index.json' for searching", err);
      }
      self.setState({ failedLoading: true });
    });

    document.addEventListener('mousedown', this.hide.bind(this));

    document.addEventListener('keydown', function(e) {
      if (self.state.isVisible) {
        if (e.key === 'Escape') {
          self.hide();
        } else if (e.key === 'ArrowUp' || (e.key === 'k' && e.ctrlKey)) {
          e.preventDefault();
          self.navigateLinks(-1);
        } else if (e.key === 'ArrowDown' || (e.key === 'j' && e.ctrlKey)) {
          e.preventDefault();
          self.navigateLinks(+1);
        } else if (e.key === 'Enter' && self.state.activeLinkIndex >= 0) {
          self.followActiveLink();
        }
      }

      if (e.key === 's' && e.target.tagName.toLowerCase() !== 'input') {
        e.preventDefault();
        self.show();
      }
    })
  },

  hide: function() {
    this.setState({ isVisible: false });
  },

  show: function() {
    if (!this.state.isVisible) {
      this.focusPlease = true;
      this.setState({ isVisible: true, activeLinkIndex: -1 });
    }
  },

  toggleVisibility: function() {
    if (this.state.isVisible) {
      this.hide();
    } else {
      this.show();
    }
  },

  navigateLinks: function(change) {
    var newActiveLinkIndex = Math.max(-1, Math.min(this.linkIndex-1, this.state.activeLinkIndex + change));
    this.navigatedByKeyboard = true;
    this.setState({ activeLinkIndex: newActiveLinkIndex });
  },

  followActiveLink: function() {
    if (!this.activeLinkAction) { return; }
    this.activeLinkAction();
  },

  updateResults: function() {
    var searchString = this.input.value;
    var results = this.state.fuse.search(searchString)

    var resultsByModule = {};

    results.forEach(function(result) {
      var moduleName = result.item.module;
      var resultsInModule = resultsByModule[moduleName] || (resultsByModule[moduleName] = []);
      resultsInModule.push(result);
    });

    var moduleResults = [];
    for (var moduleName in resultsByModule) {
      var items = resultsByModule[moduleName];
      var sumOfInverseScores = 0;
      items.forEach(function(item) { sumOfInverseScores += 1/item.score; });
      moduleResults.push({ module: moduleName, totalScore: 1/sumOfInverseScores, items: items });
    }

    moduleResults.sort(function(a, b) { return a.totalScore - b.totalScore; });

    this.setState({ searchString: searchString, isVisible: true, moduleResults: moduleResults });
  },

  componentDidUpdate: function() {
    if (this.state.isVisible && this.activeLink && this.navigatedByKeyboard) {
      var rect = this.activeLink.getClientRects()[0];
      var searchResultsTop = this.searchResults.getClientRects()[0].top;
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
  },

  componentDidMount: function() {
    addSearchPageMenuButton(this.toggleVisibility.bind(this));
  },

  render: function(props, state) {
    if (state.failedLoading) { return null; }

    var self = this;
    this.linkIndex = 0;

    var stopPropagation = function(e) { e.stopPropagation(); };

    var onMouseOver = function(e) {
      var target = e.target;
      while (target) {
        if (typeof target.hasAttribute == 'function' && target.hasAttribute('data-link-index')) {
          var linkIndex = parseInt(target.getAttribute('data-link-index'), 10);
          this.setState({ activeLinkIndex: linkIndex });
          break;
        }
        target = target.parentNode;
      }
    }.bind(this);

    var items = take(10, state.moduleResults).map(this.renderResultsInModule.bind(this));

    return (
      h('div', { id: 'search', class: state.isVisible ? '' : 'hidden' },
        h('div', { id: 'search-form', onMouseDown: stopPropagation },
          h('input', {
            placeholder: "Search in package by name",
            ref: function(input) { self.input = input; },
            onFocus: this.show.bind(this),
            onClick: this.show.bind(this),
            onInput: this.updateResults.bind(this)
          }),
        ),
        h('div', {
          id: 'search-results',
          ref: function(el) { self.searchResults = el; },
          onMouseDown: stopPropagation, onMouseOver: onMouseOver
        },
          state.searchString === ''
            ? [h(IntroMsg), h(KeyboardShortcuts)]
            :    items.length == 0
                  ? h(NoResultsMsg, { searchString: state.searchString })
                  : h('ul', null, items)
        )
      )
    );
  },

  renderResultsInModule: function(resultsInModule) {
    var items = resultsInModule.items;
    var moduleName = resultsInModule.module;
    var showAll = this.state.expanded[moduleName] || items.length <= 10;
    var visibleItems = showAll ? items : take(8, items);

    var expand = function() {
      var newExpanded = Object.assign({}, this.state.expanded);
      newExpanded[moduleName] = true;
      this.setState({ expanded: newExpanded });
    }.bind(this);

    var renderItem = function(item) {
      return h('li', { class: 'search-result' },
        this.navigationLink(baseUrl + "/" + item.link, {},
          h(DocHtml, { html: item.display_html })
        )
      );
    }.bind(this);

    return h('li', { class: 'search-module' },
      h('h4', null, moduleName),
      h('ul', null,
        visibleItems.map(function(item) { return renderItem(item.item); }),
        showAll
          ? null
          : h('li', { class: 'more-results' },
              this.actionLink(expand, {}, "show " + (items.length - visibleItems.length) + " more results from this module")
            )
      ),
    )
  },

  navigationLink: function(href, attrs) {
    var fullAttrs = Object.assign({ href: href, onClick: this.hide.bind(this) }, attrs);
    var action = function() { window.location.href = href; this.hide(); }.bind(this);
    var args = [fullAttrs, action].concat(Array.prototype.slice.call(arguments, 2));
    return this.menuLink.apply(this, args);
  },

  actionLink: function(callback, attrs) {
    var onClick = function(e) { e.preventDefault(); callback(); }
    var fullAttrs = Object.assign({ href: '#', onClick: onClick }, attrs);
    var args = [fullAttrs, callback].concat(Array.prototype.slice.call(arguments, 2));
    return this.menuLink.apply(this, args);
  },

  menuLink: function(attrs, action) {
    var children = Array.prototype.slice.call(arguments, 2);
    var linkIndex = this.linkIndex;
    if (linkIndex === this.state.activeLinkIndex) {
      attrs['class'] = (attrs['class'] ? attrs['class'] + ' ' : '') + 'active-link';
      attrs.ref = function (link) { if (link) this.activeLink = link; }.bind(this);
      this.activeLinkAction = action;
    }
    var newAttrs = Object.assign({ 'data-link-index': linkIndex }, attrs);
    var args = ['a', newAttrs].concat(children);
    this.linkIndex += 1;
    return h.apply(null, args);
  }

});

var DocHtml = createClass({

  shouldComponentUpdate: function(newProps) {
    return this.props.html !== newProps.html;
  },

  render: function(props) {
    return h('div', {dangerouslySetInnerHTML: {__html: props.html}});
  }

});

var KeyboardShortcuts = function() {
  return h('table', { class: 'keyboard-shortcuts' },
    h('tr', null,
      h('th', null, "Key"),
      h('th', null, "Shortcut")
    ),
    h('tr', null,
      h('td', null, h('span', { class: 'key' }, "s")),
      h('td', null, "Open this search box")
    ),
    h('tr', null,
      h('td', null, h('span', { class: 'key' }, "esc")),
      h('td', null, "Close this search box")
    ),
    h('tr', null,
      h('td', null,
        h('span', { class: 'key' }, "↓"), ", ",
        h('span', { class: 'key' }, "ctrl"), "+",
        h('span', { class: 'key' }, "j")
      ),
      h('td', null, "Move down in search results")
    ),
    h('tr', null,
      h('td', null,
        h('span', { class: 'key' }, "↑"), ", ",
        h('span', { class: 'key' }, "ctrl"), "+",
        h('span', { class: 'key' }, "k")
      ),
      h('td', null, "Move up in search results")
    ),
    h('tr', null,
      h('td', null, h('span', { class: 'key' }, "↵")),
      h('td', null, "Go to active search result")
    ),
  );
};

var IntroMsg = function() {
  return h('p', null,
    "You can find any exported type, constructor, class, function or pattern defined in this package by (approximate) name.",
  );
};

var NoResultsMsg = function(props) {
  var messages = [
    h('p', null,
      "Your search for '" + props.searchString + "' produced the following list of results: ",
      h('code', null, '[]'),
      "."
    ),
    h('p', null,
      h('code', null, 'Nothing'),
      " matches your query for '" + props.searchString + "'.",
    ),
    h('p', null,
      h('code', null, 'Left "no matches for \'' + props.searchString + '\'" :: Either String (NonEmpty SearchResult)'),
    )
  ];

  return messages[(props.searchString || 'a').charCodeAt(0) % messages.length];
};

return {
  init: function(docBaseUrl) {
    baseUrl = docBaseUrl || ".";
    preact.render(h(App), document.body);
  }
}
})();
