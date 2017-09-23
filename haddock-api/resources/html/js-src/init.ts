import * as util from "./haddock-util";
import * as quickJump from "./quick-jump";

function onDomReady(callback: () => void) {
  if (document.readyState === 'interactive') {
    callback();
  } else {
    document.addEventListener('readystatechange', () => {
      if (document.readyState === 'interactive') {
        callback();
      }
    });
  }
}

onDomReady(() => {
  util.addStyleMenu();
  util.resetStyle();
  util.restoreCollapsed();
  quickJump.init();
});