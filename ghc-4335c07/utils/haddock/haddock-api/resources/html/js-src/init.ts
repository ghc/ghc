import * as styleMenu from "./style-menu";
import * as detailsHelper from "./details-helper";
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
  document.body.classList.add('js-enabled');
  styleMenu.init();
  detailsHelper.init();
  quickJump.init();
});