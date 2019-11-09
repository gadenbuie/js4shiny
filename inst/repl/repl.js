/* eslint-disable no-undef */
$(document).ready(function() {
  const clearElementById = (id) => {
    document.getElementById(id).textContent = '';
  };

  document.getElementById('clear-log').addEventListener('click', () => {
    clearElementById('log');
  });

  const resizeAceEditors = () => {
    // Resize editors to avoid clipping issues in Chrome
    const editors = ['code_js', 'code_css', 'code_md'];
    editors
      .filter((id) => {
        return !!document.getElementById(id);
      })
      .forEach((id) => {
        ace.edit(id).resize();
      });
  };

  const elHideLog = document.getElementById('hide-log');
  if (elHideLog) {
    elHideLog.addEventListener('click', () => {
      const panelCode = document.querySelector('.panel-code');
      const isConsoleMinimized = panelCode.classList.contains(
        'console--minimized'
      );
      // isConsoleMinimized is the current state of the console, everything
      // below moves from the current state to the opposite state
      panelCode.classList.toggle('console--minimized', !isConsoleMinimized);
      panelCode.classList.toggle('resize--disabled', !isConsoleMinimized);
      setTimeout(resizeAceEditors, 0);
      const hideBtn = document.getElementById('hide-log');
      const btnIcon = hideBtn.querySelector('i.fa');
      btnIcon.classList.toggle('fa-window-minimize', isConsoleMinimized);
      btnIcon.classList.toggle('fa-window-restore', !isConsoleMinimized);
      hideBtn.title = `${isConsoleMinimized ? 'Hide' : 'Show'} Console Log`;
    });
  }

  // $('#example').on('change', () => { $("#log").text(''); })
  document.getElementById('example').addEventListener('change', () => {
    clearElementById('log');
  });

  // scroll console log to bottom on update
  document.getElementById('log').addEventListener('consoleLog', (e) => {
    e.target.scrollTop = e.target.scrollHeight;
  });

  function showSolutionButton(state) {
    if (state) {
      $('#show_solution').show();
    } else {
      $('#show_solution').hide();
    }
  }

  Shiny.addCustomMessageHandler('clearElementById', clearElementById);
  Shiny.addCustomMessageHandler('showSolutionButton', showSolutionButton);

  // ---- Update Tab Name ---- //
  const updateTabName = ({id, value, replacement}) => {
    const tabLink = document.querySelector(`#${id} a[data-value="${value}"]`);
    tabLink.innerHTML = replacement;
  }

  Shiny.addCustomMessageHandler('updateTabName', updateTabName)

  // ---- Resize Panels ---- //
  const siblings = (el) => {
    return [...el.parentElement.children];
  };

  const prevAdjEl = (el) => {
    const sibs = siblings(el);
    const elIdx = sibs.findIndex((e) => {
      return e === el;
    });
    return elIdx === 0 ? null : sibs[elIdx - 1];
  };

  const otherSpace = (el, dim = 'minWidth') => {
    return siblings(el)
      .map((e) => {
        return window.getComputedStyle(e).getPropertyValue(dim);
      })
      .filter((s) => {
        return s.includes('px');
      })
      .map((s) => {
        return s.replace(/[^\d]/g, '');
      })
      .map((s) => {
        return parseInt(s) || 0;
      })
      .reduce((acc, item) => {
        return acc + item;
      }, 0);
  };

  const ownMinSpace = (el, dim = 'minWidth') => {
    const size = window
      .getComputedStyle(el)
      .getPropertyValue(dim)
      .replace(/[^\d]/g, '');

    return parseInt(size) || 0;
  };

  const initializeResize = (ev) => {
    ev.preventDefault();

    let { target } = ev;
    if (
      ev.target.nodeName !== 'div' ||
      !ev.target.className.includes('resize-handle')
    ) {
      target = ev.target.closest('.resize-handle');
    }

    if (target.parentElement.className.includes('resize--disabled')) {
      // do nothing if parent has class .resize--disabled
      return null;
    }

    // find previous sibling of event target
    const elResize = prevAdjEl(target);

    // check if resizing is horizontal (otherwise vertical)
    const isResizeHorizontal = target.className.includes('horizontal');

    const elParent = elResize.parentElement;
    const minWidth = ownMinSpace(elResize, 'min-width');
    const maxWidth =
      elParent.offsetWidth - otherSpace(elResize, 'min-width') + minWidth;
    const minHeight = ownMinSpace(elResize, 'min-height');
    const maxHeight =
      elParent.offsetHeight - otherSpace(elResize, 'min-height') + minHeight;

    const startResizing = (event) => {
      if (isResizeHorizontal) {
        let newWidth = Math.min(
          event.pageX - elResize.getBoundingClientRect().left,
          maxWidth
        );
        newWidth = Math.max(newWidth, minWidth);
        // console.log({maxWidth, newWidth});
        elResize.style.width = `${newWidth}px`;
      } else {
        let newHeight = Math.min(
          event.pageY - elResize.getBoundingClientRect().top,
          maxHeight
        );
        newHeight = Math.max(newHeight, minHeight);
        // console.log({maxHeight, newHeight, pageY: ev.pageY, top: elResize.getBoundingClientRect().top});
        elResize.style.height = `${newHeight}px`;
      }
    };
    const stopResizing = () => {
      window.removeEventListener('mousemove', startResizing);
      window.removeEventListener('mouseup', stopResizing);
      setTimeout(resizeAceEditors, 0);
    };

    window.addEventListener('mousemove', startResizing);
    window.addEventListener('mouseup', stopResizing);
  };

  const paneResizeHandle = [...document.querySelectorAll('.resize-handle')];
  paneResizeHandle.forEach((el) => {
    return el.addEventListener('mousedown', initializeResize);
  });
});
