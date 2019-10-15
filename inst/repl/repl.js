/* eslint-disable no-undef */
$(document).ready(function() {
  document.getElementById("clear-log").addEventListener("click", () => {
    let log = document.getElementById("log");
    log.textContent = "";
  });

  document.getElementById('hide-log').addEventListener('click', () => {
    const panelCode = document.querySelector('.panel-code');
    const isConsoleMinimized = panelCode.classList.contains('console__minimized');
    const newBtnText = isConsoleMinimized ? 'Hide' : 'Show';
    panelCode.classList.toggle('console__minimized');
    setTimeout(() => {
      // Resize editors to avoid clipping issues in Chrome
      const editors = ['code_js', 'code_css', 'code_md'];
      editors.forEach(id => ace.edit(id).resize())
    }, 0);
    const hideBtn = document.getElementById('hide-log');
    hideBtn.innerHTML = newBtnText;
    hideBtn.title = `${isConsoleMinimized ? 'Hide' : 'Show'} Console Log`;
  });

  // $('#example').on('change', () => { $("#log").text(''); })
  document.getElementById("example").addEventListener("change", () => {
    document.getElementById("log").textContent = "";
  });

  // scroll console log to bottom on update
  document.getElementById('log').addEventListener('consoleLog', e => {
    e.target.scrollTop = e.target.scrollHeight
  });

  function showSolutionButton(state) {
    if (state) {
      $("#show_solution").show();
    } else {
      $("#show_solution").hide();
    }
  }

  Shiny.addCustomMessageHandler("showSolutionButton", showSolutionButton);
});
