/* eslint-disable no-undef */
$(document).ready(function() {
  document.getElementById("clear-log").addEventListener("click", () => {
    let log = document.getElementById("log");
    log.textContent = "";
  });

  document.getElementById('hide-log').addEventListener('click', () => {
    const panelCode = document.querySelector('.panel-code');
    const isConsoleMinimzed = panelCode.classList.contains('console__minimized');
    const newBtnText = isConsoleMinimzed ? 'Hide' : 'Show'
    panelCode.classList.toggle('console__minimized')
    document.getElementById('hide-log').innerHTML = newBtnText
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
