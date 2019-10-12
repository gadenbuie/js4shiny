/* eslint-disable no-undef */
$(document).ready(function() {
  document.getElementById("clear-log").addEventListener("click", () => {
    let log = document.getElementById("log");
    log.textContent = "";
  });

  // $('#example').on('change', () => { $("#log").text(''); })
  document.getElementById("example").addEventListener("change", () => {
    document.getElementById("log").textContent = "";
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
