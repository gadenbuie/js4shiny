// focus input element right away
document.getElementById('term').focus()

// send escape and enter key events back to Shiny
document.addEventListener('keyup', function(e) {
  if (e.keyCode === 27) {
    Shiny.onInputChange('keyEvent', 'cancel')
  }
  else if (e.keyCode === 13) {
    Shiny.onInputChange('keyEvent', 'search')
  }
})
