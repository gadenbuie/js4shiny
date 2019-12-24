/* global Shiny */
const setBoxSize = (styles) => {
  const box = document.querySelector('.box')
  for (const style in styles) {
    const newValue = styles[style]
    if (box.style[style] !== newValue) {
      if (style === 'box-sizing') {
        updateBoxSizing(box, newValue)
      } else {
        box.style[style] = newValue
      }
    }
  }
}

const updateBoxSizing = (box, boxSizing) => {
  // update box size
  const oldTransition = box.style.transition
  box.style.transition = 'none'
  setTimeout(() => (box.style.transition = oldTransition), 500)
  box.style.boxSizing = boxSizing

  // update class on container
  const boxContainer = document.querySelector('.box-container');
  ['content-box', 'border-box']
    .forEach(c => boxContainer.classList.remove('is-' + c))

  boxContainer.classList.add('is-' + boxSizing)
}

Shiny.addCustomMessageHandler('setBoxSize', setBoxSize)

const messagePresetChoice = (ev) => {
  Shiny.setInputValue('preset', ev.currentTarget.value, { priority: 'event' })
}

const presetButtons = document.querySelectorAll('.box-presets button')

presetButtons.forEach(btn => btn.addEventListener('click', messagePresetChoice))
