const citiesData = document
  .getElementById('data-cities')
  .textContent

const cities = JSON.parse(citiesData)

function updateCityAge ({ city, median_age: age, median_home_price }) {
  document.getElementById('city-header').textContent = city
  document.getElementById('city-text').textContent = city
  document.querySelector('#age').textContent = age
  document.getElementById('home-price')
    .textContent = '$' + median_home_price.toLocaleString()
}

updateCityAge(cities[0])

const btn = document.getElementById('next-city')
btn.addEventListener('click', function(event) {
  let btnValue = event.target.value
  btnValue = Number(btnValue) + 1
  if (btnValue >= cities.length) {
    btnValue = 0
  }
  btn.value = btnValue
  updateCityAge(cities[btnValue])
})

const chart = document.getElementById('chart')
chart.style.background = 'var(--gray)'
