#' @importFrom glue glue
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL


#' First Sentences of Books
#'
#' First sentences of books, downloaded from
#' <https://firstsentencesofbooks.tumblr.com/>.
#'
#' @format A data frame with 40 rows and 3 variables:
#' \describe{
#'   \item{\code{quote}}{The first sentence of the book}
#'   \item{\code{title}}{The title of the book}
#'   \item{\code{author}}{The author of the book}
#' }
"first_sentences"



#' 125 US Cities Ranked, 2019
#'
#' A ranking by U.S. news of the 125 most populous US metro areas to find the
#' best places to live.
#'
#' @references <https://data.world/dataremixed/125-us-cities-ranked-2019>,
#'  <https://realestate.usnews.com/places/rankings/best-places-to-live>
#'
#' @format A data frame with 125 rows and 26 variables:
#' \describe{
#'   \item{\code{us_news_rank}}{integer. Ranking by U.S. News}
#'   \item{\code{city}}{character. City name}
#'   \item{\code{state}}{character. State name, abbreviated}
#'   \item{\code{state_full}}{character. State name, full}
#'   \item{\code{metro_population}}{double. Population of meto area}
#'   \item{\code{average_annual_salary}}{double. Average annual salary}
#'   \item{\code{avg_temp_high_f}}{double. Average high temperature in ºF}
#'   \item{\code{avg_temp_low_f}}{double. Average low temperature in ºF}
#'   \item{\code{median_age}}{double. Median population age}
#'   \item{\code{median_home_price}}{double. Median home price}
#'   \item{\code{avg_annual_rainfall_in}}{double. Average annual rainfall in inches}
#'   \item{\code{unemployment_rate}}{double. Unemployment rate}
#'   \item{\code{median_monthly_rent}}{double. Median monthly rent cost}
#'   \item{\code{avg_commute_time_mins}}{double. Average commute times in minutes}
#'   \item{\code{percent_single}}{double. Percent of metro population that is single}
#'   \item{\code{total_students}}{double. Total number of students}
#'   \item{\code{total_teachers}}{double. Total number of teaches}
#'   \item{\code{violent_crime}}{double. Crime rates per 100,000 people}
#'   \item{\code{property_crime}}{double. Crime rates per 100,000 people}
#'   \item{\code{link}}{character. Link to url on usanews.com}
#'   \item{\code{lat}}{double. Latitude of metro area}
#'   \item{\code{lon}}{double. Longitude of metro area}
#'   \item{\code{lat_min}}{double. Latitude minimum bounding box of metro area}
#'   \item{\code{lat_max}}{double. Latitude maximum bounding box of metro area}
#'   \item{\code{lon_min}}{double. Longitude minimum bounding box of metro area}
#'   \item{\code{lon_max}}{double. Longitude maximum bounding box of metro area}
#' }
"us_cities_ranked"
