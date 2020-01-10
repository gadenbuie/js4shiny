# installs dependencies, runs R CMD check, runs covr::codecov()
do_package_checks()

if (ci_on_travis() && ci_has_env("BUILD_PKGDOWN")) {
  # creates pkgdown site and pushes to gh-pages branch
  tic::do_pkgdown()
}
