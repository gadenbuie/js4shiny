# installs dependencies, runs R CMD check, runs covr::codecov()
do_package_checks()

build_pkgdown <- ci_has_env("BUILD_PKGDOWN")
if (ci_on_travis() && build_pkgdown) {
  # creates pkgdown site and pushes to gh-pages branch
  get_stage("before_deploy") %>%
    add_step(step_setup_ssh())

  tic::do_pkgdown(deploy = TRUE)
}
