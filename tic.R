# installs dependencies, runs R CMD check, runs covr::codecov()
do_package_checks()

if (ci_on_travis()) {
  # creates pkgdown site and pushes to gh-pages branch
  get_stage("before_deploy") %>%
    add_step(step_setup_ssh("id_rsa"))

  do_pkgdown()
}
