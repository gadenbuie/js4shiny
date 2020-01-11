# installs dependencies, runs R CMD check, runs covr::codecov()
do_package_checks()

if (ci_on_travis() && ci_has_env("BUILD_PKGDOWN")) {
  # creates pkgdown site and pushes to gh-pages branch
  print(tic::ci_can_push("TRAVIS_DEPLOY_KEY"))
  get_stage("before_deploy") %>%
    add_step(step_setup_ssh("id_rsa"))

  do_pkgdown(deploy = TRUE)
}
