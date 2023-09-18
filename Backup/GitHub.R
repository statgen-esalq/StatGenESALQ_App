usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)

usethis::use_git_remote(
  "origin",
  url = "https://github.com/BrendaMonis/Shiny_app_BeP.git",
  overwrite = TRUE
)

usethis::use_git_remote(
  "origin",
  url = "https://github.com/PedroMassaro/DesignGen_App_Pedro.git",
  overwrite = TRUE
)

git remote add origin https://github.com/PedroMassaro/DesignGen_App_Pedro.git

usethis::git_default_branch_configure()

usethis::create_from_github(
  "https://github.com/PedroMassaro/DesignGen_App_Pedro.git"
)
