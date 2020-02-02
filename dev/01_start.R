# Building a Prod-Ready, Robust Shiny Application.
# 
# Each step is optional. 
# 
# 1 - On init
# 
## 1.1 - Fill the descripion & set options
## 
## Add information about the package that will contain your app

golem::fill_desc(
  pkg_name = "data.analyseR", # The Name of the package containing the App 
  pkg_title = "data.analyseR", # The Title of the package containing the App 
  pkg_description = "App that analyse your data.", # The Description of the package containing the App 
  author_first_name = "David", # Your First Name
  author_last_name = "Mateos",  # Your Last Name
  author_email = "cldav.privmath@gmail.com",      # Your Email
  repo_url = "https://github.com/alberto-mateos-mo/data.analyseR.git" # The (optional) URL of the GitHub Repo
)     

## Use this desc to set {golem} options

golem::set_golem_options()

## 1.2 - Set common Files 
## 
## If you want to use the MIT licence, README, code of conduct, lifecycle badge, and news

usethis::use_readme_rmd( open = FALSE )
usethis::use_lifecycle_badge( "Experimental" )

## 1.4 - Init Tests
## 
## Create a template for tests

golem::use_recommended_tests()

## 1.5 : Use Recommended Package

golem::use_recommended_deps()

## 1.6 Add various tools

# If you want to change the favicon (default is golem's one)
golem::remove_favicon()

# Add helper functions 
golem::use_utils_ui()
golem::use_utils_server()

# You're now set! 
# go to dev/02_dev.R
rstudioapi::navigateToFile( "dev/02_dev.R" )

