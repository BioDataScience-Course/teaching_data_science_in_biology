# Download data from Zenodo for teaching data science...
# Data from 2018-2019, 2019-20230 & 2020-2021
# Philippe Grosjean (phgrosjean@sciviews.org) &
# Guyliann Engels (Guyliann.Engels@umons.ac.be)

# This script prepares 3 datapackages.
# You can explore the metadata of each table with the datapackage.json file.


# Determine the root path -------------------------------------------------

# Indicate the root folder where these data must be downloaded here under:

root <- svMisc::pcloud() # Replace this if you have a different directory!
#root <- "~/Desktop/zenodo_repos/"
#fs::dir_create(root) # if you want create the directory

# Zenodo ------------------------------------------------------------------
## Biological data science courses at UMONS, Belgium:
### student's activity for 2020-2021: <https://zenodo.org/record/6420917>
### student's activity for 2019-2020: <https://zenodo.org/record/6420879>
### student's activity for 2018-2019: <https://zenodo.org/record/6420348>

url <- "https://zenodo.org/record/{id_zenodo}/"

(zenodo <- tibble::tibble(
  acad_year  = c("2020-2021", "2019-2020", "2018-2019"),
  id_zenodo  = c("6420917", "6420879", "6420348"),
  url_zenodo = glue::glue(url)
))

# Student's activity for 2020-2021 ----------------------------------------
acad_year <- "2020-2021"
sdd_folder <- glue::glue("sdd_{acad_year}")
data_dir <- fs::path(root, sdd_folder)
data_url <- paste0(zenodo$url_zenodo[zenodo$acad_year == acad_year], "files/")

if (!fs::dir_exists(data_dir))
  fs::dir_create(data_dir)

resources <- paste0(c(
  "assessments", "challenge", "courses", "git_log", "h5p", "learnr", "lessons",
  "projects", "shiny", "support", "users", "wooclap"),".csv")
all_resources <- c(resources, "datapackage.json", "README.md")

for (i in seq_along(all_resources)) {
  if (!fs::file_exists(fs::path(data_dir, all_resources[i]))) {
    download.file(url = paste0(data_url, all_resources[i]),
      destfile = path(data_dir, all_resources[i]))
  }
}

# Student's activity for 2019-2020 ----------------------------------------
acad_year <- "2019-2020"
sdd_folder <- glue::glue("sdd_{acad_year}")
data_dir <- fs::path(root, sdd_folder)
data_url <- paste0(zenodo$url_zenodo[zenodo$acad_year == acad_year], "files/")

if (!fs::dir_exists(data_dir))
  fs::dir_create(data_dir)


resources <- paste0(c(
  "assessment", "courses", "exam", "git_log", "lessons", "projects", "support",
  "users"),".csv")
all_resources <- c(resources, "datapackage.json", "README.md")

for (i in seq_along(all_resources)) {
  if (!fs::file_exists(fs::path(data_dir, all_resources[i]))) {
    download.file(url = paste0(data_url, all_resources[i]),
      destfile = path(data_dir, all_resources[i]))
  }
}

# Student's activity for 2018-2019 ----------------------------------------
acad_year <- "2018-2019"
sdd_folder <- glue::glue("sdd_{acad_year}")
data_dir <- fs::path(root, sdd_folder)
data_url <- paste0(zenodo$url_zenodo[zenodo$acad_year == acad_year], "files/")

if (!fs::dir_exists(data_dir))
  fs::dir_create(data_dir)

resources <- paste0(c(
  "assessment", "courses", "lessons", "projects", "users"),".csv")
all_resources <- c(resources, "datapackage.json", "README.md")

for (i in seq_along(all_resources)) {
  if (!fs::file_exists(fs::path(data_dir, all_resources[i]))) {
    download.file(url = paste0(data_url, all_resources[i]),
      destfile = path(data_dir, all_resources[i]))
  }
}
