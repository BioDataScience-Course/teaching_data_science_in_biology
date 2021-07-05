# Data preparation for teaching data science...
# Philippe Grosjean (phgrosjean@sciviews.org) &
# Guyliann Engels (Guyliann.Engels@umons.ac.be)

# Download the datasets (TODO: from where?)
# Indicate the root folder where these data are located here:
root <- svMisc::pcloud() # Replace this if you have a different directory!

# Initialisation ----------------------------------------------------------

# SciViews::R is a series of packages and additions for R
# TODO: code to install the required packages here

SciViews::R
source("R/functions.R")

# Parameterization --------------------------------------------------------

acad_year <- "2020-2021"
sdd_folder <- glue::glue("sdd_{acad_year}")
data_dir <- path(root, sdd_folder, "data")
courses <- c("A", "B", "C") # Don't include courses D & E in the analysis
institutions <- "UMONS" # Don't include Campus UCharleroi
ignore_modules <- c("A00", "A99", "B00", "B99", "C00", "C99", "D99")
users_state <- c("regular", "repeater") # Exclude staff, mobility, bridge & 0

# Import data -----------------
users <- read(path(data_dir, "users.csv"))
learnr <- read(path(data_dir, "learnr.csv"))
projects <- read(path(data_dir, "projects.csv"))
assessments <- read(path(data_dir, "assessments.csv"))

# users
users %>.%
  filter(., institution == "UMONS" & term == "Q1" &
    state %in% c("regular", "repeater", "bridge", "mobility")) %>.%
  group_by(., course) %>.%
  summarise(., user = n()) %>.%
  ungroup(.) %>.%
  filter(., course != "D") -> us_tab
# A few students are in both course A and B simultaneously
ab <- us_tab$user[us_tab$course == "A,B"]
us_tab$user[us_tab$course %in% c("A", "B")] <- us_tab$user[us_tab$course %in% c("A", "B")] + ab
us_tab <- filter(us_tab, course != "A,B")

# learnr
learnr %>.%
  filter(.,
    !app %in% c("A06Lb_recombinaison", "A99La_avis", "B00La_rappel", "B99La_avis","C99La_avis") &
    !is.na(label)) %>.%
  mutate(., course = substr(app, 1, 1), app_label = paste0(app, label)) %>.%
  filter(., course %in% c("A", "B", "C")) %>.%
  group_by(., course) %>.%
  summarise(., app = length(unique(app)), questions = length(unique(app_label))) -> learnr_tab

# projects
projects %>.%
  filter(., type %in% c("ind. github", "group github") & course != "D") %>.%
  group_by(., course, type) %>.%
  count(.) %>.%
  pivot_wider(., names_from = "type", values_from = "n") %>.%
  ungroup(.) %>.%
  select(., course, `ind. github`, `group github`) -> projects_tab

# table of the number of exercises by type ---
assessments %>.%
  filter(., type == "h5p") %>.%
  mutate(.,  app_type = paste0(app, "_" ,'type'), course = substring(app, 1, 1)) %>.%
  group_by(., course) %>.%
  summarise(., h5p = length(unique(app_type))) -> h5P_tab

us_tab %>.%
  mutate(., module = c(12, 8, 6)) %>.%
  left_join(., h5P_tab) %>.%
  left_join(., mutate(learnr_tab, learnr = paste0(app, " (", questions, ")"), .keep = "unused")) %>.%
  left_join(., projects_tab) -> tab_summary

write$csv(tab_summary, "data/sdd_infos.csv")

rm(users, us_tab, learnr, learnr_tab, assessments, projects, projects_tab, h5P_tab, tab_summary)

