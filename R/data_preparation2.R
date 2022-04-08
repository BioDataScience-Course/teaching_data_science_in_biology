# Data preparation 2 for teaching data science...
# Data from 2019-2020 & 2020-2021
# Philippe Grosjean (phgrosjean@sciviews.org) &
# Guyliann Engels (Guyliann.Engels@umons.ac.be)

# Import Raw Datasets -----------------------------------------------------
# Indicate the root folder where these data are located here:
root <- svMisc::pcloud() # Replace this if you have a different directory!
#root <- "~/Desktop/zenodo_repos/"

if (!fs::dir_exists(root))
  stop("The directory 'root' (", root, ") will contain raw data. ",
    "You must indicate an existing directory!")
# Use `data_download.R` to download raw data.
# Data will be in <root>/sdd_<acad_year>/

# Initialisation ----------------------------------------------------------

# SciViews::R is a series of packages and additions for R
SciViews::R
source("R/functions.R")

# Parameterization --------------------------------------------------------

acad_years <- c("2019-2020", "2020-2021")
sdd_folders <- glue("sdd_{acad_years}")
data_dirs <- path(root, sdd_folders)
courses <- c("A", "B") # Don't include courses C, D & E in the analysis
institutions <- "UMONS" # Don't include Campus UCharleroi
time_interval <- interval(as.time("2020/03/05"), as.time("2021/05/15"))

# Extract data from git logs ----------------------------------------------

path(data_dirs, "git_log.csv") %>.%
  purrr::map_dfr(., read) %>.%
  set_attr(., "spec", NULL) %>.% # Eliminate extra info from the readr function
  set_attr(., "comment", NULL) %>.% # Idem
  # TODO: temporary reworking of data (convert into new format)
  # eliminate this later on
  mutate(.,
    log = message,
    icourse = course,
    course = substring(github_project, 1, 1),
    # TODO: this is incorrect: modules must be collected from projects.csv
    # see multivariate_analysis.R
    modules = substring(github_project, 1, 3),
    app = github_project) %>.%
  # type and subtype are now merged into a single type column
  mutate(., type = case_when(
    subtype == "individual" ~ "ind. github",
    subtype == "group" ~ "group github",
    subtype == "challenge,group" ~ "challenge",
    subtype == "challenge, group" ~ "challenge"
  )) %>.%
  select(., -github_project, -subtype, -message) %>.%
  # For now, A03Ga_20M_urchin at Q2 is not correct because it should be
  # A07Ga_20M_urchin... do the change now. Note: currently, for icourse, we
  # have "S-BIOG-006,S-BIOG-027", but we cannot run projects across terms
  # So, that is why we use a different name for Q2!
  # Since Q2 starts on 2021-01-01, we separate both simply based on the year:
  # 2020 = Q1, 2021 = Q2
  mutate(.,
    app = ifelse(app == "A03Ga_20M_urchin" & year(date) == 2021,
      "A07Ga_20M_urchin", app),
    icourse = ifelse(app == "A03Ga_20M_urchin", "S-BIOG-006",
      ifelse(app == "A07Ga_20M_urchin", "S-BIOG-027", icourse))
  ) %>.%
  # Challenge should by C, not G => correct it
  mutate(.,
    app = ifelse(app == "C03Ga_20M_challenge", "C03Ca_20M_challenge", app)) %>.%
  mutate(., term = case_when(
    icourse %in% c("S-BIOG-006", "S-BIOG-015", "S-BIOG-025") ~ "Q1",
    icourse %in% c("S-BIOG-027", "S-BIOG-061") ~ "Q2"
  )) %>.%
  # TODO: similar corrections are required for urchin, human and zooplankton in
  # 2019-2020 !!!
  # Keep only logs from the right institution and courses
  filter(., institution %in% institutions) %>.%
  filter(., course %in% courses) %>.%
  # The "from" column specifies if the commit is by staff ("staff") or not (NA)
  # Keep only info about commits by students
  filter(., is.na(from)) %>.%
  # We only consider text change => bin_before must be NA
  filter(., is.na(bin_before)) %>.%
  # We consider only (R) Markdown
  filter(., tolower(extension) == ".rmd") %>.%
  select(., -from, -bin_before, -bin_after) %>.%
  filter(., date %within% time_interval) %>.%
  # Aggregations
  group_by(., user, date, commit, app, type, modules, course) %>.%
  summarise(.,
    add    = sum(add, na.rm = TRUE),
    change = sum(change, na.rm = TRUE)) %>.%
  ungroup(.) %>.%
  mutate(.,
    week = acad_w(date, label = TRUE),
    period = acad_2w(date, label = TRUE)) ->
  log


# Extract support data ----------------------------------------------------

path(data_dirs, "support.csv") %>.%
  map_dfr(., read) %>.%
  set_attr(., "spec", NULL) %>.% # Eliminate extra info from the readr function
  set_attr(., "comment", NULL) %>.% # Idem
  # TODO: temporary reworking of data (convert into new format)
  # eliminate this later on
  mutate(., icourse = course, term = quarter, course = case_when(
    icourse == "S-BIOG-006" ~ "A",
    icourse == "S-BIOG-027" ~ "A",
    icourse == "S-BIOG-015" ~ "B",
    icourse == "S-BIOG-061" ~ "B",
    icourse == "S-BIOG-025" ~ "C",
    icourse == "S-BIOG-043" ~ "D",
    icourse == "S-BIOG-077" ~ "E",
    icourse == "S-BIOG-921" ~ "A",
    icourse == "S-BIOG-970" ~ "A",
    icourse == "S-BIOG-937-958-959" ~ "B",
    icourse == "S-BIOG-006,S-BIOG-015" ~ "A+B", # Bridge students take 2 courses
    icourse == "S-BIOG-027,S-BIOG-061" ~ "A+B" # Idem for Q2
  )) %>.%
  select(., -quarter) %>.%
  # Restrict courses & institutions
  filter(., institution %in% institutions) %>.%
  filter(., course %in% courses) %>.%
  filter(., date %within% time_interval) %>.%
  # Allow to aggregate by week or by two weeks
  mutate(.,
    week = acad_w(date, label = TRUE),
    period = acad_2w(date, label = TRUE)
  ) %>.%
  # Indicate if a message is from staff or pupil
  # TODO: create columns "from" and "to" directly in support directly instead!
  mutate(., sender = c("student", "teacher")[
    (user %in% c("@display", "@teacher", "id2", "id3", "id6", "id78") |
    (user == "id121" & course != "S-BIOG-025")) + 1]) %>.%
  filter(., sender == "student") %>.%
  drop_na(., sender) ->
  support

# Aggregate by period or by week
support %>.%
  group_by(., period, acad_year, course) %>.%
  summarise(., us_acti_mess = length(unique(user)), message = n()) %>.%
  ungroup(.) ->
  sup_period

support %>.%
  group_by(., week, acad_year, course) %>.%
  summarise(., us_acti_mess = length(unique(user)), message = n()) %>.%
  ungroup(.) ->
  sup_week


# Modules by period -------------------------------------------------------

path(data_dirs, "lessons.csv") %>.%
  purrr::map_dfr(., read) %>.%
  set_attr(., "spec", NULL) %>.% # Eliminate extra info from the readr function
  set_attr(., "comment", NULL) %>.%
  filter(., institution %in% institutions) %>.%
  filter(., course %in% courses) %>.%
  filter(., !topic %in% c("extra_session", "install_party")) %>.%
  group_by(., acad_year, course, modules) %>.%
  summarise(., date = max(end)) %>.%
  filter(., date %within% time_interval) %>.%
  ungroup(.) %>.%
  # Allow to aggregate by week or by two weeks
  mutate(.,
    week = acad_w(date, label = TRUE),
    period = acad_2w(date, label = TRUE)
  ) %>.%
  group_by(., acad_year, period) %>.%
  summarise(., .modules = n()) ->
  modules_by_period

write$csv(modules_by_period, "data/modules_by_period.csv")

# Combine datasets ---------------------------------------------------------

log %>.%
  group_by(., user, course, period) %>.%
  summarise(., add = sum(add), change = sum(change)) %>.%
  ungroup(.) %>.%
  left_join(., modules_by_period) %>.%
  replace_na(., list(.modules = 0)) %>.%
  mutate(., .modules = factor(.modules)) ->
  log_period

write$csv(log_period, "data/log_period.csv")

support %>.%
  group_by(., user, course, period) %>.%
  summarise(., messages = n()) %>.%
  ungroup(.) %>.%
  left_join(., modules_by_period) %>.%
  replace_na(., list(.modules = 0)) %>.%
   mutate(., .modules = factor(.modules)) ->
  support_period

write$csv(support_period, "data/support_period.csv")

rm(log, log_period, support, sup_period, sup_week, support_period)
rm(modules_by_period)
