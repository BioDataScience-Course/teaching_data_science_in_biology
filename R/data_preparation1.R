# Data preparation for teaching data science...
# Data from 2020-2021
# Philippe Grosjean (phgrosjean@sciviews.org) &
# Guyliann Engels (Guyliann.Engels@umons.ac.be)

# Download the datasets (TODO: from where?)
# Indicate the root folder where these data are located here:
root <- svMisc::pcloud() # Replace this if you have a different directory!
if (!fs::dir_exists(root))
  stop("The directory 'root' (", root, ") will contain raw data. ",
    "You must indicate an existing directory!")
# Data will be in <root>/sdd_<acad_year>/data


# Initialisation ----------------------------------------------------------

# SciViews::R is a series of packages and additions for R

SciViews::R
source("R/sciviews_r_addons.R")
source("R/functions.R")


# Parameterization --------------------------------------------------------

acad_year <- "2020-2021"
sdd_folder <- glue("sdd_{acad_year}")
data_dir <- path(root, sdd_folder, "data")
courses <- c("A", "B", "C") # Don't include courses D & E in the analysis
institutions <- "UMONS" # Don't include Campus UCharleroi
ignore_modules <- c("A00", "A99", "B00", "B99", "C00", "C99", "D99")
users_state <- c("regular", "repeater") # Exclude staff, mobility, bridge & 0


# Number of modules per  course -------------------------------------------

# Count modules by course, after excluding ignore_modules
# courses$modules contains the list of modules separated by '+'
count_modules <- function(x, ignore = ignore_modules) {
  mods <- strsplit(x, "+", fixed = TRUE)
  sapply(mods, function(x) length(x[!x %in% ignore]))
}
read(path(data_dir, "courses.csv")) %>.%
  mutate(., n_modules = count_modules(modules)) %>.%
  # Sum the modules by course
  group_by(., course, institution) %>.%
  summarise(., n_modules = sum(n_modules)) %>.%
  ungroup(.) %>.%
  # Keep only the course we are interested in this study
  filter(., institution %in% {{institutions}}, course %in% {{courses}}) ->
  courses_modules


# Times of modules -------------------------------------------------------------

read(path(data_dir, "lessons.csv")) %>.%
  set_attr(., "spec", NULL) %>.% # Eliminate extra info from the readr function
  set_attr(., "comment", NULL) %>.% # Idem
  # Restrict courses and modules
  filter(., institution %in% institutions) %>.%
  filter(., course %in% courses) %>.%
  filter(., !modules %in% ignore_modules) %>.%
  # Get time interval for each module (from beginning of first lesson to
  # completion time of last one, that is just before first lesson of next
  # module, or at the deadline for last one)
  group_by(., course, modules) %>.%
  summarise(., preparation = min(preparation), start = min(start),
    end = max(end), completion = max(completion), deadline = max(deadline)) %>.%
  ungroup(.) %>.%
  mutate(.,
    interval_prepa = interval(preparation, end),
    interval_course = interval(start, end),
    interval_complete = interval(start, completion)) ->
  modules_timings
# Note: we could also get the number of modules per course from this table


# Extract users data ------------------------------------------------------

# Get a list of users in one of the three UMONS courses A, B & C
read(path(data_dir, "users.csv")) %>.%
  set_attr(., "spec", NULL) %>.% # Eliminate extra info from the readr function
  set_attr(., "comment", NULL) %>.% # Idem
  # Restrict courses
  filter(., institution %in% institutions) %>.%
  filter(., course %in% courses) %>.%
  # Keep only Q1 term data (should be the same for Q2)
  filter(., term == "Q1") %>.%
  # Eliminate staff, non enrolled (0), mobility, bridge which are special cases
  filter(., state %in% users_state) %>.%
  left_join(., courses_modules) %>.%
  select(., user, institution, icourse, icflag, course,
    enrolled, state, .n_modules = n_modules) ->
  users


# Extract metrics for support ---------------------------------------------

# Note: for support, I try to make metrics more comparable by dividing by the
# number of modules in each course
read(path(data_dir, "support.csv")) %>.%
  set_attr(., "spec", NULL) %>.% # Eliminate extra info from the readr function
  set_attr(., "comment", NULL) %>.% # Idem
  # TODO: temporary code: course should be icourse, and course == A, B, C should
  # be there too, and quarter is now named term
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
  # One student did contribute to other courses than its own => eliminate this
  # TODO: must be changed in the original table instead!!!
  filter(., !(user == "id121" &
      channel %in% c("sdd-assist", "sdd1-umons", "sdd2-umons"))) %>.%
  # Restrict courses & institutions
  filter(., institution %in% institutions) %>.%
  filter(., course %in% courses) %>.%
  # Keep only users present in our users table
  filter(., user %in% users$user) %>.%
  # Metrics calculated by users. Metrics starting with a dot (.) are
  # intermediary metrics that we should not use in the final analysis
  # (kept for verification only)
  group_by(., user) %>.%
  summarise(.,
    .q_all_abs = n(),
    .q_pub_abs = sum(public == TRUE),
    # This is too specific of the particular case of our chosen channels
    # (opposition of mail and Discord). The important point is whether students
    # dare to discuss their problelms in public channels or not (and the number
    # of questions they ask, of course)
    #.q_mail    = sum(type == "mail"),
    `q_pub%`   = .q_pub_abs / .q_all_abs * 100) %>.%
  left_join(users, .) %>.%
  replace_na(.,
    list(.q_all_abs = 0, .q_pub_abs = 0, `q_pub%` = 0)) %>.% # .q_mail = 0
  mutate(.,
    #Note: q_pub is highly correlated with q_all => do not keep it
    #q_pub  = as.numeric(.q_pub_abs / .n_modules),
    #q_mail = as.numeric(.q_mail / .n_modules),
    q_all  = as.numeric(.q_all_abs / .n_modules)) ->
  support_metrics


# Extract metrics for H5P -------------------------------------------------

# Note: for h5p, I divide by the number of exercises in each course as a first
# (simple) approach to make metrics more comparable from one course to the other
read(path(data_dir, "h5p.csv")) %>.%
  set_attr(., "spec", NULL) %>.% # Eliminate extra info from he readr function
  set_attr(., "comment", NULL) %>.% # Idem
  # TODO: there changes correspond to adaptations that still must be done in
  # the original date... To be eliminated later on!
  mutate(., icourse = course, course = substring(app, 1, 1)) %>.%
  mutate(., modules = substring(app, 1, 3)) %>.%
  # It no user is recorded, we drop the event
  drop_na(., user) %>.%
  # It is possible that students in one course do exercises in another course
  # So, we combine student and course to eliminate these cases
  filter(., paste(user, course) %in%
      unique(paste(users$user, users$course))) %>.%
  # Keep only event corresponding to the answer to the exercise
  filter(., verb == "answered") ->
  h5p

# First calculate general statistics by users and applications
h5p %>.%
  group_by(., user, app) %>.%
  summarise(.,  nitems = n(), success = any(correct == TRUE)) %>.%
  drop_na(., success) %>.% # Drop subitems in a multi-exercises H5P widget
  # because information is redundant
  # Summarize the info for each user
  group_by(., user) %>.%
  summarize(.,
    .h_done = n(),
    .h_ans  = sum(nitems),
    .h_ok   = sum(success)) %>.%
  left_join(users, .) ->
  h5p_metrics

# Fraction of H5P done in the right time period (we consider here the
# preparation interval that starts from the end of previous module and ends
# at the last lesson in the module
h5p %>.%
  left_join(., select(modules_timings, modules, interval_prepa)) %>.%
  # The following code also works with multiple intervals, but it is much
  # slower than the direct solution when there is always a single interval
  # as it is the case here
  #mutate(., h5p_intime = map2_lgl(date, interval_prepa,
  #  function(x, y) any(x %within% y))) ->
  mutate(., h5p_intime = date %within% interval_prepa) ->
  h5p2
table(h5p2$h5p_intime) # Roughly 1/3 of answers are done too late
# This is level 1 exercises => indicate how late students tend to be on the
# planned schedule!
# Now get statistics for ansewer done at the right time period
# This treatment is identical to here above, but only on event observed in time
h5p2 %>.%
  filter(., h5p_intime) %>.%
  group_by(., user, app) %>.%
  summarise(.,  nitems = n(), success = any(correct == TRUE)) %>.%
  drop_na(., success) %>.% # Drop subitems in a multi-exercises H5P widget
  # because information is redundant
  # Summarize the info for each user
  group_by(., user) %>.%
  summarize(., .h_intime = n()) %>.%
  full_join(h5p_metrics, .) %>.%
  replace_na(., list(.h_done = 0, .h_ans = 0, .h_ok = 0, .h_intime = 0)) ->
  h5p_metrics
rm(h5p2)

# Derived metrics, possibly divided by the total number of exercises/course
h5p_metrics %>.%
  group_by(., course) %>.%
  # We consider here the max on what is actually done by the students
  summarize(., .n_h5p = max(.h_done, na.rm = TRUE)) %>.%
  left_join(h5p_metrics, .) %>.%
  # Calculate final metrics
  mutate(.,
    `h_done%` = .h_done / .n_h5p * 100,
    `h_ok%` = .h_ok / .h_done * 100,
    `h_intime%` = .h_intime / .h_done * 100,
    `h_trials` = .h_ans / .h_done
  ) %>.%
  replace_na(.,
    list(`h_done%` = 0,  `h_ok%` = 0, `h_intime%` = 0, h_trials = 0)) ->
  h5p_metrics
rm(h5p)


# Extract metrics for learnr ---------------------------------------------

read(path(data_dir, "learnr.csv")) %>.%
  set_attr(., "spec", NULL) %>.% # Eliminate extra info from the readr function
  set_attr(., "comment", NULL) %>.% # Idem
  # TODO: there changes correspond to adaptations that still must be done in
  # the original date... To be eliminated later on!
  mutate(., icourse = course, course = substring(app, 1, 1)) %>.%
  mutate(., modules = substring(app, 1, 3)) %>.%
  # Eliminate event of non registered users
  drop_na(., user) %>.%
  # It is possible that students in one course do exercises in another course
  # So, we combine student and course to eliminate these cases
  filter(., paste(user, course) %in%
      unique(paste(users$user, users$course))) %>.%
  # Restrict courses and modules
  filter(., institution %in% institutions) %>.%
  filter(., course %in% courses) %>.%
  filter(., !modules %in% ignore_modules) %>.%
  # TODO: old code to be eliminated!
  #filter(., user %in% support_metrics$user) %>.%
  # Keep only apps in modules 1-12 and eliminate noscore exercises
  #filter(., !app %in% c("A00La_decouverte", "B00La_rappel")) %>.%
  # Filter out exercises that are not scored (app name ending with _noscore)
  filter(., !label %in% str_subset(label, "_noscore")) ->
  learnr

# Exercises in learnrs may be revisited by students...
# but we are only interested by the first sequence => filter out the data
# Filter  of the first work sequence on a learnr tutorial
learnr %>.%
  arrange(., user, app, label, date) %>.%
  # First sequence is determined by the date of the first correct answer
  # Reminder: for quizzes it is verb == "answered",
  # but for R code, it is verb == "submitted" !
  filter(., verb %in% c("answered", "submitted") & correct == TRUE) %>.%
  group_by(., app, label, user) %>.%
  slice_head(.) %>.% # Keep only first event
  left_join(learnr, select(., user, app, label, date_max = date),
    by = c("user", "app", "label")) %>.%
  # Also check if events occur out of time (according to preparation interval)
  left_join(., select(modules_timings, modules, interval_prepa)) %>.%
  # The following code also works with multiple intervals, but it is much
  # slower than the direct solution when there is always a single interval
  # as it is the case here
  #mutate(., .l_intime = map2_lgl(date, interval_prepa,
  #  function(x, y) any(x %within% y))) %>.%
  mutate(., .l_intime = date %within% interval_prepa) %>.%
  # Only keep events relative to the first sequence
  filter(., date <= date_max) ->
  learnr
table(learnr$.l_intime) # Roughly 1/3 of exercises done after the module ends!


# Count the number of time each verb occurs
learnr %>.%
  group_by(., user, course, verb) %>.%
  summarise(., n = n()) %>.%
  ungroup(.) %>.%
  # Create onle column for each verb
  pivot_wider(., names_from = "verb",  values_from = "n") %>.%
  replace_na(., list(answered = 0, assisted = 0, evaluated = 0,
    executed = 0, reset = 0, revealed = 0, submitted = 0)) ->
  learnr_metrics

# Determine median time (in mins) required to complete one exercise
learnr %>.%
  group_by(., course, app, label, user) %>.%
  slice_head(.) %>.%
  # Time to solve the exercise if the time difference between date of first
  # event and date to complete the exercise (date_max)
  # TODO: check what happens when the exercises is NOT completed!
  mutate(., diff = difftime(date_max, date, units = "mins")) %>.%
  #filter(., diff < 10) %>.% # Possibly do not consider too long intervals
  filter(., verb != "answered") %>.%
  group_by(., user, course) %>.%
  summarise(.,
    .l_intime = sum(.l_intime),
    l_time    = median(diff),
    .l_n_time = n()
  ) %>.%
  left_join(learnr_metrics, ., by = c("user", "course")) ->
  learnr_metrics

# Get number of correct answers (quiz answered and R code submitted separately)
learnr %>.%
  # Count correct items per user, course and verb (answered versus submitted)
  filter(., correct == TRUE) %>.%
  group_by(., user, course, verb) %>.%
  summarise(., n_correct = n()) %>.%
  # Make one column per verb
  pivot_wider(., names_from = verb, values_from = "n_correct",
    names_prefix = ".l_ok_") %>.%
  ungroup(.) %>.%
  left_join(learnr_metrics, ., by = c("user", "course")) ->
  learnr_metrics

# Calculate final metrics for learnr exercises
learnr_metrics %>.%
  group_by(., course) %>.%
  # Max number of answers in a course for all the students (used as reference)
  summarise(., .l_max_ans = max(.l_ok_answered),
    .l_max_sub = max(.l_ok_submitted)) %>.%
  left_join(learnr_metrics, ., by = "course") %>.%
  # Final metrics
  mutate(.,
    `l_done%` = (.l_ok_answered + .l_ok_submitted) /
      (.l_max_ans + .l_max_sub) * 100,
    `l_ok%` = (.l_ok_submitted - revealed) / submitted * 100,
    `l_intime%`= (.l_intime) / (.l_ok_answered + .l_ok_submitted) * 100,
    l_trials = executed / .l_ok_submitted,
    l_hints = assisted / .l_ok_submitted,
    l_time = as.numeric(l_time)) %>.%
  left_join(users, .) %>.%
  replace_na(., list(`l_done%` = 0,  `l_ok%` = 0, `l_intime%` = 0,
    l_trials = 0, l_hints = 0, l_time = 0)) ->
  learnr_metrics
# rm(learnr) using learnr data lower and remove after


# Extract metrics for git log --------------------------------------------

# First get more info about the various projects
read(path(data_dir, "projects.csv")) %>.%
  set_attr(., "spec", NULL) %>.% # Eliminate extra info from the readr function
  set_attr(., "comment", NULL) ->
  projects

# Get git log data and clean it a little bit
read(path(data_dir, "git_log.csv")) %>.%
  set_attr(., "spec", NULL) %>.% # Eliminate extra info from the readr function
  set_attr(., "comment", NULL) %>.% # Idem
  # TODO: temporary modifications before the final version of the table --------
# to be eliminated later on...
mutate(., log = message) %>.%
  mutate(., icourse = course, course = substring(github_project, 1, 1),
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
  )) ->
  log

# Check that all apps in log are also in projects before continuing
test <- unique(log$app) %in% unique(projects$app)
if (!all(test))
  stop("Missing apps in projects: ",
    paste(unique(log$app)[!test], collapse = ", "))

log %>.%
  # Get modules from projects...
  left_join(., select(projects, app, modules)) %>.%
  # TODO: this is the end of work done to adapt the dataset to new format ------
  # Keep only logs from the right institution and courses
  filter(., institution %in% institutions) %>.%
  filter(., course %in% courses) %>.%
  # The "from" column specifies if the commit is by staff ("staff") or not (NA)
  # Keep only info about commits by students
  filter(., is.na(from)) %>.%
  # We only consider text change => bin_before must be NA
  filter(., is.na(bin_before)) %>.%
  # Those columns are not needed any more
  select(., -from, -bin_before, -bin_after) %>.%
  # We consider only R Markdown files here (note: .r file could contain
  # functions.R with code copied from the course!)
  filter(., tolower(extension) %in% c(".rmd")) %>.%
  # There may be some binary data saved incorrectly with an .Rmd extension
  # so, we also eliminate all entries where change, add or delete are NA
  drop_na(., change, add, delete) %>.%
  # Do not take the challenge into account (for now)
  filter(., type != "challenge") %>.%
  # We consider only regular or repeater students (not staff, bridge, mobility)
  # One way to filter students, is to use the list in the users table
  filter(., user %in% users$user) ->
  log
table(log$type) # Roughly 45/55% group/individual

# This function create a list of intervals, given the various modules for each
# app as A01+A03+A05 => list of intervals for modules 1, 3 & 5
intervals <- function(x, y = modules_timings)
  as.list(y[y$modules %in% x, "interval_complete"])$interval_complete
# Here, we consider a GitHub project cannot be started before the beginning of
# the module and can be finished until completion of the module. So, we use
# interval_complete from modules_timings
log %>.%
  mutate(.,
    intervals = lapply(strsplit(modules, "+", fixed = TRUE), intervals)) %>.%
  mutate(.,
    intime = map2_lgl(date, intervals, function(x, y) any(x %within% y))) %>.%
  # Calculation of change made in time will be easier with this one
  mutate(., intime_change = ifelse(intime, change, 0)) ->
  log
table(log$intime) # Roughly 45% of commits are done in time

# Metrics are (separate calculation for individual and group projects):
# - % of all projects where there is at least one commit done (ind. + group)
# - number of commits (/ project)
# - number of add (/ project)
# - number of delete (/ project)
# - average change per commit
# - % changes made in time by this student
# - % of the group changes made by this student (only for group projects)

# Get number of projects with at least one commit (+ nbr of commits) per user
log %>.%
  group_by(., user, app) %>.%
  summarize(., .gi_commits = n()) %>.%
  group_by(., user) %>.%
  summarize(., .gi_proj = n(), .gi_commits = sum(.gi_commits)) %>.%
  ungroup(.) %>.%
  full_join(select(users, user, course), .) %>.%
  replace_na(., list(.gi_proj = 0, .gi_commits = 0)) ->
  log_metrics

# Add .l_max_proj as the maximum of projects
log_metrics %>.%
  group_by(., course) %>.%
  summarise(., .gi_max_proj = max(.gi_proj)) %>.%
  ungroup(.) %>.%
  full_join(log_metrics, .) ->
  log_metrics

# Complete the table with nbr of changes, additions, and deletions
# Individual projects
# Max projects (not used for now)
#log %>.%
#  filter(., type == "ind. github") %>.%
#  group_by(., course) %>.%
#  summarise(., .i_max_proj = length(unique(app))) %>.%
#  full_join(log_metrics, .) ->
#  log_metrics
# Users stats
log %>.%
  filter(., type == "ind. github") %>.%
  group_by(., user) %>.%
  summarize(., .i_proj = length(unique(app)), .i_commit = n(),
    .i_change = sum(change), .i_intime_change = sum(intime_change),
    .i_add = sum(add), .i_delete = sum(delete)) %>.%
  full_join(log_metrics, .) %>.%
  replace_na(.,
    list(.i_proj = 0, .i_commit = 0, .i_change = 0, .i_intime_change = 0,
      .i_add = 0, .i_delete = 0)) ->
  log_metrics

# Group projects (not used for now)
# Max projects (should be .gi_max_proj - .i_max_proj), but do the computation
# to allow checking it
#log %>.%
#  filter(., type == "group github") %>.%
#  group_by(., course) %>.%
#  summarise(., .g_max_proj = length(unique(app))) %>.%
#  full_join(log_metrics, .) ->
#  log_metrics
#if (any((log_metrics$.i_max_proj + log_metrics$.g_max_proj) !=
#  log_metrics$.gi_max_proj))
#  stop("Error in calculation of max projects")
# Users stats
log %>.%
  filter(., type == "group github") %>.%
  group_by(., user) %>.%
  summarize(.,  .g_proj = length(unique(app)), .g_commit = n(),
    .g_change = sum(change), .g_intime_change = sum(intime_change),
    .g_add = sum(add), .g_delete = sum(delete)) %>.%
  full_join(log_metrics, .) %>.%
  replace_na(.,
    list(.g_proj = 0, .g_commit = 0, .g_change = 0, .g_intime_change = 0,
      .g_add = 0, .g_delete = 0)) ->
  log_metrics

# For group projects, calculate total changes made by all users
log %>.%
  filter(., type == "group github") %>.%
  group_by(., project) %>.%
  summarize(., .g_tot_change = sum(change)) %>.%
  ungroup(.) %>.%
  replace_na(., list(.g_tot_change = 0)) ->
  log_tot_change
# Now, recalculate changes by user, merge with total changes per project
# and calculate % changes the user did in the group project
log %>.%
  filter(., type == "group github") %>.%
  group_by(., user, project) %>.%
  summarize(., .g_change = sum(change)) %>.%
  ungroup(.) %>.%
  replace_na(., list(.g_change = 0)) %>.%
  full_join(., log_tot_change) %>.%
  # Now, we can calculate fraction changed by each user in group projects
  mutate(., g_change_percent = .g_change / .g_tot_change * 100) %>.%
  # We aggregate these data for each user by considering the average value
  # over all group projects
  group_by(., user) %>.%
  summarise(., `g_change%` = mean(g_change_percent)) %>.%
  full_join(log_metrics, .) %>.%
  replace_na(., list(`g_change%` = 0)) ->
  log_metrics
rm(log_tot_change)

# Calculate final metrics
log_metrics %>.%
  mutate(.,
    g_commit    = .g_commit / .g_proj,
    g_change    = .g_change / .g_proj,
    g_add       = .g_add / .g_proj,
    g_delete    = .g_delete / .g_proj,
    `g_intime%` = .g_intime_change / .g_change * 100,
    i_commit    = .i_commit / .i_proj,
    i_change    = .i_change / .i_proj,
    i_add       = .i_add / .i_proj,
    i_delete    = .i_delete / .i_proj,
    `i_intime%` = .i_intime_change / .i_change * 100,
    `gi_proj%`   = .gi_proj / .gi_max_proj * 100
  ) %>.%
  replace_na(., list(g_commit = 0, g_change = 0, g_add = 0, g_delete = 0,
    `g_intime%` = 0, i_commit = 0, i_change = 0, i_add = 0, i_delete = 0,
    `i_intime%` = 0, `l_proj%` = 0)) ->
  log_metrics
rm(log)


# Assessments metrics -----------------------------------------------------

# Note: grades are recalculated /100
# We don't detail grades per level n1-4 because these measures are probably
# highly correlated with other metrics, and because one cannot simply consider
# the mean value for Q1 and Q2
read(path(data_dir, "assessments.csv")) %>.%
  set_attr(., "spec", NULL) %>.% # Eliminate extra info from the readr function
  set_attr(., "comment", NULL) %>.% # Idem
  # TODO: temporary code to set the raw tble as it should be in the final
  # version... to be eliminated later on
  mutate(., icourse = course, course = substring(app, 1, 1)) %>.%
  mutate(., app =
      ifelse(app == "A08Gb_20M_urchin", "A07Ga_20M_urchin", app)) %>.%
  # set is <user>_<icourse>_<evaluation>, grading is calculated for each level
  group_by(., set) %>.%
  # contribution of each item to final grade is score * weight for the score
  # (which give a total score /10 for one unit, * weight for the grading, which
  # weights the various units so that the final grade is /100)
  summarize(., user = user,
    grade = sum(score * score_weight * grade_weight)) %>.%
  # Mean of grades Q1/Q2 for the whole course
  # TODO: we need a course_weight here, because weights by AA may differ,
  # for instance course A in 2019-2020 => Q1 * 2/3 + Q2 * 1/3 because 8 modules
  # during Q1 and 4 modules during Q2 !!!
  group_by(., user) %>.%
  summarize(., grade = mean(grade)) %>.%
  left_join(users, .) ->
  assess_metrics


# Compile all data in a single table --------------------------------------

users %>.%
  full_join(., support_metrics) %>.%
  full_join(., h5p_metrics) %>.%
  full_join(., learnr_metrics) %>.%
  full_join(., log_metrics) %>.%
  full_join(., assess_metrics) %>.%
  # We now calculate q_prod which measures the productivity following questions
  # by dividing the total of line changes by the total of questions by this user
  mutate(., q_prod =  (.g_add + .i_add) / .q_all_abs) %>.%
  # For a user that did asked no questions at all, this is Inf
  # => just replace by the number of lines changed (as if it was just one q)
  mutate(., q_prod = ifelse(is.finite(q_prod), q_prod, .g_add + .i_add)) %>.%
  # Eliminate all temporary metrics whose name starts with a dot (.)
  select(., -starts_with(".")) %>.%
  select(., -(answered:submitted)) ->
  sdd_metrics_temp

rm(support_metrics, h5p_metrics, learnr_metrics, log_metrics, assess_metrics)
rm(users, modules_timings, projects, courses_modules)


# Final datasets ----------------------------------------------------------

plot(correlation(sdd_metrics_temp[, -(1:7)], method = "spearman")) # Zoom in!
# There is a correlation between all the metrics that reflect the % of work done
# (h_done%, l_done%, gi_proj%). One could summarize these into done%
# There is also a positive correlation between commit, change, add, and delete,
# especially for individual github projects => keep only changes
# One could also summarize these items, although correlation is lower for group

# Simplification of the metrics:
# - h_done%, l_done% & gi_proj% are highly correlated -> combine into done%
# - h_intime%, l_intime%, i_intime% highly correlated + g_intime% mildly corr.
#   (but we know why = multimodule projects) -> combine into intime%
# - i_add/i_delete highly correlated to i_commit => eliminate
# - idem for g_add/g_delete
# - rename: q_all -> question, g_change% -> g_contrib%
# - take log(x + 1) for l_time and q_all given the heavily skewed distribution
# - Keep only additional variables that may be useful (user, course & state)

sdd_metrics_temp %>.%
  transmute(.,
    user = user, course = course, state = state,
    question = log1p(q_all), `q_pub%` = `q_pub%`, q_prod = q_prod,
    `h_ok%` = `h_ok%`, h_trials = h_trials,
    `l_ok%` = `l_ok%`, l_trials = l_trials, l_hints = l_hints,
    l_time = log1p(l_time),
    i_commits = i_commit, i_changes = i_change,
    g_commits = g_commit, g_changes = g_change,
    `g_contrib%` = `g_change%`,
    `done%` = `h_done%` + `l_done%` + `gi_proj%`,
    `intime%` = `h_intime%` + `l_intime%` + `i_intime%` + `g_intime%`,
    grade = grade
  ) -> sdd_metrics
rm(sdd_metrics_temp)

# Save these data
fs::dir_create("data")
write$csv(sdd_metrics, "data/sdd_metrics.csv")

# Clean up
rm(sdd_metrics)


# Perceived workload for the learnr tutorials in the three courses -----------
wo <- read(path(data_dir, "wooclap.csv"))

c("A99Wa_perception", "B99Wa_perception", "C99Wb_perception:perception") %>%
  purrr::map_dfr(learnr_feeling, df = wo, label = "Q4") %>.%
  mutate(., course = substr(app, start = 1, stop = 1))  %>.%
  pivot_longer(.,cols = c(mental, physical, time_pressure, performance, effort, frustration),
  names_to = "category", values_to = "grade")  %>.%
  #left_join(., dplyr::distinct(courses, course, name), by = "course") %>.%
  group_by(., user, app, course) %>.%
  #filter(., user != "ECAYEO033") %>.%
  summarise(., rtlx = 10*mean(grade)) -> workload_rtlx

write$csv(workload_rtlx, "data/sdd_rtlx.csv")

rm(wo, workload_rtlx)


# Trials by question in learnr : objective workload ----------------------------

## Using learnr data compute above
learnr %>.%
  filter(., verb %in% c("executed")) %>.%
  group_by(., user, course) %>.%
  summarise(., nexecuted = n(), exercises = length(unique(paste0(app,label)))) %>.%
  mutate(.,
    l_trials_exercices =nexecuted/exercises,
    course = as.factor(course)) -> learnr_red

write$csv(learnr_red, "data/sdd_learnr.csv")

rm(learnr, learnr_red)


# Using several dataset to compute a sumary tab --------------------------------
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
