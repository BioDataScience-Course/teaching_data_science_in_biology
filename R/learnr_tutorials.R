# learnr tutorials #

# packages and functions
SciViews::R
library(lubridate)
source("R/functions.R")
glue <- glue::glue
path <- fs::path
set_attr <- data.table::setattr

# Parameterization -------------------------------------------------------------
acad_year <- "2020-2021"
sdd_folder <- glue("sdd_{acad_year}")
data_dir <- pcloud(sdd_folder, "data")
courses <- c("A", "B", "C") # Don't include BioDataScience IV et V
institutions <- "UMONS" # Dont include Campus UCharleroi
ignore_modules <- c("A00", "A99", "B00", "B99", "C00", "C99", "D99")
users_state <- c("regular", "repeater") # Exclude staff, mobility, bridge & 0
# TODO: should we merge users data from all terms instead?
users_file <- pcloud_crypto("sdd_users", glue("users_{acad_year}_Q1.csv"))


# WOOCLAP dataset : Perceived cognitive workload in learnr tutorials -----------
courses <- read(pcloud("sdd_2020-2021/data/courses.csv"))
wo <- read(pcloud("sdd_2020-2021/data/wooclap.csv"))

c("A99Wa_perception", "B99Wa_perception", "C99Wb_perception:perception") %>%
  purrr::map_dfr(learnr_feeling, df = wo, label = "Q4") %>.%
  mutate(., course = substr(app, start = 1, stop = 1))  %>.%
  pivot_longer(.,cols = c(mental, physical, time_pressure, performance, effort, frustration),
  names_to = "category", values_to = "grade")  %>.%
  left_join(., dplyr::distinct(courses, course,name), by = "course") %>.%
  group_by(., user, app, course) %>.%
  #filter(., user != "ECAYEO033") %>.%
  summarise(., rtlx = 10*mean(grade)) -> workload_rtlx

# set.seed(222)
# chart(workload_rtlx, rtlx ~ course) +
#   geom_boxplot(fill = "lightgray") +
#   geom_jitter(alpha = 0.5, width = 0.1) +
#   labs(y = "RTLX", x = "Course") +
#   stat_summary(fun.y = "mean", color = "black", size = 1) +
#   stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5) +
#   labs( y = "Raw Task Load indeX (2020-2021)")

# Objective workload with trails by learnr exercices ---------------------------
## users data : Extract users data ------------------------------------------------------
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

# Get a list of users in one of the three UMONS courses SDD I, II & III
# Only Q1 2020-2021 users (same ones in Q2)
# TODO: combine users from all terms here for better generalization!?
read(users_file) %>.%
  set_attr(., "spec", NULL) %>.% # Eliminate extra info from the readr function
  set_attr(., "comment", NULL) %>.% # Idem
  # Restrict courses
  filter(., institution %in% institutions) %>.%
  filter(., course %in% courses) %>.%
  # Eliminate staff, non enrolled (0), mobility, bridge which are special cases
  filter(., state %in% users_state) %>.%
  left_join(., courses_modules) %>.%
  select(., user, institution, icourse, icflag, course,
    enrolled, state, .n_modules = n_modules) ->
  users

# LEARNR -----------------------------------------------------------------------
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
  left_join(., select(modules_timings, modules, , interval_prepa)) %>.%
  # The following code also works with multiple intervals, but it is much
  # slower than the direct solution when there is always a single interval
  # as it is the case here
  #mutate(., .l_intime = map2_lgl(date, interval_prepa,
  #  function(x, y) any(x %within% y))) %>.%
  mutate(., .l_intime = date %within% interval_prepa) %>.%
  # Only keep events relative to the first sequence
  filter(., date <= date_max) ->
  learnr


learnr %>.%
  filter(., verb %in% c("executed")) %>.%
  group_by(., user, course) %>.%
  summarise(., nexecuted = n(), exercises = length(unique(paste0(app,label)))) %>.%
  mutate(., l_trials_exercices =nexecuted/exercises) -> learnr_red

chart(learnr_red, l_trials_exercices ~ course) +
  geom_boxplot() +
  stat_summary(fun.data = function(x) c(y = max(x) * 1.1, label = length(x)), geom = "text", hjust = 0.5)




