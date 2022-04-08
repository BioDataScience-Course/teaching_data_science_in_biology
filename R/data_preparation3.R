# Data preparation 3 for teaching data science...
# Data from 2018-2019 & 2019-2020
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

acad_years <- c("2018-2019", "2019-2020")
sdd_folders <- glue("sdd_{acad_years}")
data_dirs <- path(root, sdd_folders)


# Exam vs project ---------------------------------------------------------

users_files <- path(data_dirs, "users.csv")
users18 <- read(users_files[1])
users19 <- read(users_files[2])
assessments_files <- path(data_dirs, "assessment.csv")
assessments18 <- read(assessments_files[1])
assessments19 <- read(assessments_files[2])
exam19 <- read(path(data_dirs[2], "exam.csv"))
courses_files <- path(data_dirs, "courses.csv")
courses18 <- read(courses_files[1])
courses19 <- read(courses_files[2])

assessments18 %>.%
  mutate(., result = (biometry + coral_growth) / 2, acad_year = "2018-2019") ->
  assess_result18

left_join(rename(assess_result18, icourse = course), courses18) %>.%
  filter(., user %in% users18$user[users18$institution == "UMONS" &
    users18$term == "Q1" & users18$state == "regular"]) ->
  q1_18_regular

assessments19 %>.%
  group_by(., course, evaluation, github_project, project, user) %>.%
  summarise(., result = round(sum(score * weight), 4)) %>.%
  filter(., evaluation == "Q1") %>.%
  left_join(exam19, .) %>.%
  replace_na(., list(result = 0)) %>.%
  mutate(., acad_year = "2019-2020") ->
  assess_result19

left_join(rename(assess_result19, icourse = course), courses19) %>.%
  filter(., user %in% users19$user[users19$institution == "UMONS" &
    users19$term == "Q1" & users19$state == "regular"]) ->
  q1_19_regular

sdd_eval <- bind_rows(
  select(q1_18_regular, user, acad_year, course, result, exam),
  select(q1_19_regular, user, acad_year, course, result, exam)
)

write$csv(sdd_eval, "data/sdd_eval.csv")

rm(assess_result18, assess_result19, assessments18, assessments19, exam19)
rm(courses18, courses19, users18, users19, q1_18_regular, q1_19_regular)
rm(sdd_eval)
