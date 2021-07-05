# Transition between face-to-face and distance learning
SciViews::R
library(lubridate)
source("R/functions.R")

set_attr <- data.table::setattr

### Parameters ###
time_interval <- interval(as.time("2020/03/05"), as.time("2021/05/15"))

### Git log ###
c(pcloud("sdd_2019-2020/data/git_log.csv"),
  pcloud("sdd_2020-2021/data/git_log.csv")) %>.%
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
  # TODO: simialr corrections are required for urchin, human and zooplankton in
  # 2019-2020 !!!
  # Keep only logs from the right institution and courses
  filter(., institution %in% "UMONS") %>.%
  filter(., course %in% c("A", "B")) %>.%
  # The "from" column specifies if the commit is by staff ("staff") or not (NA)
  # Keep only info about commits by students
  filter(., is.na(from)) %>.%
  # We only consider text change => bin_before must be NA
  filter(., is.na(bin_before)) %>.%
  # We consider only (R) Markdown and R script files
  filter(., extension %in% c(".Rmd", ".rmd")) %>.% # c(".Rmd", ".R", ".r", ".md", ".rmd")
  select(., -from, -bin_before, -bin_after) %>.%
  filter(., date %within% time_interval) %>.%
  # Aggegations
  group_by(., user, date, commit, app, type, modules, course) %>.%
  summarise(., add = sum(add, na.rm = TRUE), change = sum(change, na.rm = TRUE)) %>.%
  ungroup(.) %>.%
  mutate(.,
    week = acad_w(date, label = TRUE),
    period = acad_2w(date, label = TRUE)) ->
  log

### Support data #####
c(pcloud("sdd_2019-2020/data/support.csv"),
  pcloud("sdd_2020-2021/data/support.csv")) %>.%
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
    icourse == "S-BIOG-006,S-BIOG-015" ~ "A+B", # Bridge students take two courses
    icourse == "S-BIOG-027,S-BIOG-061" ~ "A+B" # Idem for Q2
  )) %>.%
  select(., -quarter) %>.%
  # Restrict courses & institutions
  filter(., institution %in% "UMONS") %>.%
  filter(., course %in% c("A", "B")) %>.%
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

###Lessons preparation des données #####
c(pcloud("sdd_2019-2020/data/lessons.csv"),
  pcloud("sdd_2020-2021/data/lessons.csv")) %>.%
  purrr::map_dfr(., read) %>.%
  set_attr(., "spec", NULL) %>.% # Eliminate extra info from the readr function
  set_attr(., "comment", NULL) %>.%
  filter(., course != "C") %>.%
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
  summarise(., .modules = n()) -> lessons_by_period

# combine dataset --------------------------------------------------------------
log %>.%
  group_by(., user, course, period) %>.%
  summarise(., add = sum(add), change = sum(change)) %>.%
  ungroup(.) %>.%
  left_join(., lessons_by_period) %>.%
  replace_na(., list(.modules = 0)) %>.%
  mutate(., .modules = factor(.modules)) ->
  log_period

write$csv(log_period, "data/log_period.csv")

support %>.%
  group_by(., user, course, period) %>.%
  summarise(., messages = n()) %>.%
  ungroup(.) %>.%
  left_join(., lessons_by_period) %>.%
  replace_na(., list(.modules = 0)) %>.%
   mutate(., .modules = factor(.modules)) ->
  support_period

write$csv(support_period, "data/support_period.csv")

# inner_join(log_period, support_period) %>.%
#   # Calculate ration support/production as messages/add
#   mutate(.,
#     add_by_message = add/messages,
#     change_by_message = change/messages) ->
#   supp_prod_period
#
# chart(data = supp_prod_period, change_by_message ~ as.factor(period) %fill=% .modules ) +
#   #geom_vline(xintercept = c("Y1P11", "Y2P03"), alpha = 0.3, linetype = "twodash") +
#   geom_boxplot() +
#   theme(axis.text.x = element_text(angle = 90)) +
#   scale_y_log10(breaks = c(0.1, 1, 10, 100, 1000), labels = c(0.1, 1, 10, 100, 1000)) +
#   stat_summary(fun.data = function(x) c(y = max(x) + 0.5, label = length(x)), geom = "text", hjust = 0.5) +
#   scale_fill_grey(start = 0.8,
#   end = 0) +
#   labs(
#     y = "Contributions/question",
#     x = "Period",
#     fill = "Number of modules") -> pchangemessages
# pchangemessages
#
# log_period %>.%
#   filter(., !period %in%c("Y1P15", "Y1P16", "Y1P17")) %>.%
#   left_join(., lessons_by_period) %>.%
#   replace_na(., list(.modules = 0)) %>.%
#   mutate(., .modules = factor(.modules)) %>.%
#   group_by(., period, .modules) %>.%
#   summarise(.,
#     change = sum(change),
#     nus = length(unique(user)),
#     change_user = change/nus) %>.%
#   chart(data = ., change_user ~ as.factor(period) %fill=% .modules ) +
#   geom_col() +
#   theme(axis.text.x = element_text(angle = 90)) +
#   scale_fill_grey(start = 0.8,
#   end = 0) +
#   labs( y = "Contributions/student", fill = "Number of modules") -> pchangestudent2
# pchangestudent2
#
# p. <- pchangestudent2 +
#   labs(y = "Contributions/user") +
#   scale_y_continuous(
#     breaks = c(0, 250,500, 750),
#     labels = c(0, 0250, 500, "  750")) +
#   theme(
#     axis.ticks.x = element_blank(),
#     axis.text.x = element_blank(),
#     axis.title.x=element_blank())
#
# combine_charts(list(p., pchangemessages),
#   nrow = 2,
#   common.legend = TRUE, legend = "bottom",
#   heights = c(0.9, 1),
#   labels = "auto",
#   font.label = list(size = 12))
