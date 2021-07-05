SciViews::R

glue <- glue::glue
path <- fs::path
set_attr <- data.table::setattr

library(lubridate)

as.time <- as.POSIXct

acad_calendar <- tibble::tribble(
  ~year, ~institution,    ~Q1start,       ~Q1exam,    ~Q2start,      ~easter,      ~Q2exam,    ~Q3start,        ~Q3exam,
  2018,       "UMONS", "2018/09/17", "2018/12/24", "2019/02/04", "2019/04/08", "2019/05/27", "2019/07/01", "2019/08/12", # Fin Q2 = 05/19, mais que 13 semaines sinon
  2019,       "UMONS", "2019/09/16", "2019/12/23", "2020/02/03", "2020/04/06", "2020/05/25", "2020/07/13", "2020/08/10",
  2020,       "UMONS", "2020/09/14", "2020/12/21", "2021/02/01", "2021/04/05", "2021/05/24", "2021/07/05", "2021/08/09",
  2021,       "UMONS", "2021/09/20", "2021/12/27", "2022/02/07", "2022/04/04", "2022/05/30", "2022/07/04", "2022/08/15",
  #2021,         "ULB", "2021/09/13", "2021/12/27", "2022/02/07", "2022/04/04", "2022/05/23", "2022/07/04", "2022/08/15"
)

# Verification: all dates must by Mondays
wd <- function(x) lubridate::wday(x, label = TRUE)
acad_calendar %>.%
  mutate(.,
    Q1start = wd(Q1start), Q1exam = wd(Q1exam),
    Q2start = wd(Q2start), easter = wd(easter), Q2exam = wd(Q2exam),
    Q3start = wd(Q3start), Q3exam = wd(Q3exam))
# Get isoweek instead
acad_calendar %>.%
  mutate(.,
    Q1start = isoweek(Q1start), Q1exam = isoweek(Q1exam),
    Q2start = isoweek(Q2start), easter = isoweek(easter), Q2exam = isoweek(Q2exam),
    Q3start = isoweek(Q3start), Q3exam = isoweek(Q3exam)) -> acal
# Number of weeks for Q1 and Q2 (must be 14 everywhere)
transmute(acal, year = year, Q1 = Q1exam - Q1start, Q2 = Q2exam - Q2start - 2L)
# If everything is OK, we save this as an option
options(academic_calendar = acal)

# Get academic year (for 2020-2021, it is 2020 if format is "Y",
# or 20 if format is "y")
acad_y <- function(x, format = "Y", calendar = getOption("academic_calendar")) {
  # Academic year usually start on isoweek 38
  shift <- 37L * 7L * 24L * 60L * 60L # in sec (POSIXct)
  year <- lubridate::isoyear(x - shift)
  # Should we perform some corrections (using info in calendar)?
  # TODO...not very easy the way I coded this!
  # If format == "y", only keep two last digits
  switch(format,
    Y = year,
    y = as.integer(substring(as.character(year), 3L)),
    stop("Unknown format (must be 'Y' or 'y')")
  )
}

# Get academic quarters, given dates 'x' in POSIXct
# if first.year is given, we increment periods from one year to the other,
# starting with this year, e.g.: first.year = 2019 means we start at 1 for
# Q1 2019-2020, then Q1 2020-2021 is 4, Q1 2021-2022 is 7, etc.
# Q1 starts at isoweek() == 38, Q2: isoweek() == 6, Q3: isoweek() == 22
# (but there may be exceptions and academic calendar is not the same between
# UMONS and ULB for 2021-2022, for instance)
acad_q <- function(x, label = FALSE, first.year = NULL,
calendar = getOption("academic_calendar")) {
  if (!is.POSIXct(x))
    stop("'x' must be a 'POSIXct' object")
  if (!missing(first.year) && !is.numeric(first.year))
    stop("'first.year' must be a number, e.g., 2000")
  if (!is.null(first.year)) {
    l <- length(first.year)
    if (!l)
      stop("'first.year' cannot be a length zéro vector")
    if (l > 1)
      warning("only first item of 'first.year' is used")
    first.year <- as.integer(floor(first.year))[1]
  }

  year <- acad_y(x)
  year_chr <- as.character(year)
  cal_year_chr <- as.character(calendar$year)
  q1 <- structure(calendar$Q1start, names = cal_year_chr)[year_chr]
  q1[is.na(q1)] <- 38L # Default value
  q2 <- structure(calendar$Q2start, names = cal_year_chr)[year_chr]
  q2[is.na(q2)] <- 6L # Default value
  q3 <- structure(calendar$Q3start, names = cal_year_chr)[year_chr]
  q3[is.na(q3)] <- 27L # Default value
  isow <- lubridate::isoweek(x)
  quart <- rep(1L, length.out = length(isow))
  quart[isow >= q2]  <- 2L
  quart[isow >= q3] <- 3L
  quart[isow >= q1] <- 1L

  # Note, isoyear() is shifted by 37*7 days, since acad year start at isoweek 38
  shift <- 37L * 7L * 24L * 60L * 60L # must be in seconds for POSIXct
  if (isTRUE(label)) {# We return labels
    quart <- c("Q1", "Q2", "Q3")[quart]
    years <-
    if (!is.null(first.year)) {
      quart <- paste0("Y", acad_y(x) - first.year + 1L, quart)
      quart[]
    }
  } else {# We return numbers
    if (!is.null(first.year)) {
      quart <- quart + (acad_y(x) - first.year) * 3L
      quart <- as.integer(quart)
    }
  }
  quart
}
# "academic weeks" are: 1-14 for the 14 weeks in Q1 + 15-16 for exam period
# 17-30 for the 14 weeks Q2, but with 2 weeks for easter "absorbed" + 31-32 for
# exam Q2, variable number of weeks for Q3 and exams Q3 (to be worked out later)
acad_w <- function(x, label = FALSE, first.year = NULL, template = FALSE,
format = "Y%dW%02d", calendar = getOption("academic_calendar")) {
  if (!is.POSIXct(x))
    stop("'x' must be a 'POSIXct' object")
  if (!missing(first.year) && !is.numeric(first.year))
    stop("'first.year' must be a number, e.g., 2000")
  if (!is.null(first.year)) {
    l <- length(first.year)
    if (!l)
      stop("'first.year' cannot be a length zéro vector")
    if (l > 1)
      warning("only first item of 'first.year' is used")
    first.year <- as.integer(floor(first.year))[1]
  }

  year <- acad_y(x)
  y_span <- min(year, na.rm = TRUE):max(year, na.rm = TRUE) # Acad. years in x
  # Make a list of names with "year isoweek" like e.g., "2020 25"
  isoyw <- function(w, y = y_span)
    expand_grid(y = y, w = w) %>.% paste(.$y, .$w)
  isoyw2 <- function(from, to, y = y_span) {
    nms <- unlist(map2(from, to, seq))
    paste(substring(names(nms), 1L, 4L), nms)
  }
  # Get academic calendar data
  y_span_chr <- as.character(y_span)
  cal_year_chr <- as.character(calendar$year)
  # Construct vectors of transition weeks for each year from calendar data
  transitions <- function(item, default, cal_y = cal_year_chr, y = y_span_chr) {
    res <- structure(item, names = cal_y)[y]
    res[is.na(res)] <- default
    names(res) <- y
    res
  }
  q1     <- transitions(calendar$Q1start, default = 38L)
  q1e    <- transitions(calendar$Q1exam,  default = 52L)
  q2     <- transitions(calendar$Q2start, default =  6L)
  easter <- transitions(calendar$easter,  default = 14L)
  q2e    <- transitions(calendar$Q2exam,  default = 22L)
  q3     <- transitions(calendar$Q3start, default = 27L)
  q3e    <- transitions(calendar$Q3exam,  default = 33L)

  # Note: sometimes, there are 53 isoweeks in a year!
  conv <- rep(rep(34L, 53L), length(y_span)) # Conversion isoweek -> acadweek
  names(conv) <- isoyw(1:53)

  # Now we replace initial isoweeks by acadweeks in conv
  # - acadweeks 15 is flexible and 16 is 3 weeks before Q2start
  # Fill in acadweek 15 on a larger zone, then superpose acadweek 16
  # Q1 exam1 (15)
  conv[isoyw(c(50:53, 1:4))] <- 15L
  # # Q1 exam2 (16) on three weeks
  conv[isoyw2(q2 - 3, q2 - 1)] <- 16L
  # Q1 weeks (1 -> 14)
  conv[isoyw2(q1, q1 + 13)] <- 1:14
  # Q2 weeks (17 -> 30), but doubling weeks around easter
  e_weeks <- map2(easter - q2, easter - q2 + 1, seq)
  conv[isoyw2(q2, q2 + 15)] <- as.integer(unlist(
    lapply(e_weeks, function(x) sort(c(x, 1:14) + 16))))
  # Q2 exam1 (31)
  conv[isoyw2(q2e, q3 - 4)] <- 31L
  # Q2 exam2 (32) on three weeks
  conv[isoyw2(q3 - 3, q3 - 1)] <- 32L
  # Q3 1 (33)
  conv[isoyw2(q3, q3e - 1)] <- 33L
  # Q3 2 (34) is the rest

  # If we just one the template, stop here
  if (isTRUE(template))
    return(conv)

  # For our dates, we construct a vector with acad_year & isoweek
  key <- paste(year, isoweek(x))
  # We use as index to get the corresponding acadweek
  res <- as.integer(conv[key])

  if (isTRUE(label)) {# We return labels
    if (is.null(first.year))
      first.year <- min(year)
    res <- sprintf(format, year - first.year + 1, res)
  }

  res
}

# Separate academic year into periods of 14 days or more
acad_2w <- function(x, label = FALSE, first.year = NULL, template = FALSE,
format = "Y%dP%02d", calendar = getOption("academic_calendar")) {
  res <- acad_w(x, label = FALSE, template = template, calendar = calendar)
  # We merge into periods of 2 weeks
  res <- (res + 1) %/% 2

  if (isTRUE(label)) {# We return labels
    year <- acad_y(x)
    if (is.null(first.year))
      first.year <- min(year)
    res <- sprintf(format, year - first.year + 1, res)
  }

  res
}

acad1week <- function(x, first.year = 2019) {
  if (!is.POSIXct(x)) stop("'x' must be a 'POSIXct' object")
  (lubridate::isoweek(x) +
    (lubridate::year(x - (3*24*60*60)) - first.year) * 53) - 37
}
acad2week <- function(x) {
  if (!is.POSIXct(x)) stop("'x' must be a 'POSIXct' object")
  (lubridate::isoweek(x) + (lubridate::year(x - (3*24*60*60)) == 2021) * 53) %/% 2
}
acad4week <- function(x) {
  if (!is.POSIXct(x)) stop("'x' must be a 'POSIXct' object")
  (lubridate::isoweek(x) + (lubridate::year(x - (3*24*60*60)) == 2021) * 53) %/% 4
}


## RTLX NASA - TLX -------------------------------------------------------------

learnr_feeling <- function(df, apps, labels) {
  res <- dplyr::filter(df, app == apps & label == labels)

  c("(\\d):(Quelle a été l'importance de l'activité mentale et intellectuelle requise)",
    "(\\d):(Quelle a été l'importance de l'activité physique requise)",
    "(\\d):(Quelle a été l'importance de la pression temporelle causée par la rapidité nécessitée pour l'accomplissement de la tâche)",
    "(\\d):Quelle réussite pensez-vous avoir eu dans l'accomplissement de la tâche",
    "(\\d):Quel degré d'effort avez-vous dû fournir pour exécuter la tâche demandée",
    "(\\d):Pendant l'exécution du travail vous êtes-vous senti découragé"
    ) %>.%
  purrr::map_dfc(., function(str, pattern) {stringr::str_match(str, pattern = pattern)[,2]}, str = res$value) -> vec

  names(vec) <- c("mental", "physical", "time_pressure", "performance", "effort", "frustration")

  vec %>.%
    map_dfc(.,as.numeric) -> vec1

  vec1$app <- apps
  vec1$label <- labels
  vec1$user <- res$user
  return(vec1)
}
