## PROVENANCE TABLE ====
provenance <- tibble(
  filename = paste0("simulated_files/", 1:2, ".xml"),
  file_id = 1:2,
  date_created = Sys.time(),
  version = "v8.3.2",
  date_parsed =  Sys.time(),
  site = LETTERS[1:2],
  theme = "ICU",
  notes = as.character(NA)
)

icus <- tibble(
  site = rep(LETTERS[1:2], each = 2),
  icu = c("A-1", "A-2", "B-1", "B-2")
)

## EPISODES TABLE ====
df <- tibble(
  admission_date = lubridate::ymd(NULL),
  site = as.character(NULL),
  icu = as.character(NULL)
  )

for (t in seq.Date(lubridate::ymd("2016-01-01"), lubridate::ymd("2016-01-07"), "day")) {
  for (site in provenance$site) {
    for (icu in 1:2) {
      new_admissions <- 5
      df <- tibble(
        admission_date = rep(as.Date(t, origin = "1970-01-01"), new_admissions),
        site = site,
        icu = paste0(site, "-", icu)) %>%
          bind_rows(df)
    }
  }
}

episodes <- df %>%
  mutate(
    episode_id = 1:dplyr::n(),
    nhs_number = inspectEHR::generate_nhs(nrow(df)),
    start_time = hms::as_hms("06:00:00")) %>%
  mutate(start_date = lubridate::ymd_hms(
    paste(
      format(admission_date), start_time))) %>%
  select(-admission_date, -start_time)

# Set up some of the most important base features
dfs <- episodes %>%
  dplyr::rename(
    NIHR_HIC_ICU_0073 = nhs_number,
    NIHR_HIC_ICU_0411 = start_date,
    NIHR_HIC_ICU_0002 = icu
  ) %>%
  select(-site)

dfs <- dfs %>%
  mutate(
    # Steal the NHS number for PAS number, both are just unique IDs anyway
    NIHR_HIC_ICU_0001 = NIHR_HIC_ICU_0073,
    NIHR_HIC_ICU_0005 = paste0("CMP-", episode_id),
    NIHR_HIC_ICU_0017 = 1.7,
    NIHR_HIC_ICU_0018 = 75,
    NIHR_HIC_ICU_0033 = lubridate::ymd("1935-01-01"),
    NIHR_HIC_ICU_0055 = "A",
    NIHR_HIC_ICU_0058 = "A",
    NIHR_HIC_ICU_0060 = c(rep(0L, 100), rep(1L, 40)),
    NIHR_HIC_ICU_0093 = c(rep("M", 100), rep("F", 40)),
    NIHR_HIC_ICU_0399 = inspectEHR::generate_icnarc(nrow(dfs)),
    NIHR_HIC_ICU_0088 = inspectEHR::generate_icnarc(nrow(dfs)),
    NIHR_HIC_ICU_0409 = as.integer(rbeta(nrow(dfs), 2, 8) * 100)
  )

# Add in end points
dfs <- dfs %>%
  mutate(NIHR_HIC_ICU_0412 = NIHR_HIC_ICU_0411 + lubridate::hours(10)) %>%
  mutate(NIHR_HIC_ICU_0097 = c(rep("A", 100), rep("D", 40))) %>%
  mutate(NIHR_HIC_ICU_0095 = c(rep("A", 120), rep("D", 20))) %>%
  # Make the datetime of death the same as the discharge
  mutate(
    NIHR_HIC_ICU_0042 = if_else(
      NIHR_HIC_ICU_0097 == "D",
      as.Date(NIHR_HIC_ICU_0412),
      as.Date(NA)
    ),
    NIHR_HIC_ICU_0043 = if_else(
      NIHR_HIC_ICU_0097 == "D",
      hms::as_hms(NIHR_HIC_ICU_0412),
      hms::as_hms(lubridate::ymd_hms(NA))
    )
  )

## Now lets do some longitudinal data
dfl <- dfs %>%
  select(episode_id, NIHR_HIC_ICU_0411, NIHR_HIC_ICU_0412) %>%
  tidyr::nest(data = c(NIHR_HIC_ICU_0411, NIHR_HIC_ICU_0412)) %>%
  mutate(data = map(data,
    ~ seq.POSIXt(.x$NIHR_HIC_ICU_0411, .x$NIHR_HIC_ICU_0412, by = "hour"))) %>%
  tidyr::unnest(data) %>%
  dplyr::rename(datetime = data)

dfl <- dfl %>%
  dplyr::group_by(episode_id) %>%
  dplyr::arrange(episode_id, datetime) %>%
  mutate(
    NIHR_HIC_ICU_0108 = as.integer(60),
    NIHR_HIC_ICU_0116 = 5,
    NIHR_HIC_ICU_0126 = c(
      rep(c("E", "N", "T", as.character(NA)), each = 2),
      as.character(NA), as.character(NA), as.character(NA))
    )

dfs <- dfs %>%
  dplyr::group_by(episode_id)

## I'm sure there is a better way to do this bit
str_var <- select_if(dfs, is.character) %>%
  tidyr::pivot_longer(cols = -episode_id, names_to = "code_name", values_to = "string") %>%
  na.omit()

int_var <- select_if(dfs, is.integer) %>%
  tidyr::pivot_longer(cols = -episode_id, names_to = "code_name", values_to = "integer") %>%
  na.omit()

# Custom select to ensure we are only getting the stuff we want
dbl_var <- select_if(dfs, function(x) {
  is.double(x) & !lubridate::is.POSIXct(x) &
  !hms::is_hms(x) & !lubridate::is.Date(x)}) %>%
  tidyr::pivot_longer(cols = -episode_id, names_to = "code_name", values_to = "real") %>%
  na.omit()

date_var <- select_if(dfs, lubridate::is.Date) %>%
  tidyr::pivot_longer(cols = -episode_id, names_to = "code_name", values_to = "date") %>%
  na.omit()

dttm_var <- select_if(dfs, lubridate::is.POSIXct) %>%
  tidyr::pivot_longer(cols = -episode_id, names_to = "code_name", values_to = "datetime") %>%
  na.omit()

time_var <- select_if(dfs, hms::is.hms) %>%
  tidyr::pivot_longer(cols = -episode_id, names_to = "code_name", values_to = "time") %>%
  na.omit()

dbs <- bind_rows(str_var, int_var, dbl_var, date_var, dttm_var, time_var)

## Group so episode_id and datetime are brought along for the ride
dfl <- dfl %>% dplyr::group_by(episode_id, datetime)

str_var <- select_if(dfl, is.character) %>%
  tidyr::pivot_longer(cols = c(-episode_id, -datetime),
                      names_to = "code_name", values_to = "string") %>%
  na.omit()

int_var <- select_if(dfl, is.integer) %>%
  tidyr::pivot_longer(cols = c(-episode_id, -datetime),
                      names_to = "code_name", values_to = "integer") %>%
  na.omit()

dbl_var <- select_if(dfl, function(x) {
  is.double(x) & !lubridate::is.POSIXct(x) &
    !hms::is_hms(x) & !lubridate::is.Date(x)}) %>%
  tidyr::pivot_longer(cols = c(-episode_id, -datetime),
                      names_to = "code_name", values_to = "real") %>%
  na.omit()

dbl <- bind_rows(str_var, int_var, dbl_var)
dbf <- bind_rows(dbs, dbl)

## Add some metadata

dbf[dbf$code_name == "NIHR_HIC_ICU_0116", c("integer"), drop = TRUE] <-
  rep(c(1L, 3L), 770)

# Final modifications since we don't carry further metadata at present.
events <- dbf %>%
  dplyr::ungroup() %>%
  mutate(event_id = 1:dplyr::n()) %>%
  tibble::add_column(
    string2 = as.character(NA),
    string3 = as.character(NA),
    integer2 = as.integer(NA)
  ) %>%
  select(
    code_name, string, string2, string3, datetime, date, time, real,
    integer, integer2, episode_id, event_id
  )

## VARIABLES (metadata) TABLE ====
# Easiest just to pull this from the database directly
load("./data-raw/metadata_tbl.RData")
variables <- variables_tbl

episodes <- episodes %>%
  mutate(provenance = if_else(site == "A", 1, 2)) %>%
  select(-site, -icu)

use_data(episodes, events, provenance, variables, internal = TRUE, overwrite = TRUE)

## Will write this data out, for the purposes of vignettes and demo

cchic_demo <- DBI::dbConnect(RSQLite::SQLite(), "./data-raw/synthetic_db.sqlite3")

## Variables ====

DBI::dbWriteTable(
  conn = cchic_demo,
  name = "variables",
  value = variables, append = TRUE)

## Provenance

DBI::dbWriteTable(
  conn = cchic_demo,
  name = "provenance",
  value = provenance, append = TRUE)

## Episodes ====

DBI::dbWriteTable(
  conn = cchic_demo,
  name = "episodes",
  value = episodes, append = TRUE)

## Events ====

DBI::dbWriteTable(
  conn = cchic_demo,
  name = "events",
  value = events, append = TRUE)

DBI::dbDisconnect(cchic_demo)
