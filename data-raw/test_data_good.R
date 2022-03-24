## This is a somewhat dirty way to make test data.
## See synthesizEHR for a more comprehensive method.

library(dplyr)
library(purrr)
library(devtools)

## PROVENANCE TABLE ====
.provenance <- tibble(
  filename = paste0("simulated_files/", 1:2, ".xml"),
  file_id = 1:2,
  date_created = Sys.time(),
  version = "v8.3.2",
  date_parsed =  Sys.time(),
  site = LETTERS[1:2],
  theme = "ICU",
  notes = as.character(NA)
)

## EPISODES TABLE ====
df <- tibble(
  admission_date = lubridate::ymd(NULL),
  site = as.character(NULL),
  icu = as.integer(NULL)
  )

for (t in seq.Date(lubridate::ymd("2016-01-01"), lubridate::ymd("2016-02-01"), "day")) {
  for (site in .provenance$site) {
    for (icu in seq_len(2)) {
      new_admissions <- 5
      df <- tibble(
        admission_date = rep(as.Date(t, origin = "1970-01-01"), new_admissions),
        site = site,
        icu = icu) %>%
          bind_rows(df)
    }
  }
}

df <- arrange(df, site, icu, admission_date)

episodes <- df %>%
  mutate(
    episode_id = seq_len(n()),
    nhs_number = inspectEHR::generate_nhs(n()),
    start_time = hms::hms(runif(n(), 0, 24*60*60))) %>%
  mutate(start_date = lubridate::ymd_hms(
    paste(
    format(admission_date), start_time
  ))) %>%
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
    NIHR_HIC_ICU_0017 = rnorm(nrow(dfs), 1.68, 2),
    NIHR_HIC_ICU_0018 = rnorm(nrow(dfs), 75, 10),
    NIHR_HIC_ICU_0033 = sample(
      seq.Date(
        lubridate::ymd("1935-01-01"),
        lubridate::ymd("1990-01-01"), by = 1), nrow(dfs), TRUE),
    NIHR_HIC_ICU_0055 = sample(c("A", "J", "N", "T"), nrow(dfs), TRUE),
    NIHR_HIC_ICU_0058 = sample(c("A", "B", "C", "D", "E", "F", "G", "H", "J",
                                 "K", "L", "M", "N", "P", "R", "S", "Z"), nrow(dfs), TRUE),
    NIHR_HIC_ICU_0060 = sample(c(0L, 1L), nrow(dfs), TRUE, c(0.99, 0.01)),
    NIHR_HIC_ICU_0093 = sample(c("M", "F"), nrow(dfs), TRUE),
    NIHR_HIC_ICU_0399 = inspectEHR::generate_icnarc(nrow(dfs)),
    NIHR_HIC_ICU_0088 = inspectEHR::generate_icnarc(nrow(dfs)),
    NIHR_HIC_ICU_0409 = as.integer(rbeta(nrow(dfs), 2, 8) * 100)
  )

# Add in end points
dfs <- dfs %>%
  mutate(NIHR_HIC_ICU_0412 = NIHR_HIC_ICU_0411 + lubridate::seconds(rgamma(nrow(dfs), 2, 0.5))*86400) %>%
  # 97 -> unit mortality
  mutate(NIHR_HIC_ICU_0097 = sample(c("D", "A"), size = nrow(dfs), prob = c(0.1, 0.9), replace = TRUE)) %>%
  # 95 -> hospital mortality
  rowwise() %>%
  mutate(NIHR_HIC_ICU_0095 = if_else(
    NIHR_HIC_ICU_0097 == "D", "D",
    sample(c("D", "A"), size = 1, prob = c(0.1, 0.9), replace = TRUE)
  )) %>%
  ungroup() %>%
  # Make the datetime of death the same as the discharge
  mutate(
    NIHR_HIC_ICU_0042 = if_else(NIHR_HIC_ICU_0097 == "D",
      as.Date(NIHR_HIC_ICU_0412), as.Date(NA)
    ),
    NIHR_HIC_ICU_0043 = if_else(NIHR_HIC_ICU_0097 == "D",
      hms::as_hms(NIHR_HIC_ICU_0412), hms::as_hms(NA)
    )
  )

## Now lets do some longitudinal data
## Creating the basic time cadance
dfl <- dfs %>%
  select(episode_id, NIHR_HIC_ICU_0411, NIHR_HIC_ICU_0412) %>%
  tidyr::nest(data = c(NIHR_HIC_ICU_0411, NIHR_HIC_ICU_0412)) %>%
  mutate(data = map(data, ~ seq.POSIXt(.x$NIHR_HIC_ICU_0411, .x$NIHR_HIC_ICU_0412, by = "hour"))) %>%
  tidyr::unnest(data) %>%
  dplyr::rename(datetime = data)

dfl <- dfl %>%
  dplyr::group_by(episode_id) %>%
  dplyr::arrange(episode_id, datetime) %>%
  mutate(
    # HR
    NIHR_HIC_ICU_0108 = as.integer(rnorm(n = dplyr::n(), mean = 90, sd = 10)),
    # CVP
    NIHR_HIC_ICU_0116 = round(rnorm(n = dplyr::n(), mean = 5, sd = 5), digits = 2),
    # Airway
    NIHR_HIC_ICU_0126 = sample(
      c("E", "N", "T", as.character(NA)),
      size = dplyr::n(), replace = TRUE, prob = c(0.1, 0.1, 0.1, 0.7)
    )
  )

dfs <- dfs %>%
  dplyr::group_by(episode_id)

## I'm sure there is a better way to do this bit
str_var <- select_if(dfs, is.character) %>%
  tidyr::gather(key = "code_name", value = "string", -episode_id) %>%
  na.omit()

int_var <- select_if(dfs, is.integer) %>%
  tidyr::gather(key = "code_name", value = "integer", -episode_id) %>%
  na.omit()

# Custom select to ensure we are only getting the stuff we want
dbl_var <- select_if(dfs, function(x) {
  is.double(x) && !lubridate::is.POSIXct(x) && !hms::is_hms(x) && !lubridate::is.Date(x)
}) %>%
  tidyr::gather(key = "code_name", value = "real", -episode_id) %>%
  na.omit()

date_var <- select_if(dfs, lubridate::is.Date) %>%
  tidyr::gather(key = "code_name", value = "date", -episode_id) %>%
  na.omit()

dttm_var <- select_if(dfs, lubridate::is.POSIXct) %>%
  tidyr::gather(key = "code_name", value = "datetime", -episode_id) %>%
  na.omit()

time_var <- select_if(dfs, hms::is.hms) %>%
  tidyr::gather(key = "code_name", value = "time", -episode_id) %>%
  na.omit()

dbs <- bind_rows(str_var, int_var, dbl_var, date_var, dttm_var, time_var)

dfl <- dfl %>% dplyr::group_by(episode_id, datetime)
## Group so episode_id and datetime are brought along for the ride

str_var <- select_if(dfl, is.character) %>%
  tidyr::gather(key = "code_name", value = "string", -episode_id, -datetime) %>%
  na.omit()

int_var <- select_if(dfl, is.integer) %>%
  tidyr::gather(key = "code_name", value = "integer", -episode_id, -datetime) %>%
  na.omit()

dbl_var <- select_if(dfl, function(x) {
  is.double(x) && !lubridate::is.POSIXct(x) && !hms::is_hms(x) && !lubridate::is.Date(x)
}) %>%
  tidyr::gather(key = "code_name", value = "real", -episode_id, -datetime) %>%
  na.omit() %>%
  # Add in meta-data for CVP
  mutate(integer = sample(c(1:5, NA_integer_), n(), replace = TRUE))

dbl <- bind_rows(str_var, int_var, dbl_var)
dbf <- bind_rows(dbs, dbl)

# Final modifications since we don't carry metadata at present.
.events <- dbf %>%
  dplyr::ungroup() %>%
  mutate(event_id = seq_len(n())) %>%
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
.variables <- readr::read_csv("./data-raw/variables.csv")

.episodes <- episodes %>%
  mutate(provenance = if_else(site == "A", 1, 2)) %>%
  select(-site, -icu)

use_data(.episodes, .events, .provenance, .variables,
         internal = TRUE, overwrite = TRUE, compress = "xz")
