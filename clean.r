# TODO: Does PHYSCIR include ILLs?

library(dplyr)
library(purrr)
library(readr)
library(stringr)

INDIR  = "data/raw"
OUTDIR = "data/clean"


read_pls = function(path) {
  # Diff types guessed across files for these columns.
  readr::read_csv(
    path,
    col_types = readr::cols(
      LOCALE = readr::col_character(),
      PHONE = readr::col_character(),
      MICROF = readr::col_character(),
      WEB_ADDR = readr::col_character()
    )
  )
}


paths = list.files(
  INDIR,
  pattern = "_ae_|pupld",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

years = stringr::str_extract(paths, "(?i)[0-9]{2}(?=_|[a-b])") %>%
  paste0("20", .)

raw = paths %>%
  purrr::set_names(years) %>%
  # Create Fiscal Year column from `paths` vector names ("dict" "keys").
  purrr::map_dfr(read_pls, .id = "fiscal_year") %>%
  dplyr::mutate(fiscal_year = as.numeric(fiscal_year))

cln = raw %>%
  dplyr::mutate(
    # Non-response values (missing, library closed, N/A, suppressed) are
    # negative for numeric fields, "M" for alphanumeric fields.
    dplyr::across(
      where(is.numeric),
      ~dplyr::if_else(. < 0, NA_real_, .)
    ),
    dplyr::across(
      where(is.character),
      ~dplyr::if_else(. == "M", NA_character_, .)
    ),
    TOTCIR = dplyr::if_else(is.na(TOTCIR), PHYSCIR + ELMATCIR, TOTCIR),
    PHYSCIR = dplyr::if_else(is.na(PHYSCIR), TOTCIR - ELMATCIR, PHYSCIR),
    ELMATCIR = dplyr::if_else(is.na(ELMATCIR), TOTCIR - PHYSCIR, ELMATCIR)
  ) %>%
  # Drop unnecessary fields like the inconsistent LIBID.
  dplyr::select(-LIBID) %>%
  dplyr::arrange(LIBNAME, fiscal_year)

# Tests
stopifnot(
  purrr::map_chr(cln, class) %>%
    unique() %>%
    sort() %>%
    identical(c("character", "numeric"))
)

readr::write_csv(cln, file.path(OUTDIR, "library_fy.csv"))

# Checks

# How many libraries reported any data at any time?
length(unique(cln$FSCSKEY))

# How many libraries did/didn't report data for all years?
cln %>%
  dplyr::group_by(FSCSKEY) %>%
  dplyr::count() %>%
  dplyr::summarise(reported_all_years = n == length(years)) %>%
  dplyr::ungroup() %>%
  dplyr::count(reported_all_years)

# How many libraries reported in each year?
dplyr::count(cln, fiscal_year)
