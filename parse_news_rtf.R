# parse_news_rtf.R
# Parses Factiva RTF exports from TV news transcripts into a structured CSV
# with phrase frequency counts for abortion/reproductive rights research.
#
# Usage: Rscript parse_news_rtf.R
# Output: news_stories.csv in the same directory as the script
#
# Required packages: stringr, dplyr, readr
# Install with: install.packages(c("stringr", "dplyr", "readr"))

library(stringr)
library(dplyr)
library(readr)

# R <4.4 doesn't always export %||%; define it here to be safe
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && nchar(a) > 0) a else b

# ---------------------------------------------------------------------------
# CONFIGURATION: edit these to match your files and research needs
# ---------------------------------------------------------------------------

# Directory containing your RTF files (use absolute path or "." for current dir)
RTF_DIR <- "."

# Output CSV path
OUTPUT_CSV <- "news_stories.csv"

# Phrases to count in each story (case-insensitive).
# Each entry is: label = c(variant1, variant2, ...)
# Variants are summed into a single count column.
PHRASE_LIST <- list(
  pro_life               = c("pro-life", "pro life"),
  pro_choice             = c("pro-choice", "pro choice"),
  abortion_rights        = c("abortion rights"),
  abortion_access        = c("abortion access"),
  abortion_ban           = c("abortion ban"),
  abortion_pill          = c("abortion pill"),
  abortion_care          = c("abortion care"),
  reproductive_rights    = c("reproductive rights"),
  reproductive_health    = c("reproductive health"),
  reproductive_freedom   = c("reproductive freedom"),
  reproductive_choice    = c("reproductive choice"),
  late_term_abortion     = c("late-term abortion", "late term abortion"),
  partial_birth_abortion = c("partial-birth abortion", "partial birth abortion"),
  fetal_heartbeat        = c("fetal heartbeat"),
  heartbeat_bill         = c("heartbeat bill"),
  gestational_age        = c("gestational age"),
  viability              = c("viability"),
  womens_health          = c("women's health", "womens health"),
  family_planning        = c("family planning"),
  planned_parenthood     = c("planned parenthood"),
  roe_v_wade             = c("roe v. wade", "roe vs. wade", "roe v wade"),
  dobbs                  = c("dobbs"),
  terminate_pregnancy    = c("terminate a pregnancy", "termination of pregnancy"),
  anti_abortion          = c("anti-abortion"),
  abortion_opponent      = c("abortion opponent", "abortion foe"),
  fetus                  = c("fetus"),
  unborn                 = c("unborn", "unborn child", "unborn baby"),
  right_to_life          = c("right to life"),
  right_to_choose        = c("right to choose"),
  bodily_autonomy        = c("bodily autonomy"),
  mifepristone           = c("mifepristone"),
  misoprostol            = c("misoprostol"),
  medication_abortion    = c("medication abortion")
)

# Weekly airtime hours per network (for normalization)
# Adjust these to reflect the actual hours of news programming per week
NETWORK_HOURS <- list(
  "NBC"  = 22,
  "CBS"  = 22,
  "ABC"  = 22,
  "FOX"  = 22,
  "PBS"  = 5
)

# ---------------------------------------------------------------------------
# FUNCTIONS
# ---------------------------------------------------------------------------

# Strip RTF control codes and return clean plain text
strip_rtf <- function(rtf_text) {
  text <- rtf_text
  # Remove \uc2 markers (Factiva Unicode escape prefix)
  text <- gsub("\\\\uc2 ?", " ", text, fixed = FALSE, perl = TRUE)
  # Remove RTF control words: backslash + letters + optional sign+digits + optional space
  text <- gsub("\\\\[a-zA-Z]+[-]?[0-9]* ?", " ", text, perl = TRUE)
  # Remove remaining backslashes and braces
  text <- gsub("[\\{\\}]", " ", text, perl = TRUE)
  # Collapse whitespace
  text <- gsub("\\s+", " ", text, perl = TRUE)
  trimws(text)
}

# Parse a single story block (everything after {*\bkmkstart tocN})
parse_story <- function(raw_block, file_network) {
  # Strip the leading "N}{*\bkmkend tocN}" prefix using fixed split
  end_marker <- paste0("\\bkmkend toc")
  if (str_detect(raw_block, fixed(end_marker))) {
    block <- str_split(raw_block, fixed(end_marker), n = 2)[[1]][2]
    # Remove the closing "N}" from the marker
    block <- sub("^\\d+\\}", "", block)
  } else {
    block <- raw_block
  }

  # --- Headline ---
  # RTF structure: \b \uc2 HEADLINE\b0\par  (literal backslashes in file)
  # Use fixed splits to extract reliably
  headline <- NA_character_
  if (str_detect(block, fixed("\\b \\uc2 "))) {
    hl_parts <- str_split(block, fixed("\\b \\uc2 "), n = 2)[[1]]
    if (length(hl_parts) > 1) {
      hl_raw <- str_split(hl_parts[2], fixed("\\b0\\par"), n = 2)[[1]][1]
      headline <- str_squish(strip_rtf(hl_raw))
    }
  }

  # --- Body text (everything after copyright notice) ---
  body_marker <- "copyright or other notice from copies of the content."
  if (str_detect(block, fixed(body_marker))) {
    body_raw <- str_split(block, fixed(body_marker), n = 2)[[1]][2]
  } else {
    body_raw <- block
  }
  body_text <- strip_rtf(body_raw)

  # --- Metadata (first part before copyright) ---
  meta_raw <- str_split(block, fixed("Content and programming copyright"), n = 2)[[1]][1]
  meta_clean <- strip_rtf(meta_raw)
  meta_lines <- str_split(meta_clean, "\\s{2,}")[[1]]
  meta_lines <- meta_lines[nchar(str_squish(meta_lines)) > 0]

  # Date: looks like "17 May 2023" or "June 17, 2023"
  date_raw <- str_extract(meta_clean,
    "\\d{1,2} (?:January|February|March|April|May|June|July|August|September|October|November|December) \\d{4}|(?:January|February|March|April|May|June|July|August|September|October|November|December) \\d{1,2},? \\d{4}")
  date_parsed <- tryCatch(
    as.Date(date_raw, format = c("%d %B %Y", "%B %d %Y", "%B %d, %Y")),
    error = function(e) NA
  )

  # Show name: bold text before b0
  show_name <- headline  # fallback

  # Source line: e.g. "NBC News: Nightly News"
  source_line <- str_extract(meta_clean, "NBC News:[^\\n]+|CBS News:[^\\n]+|ABC News:[^\\n]+|FOX News:[^\\n]+|PBS NewsHour[^\\n]*")
  if (is.na(source_line)) {
    source_line <- str_extract(meta_clean, "(?:NBC|CBS|ABC|Fox|PBS)[^\\.\\n]{3,50}")
  }

  # Network: derive from file_network or source line
  network <- file_network
  if (is.na(network) || network == "") {
    network <- str_extract(toupper(meta_clean), "\\b(NBC|CBS|ABC|FOX|PBS)\\b")
  }

  # --- Phrase counts ---
  body_lower <- tolower(body_text)
  phrase_counts <- sapply(PHRASE_LIST, function(variants) {
    sum(sapply(variants, function(v) str_count(body_lower, fixed(tolower(v)))))
  })

  show_clean <- str_squish(str_replace_all(
    source_line %||% "", "NBC News:|CBS News:|ABC News:|FOX News:|PBS NewsHour", ""))
  # Remove source code suffix (e.g. "NTLN English", "MTPR English")
  show_clean <- str_squish(str_remove(show_clean, "\\s+[A-Z]{3,6}\\s+English.*$"))

  as_tibble(c(
    list(
      network    = network,
      show       = show_clean,
      story_date = date_parsed,
      headline   = headline,
      story_text = body_text,
      word_count = str_count(body_text, "\\S+")
    ),
    as.list(phrase_counts)
  ))
}

# Infer network from filename
network_from_filename <- function(fname) {
  fname_upper <- toupper(basename(fname))
  if (str_detect(fname_upper, "NBC"))      return("NBC")
  if (str_detect(fname_upper, "CBS"))      return("CBS")
  if (str_detect(fname_upper, "ABC"))      return("ABC")
  if (str_detect(fname_upper, "FOX"))      return("FOX")
  if (str_detect(fname_upper, "PBS"))      return("PBS")
  return(NA_character_)
}

# Process one RTF file, return a data frame of all stories
process_rtf_file <- function(filepath) {
  message("Processing: ", basename(filepath))

  raw <- read_file(filepath, locale = locale(encoding = "latin1"))
  network <- network_from_filename(filepath)

  # Split on Factiva bookmark markers: literal string {\*\bkmkstart toc
  MARKER <- paste0("{", "\\*\\bkmkstart toc")
  story_blocks <- str_split(raw, fixed(MARKER))[[1]]

  if (length(story_blocks) <= 1) {
    warning("No story blocks found in: ", filepath)
    return(NULL)
  }

  # First block is the TOC / header — skip it
  story_blocks <- story_blocks[-1]

  message("  Found ", length(story_blocks), " stories")

  results <- lapply(story_blocks, function(block) {
    tryCatch(
      parse_story(block, network),
      error = function(e) {
        message("  Warning: could not parse a story block: ", conditionMessage(e))
        NULL
      }
    )
  })

  bind_rows(Filter(Negate(is.null), results))
}

# ---------------------------------------------------------------------------
# MAIN
# ---------------------------------------------------------------------------

# Find all RTF files
rtf_files <- list.files(RTF_DIR, pattern = "\\.rtf$", full.names = TRUE, ignore.case = TRUE)

if (length(rtf_files) == 0) {
  stop("No RTF files found in: ", RTF_DIR,
       "\nSet RTF_DIR to the folder containing your files.")
}

message("Found ", length(rtf_files), " RTF file(s)")

# Parse all files
all_stories <- lapply(rtf_files, process_rtf_file)
df <- bind_rows(Filter(Negate(is.null), all_stories))

message("\nTotal stories parsed: ", nrow(df))

# Sort by date
df <- df %>% arrange(story_date, network, show)

# Write CSV
write_csv(df, OUTPUT_CSV, na = "")
message("Saved to: ", OUTPUT_CSV)

# ---------------------------------------------------------------------------
# SUMMARY STATS (printed to console)
# ---------------------------------------------------------------------------

message("\n=== Stories per network ===")
df %>%
  count(network, sort = TRUE) %>%
  print()

message("\n=== Date range ===")
message("Earliest: ", min(df$story_date, na.rm = TRUE))
message("Latest:   ", max(df$story_date, na.rm = TRUE))

# Top phrases overall
phrase_cols <- names(PHRASE_LIST)
phrase_totals <- df %>%
  summarise(across(all_of(phrase_cols), sum, na.rm = TRUE)) %>%
  tidyr::pivot_longer(everything(), names_to = "phrase", values_to = "count") %>%
  arrange(desc(count)) %>%
  filter(count > 0)

message("\n=== Top phrases across all stories ===")
print(phrase_totals, n = 20)
