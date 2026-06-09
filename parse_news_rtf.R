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

  # --- Core abortion terminology ---
  pro_life               = c("pro-life", "pro life"),
  pro_choice             = c("pro-choice", "pro choice"),
  abortion_rights        = c("abortion rights"),
  abortion_access        = c("abortion access"),
  abortion_care          = c("abortion care"),
  abortion_pill          = c("abortion pill"),
  medication_abortion    = c("medication abortion"),
  mifepristone           = c("mifepristone"),
  misoprostol            = c("misoprostol"),
  reproductive_rights    = c("reproductive rights"),
  reproductive_health    = c("reproductive health"),
  reproductive_freedom   = c("reproductive freedom"),
  reproductive_choice    = c("reproductive choice"),
  roe_v_wade             = c("roe v. wade", "roe vs. wade", "roe v wade"),
  dobbs                  = c("dobbs"),
  planned_parenthood     = c("planned parenthood"),
  family_planning        = c("family planning"),
  womens_health          = c("women's health", "womens health"),

  # --- Restriction framing: how the policy action is labeled ---
  # "ban" implies total prohibition; "restriction/limit" is softer
  abortion_ban           = c("abortion ban"),
  abortion_restriction   = c("abortion restriction", "abortion restrictions"),
  abortion_limit         = c("abortion limit", "abortion limits", "abortion limitation"),
  abortion_law           = c("abortion law", "abortion laws", "abortion legislation"),
  prolife_law            = c("pro-life law", "life-protecting law"),

  # --- "Overturning Roe" framing variants ---
  overturned_roe         = c("overturned roe", "struck down roe", "ended roe"),
  returned_to_states     = c("returned to the states", "return it to the states",
                              "left it to the states", "left to the states"),

  # --- Fetal/embryo language: clinical vs. political ---
  fetus                  = c("fetus", "fetal"),
  embryo                 = c("embryo"),
  unborn                 = c("unborn", "unborn child", "unborn baby"),
  baby_fetus             = c("unborn baby", "baby in the womb", "preborn"),
  cardiac_activity       = c("cardiac activity"),         # clinical term
  fetal_heartbeat        = c("fetal heartbeat", "heartbeat bill", "heartbeat law"),
  gestational_age        = c("gestational age", "weeks pregnant", "weeks gestation"),
  viability              = c("viability", "viable"),
  late_term_abortion     = c("late-term abortion", "late term abortion"),
  partial_birth_abortion = c("partial-birth abortion", "partial birth abortion"),

  # --- Gendered vs. gender-neutral language for pregnant people ---
  # Shift from "women" to "people/persons" is an ideological marker
  pregnant_women         = c("pregnant woman", "pregnant women"),
  pregnant_people        = c("pregnant person", "pregnant people", "birthing person",
                              "birthing people"),
  woman_choice           = c("woman's choice", "women's choice"),
  womens_body            = c("woman's body", "women's bodies", "her body"),
  mother_language        = c("mother", "expectant mother", "new mother"),

  # --- Whose rights / whose decision framing ---
  right_to_life          = c("right to life"),
  right_to_choose        = c("right to choose"),
  bodily_autonomy        = c("bodily autonomy", "bodily integrity"),
  terminate_pregnancy    = c("terminate a pregnancy", "termination of pregnancy",
                              "end a pregnancy", "ending a pregnancy"),
  anti_abortion          = c("anti-abortion"),
  abortion_opponent      = c("abortion opponent", "abortion foe", "abortion critic"),

  # --- Labels used for each side (tracks whose language the anchor adopts) ---
  # Pro-restriction side labels
  label_prolife          = c("pro-life", "pro life", "life advocate",
                              "life supporter", "right to life advocate"),
  label_antiabortion     = c("anti-abortion", "abortion opponent", "abortion foe"),
  # Pro-access side labels
  label_prochoice        = c("pro-choice", "pro choice", "choice advocate"),
  label_abortionrights   = c("abortion rights advocate", "abortion rights supporter",
                              "abortion rights activist"),
  label_activist         = c("abortion activist", "reproductive rights activist"),

  # --- Named advocacy organizations (which groups get cited) ---
  org_planned_parenthood = c("planned parenthood"),
  org_naral              = c("naral", "naral pro-choice"),
  org_aclu               = c("aclu", "american civil liberties union"),
  org_susan_b_anthony    = c("susan b. anthony", "sba list", "susan b anthony"),
  org_nrl                = c("national right to life", "nrlc"),
  org_aul                = c("americans united for life"),
  org_march_for_life     = c("march for life"),
  org_guttmacher         = c("guttmacher"),

  # --- Medical framing: clinical language signals different perspective ---
  clinical_language      = c("uterus", "uterine", "cervix", "ectopic",
                              "miscarriage management", "dilation and", "d&c", "d&e"),
  medical_necessity      = c("medical necessity", "medically necessary",
                              "medical exception", "health exception",
                              "life of the mother", "save her life",
                              "maternal health", "maternal mortality"),
  rape_incest_exception  = c("rape", "incest", "rape exception", "incest exception",
                              "victim of rape", "victim of incest"),

  # --- Story effect framing: patient impact vs. policy debate ---
  patient_impact         = c("forced to travel", "couldn't get care",
                              "denied care", "turned away", "couldn't afford",
                              "had to leave the state", "left the state for"),
  state_action           = c("state law", "state ban", "state legislature",
                              "state lawmakers", "state passed"),
  federal_action         = c("federal law", "federal ban", "congress",
                              "federal legislation", "nationwide ban"),
  supreme_court          = c("supreme court"),

  # --- Political figures (whose voice anchors the story) ---
  biden                  = c("biden", "joe biden"),
  trump                  = c("trump", "donald trump"),
  democrats              = c("democrat", "democrats", "democratic"),
  republicans            = c("republican", "republicans", "gop"),

  # --- Electoral framing: links abortion to voting and elections ---
  vote_voting            = c("vote", "voting", "voters", "voted"),
  swing_state            = c("swing state", "swing states", "battleground state",
                              "battleground states"),
  ballot_measure         = c("ballot measure", "ballot measures", "ballot initiative",
                              "referendum on abortion"),
  congressional_race     = c("congressional race", "senate race", "house race",
                              "midterm", "midterms", "presidential primary",
                              "presidential election"),

  # --- Policy mechanism terms ---
  trigger_law            = c("trigger law", "trigger laws", "trigger ban"),
  six_week_ban           = c("six-week ban", "six week ban", "6-week ban",
                              "6 week ban", "heartbeat bill", "heartbeat law",
                              "heartbeat act"),
  fifteen_week_ban       = c("15-week ban", "fifteen-week ban", "15 week ban"),
  twenty_week_ban        = c("20-week ban", "twenty-week ban", "20 week ban"),

  # --- "Chemical abortion" vs "medication abortion" framing ---
  # "chemical abortion" is politically charged; "medication abortion" is clinical
  chemical_abortion      = c("chemical abortion"),
  # medication_abortion already tracked above

  # --- "Pre-born" / "unborn baby" — emotive fetal language ---
  preborn                = c("preborn", "pre-born"),
  unborn_child_baby      = c("unborn child", "unborn baby"),
  # "fetus" already tracked; this captures the more political variants

  # --- Legitimacy/authority language ---
  physician_prescribed   = c("physician-prescribed", "doctor-prescribed",
                              "prescribed by a doctor", "prescribed by a physician"),
  fda_approved           = c("fda-approved", "fda approved", "approved by the fda"),

  # --- "Reproductive justice" — a specific movement frame distinct from "rights" ---
  reproductive_justice   = c("reproductive justice"),

  # --- Real-world / human consequences framing ---
  real_world_effects     = c("real-world", "real world consequences",
                              "human consequences", "real consequences",
                              "lived experience"),
  women_affected         = c("women affected", "women impacted", "patients affected",
                              "women who", "women are"),

  # --- "Every life matters" / moral framing language ---
  every_life             = c("every life", "every human life", "sanctity of life",
                              "sacred life", "human dignity"),
  womens_freedom         = c("women's reproductive freedom", "reproductive freedom",
                              "women's freedom"),

  # --- Pro-life side name variants (tracks which label each network uses) ---
  # Captures the full spectrum from neutral to politically coded
  called_prolife         = c("pro-life"),
  called_antiabortion    = c("anti-abortion"),
  called_probaby         = c("pro-baby"),
  called_righttolife     = c("right to life"),

  # --- Pro-choice side name variants ---
  called_prochoice       = c("pro-choice"),
  called_abortionrights  = c("abortion rights"),
  called_reprofreedom    = c("reproductive freedom"),
  called_reprojustice    = c("reproductive justice")
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
  # Format A blocks start with "N}{*\bkmkend tocN}" — strip that prefix
  end_marker <- paste0("\\bkmkend toc")
  if (str_detect(raw_block, fixed(end_marker))) {
    block <- str_split(raw_block, fixed(end_marker), n = 2)[[1]][2]
    block <- sub("^\\d+\\}", "", block)
  } else {
    # Format B: block starts directly with \par or \b headline
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

  show_clean <- source_line %||% ""
  # Remove network prefixes like "NBC News:", "Fox News:", "Fox News Channel:"
  show_clean <- str_replace(show_clean, "^(NBC|CBS|ABC|Fox|PBS) News(Hour)?(\\s+Channel)?:\\s*", "")
  # Remove trailing Factiva 3-6 char uppercase source codes and everything after
  # e.g. "Nightly News NTLN English" -> "Nightly News", "MacCallum HUN" -> "MacCallum"
  show_clean <- str_remove(show_clean, "\\s+[A-Z]{3,6}(\\s.*)?$")
  show_clean <- str_squish(show_clean)
  # If show name is suspiciously long (>60 chars) it's probably a headline not a show — clear it
  if (nchar(show_clean) > 60) show_clean <- NA_character_

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

  # Factiva exports come in two formats:
  #   Format A (multi-story with TOC): stories split by {\*\bkmkstart toc
  #   Format B (no TOC):               stories split by \page
  BOOKMARK_MARKER <- paste0("{", "\\*\\bkmkstart toc")

  if (str_detect(raw, fixed(BOOKMARK_MARKER))) {
    # Format A
    story_blocks <- str_split(raw, fixed(BOOKMARK_MARKER))[[1]][-1]  # drop TOC
  } else {
    # Format B — split on \page, keep only blocks that contain story metadata
    story_blocks <- str_split(raw, fixed("\\page"))[[1]]
    # Keep only blocks that have a date and source line
    story_blocks <- story_blocks[str_detect(story_blocks, fixed("\\uc2")) &
                                   str_detect(story_blocks, "\\d{4}")]
  }

  if (length(story_blocks) == 0) {
    warning("No story blocks found in: ", filepath)
    return(NULL)
  }

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
