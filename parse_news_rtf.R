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
  abortion_opponent      = c("abortion opponent", "abortion opponents",
                              "opponents of abortion", "opponent of abortion",
                              "abortion foe", "abortion foes",
                              "abortion critic", "abortion critics"),

  # --- Labels used for each side (tracks whose language the anchor adopts) ---
  # Pro-restriction side labels
  label_prolife          = c("pro-life", "pro life", "life advocate",
                              "life supporter", "right to life advocate"),
  label_antiabortion     = c("anti-abortion", "abortion opponent", "abortion opponents",
                              "opponents of abortion", "abortion foe", "abortion foes"),
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
                              "had to leave the state", "left the state for",
                              "cross state lines", "across state lines",
                              "forcing patients", "forcing women",
                              "forcing many women", "look elsewhere for care",
                              "travel to another state", "unable to access",
                              "unable to get an abortion", "leaving the country",
                              "can't get an abortion", "cannot get an abortion",
                              "denied an abortion", "denied the procedure",
                              "couldn't get an abortion", "sought care in",
                              "had to go to another", "drove to another"),
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

  # --- Religious framing (count column — fires even when not primary trigger) ---
  # Catholic
  religion_catholic      = c("pope", "pope francis", "vatican", "catholic church",
                              "catholic bishop", "bishop", "archbishop", "cardinal",
                              "diocese", "papal", "usccb",
                              "united states conference of catholic bishops"),
  # Evangelical / Protestant
  religion_evangelical   = c("evangelical", "southern baptist", "evangelical christian",
                              "evangelical church", "methodist", "presbyterian",
                              "lutheran", "protestant"),
  # Jewish (notable: Jewish law often requires abortion — a distinct angle)
  religion_jewish        = c("jewish law", "halacha", "halakha", "rabbi",
                              "jewish community", "jewish values", "religious jewish"),
  # Muslim
  religion_muslim        = c("imam", "islamic", "muslim community"),
  # Religious liberty / conscience framing
  religion_liberty       = c("religious freedom", "religious liberty",
                              "religious exemption", "conscience clause",
                              "conscience exemption", "faith-based"),
  # Progressive religious voices (often underreported)
  religion_progressive   = c("progressive faith", "progressive religion",
                              "religious coalition", "clergy for choice",
                              "religious left", "faith community", "faith leader"),
  # Moral / scriptural language
  # NOTE: avoid short substrings like "sin" (matches "decision", "single", etc.)
  religion_moral         = c("god's will", "god's plan", "playing god",
                              "prayer vigil", "praying outside", "prayer outside",
                              "abortion is a sin", "it is a sin",
                              "moral teaching", "church teaching",
                              "scripture", "biblical", "the bible says",
                              "sanctity of life"),

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
# TRIGGER CLASSIFICATION
# Searched only in the first 200 words (story lede) — the news peg is always
# near the top of a broadcast story. Priority order determines the primary
# trigger when multiple fire; earlier entries win ties.
# ---------------------------------------------------------------------------

TRIGGER_LIST <- list(

  # 1. Supreme Court decisions — most specific, check first
  supreme_court_ruling = c(
    "the court ruled", "the court decided", "the justices ruled",
    "supreme court ruled", "supreme court decided", "supreme court struck",
    "supreme court upheld", "supreme court blocked", "supreme court heard",
    "supreme court taking up", "supreme court takes up", "supreme court will hear",
    "supreme court is taking", "supreme court weighs", "supreme court considers",
    "justices will hear", "high court ruled", "high court decided",
    "supreme court arguments", "oral arguments", "the justices", "the high court",
    "dobbs", "whole woman's health", "fda v.", "alliance for hippocratic",
    "june medical", "planned parenthood v.", "moyle v.", "idaho v."
  ),

  # 2. Federal / executive action
  federal_executive = c(
    "executive order", "president signed", "white house announced",
    "biden signed", "trump signed", "administration announced",
    "department of justice", "federal judge ruled", "federal court ruled",
    "appeals court ruled", "circuit court ruled", "appeals court",
    "congress passed", "senate passed", "house passed",
    "federal law", "nationwide ban", "federal funding", "title x",
    "fda announced", "fda approved", "fda ruled", "fda said",
    "attorney general", "justice department"
  ),

  # 3. State legislation or court decisions
  state_legislation = c(
    "governor signed", "legislature passed", "state lawmakers passed",
    "took effect", "went into effect", "state law", "state ban",
    "state supreme court", "state court ruled", "state attorney general",
    "state legislature", "passed a law", "new law in",
    "the law", "the new law", "the ban", "ban takes effect",
    "ban went into", "now illegal in", "now banned in",
    "republican-led", "democratic-led", "lawmakers in",
    "trigger law", "trigger laws", "trigger ban"
  ),

  # 4. Elections and campaigns
  election_campaign = c(
    "election day", "on the ballot", "ballot measure", "ballot initiative",
    "voters approved", "voters rejected", "voters decided",
    "campaign promise", "running on", "running for", "midterm",
    "primary election", "general election", "swing state",
    "congressional race", "senate race", "gubernatorial"
  ),

  # 5. Protests, marches, mobilization
  protest_mobilization = c(
    "marched", "march for life", "women's march", "protesters gathered",
    "demonstrators", "rally", "rallied", "took to the streets",
    "protest outside", "clinic escort", "counter-protest"
  ),

  # 6. Named patient / medical case (human-impact peg)
  patient_medical_case = c(
    "was denied", "denied care", "couldn't get care", "forced to travel",
    "had to leave the state", "left the state for", "died after",
    "died because", "nearly died", "bleeding", "ectopic",
    "miscarriage", "complications", "her story", "her case",
    "a woman in", "a patient in", "a mother in",
    "she was", "she had", "she couldn't", "she needed",
    "could not get", "unable to get care", "sought an abortion",
    "seeking an abortion", "needed an abortion", "wanted an abortion",
    "rape survivor", "rape victim", "10-year-old", "10 year old",
    "cross state lines", "across state lines", "forcing patients",
    "forcing women", "look elsewhere for care", "travel to another state",
    "denied an abortion", "denied the procedure", "couldn't get an abortion",
    "sought care in", "had to go to another"
  ),

  # 7. Political controversy / scandal (e.g. Herschel Walker)
  political_controversy = c(
    "accused of", "allegations", "allegedly paid", "paid for an abortion",
    "paid for her abortion", "paid for a woman", "paid for his",
    "hypocrisy", "scandal", "controversy", "under fire",
    "facing criticism", "called out", "strongly denies", "is denying"
  ),

  # 8. Anniversary / retrospective
  anniversary_retrospective = c(
    "anniversary", "years ago today", "one year after", "two years after",
    "three years after", "since roe", "since dobbs", "since the ruling",
    "looking back", "five years", "50 years", "50th anniversary"
  ),

  # 9. Poll / survey / research findings
  poll_research = c(
    "new poll", "new survey", "according to a poll", "according to a survey",
    "a new study", "new research", "according to new data",
    "polling shows", "survey found", "study found", "report found",
    "according to the guttmacher", "cdc data", "new numbers"
  ),

  # 10. Religion — fires last so patient/political stories that mention clergy
  # in passing aren't misclassified. Uses specific institutional phrases only,
  # not generic words like "prayer" or "sin" that appear in any context.
  religion = c(
    # Catholic institutional phrases
    "pope francis", "the vatican", "catholic church", "catholic bishop",
    "archbishop", "cardinal", "diocese", "papal", "encyclical",
    "united states conference of catholic bishops", "usccb",
    # Evangelical / Protestant institutional
    "evangelical", "southern baptist", "evangelical christian",
    "evangelical church",
    # Jewish (Jewish law requires abortion in some cases — major post-Dobbs angle)
    "jewish law", "halacha", "halakha", "rabbi",
    "jewish values", "under jewish law",
    # Muslim
    "islamic law", "muslim community", "according to islam",
    # Religious liberty framing
    "religious freedom", "religious liberty", "religious exemption",
    "conscience clause", "conscience exemption",
    # Progressive religious voices
    "progressive faith", "religious coalition", "clergy for choice",
    "religious left", "religious right", "faith-based organization",
    # Doctrinal/scriptural framing
    "god's will", "god's plan", "church teaching",
    "scripture", "biblical teaching", "the bible teaches"
  )
)

# Assign a single primary trigger to a story based on its lede.
# Returns the name of the first trigger category that fires, or "other".
classify_trigger <- function(lede_lower) {
  for (trigger_name in names(TRIGGER_LIST)) {
    variants <- TRIGGER_LIST[[trigger_name]]
    if (any(sapply(variants, function(v) str_detect(lede_lower, fixed(tolower(v)))))) {
      return(trigger_name)
    }
  }
  return("other")
}

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
  # Remove embedded image groups ({\pict...}) before stripping — their hex data
  # can span thousands of chars and contains spurious keyword matches.
  meta_raw <- gsub("\\{[^{}]*\\\\pict[^{}]*\\}", " ", meta_raw, perl = TRUE)
  meta_clean <- strip_rtf(meta_raw)
  meta_lines <- str_split(meta_clean, "\\s{2,}")[[1]]
  meta_lines <- meta_lines[nchar(str_squish(meta_lines)) > 0]

  # Date: looks like "17 May 2023" or "June 17, 2023"
  date_raw <- str_extract(meta_clean,
    "\\d{1,2} (?:January|February|March|April|May|June|July|August|September|October|November|December) \\d{4}|(?:January|February|March|April|May|June|July|August|September|October|November|December) \\d{1,2},? \\d{4}")
  date_parsed <- as.Date(NA_character_)
  for (fmt in c("%d %B %Y", "%B %d %Y", "%B %d, %Y")) {
    d <- tryCatch(as.Date(date_raw, format = fmt), error = function(e) as.Date(NA_character_))
    if (!is.na(d)) { date_parsed <- d; break }
  }

  # Show name: bold text before b0
  show_name <- headline  # fallback

  # Source line: e.g. "NBC News: Nightly News" or "The New York Times"
  source_line <- str_extract(meta_clean,
    paste0("NBC News:[^\\n]+|CBS News:[^\\n]+|ABC News:[^\\n]+|FOX News:[^\\n]+|",
           "PBS NewsHour[^\\n]*|MSNBC:[^\\n]+|",
           "The New York Times[^\\n]*|The Guardian[^\\n]*|",
           "The Wall Street Journal[^\\n]*|Washington Post[^\\n]*"))
  if (is.na(source_line)) {
    source_line <- str_extract(meta_clean, "(?:NBC|CBS|ABC|Fox|PBS|MSNBC)[^\\.\\n]{3,50}")
  }

  # Network: derive from file_network or source line
  network <- file_network
  if (is.na(network) || network == "") {
    network <- str_extract(toupper(meta_clean), "\\b(NBC|CBS|ABC|FOX|PBS|MSNBC)\\b")
  }

  # --- Factiva subject/region tag (first token before show name) ---
  # e.g. "News; Domestic", "News; International", "News"
  factiva_region <- str_extract(meta_clean, "News;?\\s*(?:Domestic|International)?")
  factiva_region <- str_squish(factiva_region %||% "")

  # --- Publication (for newspapers) and show (for TV) ---
  # TV: clean up network prefix and source code suffix
  # Newspapers: publication name is the source line itself
  media_type <- if_else(
    str_detect(toupper(source_line %||% ""),
               "NBC|CBS|ABC|FOX|PBS|MSNBC"),
    "tv", "print"
  )

  show_clean <- source_line %||% ""
  # Remove network prefixes
  show_clean <- str_replace(show_clean,
    "^(NBC|CBS|ABC|Fox|PBS|MSNBC) News(Hour)?(\\s+Channel)?:\\s*", "")
  show_clean <- str_replace(show_clean, "^MSNBC:\\s*", "")
  # Remove trailing Factiva 3-6 char source codes e.g. "NTLN English"
  show_clean <- str_remove(show_clean, "\\s+[A-Z]{3,6}(\\s.*)?$")
  show_clean <- str_squish(show_clean)
  if (nchar(show_clean) > 60) show_clean <- NA_character_

  # Publication = cleaned source for print; show name for TV
  publication <- if (media_type == "print") {
    str_extract(source_line %||% "",
      "New York Times|The Guardian|Wall Street Journal|Washington Post|[A-Z][^,\\n]{3,40}")
  } else {
    NA_character_
  }

  # --- Section (Opinion, News, Features, etc.) ---
  # Only scan the first 600 chars of meta_clean — the section label appears near the
  # top of each Factiva block, well before any embedded image hex data that could
  # contain spurious keyword matches.
  meta_head <- substr(meta_clean, 1, 600)
  section_raw <- str_extract(meta_head,
    "(?i)(opinion|editorial|op.ed|commentary|review & outlook|letters to the editor|news|features?|world|politics?|health|science)")
  # Flag obvious opinion content
  is_opinion <- as.integer(
    str_detect(tolower(meta_head), "opinion|editorial|op-ed|op ed|commentary|review & outlook|letters to the editor") |
    str_detect(tolower(headline %||% ""), "opinion|editorial|op-ed|commentary")
  )

  # --- US story filter ---
  # TV networks are always US; for print, flag non-US stories by checking
  # whether the story contains US geographic anchors or an explicit international tag.
  US_STATES <- paste(c(
    "alabama","alaska","arizona","arkansas","california","colorado","connecticut",
    "delaware","florida","georgia","idaho","illinois","indiana","iowa","kansas",
    "kentucky","louisiana","maine","maryland","massachusetts","michigan","minnesota",
    "mississippi","missouri","montana","nebraska","nevada","new hampshire",
    "new jersey","new mexico","new york","north carolina","north dakota","ohio",
    "oklahoma","oregon","pennsylvania","rhode island","south carolina","south dakota",
    "tennessee","texas","utah","vermont","virginia","washington","west virginia",
    "wisconsin","wyoming","washington d.c.","district of columbia",
    "congress","senate","supreme court","white house","planned parenthood",
    "united states","american","u.s."
  ), collapse = "|")
  is_us_story <- as.integer(
    media_type == "tv" |
    str_detect(factiva_region, "Domestic") |
    str_detect(tolower(body_text), US_STATES)
  )

  # --- Abortion segment lede: 200 words starting where abortion is first mentioned ---
  # These are whole broadcast transcripts; the abortion segment can appear anywhere.
  # Find the first abortion-related word, then take 200 words from that point.
  ABORTION_ANCHORS <- c("abortion", "roe", "dobbs", "mifepristone",
                         "reproductive rights", "pro-life", "pro-choice")
  body_words  <- str_split(body_text, "\\s+")[[1]]
  body_lower_words <- tolower(body_words)
  seg_start <- NA_integer_
  for (anchor in ABORTION_ANCHORS) {
    hits <- which(str_detect(body_lower_words, fixed(anchor)))
    if (length(hits) > 0) {
      seg_start <- max(1, hits[1] - 20)  # 20 words of run-up for context
      break
    }
  }
  if (!is.na(seg_start)) {
    story_lede <- paste(body_words[seg_start:min(length(body_words), seg_start + 199)],
                        collapse = " ")
  } else {
    story_lede <- paste(head(body_words, 200), collapse = " ")
  }
  lede_lower <- tolower(story_lede)

  # --- Phrase counts (full story text) ---
  body_lower <- tolower(body_text)

  # --- Trigger classification ---
  # Primary: use the abortion segment lede (200 words) — captures what prompted
  # THIS story specifically.
  # Fallback: if lede returns "other", search the full text — catches patient/
  # medical-case stories where the triggering legislation is named later in the
  # piece (e.g. "she was denied care under [state]'s new law").
  story_trigger <- classify_trigger(lede_lower)
  if (story_trigger == "other") {
    story_trigger <- classify_trigger(body_lower)
  }
  phrase_counts <- sapply(PHRASE_LIST, function(variants) {
    sum(sapply(variants, function(v) str_count(body_lower, fixed(tolower(v)))))
  })

  as_tibble(c(
    list(
      network        = network,
      media_type     = media_type,
      show           = show_clean,
      publication    = publication,
      section        = section_raw %||% NA_character_,
      is_opinion     = is_opinion,
      is_us_story    = is_us_story,
      factiva_region = factiva_region,
      story_date     = date_parsed,
      headline       = headline,
      story_trigger  = story_trigger,
      story_lede     = story_lede,
      story_text     = body_text,
      word_count     = str_count(body_text, "\\S+"),
      core_abortion_count = sum(sapply(
        c("abortion", "roe v. wade", "roe v wade", "dobbs", "mifepristone",
          "misoprostol", "abortion ban", "abortion law", "abortion right",
          "abortion access", "abortion care", "abortion restrict",
          "reproductive right", "reproductive health", "reproductive freedom",
          "pro-life", "pro-choice", "pro life", "pro choice",
          "abortion pill", "medication abortion", "planned parenthood",
          "fetal heartbeat", "six-week", "gestational", "terminate the pregnancy",
          "termination of pregnancy"),
        function(v) str_count(body_lower, fixed(tolower(v)))
      )),
      is_live_blog   = as.integer(
        str_detect(tolower(headline %||% ""), "as it happened|live updates|live blog|rolling coverage") |
        (media_type == "print" & str_count(body_text, "\\S+") > 3000)
      )
    ),
    as.list(phrase_counts)
  ))
}

# Infer network/outlet from filename
network_from_filename <- function(fname) {
  fname_upper <- toupper(basename(fname))
  if (str_detect(fname_upper, "MSNBC"))    return("MSNBC")   # check before NBC
  if (str_detect(fname_upper, "NBC"))      return("NBC")
  if (str_detect(fname_upper, "CBS"))      return("CBS")
  if (str_detect(fname_upper, "ABC"))      return("ABC")
  if (str_detect(fname_upper, "FOX"))      return("FOX")
  if (str_detect(fname_upper, "PBS"))      return("PBS")
  if (str_detect(fname_upper, "NYT|NEW.YORK.TIMES")) return("NYT")
  if (str_detect(fname_upper, "GUARDIAN|GUARD")) return("GUARDIAN")
  if (str_detect(fname_upper, "WSJ|WALL.STREET"))    return("WSJ")
  if (str_detect(fname_upper, "WAPO|WASHPOST|WASHINGTON.POST")) return("WAPO")
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

message("\n=== Story triggers (all networks) ===")
df %>%
  count(story_trigger, sort = TRUE) %>%
  print()

message("\n=== Story triggers by network ===")
df %>%
  count(network, story_trigger) %>%
  tidyr::pivot_wider(names_from = story_trigger, values_from = n, values_fill = 0) %>%
  print()

# ---------------------------------------------------------------------------
# AIRTIME NORMALIZATION (TV only)
# Weekly broadcast hours per network, used to compute stories per 100 hrs.
# Print outlets (NYT, WSJ, Guardian) are excluded — daily publication
# doesn't map cleanly to an hourly airtime equivalent.
# ---------------------------------------------------------------------------

TV_HOURS_PER_WEEK <- c(
  ABC   = 13.5,
  CBS   = 13.5,
  NBC   = 18.5,
  FOX   =  67,
  MSNBC =  45,
  PBS   =   5
)

# Compute study period in weeks from the actual data
tv_df <- df %>%
  filter(media_type == "tv", !is.na(story_date), is_live_blog == 0)

study_start <- min(tv_df$story_date, na.rm = TRUE)
study_end   <- max(tv_df$story_date, na.rm = TRUE)
study_weeks <- as.numeric(difftime(study_end, study_start, units = "weeks"))

airtime_norm <- tv_df %>%
  count(network, name = "stories") %>%
  filter(network %in% names(TV_HOURS_PER_WEEK)) %>%
  mutate(
    hrs_per_week      = TV_HOURS_PER_WEEK[network],
    total_hours       = round(hrs_per_week * study_weeks),
    stories_per_100hr = round(stories / total_hours * 100, 2)
  )

message("\n=== TV airtime normalization ===")
message(sprintf("Study period: %s to %s (%.1f weeks)", study_start, study_end, study_weeks))
print(airtime_norm)
