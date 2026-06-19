# parse_news_rtf_labor.R
# Parses Factiva RTF exports of TV news transcripts (filename pattern
# "<network> labor <n>.rtf") into a structured CSV with phrase frequency
# counts for labor/worker-organizing research. Mirrors parse_news_rtf.R
# (the abortion-coverage parser) in structure, but uses a labor-specific
# PHRASE_LIST, TRIGGER_LIST, and anchor terms.
#
# Usage: Rscript parse_news_rtf_labor.R
# Output: labor_stories.csv in the same directory as the script
#
# Required packages: stringr, dplyr, readr, tidyr

library(stringr)
library(dplyr)
library(readr)

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && nchar(a) > 0) a else b

# ---------------------------------------------------------------------------
# CONFIGURATION
# ---------------------------------------------------------------------------

RTF_DIR <- "."
OUTPUT_CSV <- "labor_stories.csv"

# Only process files whose name contains "labor" (so this script can share
# a directory with the abortion-corpus RTFs without double-counting them).
RTF_FILE_PATTERN <- "labor.*\\.rtf$"

# Phrases to count in each story (case-insensitive), consolidated from two
# source term lists covering pro-labor, anti-labor, neutral/technical, and
# NLRB/legal-process framing.
PHRASE_LIST <- list(

  # --- Legitimizing worker action (agency verbs, structural framing) ---
  workers_say              = c("workers say", "workers said"),
  workers_claim            = c("workers claim", "workers claimed"),
  workers_organized        = c("workers organized", "organized a union", "unionized"),
  workers_won_concessions  = c("won concessions", "workers won"),
  union_election           = c("union election", "unionization election", "representation election"),
  collective_bargaining_rights = c("collective bargaining rights"),
  worker_safety_concerns   = c("worker safety", "workplace safety concerns"),
  retaliation              = c("retaliation", "illegal retaliation", "retaliated against"),
  misclassification        = c("misclassification", "misclassified as"),
  low_wages                = c("low wages", "poverty wages", "low-wage workers"),
  unsafe_conditions        = c("unsafe conditions", "unsafe working conditions"),
  historic_contract        = c("historic contract", "landmark contract", "historic agreement"),
  record_turnout_workers   = c("record turnout among workers", "record voter turnout among workers"),
  grassroots_organizing    = c("grassroots organizing", "grassroots campaign"),

  # --- Structural framing ---
  profits_wages_gap        = c("profits surged while wages", "record profits while wages",
                                 "corporate profits surged"),
  power_imbalance          = c("power imbalance"),
  industry_wide_pattern    = c("industry-wide pattern", "industrywide pattern"),
  systemic_issues          = c("systemic issues", "systemic problem"),
  anti_union_campaign      = c("anti-union campaign", "union-busting", "union busting"),

  # --- Delegitimizing / minimizing workers ---
  union_bosses             = c("union bosses", "union boss"),
  union_demands            = c("union demands"),
  militant_workers         = c("militant workers", "militant union"),
  union_pressure           = c("union pressure"),
  union_threat             = c("union threat"),
  strike_threat            = c("strike threat"),
  walked_off_job           = c("walked off the job"),
  wildcat_strike           = c("wildcat strike"),
  scab_replacement_workers = c("scab", "scabs", "replacement workers", "strikebreaker", "strikebreakers"),

  # --- Economic alarmism ---
  wage_hikes_inflation     = c("wage hikes could fuel inflation", "wage growth could fuel inflation",
                                 "wages could fuel inflation"),
  burdensome_labor_costs   = c("burdensome labor costs", "rising labor costs"),
  threat_to_competitiveness = c("threat to competitiveness", "hurt competitiveness"),
  job_killing_regulations  = c("job-killing regulations", "job killing regulations"),
  crippling_strike         = c("crippling strike", "crippled by the strike"),
  supply_chain_disruption  = c("disruptions to supply chains", "supply chain disruptions",
                                 "disrupt supply chains"),
  businesses_warn          = c("businesses warn", "business groups warn", "employers warn"),

  # --- Employer-centric framing ---
  labor_shortage           = c("labor shortage", "worker shortage"),
  flexibility_for_employers = c("flexibility for employers"),
  streamlining_labor_rules = c("streamlining labor rules", "streamline labor regulations"),
  modernizing_workforce    = c("modernizing the workforce", "modernize the workforce"),
  right_sizing             = c("right-sizing", "right sizing", "workforce optimization"),
  at_will_employment       = c("at-will employment", "at will employment"),

  # --- Disruption / victimhood verbs ---
  disrupted_halted_crippled = c("disrupted operations", "halted production", "crippled operations"),
  suffered_forced           = c("the company suffered", "was forced to", "were forced to"),

  # --- Neutral / technical terms ---
  collective_bargaining    = c("collective bargaining"),
  contract_negotiations    = c("contract negotiations", "contract talks"),
  labor_dispute            = c("labor dispute"),
  work_stoppage            = c("work stoppage"),
  right_to_work            = c("right-to-work", "right to work law", "right-to-work law"),
  employment_trends        = c("employment trends"),
  wage_growth              = c("wage growth"),
  labor_market_tightness   = c("labor market tightness", "tight labor market"),
  productivity             = c("productivity"),

  # --- Ambiguous framing ---
  flexibility              = c("flexibility"),
  gig_economy              = c("gig economy", "gig worker", "gig workers"),
  independent_contractor   = c("independent contractor", "independent contractors"),
  shared_sacrifice         = c("shared sacrifice"),
  economic_headwinds       = c("economic headwinds"),
  compromise_deal          = c("compromise deal"),
  industry_analysts_say    = c("industry analysts say", "analysts say"),

  # --- Balance-seeking phrases ---
  both_sides               = c("both sides"),
  stakeholders             = c("stakeholders"),
  according_to_company_workers = c("according to the company", "according to workers"),
  dispute_centers_on       = c("the dispute centers on", "dispute centers on"),

  # --- NLRB / legal-process specific ---
  nlrb_election            = c("nlrb election", "n.l.r.b. election"),
  decertification_election = c("decertification election"),
  unfair_labor_practice    = c("unfair labor practice", "unfair labor practices", "u.l.p."),
  successor_employer       = c("successor employer"),
  joint_employer           = c("joint employer", "joint-employer"),
  pro_act                  = c("pro act", "pro-act", "protecting the right to organize act"),
  section_7_rights         = c("section 7", "section seven rights"),
  duty_to_bargain          = c("duty to bargain"),
  cemex_decision           = c("cemex"),
  card_check               = c("card check"),
  captive_audience_meeting = c("captive audience meeting", "captive-audience meeting"),
  first_contract           = c("first contract", "bargaining in bad faith"),
  binding_arbitration      = c("binding arbitration"),
  no_strike_clause         = c("no-strike clause", "no strike clause"),
  open_closed_shop         = c("open shop", "closed shop"),

  # --- Named unions / advocacy actors ---
  org_starbucks_workers_united = c("starbucks workers united"),
  org_amazon_labor_union   = c("amazon labor union"),
  org_uaw                  = c("uaw", "united auto workers"),
  org_teamsters            = c("teamsters"),
  org_afl_cio              = c("afl-cio", "afl cio"),
  org_seiu                 = c("seiu", "service employees international union"),

  # --- General bias-detection techniques applied to labor actors ---
  loaded_label_rioters_mob = c("mob", "mobs"),
  loaded_label_demonstrators = c("demonstrators", "marchers", "picketers"),
  judgment_verb_claimed    = c("claimed", "admitted", "conceded", "boasted"),
  judgment_verb_demanded   = c("demanded", "insisted"),
  vague_authority_critics  = c("critics say", "some experts believe", "many are calling for",
                                 "it is widely accepted that"),
  value_adjective_controversial = c("controversial decision", "controversial deal"),
  value_adjective_radical_extreme = c("radical demands", "extreme demands"),
  value_adjective_historic_disastrous = c("historic triumph", "disastrous deal", "disastrous policy"),

  # --- Other labor-relevant 2021-2026 trends ---
  essential_workers        = c("essential workers", "frontline workers"),
  return_to_office         = c("return-to-office", "return to office", "rto mandate"),
  ai_automation_displacement = c("automation", "ai replacing workers", "artificial intelligence",
                                   "algorithmic management")
)

# ---------------------------------------------------------------------------
# TRIGGER CLASSIFICATION
# Searched in the first 200 words of the labor-segment lede. Priority order
# determines the primary trigger when multiple fire; earlier entries win ties.
# ---------------------------------------------------------------------------

TRIGGER_LIST <- list(

  # 1. NLRB rulings / legal process — most specific, check first
  nlrb_legal_process = c(
    "nlrb ruled", "nlrb decided", "the board ruled", "national labor relations board",
    "nlrb election", "decertification election", "unfair labor practice",
    "filed a complaint with the nlrb", "nlrb complaint", "cemex", "joint employer rule",
    "administrative law judge", "regional director"
  ),

  # 2. Strikes / walkouts / work stoppages
  strike_walkout = c(
    "went on strike", "walked off the job", "authorized a strike", "strike vote",
    "strike threat", "picket line", "picketed", "work stoppage", "walkout",
    "wildcat strike", "lockout", "locked out"
  ),

  # 3. Contract negotiations / collective bargaining
  contract_negotiation = c(
    "contract negotiations", "contract talks", "collective bargaining",
    "reached a deal", "reached an agreement", "ratified a contract",
    "rejected the contract", "tentative agreement", "first contract",
    "bargaining table", "bargaining in bad faith"
  ),

  # 4. Union organizing campaigns
  union_organizing = c(
    "union election", "voted to unionize", "voted to join the union",
    "filed for a union election", "organizing campaign", "card check",
    "captive audience meeting", "union drive", "unionization effort",
    "amazon labor union", "starbucks workers united"
  ),

  # 5. Legislation / policy action
  legislation_policy = c(
    "right-to-work law", "minimum wage law", "passed a law", "governor signed",
    "department of labor", "labor secretary", "executive order", "pro act",
    "protecting the right to organize act", "state legislature", "ballot measure"
  ),

  # 6. Workplace safety incidents
  workplace_safety_incident = c(
    "workplace injury", "workplace death", "osha investigation", "osha cited",
    "safety violation", "unsafe conditions", "worker died", "worker was killed"
  ),

  # 7. Elections / political campaigns
  election_political = c(
    "election day", "on the ballot", "campaign promise", "running for",
    "midterm", "primary election", "general election", "presidential election"
  ),

  # 8. Corporate dispute / layoffs
  corporate_dispute = c(
    "layoffs", "laid off", "mass layoff", "workforce reduction", "right-sizing",
    "workforce optimization", "return-to-office mandate"
  ),

  # 9. Anniversary / retrospective
  anniversary_retrospective = c(
    "anniversary", "years ago today", "one year after", "looking back",
    "since the strike", "since the walkout"
  )
)

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
# FUNCTIONS (shared parsing logic, adapted from parse_news_rtf.R)
# ---------------------------------------------------------------------------

strip_rtf <- function(rtf_text) {
  text <- rtf_text
  text <- gsub("\\\\uc2 ?", " ", text, fixed = FALSE, perl = TRUE)
  text <- gsub("\\\\[a-zA-Z]+[-]?[0-9]* ?", " ", text, perl = TRUE)
  text <- gsub("[\\{\\}]", " ", text, perl = TRUE)
  text <- gsub("\\s+", " ", text, perl = TRUE)
  trimws(text)
}

parse_story <- function(raw_block, file_network) {
  end_marker <- paste0("\\bkmkend toc")
  if (str_detect(raw_block, fixed(end_marker))) {
    block <- str_split(raw_block, fixed(end_marker), n = 2)[[1]][2]
    block <- sub("^\\d+\\}", "", block)
  } else {
    block <- raw_block
  }

  headline <- NA_character_
  if (str_detect(block, fixed("\\b \\uc2 "))) {
    hl_parts <- str_split(block, fixed("\\b \\uc2 "), n = 2)[[1]]
    if (length(hl_parts) > 1) {
      hl_raw <- str_split(hl_parts[2], fixed("\\b0\\par"), n = 2)[[1]][1]
      headline <- str_squish(strip_rtf(hl_raw))
    }
  }

  body_marker <- "copyright or other notice from copies of the content."
  if (str_detect(block, fixed(body_marker))) {
    body_raw <- str_split(block, fixed(body_marker), n = 2)[[1]][2]
  } else {
    body_raw <- block
  }
  body_text <- strip_rtf(body_raw)

  meta_raw <- str_split(block, fixed("Content and programming copyright"), n = 2)[[1]][1]
  meta_raw <- gsub("\\{[^{}]*\\\\pict[^{}]*\\}", " ", meta_raw, perl = TRUE)
  meta_clean <- strip_rtf(meta_raw)

  date_raw <- str_extract(meta_clean,
    "\\d{1,2} (?:January|February|March|April|May|June|July|August|September|October|November|December) \\d{4}|(?:January|February|March|April|May|June|July|August|September|October|November|December) \\d{1,2},? \\d{4}")
  date_parsed <- as.Date(NA_character_)
  for (fmt in c("%d %B %Y", "%B %d %Y", "%B %d, %Y")) {
    d <- tryCatch(as.Date(date_raw, format = fmt), error = function(e) as.Date(NA_character_))
    if (!is.na(d)) { date_parsed <- d; break }
  }

  source_line <- str_extract(meta_clean,
    paste0("NBC News:[^\\n]+|CBS News:[^\\n]+|ABC News:[^\\n]+|FOX News:[^\\n]+|",
           "PBS NewsHour[^\\n]*|MSNBC:[^\\n]+|",
           "The New York Times[^\\n]*|The Guardian[^\\n]*|",
           "The Wall Street Journal[^\\n]*|Washington Post[^\\n]*"))
  if (is.na(source_line)) {
    source_line <- str_extract(meta_clean, "(?:NBC|CBS|ABC|Fox|PBS|MSNBC)[^\\.\\n]{3,50}")
  }

  network <- file_network
  if (is.na(network) || network == "") {
    network <- str_extract(toupper(meta_clean), "\\b(NBC|CBS|ABC|FOX|PBS|MSNBC)\\b")
  }

  factiva_region <- str_extract(meta_clean, "News;?\\s*(?:Domestic|International)?")
  factiva_region <- str_squish(factiva_region %||% "")

  media_type <- if_else(
    str_detect(toupper(source_line %||% ""), "NBC|CBS|ABC|FOX|PBS|MSNBC"),
    "tv", "print"
  )

  show_clean <- source_line %||% ""
  show_clean <- str_replace(show_clean,
    "^(NBC|CBS|ABC|Fox|PBS|MSNBC) News(Hour)?(\\s+Channel)?:\\s*", "")
  show_clean <- str_replace(show_clean, "^MSNBC:\\s*", "")
  show_clean <- str_remove(show_clean, "\\s+[A-Z]{3,6}(\\s.*)?$")
  show_clean <- str_squish(show_clean)
  if (nchar(show_clean) > 60) show_clean <- NA_character_

  publication <- if (media_type == "print") {
    str_extract(source_line %||% "",
      "New York Times|The Guardian|Wall Street Journal|Washington Post|[A-Z][^,\\n]{3,40}")
  } else {
    NA_character_
  }

  meta_head <- substr(meta_clean, 1, 600)
  section_raw <- str_extract(meta_head,
    "(?i)(opinion|editorial|op.ed|commentary|review & outlook|letters to the editor|news|features?|world|politics?|business|economy)")
  is_opinion <- as.integer(
    str_detect(tolower(meta_head), "opinion|editorial|op-ed|op ed|commentary|review & outlook|letters to the editor") |
    str_detect(tolower(headline %||% ""), "opinion|editorial|op-ed|commentary")
  )

  is_us_story <- as.integer(
    media_type == "tv" | str_detect(factiva_region, "Domestic")
  )

  # --- Isolate the labor segment within the document ---
  # Many TV "documents" are full broadcast rundowns (multiple unrelated stories
  # stitched into one Factiva record, sometimes 10,000+ words). Counting phrases
  # over the whole rundown would pick up unrelated segments (Iran strikes,
  # weather, etc.), so we find the densest cluster of labor-anchor hits and
  # treat that cluster (with padding) as the actual labor story, discarding
  # the rest of the rundown from word_count/phrase counts/story_text.
  LABOR_ANCHORS <- c("union", "strike", "labor", "nlrb", "collective bargaining",
                     "worker", "workers", "picket")
  body_words  <- str_split(body_text, "\\s+")[[1]]
  body_lower_words <- tolower(body_words)

  anchor_hits <- sort(unique(unlist(lapply(LABOR_ANCHORS, function(a) {
    which(str_detect(body_lower_words, fixed(a)))
  }))))

  GAP_THRESHOLD <- 80   # word-distance gap that splits hits into separate clusters
  PAD_BEFORE <- 30
  PAD_AFTER  <- 150

  if (length(anchor_hits) > 0) {
    gaps <- c(0, diff(anchor_hits))
    cluster_id <- cumsum(gaps > GAP_THRESHOLD)
    cluster_sizes <- table(cluster_id)
    best_cluster <- as.integer(names(cluster_sizes)[which.max(cluster_sizes)])
    cluster_positions <- anchor_hits[cluster_id == best_cluster]

    seg_start <- max(1, min(cluster_positions) - PAD_BEFORE)
    seg_end   <- min(length(body_words), max(cluster_positions) + PAD_AFTER)

    labor_text  <- paste(body_words[seg_start:seg_end], collapse = " ")
    story_lede  <- paste(body_words[seg_start:min(seg_end, seg_start + 199)], collapse = " ")
  } else {
    # No anchor matched (shouldn't normally happen since file was retrieved
    # under the labor subject tag) — fall back to first 400 words.
    labor_text <- paste(head(body_words, 400), collapse = " ")
    story_lede <- paste(head(body_words, 200), collapse = " ")
  }

  lede_lower  <- tolower(story_lede)
  body_lower  <- tolower(labor_text)
  body_text   <- labor_text   # downstream word_count/story_text use the isolated segment

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
      core_labor_count = sum(sapply(
        c("union", "strike", "labor", "nlrb", "collective bargaining",
          "worker", "workers", "picket", "organizing", "contract negotiation"),
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

network_from_filename <- function(fname) {
  fname_upper <- toupper(basename(fname))
  if (str_detect(fname_upper, "MSNBC"))    return("MSNBC")
  if (str_detect(fname_upper, "NBC"))      return("NBC")
  if (str_detect(fname_upper, "CBS"))      return("CBS")
  if (str_detect(fname_upper, "ABC"))      return("ABC")
  if (str_detect(fname_upper, "FOX"))      return("FOX")
  if (str_detect(fname_upper, "PBS"))      return("PBS")
  if (str_detect(fname_upper, "NYT|NEW.YORK.TIMES")) return("NYT")
  if (str_detect(fname_upper, "GUARDIAN|GUARD")) return("GUARDIAN")
  if (str_detect(fname_upper, "WSJ|WALL.STREET"))    return("WSJ")
  return(NA_character_)
}

process_rtf_file <- function(filepath) {
  message("Processing: ", basename(filepath))

  raw <- read_file(filepath, locale = locale(encoding = "latin1"))
  network <- network_from_filename(filepath)

  BOOKMARK_MARKER <- paste0("{", "\\*\\bkmkstart toc")

  if (str_detect(raw, fixed(BOOKMARK_MARKER))) {
    story_blocks <- str_split(raw, fixed(BOOKMARK_MARKER))[[1]][-1]
  } else {
    story_blocks <- str_split(raw, fixed("\\page"))[[1]]
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

all_rtf <- list.files(RTF_DIR, pattern = "\\.rtf$", full.names = TRUE, ignore.case = TRUE)
rtf_files <- all_rtf[str_detect(tolower(basename(all_rtf)), "labor")]

if (length(rtf_files) == 0) {
  stop("No labor RTF files found in: ", RTF_DIR)
}

message("Found ", length(rtf_files), " labor RTF file(s)")

all_stories <- lapply(rtf_files, process_rtf_file)
df <- bind_rows(Filter(Negate(is.null), all_stories))

message("\nTotal stories parsed: ", nrow(df))

df <- df %>% arrange(story_date, network, show)

write_csv(df, OUTPUT_CSV, na = "")
message("Saved to: ", OUTPUT_CSV)

# ---------------------------------------------------------------------------
# SUMMARY STATS
# ---------------------------------------------------------------------------

message("\n=== Stories per network ===")
df %>% count(network, sort = TRUE) %>% print()

message("\n=== Date range ===")
message("Earliest: ", min(df$story_date, na.rm = TRUE))
message("Latest:   ", max(df$story_date, na.rm = TRUE))

phrase_cols <- names(PHRASE_LIST)
phrase_totals <- df %>%
  summarise(across(all_of(phrase_cols), sum, na.rm = TRUE)) %>%
  tidyr::pivot_longer(everything(), names_to = "phrase", values_to = "count") %>%
  arrange(desc(count)) %>%
  filter(count > 0)

message("\n=== Top phrases across all stories ===")
print(phrase_totals, n = 30)

message("\n=== Story triggers (all networks) ===")
df %>% count(story_trigger, sort = TRUE) %>% print()

message("\n=== Story triggers by network ===")
df %>%
  count(network, story_trigger) %>%
  tidyr::pivot_wider(names_from = story_trigger, values_from = n, values_fill = 0) %>%
  print()
