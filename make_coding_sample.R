library(readr)
library(dplyr)

set.seed(20260613)   # reproducible sample

d <- read_csv("news_stories.csv", show_col_types = FALSE) |>
  filter(
    media_type          == "print",
    !is.na(story_date),
    is_live_blog        == 0,
    core_abortion_count >= 3,
    !is.na(headline)
  ) |>
  mutate(
    period = case_when(
      story_date < as.Date("2022-06-24") ~ "pre_dobbs",
      story_date < as.Date("2025-01-20") ~ "post_dobbs",
      TRUE                               ~ "trump_ii"
    )
  )

# Stratified sample: 10 stories per network × period cell (take fewer if cell is small)
sample_n_safe <- function(df, n) {
  if (nrow(df) <= n) return(df)
  slice_sample(df, n = n)
}

sample_df <- d |>
  group_by(network, period) |>
  group_modify(~ sample_n_safe(.x, 10)) |>
  ungroup()

message(sprintf("Total stories in sample: %d", nrow(sample_df)))
message("Breakdown:")
print(count(sample_df, network, period))

# Columns to keep: identifiers + automated variables + 300-word lede for context
# Blank columns for manual coding
output <- sample_df |>
  mutate(
    # Truncate lede to ~300 words for the coding sheet
    lede_for_coding = sapply(story_lede, function(x) {
      words <- strsplit(if (is.na(x)) "" else x, "\\s+")[[1]]
      paste(words[1:min(300, length(words))], collapse = " ")
    })
  ) |>
  mutate(
    story_id = row_number(),
    # blank columns for manual coding
    coded_affected_woman  = NA_integer_,
    coded_provider        = NA_integer_,
    coded_politician      = NA_integer_,
    coded_expert          = NA_integer_,
    coded_religious       = NA_integer_,
    coded_anti_advocate   = NA_integer_,
    coded_pro_advocate    = NA_integer_,
    coded_source_count    = NA_integer_,
    coded_balance         = NA_character_,
    coded_primary_frame   = NA_character_,
    coded_is_opinion      = NA_integer_,
    coded_notes           = NA_character_
  ) |>
  select(
    story_id, network, period, story_date, headline, section, word_count,
    is_opinion, story_trigger,
    src_affected_woman, src_provider, src_politician,
    src_religious, src_expert,
    src_antiabortion_advocate, src_prochoice_advocate,
    pro_life, pro_choice, abortion_rights, abortion_ban,
    patient_impact, women_affected, medical_necessity,
    lede_for_coding,
    starts_with("coded_")
  )

write_csv(output, "manual_coding_sample.csv", na = "")
message(sprintf("Saved: manual_coding_sample.csv (%d rows, %d columns)",
                nrow(output), ncol(output)))
