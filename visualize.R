library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)      # for rollmean
library(scales)

# ---------------------------------------------------------------------------
# CONFIG
# ---------------------------------------------------------------------------

TV_HOURS_PER_WEEK <- c(
  ABC   = 13.5,
  CBS   = 13.5,
  NBC   = 18.5,
  FOX   = 67,
  MSNBC = 45,
  PBS   = 5
)

# Administration periods
ADMIN_PERIODS <- tibble(
  label      = c("Biden", "Trump II"),
  start      = as.Date(c("2021-01-20", "2025-01-20")),
  end        = as.Date(c("2025-01-19", "2026-06-30")),
  fill_color = c("#d6e4f0", "#fde8d8")
)

# Key event markers
KEY_EVENTS <- tibble(
  date  = as.Date(c("2022-06-24", "2025-01-20")),
  label = c("Dobbs", "Trump II"),
  color = c("firebrick", "darkorange")
)

# Bari Weiss joins CBS
WEISS_DATE <- as.Date("2025-10-01")

# ---------------------------------------------------------------------------
# DATA PREP
# ---------------------------------------------------------------------------

d <- read_csv("news_stories.csv", show_col_types = FALSE) |>
  filter(
    media_type       == "tv",
    !is.na(story_date),
    is_live_blog     == 0,
    core_abortion_count >= 3,
    network %in% names(TV_HOURS_PER_WEEK)
  )

# Monthly story counts per network
monthly <- d |>
  mutate(month = as.Date(format(story_date, "%Y-%m-01"))) |>
  count(network, month) |>
  # Fill in missing months with 0
  complete(network, month = seq(min(month), max(month), by = "month"),
           fill = list(n = 0))

# Compute hours aired per month (approx 4.33 weeks/month)
monthly <- monthly |>
  mutate(
    hrs_per_week  = TV_HOURS_PER_WEEK[network],
    hrs_per_month = hrs_per_week * 4.33,
    per_100hr     = n / hrs_per_month * 100
  )

# 3-month rolling mean to smooth small-n networks
monthly <- monthly |>
  arrange(network, month) |>
  group_by(network) |>
  mutate(per_100hr_smooth = rollmean(per_100hr, k = 3, fill = NA, align = "center")) |>
  ungroup()

# Network display order and colors
NET_COLORS <- c(
  ABC   = "#1f77b4",
  CBS   = "#2ca02c",
  NBC   = "#9467bd",
  FOX   = "#d62728",
  MSNBC = "#17becf",
  PBS   = "#8c564b"
)

monthly$network <- factor(monthly$network, levels = names(NET_COLORS))

# ---------------------------------------------------------------------------
# CHART 1: Normalized story volume over time
# ---------------------------------------------------------------------------

p1 <- ggplot(monthly, aes(x = month, y = per_100hr_smooth, color = network)) +

  # Administration background shading
  geom_rect(data = ADMIN_PERIODS,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = label),
            inherit.aes = FALSE, alpha = 0.12) +
  scale_fill_manual(values = c("Biden" = "#d6e4f0", "Trump II" = "#fde8d8"),
                    name = "Administration") +

  # Key event vertical lines
  geom_vline(data = KEY_EVENTS,
             aes(xintercept = date, color = label),
             linetype = "dashed", linewidth = 0.7, inherit.aes = FALSE) +
  scale_color_manual(
    values = c(NET_COLORS,
               "Dobbs"    = "firebrick",
               "Trump II" = "darkorange"),
    name = NULL
  ) +

  # Bari Weiss / CBS marker
  geom_vline(xintercept = WEISS_DATE, linetype = "dotted",
             color = "darkgreen", linewidth = 0.7) +
  annotate("text", x = WEISS_DATE, y = Inf,
           label = "Weiss/CBS", hjust = -0.1, vjust = 1.5,
           size = 2.8, color = "darkgreen") +

  # Lines
  geom_line(linewidth = 0.9, na.rm = TRUE) +

  # Event labels
  geom_text(data = KEY_EVENTS,
            aes(x = date, y = Inf, label = label, color = label),
            hjust = -0.15, vjust = 1.5, size = 2.8, inherit.aes = FALSE) +

  scale_x_date(date_breaks = "6 months", date_labels = "%b '%y",
               expand = expansion(mult = c(0.01, 0.03))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +

  labs(
    title    = "Abortion Coverage by TV Network, 2021–2026",
    subtitle = "Stories per 100 hours of airtime · 3-month rolling average · Stories with 3+ core abortion mentions",
    x        = NULL,
    y        = "Stories per 100 hours",
    caption  = "Sources: Factiva RTF exports; airtime estimates based on network schedules"
  ) +

  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 9, color = "gray40"),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    legend.position  = "right",
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 7, color = "gray50")
  ) +

  guides(
    color = guide_legend(order = 1, override.aes = list(linewidth = 1.5)),
    fill  = guide_legend(order = 2)
  )

ggsave("chart1_tv_volume_normalized.png", p1,
       width = 12, height = 6, dpi = 150)

message("Saved: chart1_tv_volume_normalized.png")

# ---------------------------------------------------------------------------
# CHART 1b: Raw story counts (no normalization)
# ---------------------------------------------------------------------------

monthly <- monthly |>
  arrange(network, month) |>
  group_by(network) |>
  mutate(n_smooth = rollmean(n, k = 3, fill = NA, align = "center")) |>
  ungroup()

p1b <- ggplot(monthly, aes(x = month, y = n_smooth, color = network)) +

  geom_rect(data = ADMIN_PERIODS,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = label),
            inherit.aes = FALSE, alpha = 0.12) +
  scale_fill_manual(values = c("Biden" = "#d6e4f0", "Trump II" = "#fde8d8"),
                    name = "Administration") +

  geom_vline(data = KEY_EVENTS,
             aes(xintercept = date, color = label),
             linetype = "dashed", linewidth = 0.7, inherit.aes = FALSE) +
  scale_color_manual(
    values = c(NET_COLORS,
               "Dobbs"    = "firebrick",
               "Trump II" = "darkorange"),
    name = NULL
  ) +

  geom_vline(xintercept = WEISS_DATE, linetype = "dotted",
             color = "darkgreen", linewidth = 0.7) +
  annotate("text", x = WEISS_DATE, y = Inf,
           label = "Weiss/CBS", hjust = -0.1, vjust = 1.5,
           size = 2.8, color = "darkgreen") +

  geom_line(linewidth = 0.9, na.rm = TRUE) +

  geom_text(data = KEY_EVENTS,
            aes(x = date, y = Inf, label = label, color = label),
            hjust = -0.15, vjust = 1.5, size = 2.8, inherit.aes = FALSE) +

  scale_x_date(date_breaks = "6 months", date_labels = "%b '%y",
               expand = expansion(mult = c(0.01, 0.03))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +

  labs(
    title    = "Abortion Coverage by TV Network, 2021–2026",
    subtitle = "Raw story count per month · 3-month rolling average · Stories with 3+ core abortion mentions",
    x        = NULL,
    y        = "Stories per month",
    caption  = "Sources: Factiva RTF exports"
  ) +

  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 9, color = "gray40"),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    legend.position  = "right",
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 7, color = "gray50")
  ) +

  guides(
    color = guide_legend(order = 1, override.aes = list(linewidth = 1.5)),
    fill  = guide_legend(order = 2)
  )

ggsave("chart1b_tv_volume_raw.png", p1b,
       width = 12, height = 6, dpi = 150)

message("Saved: chart1b_tv_volume_raw.png")

# ---------------------------------------------------------------------------
# CHART 2: Framing language trends over time, by network
# Four phrase pairs showing opposing framings
# Expressed as mentions per 100 stories, 3-month rolling average
# ---------------------------------------------------------------------------

# Define phrase groups — each group is summed into one score
PHRASE_GROUPS <- list(
  "Pro-life framing"      = c("pro_life", "label_prolife", "called_prolife"),
  "Pro-choice framing"    = c("pro_choice", "label_prochoice", "called_prochoice",
                               "label_abortionrights"),
  "Fetal/unborn language" = c("fetus", "unborn", "baby_fetus", "preborn"),
  "Pregnant women/people" = c("pregnant_women", "pregnant_people", "mother_language"),
  "Ban/restriction frame" = c("abortion_ban", "abortion_restriction"),
  "Rights/access frame"   = c("abortion_rights", "abortion_access", "reproductive_rights",
                               "bodily_autonomy"),
  "Patient impact"        = c("patient_impact", "women_affected"),
  "Medical/clinical"      = c("medical_necessity")
)

# Pair labels for faceting (4 pairs, each with 2 lines)
PAIRS <- list(
  list(a = "Pro-life framing",      b = "Pro-choice framing",
       label = "Pro-life vs. Pro-choice framing"),
  list(a = "Fetal/unborn language", b = "Pregnant women/people",
       label = "Fetal/unborn vs. Pregnant women framing"),
  list(a = "Ban/restriction frame", b = "Rights/access frame",
       label = "Ban/restriction vs. Rights/access framing"),
  list(a = "Patient impact",        b = "Medical/clinical",
       label = "Patient impact vs. Medical/clinical language")
)

tv_framing <- d |>
  filter(media_type == "tv", is_live_blog == 0,
         core_abortion_count >= 3, !is.na(story_date),
         network %in% names(NET_COLORS)) |>
  mutate(month = as.Date(format(story_date, "%Y-%m-01")))

# Pre-compute group sums as new columns, then aggregate monthly
for (grp_name in names(PHRASE_GROUPS)) {
  cols <- intersect(PHRASE_GROUPS[[grp_name]], names(tv_framing))
  tv_framing[[grp_name]] <- rowSums(tv_framing[, cols, drop = FALSE], na.rm = TRUE)
}

grp_names <- names(PHRASE_GROUPS)

rates <- tv_framing |>
  group_by(network, month) |>
  summarise(
    n_stories = n(),
    across(all_of(grp_names), \(x) sum(x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  mutate(across(all_of(grp_names),
                \(x) x / n_stories * 100,
                .names = "rate_{.col}"))

# Build long-form data for each pair
pair_data <- lapply(PAIRS, function(p) {
  rates |>
    mutate(
      val_a = .data[[paste0("rate_", p$a)]],
      val_b = .data[[paste0("rate_", p$b)]]
    ) |>
    select(network, month, val_a, val_b) |>
    tidyr::pivot_longer(c(val_a, val_b),
                        names_to = "side",
                        values_to = "rate_per_100") |>
    mutate(
      phrase_group = if_else(side == "val_a", p$a, p$b),
      pair_label   = p$label,
      line_type    = if_else(side == "val_a", "solid", "dashed")
    )
}) |> bind_rows()

# Smooth within network + phrase_group
pair_data <- pair_data |>
  arrange(network, phrase_group, month) |>
  group_by(network, phrase_group) |>
  mutate(rate_smooth = rollmean(rate_per_100, k = 3, fill = NA, align = "center")) |>
  ungroup()

pair_data$network     <- factor(pair_data$network, levels = names(NET_COLORS))
pair_data$pair_label  <- factor(pair_data$pair_label,
                                 levels = sapply(PAIRS, `[[`, "label"))

# Colour: use phrase group label mapped to two colours per pair
FRAME_COLORS <- c(
  "Pro-life framing"      = "#d62728",
  "Pro-choice framing"    = "#1f77b4",
  "Fetal/unborn language" = "#d62728",
  "Pregnant women/people" = "#1f77b4",
  "Ban/restriction frame" = "#d62728",
  "Rights/access frame"   = "#1f77b4",
  "Patient impact"        = "#2ca02c",
  "Medical/clinical"      = "#9467bd"
)

p2 <- ggplot(pair_data,
             aes(x = month, y = rate_smooth,
                 color = phrase_group, linetype = phrase_group,
                 group = interaction(network, phrase_group))) +

  # Admin shading
  geom_rect(data = ADMIN_PERIODS,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = label),
            inherit.aes = FALSE, alpha = 0.10) +
  scale_fill_manual(values = c("Biden" = "#d6e4f0", "Trump II" = "#fde8d8"),
                    name = "Administration") +

  # Dobbs line
  geom_vline(xintercept = as.Date("2022-06-24"),
             linetype = "dashed", color = "firebrick", linewidth = 0.5) +

  geom_line(linewidth = 0.75, na.rm = TRUE) +

  scale_color_manual(values = FRAME_COLORS, name = "Phrase group") +
  scale_linetype_manual(
    values = setNames(
      rep(c("solid","dashed"), 4),
      c("Pro-life framing","Pro-choice framing",
        "Fetal/unborn language","Pregnant women/people",
        "Ban/restriction frame","Rights/access frame",
        "Patient impact","Medical/clinical")
    ),
    name = "Phrase group"
  ) +

  facet_grid(pair_label ~ network, scales = "free_y") +

  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.02))) +

  labs(
    title    = "Abortion Framing Language by Network, 2021–2026",
    subtitle = "Mentions per 100 stories · 3-month rolling average · TV only",
    x        = NULL,
    y        = "Mentions per 100 stories",
    caption  = "Dashed vertical line = Dobbs decision (Jun 2022). Sources: Factiva RTF exports"
  ) +

  theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 8, color = "gray40"),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y      = element_text(size = 7),
    strip.text.x     = element_text(face = "bold", size = 9),
    strip.text.y     = element_text(size = 7.5, angle = 0, hjust = 0),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 7, color = "gray50")
  )

ggsave("chart2_framing_language.png", p2,
       width = 16, height = 12, dpi = 150)

message("Saved: chart2_framing_language.png")
