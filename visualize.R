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
