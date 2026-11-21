# -----------------------------------------------------------------------------
# Script:   00_theme.R
# Purpose:  One place for the look of every chart in the project: Amazon-style
#           colours (white, orange, black), my ggplot2 theme, colour scales,
#           number formats and a helper that saves charts as 300 dpi PNGs.
#           Sourced by 05_visualize.R and 06_dashboard.Rmd.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})

# Palette ---------------------------------------------------------------------
# Colours picked to match the Amazon site
amz <- list(
  orange       = "#FF9900",   # main accent, used for the key series
  orange_dark  = "#C45500",
  orange_light = "#FFC266",
  orange_pale  = "#FFF3E0",
  black        = "#131921",   # titles and headline numbers
  ink          = "#232F3E",   # second series, contrast to orange
  slate        = "#4F5B66",
  grey         = "#8D99A6",
  grey_light   = "#D5D9D9",
  grid         = "#ECECEC",
  white        = "#FFFFFF",
  text_muted   = "#565959"
)

# Category colours always go in this order, so a series keeps the same colour
# from chart to chart. Six colours max, they are never recycled.
amz_categorical <- c(amz$orange, amz$ink, amz$orange_dark, amz$grey, amz$orange_light, amz$slate)

# For heatmaps: low values white, high values dark orange
amz_sequential <- c(amz$white, amz$orange_pale, amz$orange_light, amz$orange, amz$orange_dark)

# Theme -----------------------------------------------------------------------
# Minimal base, only horizontal gridlines, titles aligned left, legend on top.
theme_amz <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.background    = element_rect(fill = amz$white, colour = NA),
      panel.background   = element_rect(fill = amz$white, colour = NA),
      panel.grid.major.y = element_line(colour = amz$grid, linewidth = 0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.line.x        = element_line(colour = amz$grey_light, linewidth = 0.5),
      axis.ticks         = element_blank(),
      axis.text          = element_text(colour = amz$text_muted, size = rel(0.85)),
      axis.title         = element_text(colour = amz$text_muted, size = rel(0.85)),
      axis.title.y       = element_text(colour = amz$text_muted, size = rel(0.85), angle = 90,
                                        margin = margin(r = 8)),
      axis.title.x       = element_text(colour = amz$text_muted, size = rel(0.85),
                                        margin = margin(t = 8)),
      plot.title         = element_text(colour = amz$black, face = "bold", size = rel(1.35),
                                        hjust = 0, margin = margin(b = 4)),
      plot.subtitle      = element_text(colour = amz$text_muted, size = rel(0.95),
                                        hjust = 0, margin = margin(b = 14)),
      plot.caption       = element_text(colour = amz$grey, size = rel(0.75), hjust = 0,
                                        margin = margin(t = 12)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.margin        = margin(18, 22, 14, 18),
      legend.position    = "top",
      legend.justification = "left",
      legend.title       = element_blank(),
      legend.text        = element_text(colour = amz$black, size = rel(0.85)),
      legend.key.size    = unit(0.9, "lines"),
      legend.margin      = margin(0, 0, 4, 0),
      strip.text         = element_text(colour = amz$black, face = "bold", size = rel(0.9),
                                        hjust = 0, margin = margin(b = 6, t = 6)),
      panel.spacing      = unit(1.2, "lines")
    )
}

# Set as the default, plus orange as the default geom colour, so the charts
# don't need it set every time
theme_set(theme_amz())
update_geom_defaults("line",  list(colour = amz$orange, linewidth = 1))
update_geom_defaults("point", list(colour = amz$orange, size = 2.4))
update_geom_defaults("col",   list(fill = amz$orange))
update_geom_defaults("bar",   list(fill = amz$orange))
update_geom_defaults("text",  list(colour = amz$black, size = 3.4))

# Scales ----------------------------------------------------------------------
scale_colour_amz <- function(...) scale_colour_manual(values = amz_categorical, ...)
scale_fill_amz   <- function(...) scale_fill_manual(values = amz_categorical, ...)
scale_fill_amz_seq <- function(...) scale_fill_gradientn(colours = amz_sequential, ...)

# Number formats --------------------------------------------------------------
# Short dollar labels for axes and bars: $950, $63K, $5.4M
fmt_usd <- function(x) {
  out <- ifelse(abs(x) >= 1e6, paste0("$", formatC(x / 1e6, format = "f", digits = 1), "M"),
         ifelse(abs(x) >= 1e3, paste0("$", formatC(x / 1e3, format = "f", digits = 0), "K"),
                paste0("$", formatC(x, format = "f", digits = 0))))
  out[is.na(x)] <- NA
  out
}
fmt_usd0  <- label_dollar(accuracy = 1)
fmt_pct   <- label_percent(accuracy = 1)
fmt_pct1  <- label_percent(accuracy = 0.1)
fmt_delta <- function(x) ifelse(is.na(x), "", sprintf("%+.1f%%", 100 * x))

# Same source line under every chart
chart_caption <- "Source: Amazon synthetic e-commerce dataset, 2021-2025"

# Export ----------------------------------------------------------------------
# Saves to Output/charts. Uses ragg if it is installed (sharper text),
# otherwise the normal PNG device.
save_chart <- function(plot, name, width = 10, height = 6, dpi = 300, dir = "Output/charts") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  file <- file.path(dir, paste0(name, ".png"))
  if (requireNamespace("ragg", quietly = TRUE)) {
    ggsave(file, plot, width = width, height = height, dpi = dpi, device = ragg::agg_png, bg = "white")
  } else {
    ggsave(file, plot, width = width, height = height, dpi = dpi, bg = "white")
  }
  invisible(file)
}
