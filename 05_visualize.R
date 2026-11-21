# -----------------------------------------------------------------------------
# Script:   05_visualize.R
# Purpose:  Build all charts for the report and dashboard, grouped by
#           dashboard page: sales overview, products and categories,
#           customers, marketing and channels, fulfilment and quality. Each
#           chart is saved as a 300 dpi PNG and also kept in a list of ggplot
#           objects for 06_dashboard.Rmd.
#
# Input:    Output/03_analysis_tables.rds, R/00_theme.R
# Output:   Output/charts/*.png, Output/05_charts.rds
# -----------------------------------------------------------------------------

# Packages --------------------------------------------------------------------
pkgs <- c("dplyr", "tidyr", "lubridate", "ggplot2", "scales", "forcats", "ragg")
missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(forcats)
})

# Theme file is found whether the scripts sit in R/ or in the project root
theme_file <- if (file.exists("R/00_theme.R")) "R/00_theme.R" else "00_theme.R"
source(theme_file)

output_dir <- "Output"
a <- readRDS(file.path(output_dir, "03_analysis_tables.rds"))
# Every chart goes into this list and is exported at the end
charts <- list()

years       <- sort(unique(a$kpi_year$year))
latest_year <- max(years)

# =============================================================================
# Page 1 - Sales overview
# =============================================================================

# Monthly revenue trend -------------------------------------------------------
# Revenue as area + line, dashed 3-month average, label on each year's peak
trend <- a$kpi_month %>% select(month, revenue, revenue_3m_avg)
peaks <- trend %>% group_by(year = year(month)) %>% slice_max(revenue, n = 1) %>% ungroup()

charts$revenue_trend <- ggplot(trend, aes(month, revenue)) +
  geom_area(fill = amz$orange_pale, colour = NA) +
  geom_line(colour = amz$orange, linewidth = 1.1) +
  geom_line(aes(y = revenue_3m_avg), colour = amz$ink, linewidth = 0.7, linetype = "22", na.rm = TRUE) +
  geom_point(data = peaks, colour = amz$orange, size = 2.8) +
  geom_text(data = peaks, aes(label = fmt_usd(revenue)), vjust = -1, size = 3.2, fontface = "bold") +
  scale_y_continuous(labels = fmt_usd, expand = expansion(mult = c(0, 0.15))) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y", expand = expansion(mult = c(0.01, 0.03))) +
  coord_cartesian(clip = "off") +
  labs(title = "Monthly revenue keeps climbing, with bigger holiday peaks each year",
       subtitle = "Net item revenue per month (orange) and 3-month moving average (dashed). Labels mark each year's peak month.",
       x = NULL, y = NULL, caption = chart_caption)

# Yearly revenue and growth ---------------------------------------------------
# Amount on top of each bar, YoY growth at the bottom
yr <- a$kpi_year %>% mutate(year = factor(year))

charts$revenue_year <- ggplot(yr, aes(year, revenue)) +
  geom_col(width = 0.62, fill = amz$orange) +
  geom_text(aes(label = fmt_usd(revenue)), vjust = -0.6, fontface = "bold", size = 3.8) +
  geom_text(aes(y = 0, label = ifelse(is.na(revenue_yoy), "Base year", paste(fmt_delta(revenue_yoy), "YoY"))),
            vjust = -1.2, colour = amz$black, size = 3.3) +
  scale_y_continuous(labels = fmt_usd, expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Revenue grew every year, but the pace changed",
       subtitle = "Annual net item revenue with year-over-year growth",
       x = NULL, y = NULL, caption = chart_caption)

# Actual vs target ------------------------------------------------------------
# Target drawn as a black line across each bar, label = % of target achieved
tgt <- a$kpi_year %>% mutate(year = factor(year))

charts$revenue_vs_target <- ggplot(tgt, aes(year)) +
  geom_col(aes(y = revenue, fill = "Actual"), width = 0.62) +
  geom_errorbar(aes(ymin = target, ymax = target, colour = "Target"), width = 0.75, linewidth = 1.3) +
  geom_text(aes(y = pmax(revenue, target), label = fmt_pct(target_attainment)),
            vjust = -0.9, fontface = "bold", size = 3.6) +
  scale_fill_manual(values = c(Actual = amz$orange)) +
  scale_colour_manual(values = c(Target = amz$black)) +
  scale_y_continuous(labels = fmt_usd, expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Targets were missed in 2022 and 2023, beaten in 2024 and 2025",
       subtitle = "Actual revenue vs annual target, label = % of target achieved",
       x = NULL, y = NULL, caption = chart_caption)

# Core KPIs by year (small multiples) -----------------------------------------
# Four KPIs side by side, each with its own y scale and values on the points
kpi_long <- a$kpi_year %>%
  transmute(year, `Orders` = orders, `Average order value` = aov, `Gross margin` = gross_margin,
            `Active customers` = customers) %>%
  pivot_longer(-year, names_to = "kpi") %>%
  mutate(kpi = factor(kpi, levels = c("Orders", "Active customers", "Average order value", "Gross margin")),
         label = case_when(kpi == "Gross margin" ~ sprintf("%.1f%%", 100 * value),
                           kpi == "Average order value" ~ paste0("$", round(value)),
                           TRUE ~ format(round(value), big.mark = ",", trim = TRUE)))

charts$kpi_multiples <- ggplot(kpi_long, aes(year, value)) +
  geom_line(colour = amz$orange, linewidth = 1.1) +
  geom_point(colour = amz$orange, size = 2.6) +
  geom_text(aes(label = label), vjust = -1, size = 3) +
  facet_wrap(~ kpi, scales = "free_y", nrow = 1) +
  scale_y_continuous(expand = expansion(mult = c(0.15, 0.3)), labels = NULL) +
  scale_x_continuous(breaks = years, expand = expansion(add = 0.45)) +
  coord_cartesian(clip = "off") +
  labs(title = "Core sales KPIs by year",
       subtitle = "Each panel has its own scale",
       x = NULL, y = NULL, caption = chart_caption) +
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_blank())

# Seasonality heatmap ---------------------------------------------------------
# Same index as in 04 (100 = average month), white text on the darkest cells
season <- a$kpi_month %>%
  group_by(year) %>%
  mutate(index = revenue / mean(revenue) * 100) %>%
  ungroup() %>%
  mutate(month_lbl = month(month, label = TRUE), year = factor(year))

charts$seasonality <- ggplot(season, aes(month_lbl, fct_rev(year), fill = index)) +
  geom_tile(colour = amz$white, linewidth = 1.2) +
  geom_text(aes(label = round(index), colour = index > 150), size = 3.2, show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = amz$white, `FALSE` = amz$black)) +
  scale_fill_amz_seq(name = "Index", limits = c(50, 170), oob = squish) +
  labs(title = "November-December and July drive the year",
       subtitle = "Monthly revenue index, 100 = average month of that year",
       x = NULL, y = NULL, caption = chart_caption) +
  theme(panel.grid = element_blank(), axis.line.x = element_blank(),
        legend.position = "right", legend.title = element_text(colour = amz$text_muted, size = 9))

# Revenue by country ----------------------------------------------------------
# Stacked by country to show what UK and DE add
geo <- a$geo_year %>%
  group_by(year, country) %>%
  summarise(revenue = sum(revenue), .groups = "drop") %>%
  mutate(country = factor(country, levels = c("US", "CA", "UK", "DE")))

charts$country_mix <- ggplot(geo, aes(factor(year), revenue, fill = country)) +
  geom_col(width = 0.62, colour = amz$white, linewidth = 0.5) +
  scale_fill_manual(values = c(US = amz$ink, CA = amz$grey, UK = amz$orange, DE = amz$orange_light)) +
  scale_y_continuous(labels = fmt_usd, expand = expansion(mult = c(0, 0.05))) +
  labs(title = "International expansion adds a growing slice of revenue",
       subtitle = "Revenue by customer country. UK launched in 2023, Germany in 2024",
       x = NULL, y = NULL, caption = chart_caption)

# =============================================================================
# Page 2 - Products and categories
# =============================================================================

# Category share, first vs latest year (dumbbell) -----------------------------
# Grey dot = first year, coloured dot = latest year. Orange if the category
# gained share.
share_cmp <- a$category_year %>%
  filter(year %in% c(min(years), latest_year)) %>%
  select(year, top_category, revenue_share) %>%
  mutate(period = if_else(year == latest_year, "latest", "first")) %>%
  select(-year) %>%
  pivot_wider(names_from = period, values_from = revenue_share) %>%
  mutate(direction = if_else(latest >= first, "Gained share", "Lost share"),
         top_category = fct_reorder(top_category, latest))

charts$category_slope <- ggplot(share_cmp, aes(y = top_category)) +
  geom_segment(aes(x = first, xend = latest, yend = top_category, colour = direction), linewidth = 1.6) +
  geom_point(aes(x = first), colour = amz$grey, size = 3.4) +
  geom_point(aes(x = latest, colour = direction), size = 3.8) +
  geom_text(aes(x = latest, label = fmt_pct1(latest), hjust = if_else(latest >= first, -0.35, 1.35)),
            size = 3.2) +
  scale_colour_manual(values = c(`Gained share` = amz$orange, `Lost share` = amz$ink)) +
  scale_x_continuous(labels = fmt_pct, expand = expansion(mult = c(0.08, 0.1))) +
  labs(title = "Electronics loses share as Home, Beauty, Pet and Grocery grow",
       subtitle = paste0("Share of revenue by category: grey dot = ", min(years),
                         ", coloured dot = ", latest_year),
       x = NULL, y = NULL, caption = chart_caption) +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = amz$grid),
        axis.line.x = element_blank())

# Category growth, latest year ------------------------------------------------
# YoY change per category, declines shown in dark
cat_growth <- a$category_year %>%
  filter(year == latest_year) %>%
  mutate(top_category = fct_reorder(top_category, revenue_yoy),
         sign = if_else(revenue_yoy >= 0, "Growth", "Decline"))

charts$category_growth <- ggplot(cat_growth, aes(revenue_yoy, top_category, fill = sign)) +
  geom_col(width = 0.65) +
  geom_vline(xintercept = 0, colour = amz$grey, linewidth = 0.5) +
  geom_text(aes(label = fmt_delta(revenue_yoy), hjust = if_else(revenue_yoy >= 0, -0.15, 1.15)),
            size = 3.3) +
  scale_fill_manual(values = c(Growth = amz$orange, Decline = amz$ink), guide = "none") +
  scale_x_continuous(labels = fmt_pct, expand = expansion(mult = 0.2)) +
  labs(title = paste("Revenue growth by category,", latest_year),
       subtitle = "Year-over-year change in net item revenue",
       x = NULL, y = NULL, caption = chart_caption) +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = amz$grid),
        axis.line.x = element_blank())

# Category revenue over time (small multiples) --------------------------------
# Free y scales so the smaller categories still show their shape
cat_month <- a$category_month %>%
  group_by(top_category) %>%
  mutate(total = sum(revenue)) %>%
  ungroup() %>%
  mutate(top_category = fct_reorder(top_category, total, .desc = TRUE))

charts$category_trend <- ggplot(cat_month, aes(month, revenue)) +
  geom_area(fill = amz$orange_pale) +
  geom_line(colour = amz$orange, linewidth = 0.7) +
  facet_wrap(~ top_category, ncol = 3, scales = "free_y") +
  scale_y_continuous(labels = fmt_usd, n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Monthly revenue by category",
       subtitle = "Each panel has its own scale, so shapes and trends are comparable",
       x = NULL, y = NULL, caption = chart_caption)

# Top 10 products -------------------------------------------------------------
# Labelled with product id and subcategory, sorted by revenue
top10 <- a$product_summary %>%
  slice_max(revenue, n = 10) %>%
  mutate(label = paste0("#", product_id, " ", subcategory),
         label = fct_reorder(label, revenue))

charts$top_products <- ggplot(top10, aes(revenue, label)) +
  geom_col(width = 0.65, fill = amz$orange) +
  geom_text(aes(label = fmt_usd(revenue)), hjust = -0.15, size = 3.3) +
  scale_x_continuous(labels = fmt_usd, expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Top 10 products by revenue, 2021-2025",
       subtitle = "Product id and subcategory",
       x = NULL, y = NULL, caption = chart_caption) +
  theme(panel.grid.major.y = element_blank(), axis.line.x = element_blank())

# 1P vs 3P --------------------------------------------------------------------
# Revenue share by seller type, 100% stacked
charts$seller_mix <- ggplot(a$seller_year, aes(factor(year), revenue_share, fill = seller_type)) +
  geom_col(width = 0.62, colour = amz$white, linewidth = 0.6) +
  geom_text(aes(label = fmt_pct(revenue_share)), position = position_stack(vjust = 0.5),
            colour = amz$white, fontface = "bold", size = 3.4) +
  scale_fill_manual(values = c(`1P` = amz$ink, `3P` = amz$orange),
                    labels = c(`1P` = "First-party (1P)", `3P` = "Marketplace sellers (3P)")) +
  scale_y_continuous(labels = fmt_pct, expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Marketplace sellers generate two-thirds of revenue",
       subtitle = "Share of revenue by seller type",
       x = NULL, y = NULL, caption = chart_caption)

# =============================================================================
# Page 3 - Customers and retention
# =============================================================================

# New vs returning ------------------------------------------------------------
# Stacked revenue with each part's share written inside
nr <- a$orders_enriched %>%
  filter(!is_cancelled) %>%
  group_by(year = order_year, customer_type) %>%
  summarise(revenue = sum(items_subtotal), .groups = "drop") %>%
  group_by(year) %>% mutate(share = revenue / sum(revenue)) %>% ungroup()

charts$new_vs_returning <- ggplot(nr, aes(factor(year), revenue, fill = customer_type)) +
  geom_col(width = 0.62, colour = amz$white, linewidth = 0.6) +
  geom_text(aes(label = fmt_pct(share)), position = position_stack(vjust = 0.5),
            colour = amz$white, fontface = "bold", size = 3.4) +
  scale_fill_manual(values = c(New = amz$ink, Returning = amz$orange)) +
  scale_y_continuous(labels = fmt_usd, expand = expansion(mult = c(0, 0.04))) +
  labs(title = "Returning customers now bring most of the revenue",
       subtitle = "Revenue from first orders vs repeat orders",
       x = NULL, y = NULL, caption = chart_caption)

# Cohort retention heatmap ----------------------------------------------------
# Months 1-12 only, month 0 is always 100%
coh <- a$cohort_year_retention %>%
  filter(months_since >= 1, months_since <= 12) %>%
  mutate(cohort_year = factor(cohort_year))

charts$cohort_retention <- ggplot(coh, aes(factor(months_since), fct_rev(cohort_year), fill = retention)) +
  geom_tile(colour = amz$white, linewidth = 1.2) +
  geom_text(aes(label = fmt_pct1(retention), colour = retention > 0.105), size = 3, show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = amz$white, `FALSE` = amz$black)) +
  scale_fill_amz_seq(labels = fmt_pct, name = NULL) +
  labs(title = "Newer customer cohorts come back more often",
       subtitle = "Share of first-time buyers who order again N months after their first order",
       x = "Months since first order", y = "Cohort (year of first order)", caption = chart_caption) +
  theme(panel.grid = element_blank(), axis.line.x = element_blank(), legend.position = "right")

# Prime share of orders -------------------------------------------------------
charts$prime_share <- ggplot(a$kpi_year, aes(factor(year), prime_share)) +
  geom_col(width = 0.62, fill = amz$orange) +
  geom_text(aes(label = fmt_pct(prime_share)), vjust = -0.6, fontface = "bold", size = 3.6) +
  scale_y_continuous(labels = fmt_pct, expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Prime members' share of orders more than quadrupled",
       subtitle = "Share of orders placed by Prime members",
       x = NULL, y = NULL, caption = chart_caption)

# RFM segments ----------------------------------------------------------------
# Share of customers next to share of revenue for each segment
rfm <- a$customer_summary %>%
  group_by(rfm_segment) %>%
  summarise(customers = n(), revenue = sum(revenue), .groups = "drop") %>%
  mutate(share_rev = revenue / sum(revenue), share_cust = customers / sum(customers),
         rfm_segment = fct_reorder(rfm_segment, share_rev)) %>%
  pivot_longer(c(share_cust, share_rev), names_to = "metric", values_to = "share") %>%
  mutate(metric = recode(metric, share_cust = "Share of customers", share_rev = "Share of revenue"))

charts$rfm_segments <- ggplot(rfm, aes(share, rfm_segment, fill = metric)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = fmt_pct(share)), position = position_dodge(width = 0.75),
            hjust = -0.2, size = 3.1) +
  scale_fill_manual(values = c(`Share of customers` = amz$grey, `Share of revenue` = amz$orange)) +
  scale_x_continuous(labels = fmt_pct, expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Champions are 9% of customers but over a quarter of revenue",
       subtitle = "RFM segments (recency, frequency, monetary) as of 31 Dec 2025",
       x = NULL, y = NULL, caption = chart_caption) +
  theme(panel.grid.major.y = element_blank(), axis.line.x = element_blank())

# =============================================================================
# Page 4 - Marketing and channels
# =============================================================================

# Channel revenue share -------------------------------------------------------
# Readable channel names and one fixed colour per channel, reused in the paid
# efficiency chart
ch_levels <- c("organic_search", "paid_search", "social_media", "email", "affiliate", "direct")
ch_labels <- c("Organic search", "Paid search", "Social media", "Email", "Affiliate", "Direct")
ch_cols   <- setNames(amz_categorical, ch_labels)

ch <- a$channel_year %>%
  mutate(channel = factor(channel, levels = ch_levels, labels = ch_labels))

charts$channel_share <- ggplot(ch, aes(year, revenue_share, colour = channel)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.6) +
  scale_colour_manual(values = ch_cols) +
  scale_y_continuous(labels = fmt_pct) +
  scale_x_continuous(breaks = years) +
  labs(title = "Social media is the fastest-growing sales channel",
       subtitle = "Share of revenue by traffic source",
       x = NULL, y = NULL, caption = chart_caption)

# ROAS and CAC for paid channels ----------------------------------------------
# Two panels instead of a dual axis. Channels without spend are left out.
paid <- ch %>%
  filter(spend > 0) %>%
  select(year, channel, ROAS = roas, `CAC (USD)` = cac) %>%
  pivot_longer(c(ROAS, `CAC (USD)`), names_to = "metric")

charts$paid_efficiency <- ggplot(paid, aes(year, value, colour = channel)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  facet_wrap(~ metric, scales = "free_y") +
  scale_colour_manual(values = ch_cols) +
  scale_x_continuous(breaks = years) +
  labs(title = "Paid marketing efficiency recovers after 2022",
       subtitle = "Return on ad spend (revenue / spend) and cost per new customer, paid channels only",
       x = NULL, y = NULL, caption = chart_caption)

# Conversion rate -------------------------------------------------------------
# Orders / sessions per month with a loess trend line
charts$conversion <- ggplot(a$kpi_month, aes(month, conversion_rate)) +
  geom_line(colour = amz$orange, linewidth = 1) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = amz$ink,
              linewidth = 0.7, linetype = "22", span = 0.5) +
  scale_y_continuous(labels = fmt_pct1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Site conversion dipped in 2022 and has improved since",
       subtitle = "Orders / sessions per month, dashed = trend",
       x = NULL, y = NULL, caption = chart_caption)

# Device and payment mix ------------------------------------------------------
# Device as 100% stacked bars, payment methods as one line each
dev <- a$device_year %>%
  mutate(device = factor(device, levels = c("desktop", "mobile_web", "mobile_app"),
                         labels = c("Desktop", "Mobile web", "Mobile app")))

charts$device_mix <- ggplot(dev, aes(factor(year), share, fill = device)) +
  geom_col(width = 0.62, colour = amz$white, linewidth = 0.6) +
  geom_text(aes(label = fmt_pct(share)), position = position_stack(vjust = 0.5),
            colour = amz$white, fontface = "bold", size = 3.2) +
  scale_fill_manual(values = c(Desktop = amz$ink, `Mobile web` = amz$grey, `Mobile app` = amz$orange)) +
  scale_y_continuous(labels = fmt_pct, expand = expansion(mult = c(0, 0.02))) +
  labs(title = "The mobile app almost doubles its share of orders",
       subtitle = "Share of orders by device", x = NULL, y = NULL, caption = chart_caption)

pay <- a$payment_year %>%
  mutate(payment_method = factor(payment_method,
                                 levels = c("credit_card", "debit_card", "digital_wallet", "bnpl", "gift_card"),
                                 labels = c("Credit card", "Debit card", "Digital wallet", "Buy now pay later", "Gift card")))

charts$payment_mix <- ggplot(pay, aes(year, share, colour = payment_method)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.4) +
  scale_colour_manual(values = setNames(amz_categorical[c(2, 4, 1, 3, 5)], levels(pay$payment_method))) +
  scale_y_continuous(labels = fmt_pct) +
  scale_x_continuous(breaks = years) +
  labs(title = "Digital wallets and buy-now-pay-later replace cards",
       subtitle = "Share of orders by payment method", x = NULL, y = NULL, caption = chart_caption)

# Campaign ROI ----------------------------------------------------------------
# Yearly runs of the same campaign combined (year removed from the name)
camp <- a$campaign_perf %>%
  mutate(campaign = sub(" [0-9]{4}$", "", campaign_name)) %>%
  group_by(campaign) %>%
  summarise(budget = sum(budget_usd), revenue = sum(revenue), gross_profit = sum(gross_profit),
            .groups = "drop") %>%
  mutate(roi = (gross_profit - budget) / budget,
         campaign = fct_reorder(campaign, roi),
         sign = if_else(roi >= 0, "Positive", "Negative"))

charts$campaign_roi <- ggplot(camp, aes(roi, campaign, fill = sign)) +
  geom_col(width = 0.65) +
  geom_vline(xintercept = 0, colour = amz$grey) +
  geom_text(aes(label = fmt_pct(roi), hjust = if_else(roi >= 0, -0.15, 1.15)), size = 3.2) +
  scale_fill_manual(values = c(Positive = amz$orange, Negative = amz$ink), guide = "none") +
  scale_x_continuous(labels = fmt_pct, expand = expansion(mult = 0.18)) +
  labs(title = "Campaign return on investment, 2021-2025 combined",
       subtitle = "(Gross profit of attributed orders - budget) / budget",
       x = NULL, y = NULL, caption = chart_caption) +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = amz$grid),
        axis.line.x = element_blank())

# =============================================================================
# Page 5 - Fulfilment, returns and reviews
# =============================================================================

# Late delivery rate by carrier -----------------------------------------------
# Late = delivered after the promised date
carrier <- a$logistics_month %>%
  group_by(year, carrier) %>%
  summarise(late_rate = sum(late) / sum(delivered), .groups = "drop")

charts$late_by_carrier <- ggplot(carrier, aes(year, late_rate, colour = carrier)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.6) +
  scale_colour_manual(values = amz_categorical[1:4]) +
  scale_y_continuous(labels = fmt_pct, limits = c(0, NA)) +
  scale_x_continuous(breaks = years) +
  labs(title = "The 2022 delivery crunch hit every carrier",
       subtitle = "Share of delivered orders that arrived after the promised date",
       x = NULL, y = NULL, caption = chart_caption)

# Delivery days ---------------------------------------------------------------
# Monthly average days from order to delivery
charts$delivery_days <- ggplot(a$kpi_month, aes(month, avg_delivery_days)) +
  geom_line(colour = amz$orange, linewidth = 1) +
  scale_y_continuous(limits = c(0, NA), labels = label_number(accuracy = 0.1, suffix = " d")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Average delivery time halved since the 2022 peak",
       subtitle = "Days from order to delivery, monthly average",
       x = NULL, y = NULL, caption = chart_caption)

# Return rate heatmap ---------------------------------------------------------
# Return rate by category and order year, white text on the darkest cells
ret <- a$category_year %>%
  mutate(top_category = fct_reorder(top_category, return_rate))

charts$return_heatmap <- ggplot(ret, aes(factor(year), top_category, fill = return_rate)) +
  geom_tile(colour = amz$white, linewidth = 1.2) +
  geom_text(aes(label = fmt_pct1(return_rate), colour = return_rate > 0.25), size = 3.1, show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = amz$white, `FALSE` = amz$black)) +
  scale_fill_amz_seq(labels = fmt_pct, name = NULL) +
  labs(title = "Fashion has the highest returns; all categories improved after 2022",
       subtitle = "Share of order lines returned, by category and order year",
       x = NULL, y = NULL, caption = chart_caption) +
  theme(panel.grid = element_blank(), axis.line.x = element_blank(), legend.position = "right")

# Return reasons --------------------------------------------------------------
# Reason codes tidied for display: no underscores, first letter capital
reasons <- a$returns_reason_year %>%
  group_by(return_reason) %>%
  summarise(returns = sum(returns), .groups = "drop") %>%
  mutate(share = returns / sum(returns),
         return_reason = gsub("_", " ", return_reason),
         return_reason = paste0(toupper(substr(return_reason, 1, 1)), substring(return_reason, 2)),
         return_reason = fct_reorder(return_reason, share))

charts$return_reasons <- ggplot(reasons, aes(share, return_reason)) +
  geom_col(width = 0.65, fill = amz$orange) +
  geom_text(aes(label = fmt_pct(share)), hjust = -0.2, size = 3.3) +
  scale_x_continuous(labels = fmt_pct, expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Why customers return items",
       subtitle = "Share of all returns, 2021-2025", x = NULL, y = NULL, caption = chart_caption) +
  theme(panel.grid.major.y = element_blank(), axis.line.x = element_blank())

# Ratings ---------------------------------------------------------------------
# Average rating per year, then late vs on-time ratings side by side
rating <- a$kpi_year %>% select(year, avg_rating)

charts$avg_rating <- ggplot(rating, aes(factor(year), avg_rating, group = 1)) +
  geom_line(colour = amz$orange, linewidth = 1.2) +
  geom_point(colour = amz$orange, size = 3.2) +
  geom_text(aes(label = sprintf("%.2f", avg_rating)), vjust = -1.1, fontface = "bold", size = 3.6) +
  scale_y_continuous(limits = c(3.4, 4.4), breaks = seq(3.4, 4.4, 0.2)) +
  labs(title = "Customer ratings fell with the 2022 delays, then recovered",
       subtitle = "Average review rating (1-5 stars)", x = NULL, y = NULL, caption = chart_caption)

lr <- a$late_vs_rating %>%
  mutate(status = if_else(is_late, "Late delivery", "On time"))

charts$late_vs_rating <- ggplot(lr, aes(factor(year), avg_rating, fill = status)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.66) +
  geom_text(aes(label = sprintf("%.1f", avg_rating)), position = position_dodge(width = 0.72),
            vjust = -0.6, size = 3.2) +
  scale_fill_manual(values = c(`On time` = amz$orange, `Late delivery` = amz$ink)) +
  scale_y_continuous(limits = c(0, 5), expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Late deliveries cost about 0.7 stars in ratings",
       subtitle = "Average rating of reviewed items by delivery outcome",
       x = NULL, y = NULL, caption = chart_caption)

# Stockouts -------------------------------------------------------------------
# Monthly share of products that ran out at least once
stock <- a$inventory_category_month %>%
  group_by(month) %>%
  summarise(stockout_rate = sum(stockout_products) / sum(products), .groups = "drop")

charts$stockouts <- ggplot(stock, aes(month, stockout_rate)) +
  geom_col(fill = amz$orange, width = 25) +
  scale_y_continuous(labels = fmt_pct, expand = expansion(mult = c(0, 0.08))) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Stockouts spiked during the 2022 supply crunch",
       subtitle = "Share of active products out of stock at some point in the month",
       x = NULL, y = NULL, caption = chart_caption)

# Export ----------------------------------------------------------------------
# Default size 10 x 6 in, a few charts need their own width or height
sizes <- list(category_trend = c(11, 8), kpi_multiples = c(12, 4.5), seasonality = c(11, 5),
              top_products = c(10, 6.5), category_slope = c(10, 6))

for (nm in names(charts)) {
  sz <- if (!is.null(sizes[[nm]])) sizes[[nm]] else c(10, 6)
  save_chart(charts[[nm]], nm, width = sz[1], height = sz[2])
}

saveRDS(charts, file.path(output_dir, "05_charts.rds"))
message(length(charts), " charts saved to ", file.path(output_dir, "charts"))
