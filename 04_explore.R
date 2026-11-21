# -----------------------------------------------------------------------------
# Script:   04_explore.R
# Purpose:  Explore the sales data before building the charts: yearly
#           scorecard, growth drivers, seasonality, category and product mix,
#           customers, channels, delivery, returns and reviews. Results are
#           printed to the console and saved as CSV tables for reporting.
#
# Input:    Output/03_analysis_tables.rds
# Output:   Output/tables/*.csv, Output/04_explore_results.rds
# -----------------------------------------------------------------------------

# Packages --------------------------------------------------------------------
pkgs <- c("dplyr", "tidyr", "lubridate", "readr", "tibble")
missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(readr)
  library(tibble)
})

output_dir <- "Output"
table_dir  <- file.path(output_dir, "tables")
dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)

a <- readRDS(file.path(output_dir, "03_analysis_tables.rds"))
options(width = 140, pillar.sigfig = 4)

# Small helpers: console section titles, CSV export, and rates shown as %
section <- function(title) cat("\n", strrep("=", 70), "\n ", title, "\n", strrep("=", 70), "\n", sep = "")
export  <- function(df, name) write_csv(df, file.path(table_dir, paste0(name, ".csv")))
pct     <- function(x, d = 1) round(100 * x, d)

# 1. Yearly scorecard ---------------------------------------------------------
# One row per year with the headline KPIs, rounded so it reads easily
section("1. Yearly sales scorecard")
scorecard <- a$kpi_year %>%
  transmute(year,
            revenue_m        = round(revenue / 1e6, 2),
            revenue_yoy_pct  = pct(revenue_yoy),
            orders,
            aov              = round(aov, 1),
            customers,
            gross_margin_pct = pct(gross_margin),
            target_pct       = pct(target_attainment),
            discount_pct     = pct(discount_rate),
            cancel_pct       = pct(cancel_rate),
            return_pct       = pct(return_rate),
            late_pct         = pct(late_rate),
            avg_rating       = round(avg_rating, 2),
            returning_pct    = pct(returning_share),
            prime_pct        = pct(prime_share),
            conversion_pct   = pct(conversion_rate, 2),
            roas             = round(roas, 1),
            cac              = round(cac, 1))
print(scorecard)
export(scorecard, "yearly_scorecard")

# 2. Growth drivers: orders vs basket value -----------------------------------
# Is revenue growth coming from more orders or from bigger baskets?
section("2. Revenue growth split into volume and order value")
growth <- a$kpi_year %>%
  transmute(year,
            revenue_growth_pct = pct(revenue_yoy),
            orders_growth_pct  = pct(orders_yoy),
            aov_growth_pct     = pct(aov_yoy),
            customers_growth_pct = pct(customers_yoy),
            orders_per_customer = round(orders_per_customer, 2))
print(growth)
export(growth, "growth_drivers")

# 3. Seasonality --------------------------------------------------------------
# Each month as an index of its year's average (100 = normal month), then the
# top sales days
section("3. Seasonality: average monthly revenue index (year average = 100)")
seasonality <- a$kpi_month %>%
  group_by(year) %>%
  mutate(index = 100 * revenue / mean(revenue)) %>%
  ungroup() %>%
  mutate(month = month(month, label = TRUE)) %>%
  select(year, month, index) %>%
  pivot_wider(names_from = year, values_from = index) %>%
  mutate(across(-month, ~ round(.x)))
print(seasonality)
export(seasonality, "seasonality_index")

section("Top 10 sales days")
top_days <- a$orders_enriched %>%
  filter(!is_cancelled) %>%
  group_by(order_date) %>%
  summarise(orders = n(), revenue = round(sum(items_subtotal)), .groups = "drop") %>%
  left_join(a$calendar %>% select(date, day_of_week, event_name), by = c("order_date" = "date")) %>%
  slice_max(revenue, n = 10)
print(top_days)

# Event lift = event-day revenue / median non-event day of the same year
event_lift <- a$orders_enriched %>%
  filter(!is_cancelled) %>%
  group_by(order_date) %>%
  summarise(revenue = sum(items_subtotal), .groups = "drop") %>%
  left_join(a$calendar %>% select(date, event_name), by = c("order_date" = "date")) %>%
  mutate(year = year(order_date)) %>%
  group_by(year) %>%
  mutate(normal_day = median(revenue[event_name == "None"])) %>%
  filter(event_name != "None") %>%
  group_by(event_name) %>%
  summarise(avg_lift_x = round(mean(revenue / normal_day), 2), .groups = "drop") %>%
  arrange(desc(avg_lift_x))
cat("\nRevenue on event days vs a typical day of the same year:\n")
print(event_lift)
export(event_lift, "event_lift")

# 4. Categories and products --------------------------------------------------
# Share shift 2021 -> 2025, latest-year KPIs per category, top products, how
# concentrated revenue is (Pareto) and the 1P/3P split
section("4. Category revenue share by year (%)")
cat_share <- a$category_year %>%
  select(year, top_category, revenue_share) %>%
  mutate(revenue_share = pct(revenue_share)) %>%
  pivot_wider(names_from = year, values_from = revenue_share) %>%
  mutate(change_pp = `2025` - `2021`) %>%
  arrange(desc(`2025`))
print(cat_share)
export(cat_share, "category_share")

cat_kpi <- a$category_year %>%
  filter(year == max(year)) %>%
  transmute(top_category, revenue = round(revenue), yoy_pct = pct(revenue_yoy),
            margin_pct = pct(gross_margin), return_pct = pct(return_rate),
            target_pct = pct(target_attainment)) %>%
  arrange(desc(revenue))
cat("\nCategory KPIs, latest year:\n")
print(cat_kpi)
export(cat_kpi, "category_kpis_latest_year")

section("Top 15 products by revenue (2021-2025)")
top_products <- a$product_summary %>%
  slice_head(n = 15) %>%
  transmute(revenue_rank, product_id, brand, subcategory, top_category,
            revenue = round(revenue), units, avg_rating = round(avg_rating, 2),
            return_pct = pct(return_rate))
print(top_products)
export(a$product_summary, "product_summary")

pareto <- a$product_summary %>%
  arrange(desc(revenue)) %>%
  mutate(cum_share = cumsum(revenue) / sum(revenue), rank_share = row_number() / n())
cat("\nShare of products generating 80% of revenue:",
    pct(min(pareto$rank_share[pareto$cum_share >= 0.8])), "%\n")

section("1P vs 3P marketplace")
print(a$seller_year %>% mutate(revenue_share = pct(revenue_share), revenue = round(revenue),
                               commission_income = round(commission_income)))
export(a$seller_year, "seller_type_year")

# 5. Customers ----------------------------------------------------------------
# New vs returning, Prime vs non-Prime, retention at 1/3/6/12 months, RFM
# segments, age bands and countries
section("5. Customers: new vs returning revenue")
cust_mix <- a$orders_enriched %>%
  filter(!is_cancelled) %>%
  group_by(year = order_year, customer_type) %>%
  summarise(orders = n(), revenue = sum(items_subtotal), aov = mean(items_subtotal), .groups = "drop") %>%
  group_by(year) %>% mutate(revenue_share = pct(revenue / sum(revenue))) %>% ungroup() %>%
  mutate(revenue = round(revenue), aov = round(aov, 1))
print(cust_mix)
export(cust_mix, "new_vs_returning")

prime_cmp <- a$orders_enriched %>%
  filter(!is_cancelled) %>%
  group_by(year = order_year, is_prime_order) %>%
  summarise(orders = n(), aov = round(mean(items_subtotal), 1),
            items_per_order = round(mean(n_lines), 2), .groups = "drop")
cat("\nPrime vs non-Prime orders:\n")
print(prime_cmp)
export(prime_cmp, "prime_vs_nonprime")

cat("\nRetention by cohort year (share of first-time buyers ordering again in month N):\n")
retention_tbl <- a$cohort_year_retention %>%
  filter(months_since %in% c(1, 3, 6, 12)) %>%
  mutate(retention = pct(retention)) %>%
  pivot_wider(names_from = months_since, values_from = retention, names_prefix = "month_")
print(retention_tbl)
export(retention_tbl, "cohort_retention_summary")

rfm <- a$customer_summary %>%
  group_by(rfm_segment) %>%
  summarise(customers = n(), share_pct = pct(n() / nrow(a$customer_summary)),
            avg_orders = round(mean(orders), 2), avg_revenue = round(mean(revenue)),
            revenue_share_pct = pct(sum(revenue) / sum(a$customer_summary$revenue)),
            .groups = "drop") %>%
  arrange(desc(revenue_share_pct))
cat("\nRFM segments:\n")
print(rfm)
export(rfm, "rfm_segments")

age_tbl <- a$orders_enriched %>%
  filter(!is_cancelled, !is.na(age_band)) %>%
  group_by(age_band) %>%
  summarise(orders = n(), revenue = round(sum(items_subtotal)), aov = round(mean(items_subtotal), 1),
            .groups = "drop")
cat("\nSales by age band:\n")
print(age_tbl)

geo_tbl <- a$geo_year %>%
  group_by(year, country) %>%
  summarise(revenue = round(sum(revenue)), customers = sum(customers), .groups = "drop") %>%
  pivot_wider(names_from = country, values_from = revenue, id_cols = year, values_fill = 0)
cat("\nRevenue by country:\n")
print(geo_tbl)
export(geo_tbl, "revenue_by_country")

# 6. Channels and campaigns ---------------------------------------------------
# Channel share, conversion, ROAS and CAC, device and payment mix, campaigns
# ranked by ROI
section("6. Channel performance by year")
ch_tbl <- a$channel_year %>%
  transmute(year, channel, revenue_share_pct = pct(revenue_share),
            conversion_pct = pct(conversion_rate, 2), spend = round(spend),
            roas = round(roas, 1), cac = round(cac, 1))
print(ch_tbl, n = Inf)
export(ch_tbl, "channel_year")

cat("\nDevice share of orders (%):\n")
print(a$device_year %>% mutate(share = pct(share)) %>% select(-orders) %>%
        pivot_wider(names_from = device, values_from = share))

cat("\nPayment method share of orders (%):\n")
pay_tbl <- a$payment_year %>% mutate(share = pct(share)) %>%
  select(year, payment_method, share) %>%
  pivot_wider(names_from = payment_method, values_from = share)
print(pay_tbl)
export(pay_tbl, "payment_mix")

cat("\nTop 10 campaigns by ROI:\n")
camp_tbl <- a$campaign_perf %>%
  transmute(campaign_name, campaign_type, channel, budget_usd, orders,
            revenue = round(revenue), roi_pct = pct(roi), roas = round(roas, 1)) %>%
  arrange(desc(roi_pct))
print(head(camp_tbl, 10))
export(camp_tbl, "campaign_performance")

# 7. Delivery, returns and reviews --------------------------------------------
# Carrier late rates and delivery times, how late delivery affects ratings and
# returns, return reasons, rating mix and stockouts
section("7. Delivery performance by carrier and year")
carrier_tbl <- a$logistics_month %>%
  group_by(year, carrier) %>%
  summarise(shipments = sum(shipments), late_pct = pct(sum(late) / sum(delivered)),
            avg_days = round(weighted.mean(avg_delivery_days, delivered, na.rm = TRUE), 2),
            .groups = "drop")
print(carrier_tbl, n = Inf)
export(carrier_tbl, "carrier_performance")

cat("\nLate delivery vs rating and returns:\n")
print(a$late_vs_rating %>% mutate(avg_rating = round(avg_rating, 2), return_rate = pct(return_rate)))

cat("\nTop return reasons by category (all years):\n")
reason_tbl <- a$returns_reason_year %>%
  group_by(top_category, return_reason) %>%
  summarise(returns = sum(returns), .groups = "drop") %>%
  group_by(top_category) %>%
  slice_max(returns, n = 1) %>%
  ungroup()
print(reason_tbl)
export(a$returns_reason_year, "returns_by_reason")

cat("\nRating distribution by year (%):\n")
print(a$rating_year %>% mutate(share = pct(share)) %>% select(-reviews) %>%
        pivot_wider(names_from = rating, values_from = share, names_prefix = "stars_"))

cat("\nStockout rate by year (% of active products with a stockout in the month):\n")
stock_tbl <- a$inventory_category_month %>%
  group_by(year) %>%
  summarise(stockout_pct = pct(sum(stockout_products) / sum(products)), .groups = "drop")
print(stock_tbl)

# 8. Correlation of monthly KPIs ----------------------------------------------
# Quick check of which monthly KPIs move together
section("8. Correlation between monthly KPIs")
cor_mat <- a$kpi_month %>%
  select(revenue, orders, aov, discount_rate, late_rate, return_rate, avg_rating,
         conversion_rate, prime_share) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2)
print(cor_mat)

# Save ------------------------------------------------------------------------
# Saved so the dashboard text can quote these numbers
explore <- list(scorecard = scorecard, growth = growth, seasonality = seasonality,
                event_lift = event_lift, category_share = cat_share, top_products = top_products,
                customer_mix = cust_mix, prime = prime_cmp, retention = retention_tbl, rfm = rfm,
                channels = ch_tbl, campaigns = camp_tbl, carriers = carrier_tbl,
                kpi_correlation = cor_mat)
saveRDS(explore, file.path(output_dir, "04_explore_results.rds"))
message("\nTables exported to ", table_dir)
