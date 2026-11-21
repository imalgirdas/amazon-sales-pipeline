# -----------------------------------------------------------------------------
# Script:   03_transform.R
# Purpose:  Join the cleaned tables into analysis tables and build the KPI
#           tables that 04, 05 and 06 read from: sales lines, enriched orders,
#           customer and product summaries, monthly and yearly KPIs, and
#           category, channel, cohort and logistics views.
#
# Input:    Output/02_clean_data.rds
# Output:   Output/03_analysis_tables.rds
# -----------------------------------------------------------------------------

# Packages --------------------------------------------------------------------
pkgs <- c("dplyr", "tidyr", "lubridate", "tibble")
missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing)

library(dplyr)
library(tidyr)
library(lubridate)
library(tibble)

output_dir <- "Output"
cl <- readRDS(file.path(output_dir, "02_clean_data.rds"))
# Same reference date as in 02 (last day of the data)
ref_date <- as.Date("2025-12-31")

# Division that gives NA instead of Inf/NaN when the denominator is 0 or NA
safe_div <- function(x, y) ifelse(is.na(y) | y == 0, NA_real_, x / y)

# Customer first order (new vs returning) -------------------------------------
# First non-cancelled order per customer. Sets new vs returning and the cohort.
first_orders <- cl$orders %>%
  filter(!is_cancelled) %>%
  group_by(customer_id) %>%
  summarise(first_order_date = min(order_date), .groups = "drop") %>%
  mutate(cohort_month = floor_date(first_order_date, "month"),
         cohort_year  = year(first_order_date))

# Enriched orders (one row per order) -----------------------------------------
# Customer, payment, return, profit, shipping and campaign details joined onto
# each order. Cancelled orders stay in with net_sales = 0.
final_payment <- cl$payments %>%
  filter(is_final) %>%
  select(order_id, payment_method, installments)

failed_attempts <- cl$payments %>%
  group_by(order_id) %>%
  summarise(failed_attempts = sum(is_failed), .groups = "drop")

order_returns <- cl$returns %>%
  group_by(order_id) %>%
  summarise(returned_lines = n(), refund_amount = sum(refund_amount), .groups = "drop")

order_profit <- cl$order_items %>%
  group_by(order_id) %>%
  summarise(order_cost = sum(line_cost), gross_profit = sum(gross_profit), .groups = "drop")

orders_enriched <- cl$orders %>%
  left_join(cl$customers %>% select(customer_id, country, region, age_band, gender,
                                    is_prime_member, signup_date), by = "customer_id") %>%
  left_join(first_orders, by = "customer_id") %>%
  left_join(final_payment, by = "order_id") %>%
  left_join(failed_attempts, by = "order_id") %>%
  left_join(order_returns, by = "order_id") %>%
  left_join(order_profit, by = "order_id") %>%
  left_join(cl$shipments %>% select(order_id, warehouse_id, carrier, delivery_days, days_late,
                                    is_late, is_delivered, shipping_cost), by = "order_id") %>%
  left_join(cl$campaigns %>% select(campaign_id, campaign_name, campaign_type), by = "campaign_id") %>%
  mutate(
    customer_type  = if_else(!is.na(first_order_date) & order_date == first_order_date,
                             "New", "Returning"),
    returned_lines = coalesce(returned_lines, 0L),
    refund_amount  = coalesce(refund_amount, 0),
    has_return     = returned_lines > 0,
    net_sales      = if_else(is_cancelled, 0, items_subtotal - refund_amount)
  )

# Sales lines (one row per order line) ----------------------------------------
# Main fact table: every order line with its order, product, seller, return and
# review fields. 3P lines also get the commission earned.
line_returns <- cl$returns %>%
  select(order_item_id, quantity_returned, return_reason, return_status, refund_amount)

line_reviews <- cl$reviews %>%
  select(order_item_id, rating, sentiment)

sales_lines <- cl$order_items %>%
  inner_join(orders_enriched %>%
               select(order_id, customer_id, order_date, order_year, order_month, order_status,
                      is_cancelled, channel, device, campaign_id, campaign_type, is_prime_order,
                      customer_type, country, region, age_band, is_late),
             by = "order_id") %>%
  left_join(cl$products %>% select(product_id, brand, subcategory, top_category, price_band,
                                   launch_date), by = "product_id") %>%
  left_join(cl$sellers %>% select(seller_id, seller_type, commission_rate), by = "seller_id") %>%
  left_join(line_returns, by = "order_item_id") %>%
  left_join(line_reviews, by = "order_item_id") %>%
  mutate(
    is_returned       = !is.na(quantity_returned),
    refund_amount     = coalesce(refund_amount, 0),
    net_revenue       = line_total - refund_amount,
    commission_income = if_else(seller_type == "3P", line_total * commission_rate, 0),
    is_new_product    = year(launch_date) == order_year
  )

# Same lines without cancelled orders, used for most summaries below
valid_lines <- sales_lines %>% filter(!is_cancelled)

# Monthly KPIs ----------------------------------------------------------------
# Revenue = items subtotal of non-cancelled orders, before tax and shipping.
# Cancellations, targets, sessions/spend and line returns/ratings are joined
# in separately, then the rates are calculated. YoY uses lag(12), so same
# month last year.
kpi_month_core <- orders_enriched %>%
  filter(!is_cancelled) %>%
  group_by(month = order_month) %>%
  summarise(
    orders            = n(),
    revenue           = sum(items_subtotal),
    gross_profit      = sum(gross_profit),
    discounts         = sum(discount_total),
    refunds           = sum(refund_amount),
    units             = sum(units),
    customers         = n_distinct(customer_id),
    new_customers     = sum(customer_type == "New"),
    prime_orders      = sum(is_prime_order),
    late_deliveries   = sum(is_late, na.rm = TRUE),
    delivered         = sum(is_delivered, na.rm = TRUE),
    avg_delivery_days = mean(delivery_days, na.rm = TRUE),
    .groups = "drop"
  )

kpi_month <- kpi_month_core %>%
  left_join(cl$orders %>% group_by(month = order_month) %>%
              summarise(all_orders = n(), cancelled = sum(is_cancelled), .groups = "drop"),
            by = "month") %>%
  left_join(cl$sales_targets %>% group_by(month) %>%
              summarise(target = sum(revenue_target_usd), .groups = "drop"), by = "month") %>%
  left_join(cl$marketing_daily %>% group_by(month) %>%
              summarise(sessions = sum(sessions), spend = sum(spend_usd), .groups = "drop"),
            by = "month") %>%
  left_join(valid_lines %>% group_by(month = order_month) %>%
              summarise(lines = n(), returned_lines = sum(is_returned),
                        avg_rating = mean(rating, na.rm = TRUE), .groups = "drop"),
            by = "month") %>%
  arrange(month) %>%
  mutate(
    year               = year(month),
    aov                = revenue / orders,
    gross_margin       = gross_profit / revenue,
    net_revenue        = revenue - refunds,
    discount_rate      = discounts / (revenue + discounts),
    cancel_rate        = cancelled / all_orders,
    return_rate        = returned_lines / lines,
    late_rate          = late_deliveries / delivered,
    prime_share        = prime_orders / orders,
    returning_share    = 1 - new_customers / orders,
    conversion_rate    = orders / sessions,
    roas               = revenue / spend,
    cac                = safe_div(spend, new_customers),
    target_attainment  = revenue / target,
    revenue_yoy        = revenue / lag(revenue, 12) - 1,
    orders_yoy         = orders / lag(orders, 12) - 1,
    revenue_ytd        = ave(revenue, year, FUN = cumsum),
    revenue_3m_avg     = (revenue + lag(revenue) + lag(revenue, 2)) / 3
  )

# Yearly KPIs -----------------------------------------------------------------
# Monthly totals summed to years, so both levels use the same definitions.
# Customers and delivery days come straight from orders (distinct counts and
# averages can't be summed), ratings by review year.
kpi_year <- kpi_month %>%
  group_by(year) %>%
  summarise(across(c(orders, all_orders, cancelled, revenue, gross_profit, discounts, refunds,
                     units, new_customers, prime_orders, late_deliveries, delivered, target,
                     sessions, spend, lines, returned_lines), sum), .groups = "drop") %>%
  left_join(orders_enriched %>% filter(!is_cancelled) %>% group_by(year = order_year) %>%
              summarise(customers = n_distinct(customer_id),
                        avg_delivery_days = mean(delivery_days, na.rm = TRUE), .groups = "drop"),
            by = "year") %>%
  left_join(cl$reviews %>% group_by(year = year(review_date)) %>%
              summarise(avg_rating = mean(rating), reviews = n(), .groups = "drop"), by = "year") %>%
  mutate(
    aov               = revenue / orders,
    gross_margin      = gross_profit / revenue,
    net_revenue       = revenue - refunds,
    discount_rate     = discounts / (revenue + discounts),
    cancel_rate       = cancelled / all_orders,
    return_rate       = returned_lines / lines,
    late_rate         = late_deliveries / delivered,
    prime_share       = prime_orders / orders,
    returning_share   = 1 - new_customers / orders,
    orders_per_customer = orders / customers,
    conversion_rate   = orders / sessions,
    roas              = revenue / spend,
    cac               = spend / new_customers,
    target_attainment = revenue / target,
    revenue_yoy       = revenue / lag(revenue) - 1,
    orders_yoy        = orders / lag(orders) - 1,
    aov_yoy           = aov / lag(aov) - 1,
    customers_yoy     = customers / lag(customers) - 1
  )

# Category performance --------------------------------------------------------
# Revenue by top category per month and year, with share of total, margin,
# return rate and target achieved
category_month <- valid_lines %>%
  group_by(month = order_month, year = order_year, top_category) %>%
  summarise(revenue = sum(line_total), gross_profit = sum(gross_profit), units = sum(quantity),
            lines = n(), returned_lines = sum(is_returned), refunds = sum(refund_amount),
            avg_rating = mean(rating, na.rm = TRUE), .groups = "drop") %>%
  left_join(cl$sales_targets %>% select(month, top_category, target = revenue_target_usd),
            by = c("month", "top_category")) %>%
  group_by(month) %>%
  mutate(revenue_share = revenue / sum(revenue)) %>%
  ungroup() %>%
  mutate(gross_margin = gross_profit / revenue,
         return_rate  = returned_lines / lines,
         target_attainment = revenue / target)

category_year <- category_month %>%
  group_by(year, top_category) %>%
  summarise(across(c(revenue, gross_profit, units, lines, returned_lines, refunds, target), sum),
            .groups = "drop") %>%
  group_by(year) %>%
  mutate(revenue_share = revenue / sum(revenue)) %>%
  group_by(top_category) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(revenue_yoy = revenue / lag(revenue) - 1) %>%
  ungroup() %>%
  mutate(gross_margin = gross_profit / revenue,
         return_rate  = returned_lines / lines,
         target_attainment = revenue / target)

# Product summary -------------------------------------------------------------
# Lifetime numbers per product, ranked by revenue
product_summary <- valid_lines %>%
  group_by(product_id, brand, subcategory, top_category, seller_type, price_band) %>%
  summarise(revenue = sum(line_total), gross_profit = sum(gross_profit), units = sum(quantity),
            orders = n_distinct(order_id), returned_lines = sum(is_returned), lines = n(),
            avg_rating = mean(rating, na.rm = TRUE), reviews = sum(!is.na(rating)),
            first_sale = min(order_date), last_sale = max(order_date), .groups = "drop") %>%
  mutate(return_rate = returned_lines / lines,
         gross_margin = gross_profit / revenue,
         revenue_rank = min_rank(desc(revenue))) %>%
  arrange(revenue_rank)

# Seller summary (1P vs 3P) ---------------------------------------------------
# Amazon's own sales (1P) vs marketplace sellers (3P), plus commission income
seller_year <- valid_lines %>%
  group_by(year = order_year, seller_type) %>%
  summarise(revenue = sum(line_total), commission_income = sum(commission_income),
            sellers = n_distinct(seller_id), units = sum(quantity), .groups = "drop") %>%
  group_by(year) %>%
  mutate(revenue_share = revenue / sum(revenue)) %>%
  ungroup()

# Customer summary (RFM and lifetime value) -----------------------------------
# One row per customer. R and M scores are quintiles, F uses fixed order-count
# bands because most customers only have a few orders.
customer_summary <- orders_enriched %>%
  filter(!is_cancelled) %>%
  group_by(customer_id, country, region, age_band, gender, is_prime_member,
           cohort_month, cohort_year) %>%
  summarise(orders = n(), revenue = sum(items_subtotal), net_sales = sum(net_sales),
            gross_profit = sum(gross_profit), first_order = min(order_date),
            last_order = max(order_date), returns = sum(has_return), .groups = "drop") %>%
  mutate(recency_days  = as.integer(ref_date - last_order),
         tenure_days   = as.integer(ref_date - first_order),
         aov           = revenue / orders,
         is_repeat     = orders > 1,
         r_score = ntile(-recency_days, 5),
         f_score = case_when(orders == 1 ~ 1L, orders == 2 ~ 2L, orders == 3 ~ 3L,
                             orders <= 5 ~ 4L, TRUE ~ 5L),
         m_score = ntile(revenue, 5),
         # segments are checked top to bottom, first match wins
         rfm_segment = case_when(
           r_score >= 4 & f_score >= 4                 ~ "Champions",
           r_score >= 3 & f_score >= 3                 ~ "Loyal",
           r_score >= 4 & f_score <= 2                 ~ "New / Promising",
           r_score <= 2 & f_score >= 3                 ~ "At Risk",
           r_score <= 2 & f_score <= 2                 ~ "Lost / Hibernating",
           TRUE                                        ~ "Need Attention"))

# Cohort retention ------------------------------------------------------------
# Cohort = month of first order. For each month after that, share of the cohort
# that ordered again. Also rolled up to cohort year.
cohort_retention <- orders_enriched %>%
  filter(!is_cancelled, !is.na(cohort_month)) %>%
  distinct(customer_id, cohort_month, order_month) %>%
  mutate(months_since = (year(order_month) - year(cohort_month)) * 12 +
                        month(order_month) - month(cohort_month)) %>%
  count(cohort_month, months_since, name = "active_customers") %>%
  group_by(cohort_month) %>%
  mutate(cohort_size = active_customers[months_since == 0],
         retention   = active_customers / cohort_size) %>%
  ungroup() %>%
  mutate(cohort_year = year(cohort_month))

cohort_year_retention <- cohort_retention %>%
  group_by(cohort_year, months_since) %>%
  summarise(retention = sum(active_customers) / sum(cohort_size), .groups = "drop")

# Channel and marketing -------------------------------------------------------
# Orders and revenue per traffic channel, joined to sessions and spend from the
# daily marketing table. CAC only where the channel has spend. Device and
# payment mix per year at the end.
channel_month <- orders_enriched %>%
  filter(!is_cancelled) %>%
  group_by(month = order_month, channel) %>%
  summarise(orders = n(), revenue = sum(items_subtotal),
            new_customers = sum(customer_type == "New"), .groups = "drop") %>%
  left_join(cl$marketing_daily %>% group_by(month, channel) %>%
              summarise(sessions = sum(sessions), clicks = sum(clicks),
                        impressions = sum(impressions), spend = sum(spend_usd), .groups = "drop"),
            by = c("month", "channel")) %>%
  mutate(year = year(month),
         conversion_rate = orders / sessions,
         roas = safe_div(revenue, spend),
         cac  = if_else(spend > 0, safe_div(spend, new_customers), NA_real_))

channel_year <- channel_month %>%
  group_by(year, channel) %>%
  summarise(across(c(orders, revenue, new_customers, sessions, clicks, impressions, spend), sum),
            .groups = "drop") %>%
  group_by(year) %>%
  mutate(revenue_share = revenue / sum(revenue)) %>%
  ungroup() %>%
  mutate(conversion_rate = orders / sessions,
         ctr  = safe_div(clicks, impressions),
         cpc  = safe_div(spend, clicks),
         roas = safe_div(revenue, spend),
         cac  = if_else(spend > 0, safe_div(spend, new_customers), NA_real_))

device_year <- orders_enriched %>%
  filter(!is_cancelled) %>%
  count(year = order_year, device, name = "orders") %>%
  group_by(year) %>% mutate(share = orders / sum(orders)) %>% ungroup()

payment_year <- orders_enriched %>%
  filter(!is_cancelled, !is.na(payment_method)) %>%
  group_by(year = order_year, payment_method) %>%
  summarise(orders = n(), revenue = sum(order_total), .groups = "drop") %>%
  group_by(year) %>% mutate(share = orders / sum(orders)) %>% ungroup()

# Campaign performance --------------------------------------------------------
# ROI is based on gross profit, not revenue: (gross profit - budget) / budget
campaign_perf <- cl$campaigns %>%
  left_join(orders_enriched %>% filter(!is_cancelled, has_campaign) %>%
              group_by(campaign_id) %>%
              summarise(orders = n(), revenue = sum(items_subtotal),
                        gross_profit = sum(gross_profit), discounts = sum(discount_total),
                        new_customers = sum(customer_type == "New"), .groups = "drop"),
            by = "campaign_id") %>%
  mutate(across(c(orders, revenue, gross_profit, discounts, new_customers), ~ coalesce(.x, 0)),
         roi          = (gross_profit - budget_usd) / budget_usd,
         roas         = revenue / budget_usd,
         revenue_per_day = revenue / duration_days)

# Logistics -------------------------------------------------------------------
# Late rate and delivery days by carrier per month and by warehouse per year
logistics_month <- orders_enriched %>%
  filter(!is_cancelled, !is.na(carrier)) %>%
  group_by(month = order_month, carrier) %>%
  summarise(shipments = n(), delivered = sum(is_delivered),
            late = sum(is_late), avg_delivery_days = mean(delivery_days, na.rm = TRUE),
            shipping_cost = sum(shipping_cost), .groups = "drop") %>%
  mutate(year = year(month), late_rate = late / delivered)

warehouse_year <- orders_enriched %>%
  filter(!is_cancelled, !is.na(warehouse_id)) %>%
  group_by(year = order_year, warehouse_id) %>%
  summarise(shipments = n(), late_rate = mean(is_late[is_delivered]),
            avg_delivery_days = mean(delivery_days, na.rm = TRUE),
            revenue = sum(items_subtotal), .groups = "drop") %>%
  left_join(cl$warehouses %>% select(warehouse_id, warehouse_name, region), by = "warehouse_id")

# Returns and reviews ---------------------------------------------------------
# Returns by reason, rating distribution, and late vs on-time ratings
returns_reason_year <- valid_lines %>%
  filter(is_returned) %>%
  count(year = order_year, top_category, return_reason, name = "returns")

rating_year <- cl$reviews %>%
  count(year = year(review_date), rating, name = "reviews") %>%
  group_by(year) %>% mutate(share = reviews / sum(reviews)) %>% ungroup()

late_vs_rating <- valid_lines %>%
  filter(!is.na(rating), !is.na(is_late)) %>%
  group_by(year = order_year, is_late) %>%
  summarise(avg_rating = mean(rating), reviews = n(),
            return_rate = mean(is_returned), .groups = "drop")

# Inventory -------------------------------------------------------------------
# Stockout rate = share of products with at least one stockout day in the month
inventory_category_month <- cl$inventory_monthly %>%
  left_join(cl$products %>% select(product_id, top_category), by = "product_id") %>%
  group_by(month, top_category) %>%
  summarise(products = n(), stockout_products = sum(stockout_days > 0),
            stockout_days = sum(stockout_days), units_sold = sum(units_sold),
            stock_value = sum(stock_value_usd), closing_stock = sum(closing_stock),
            .groups = "drop") %>%
  mutate(year = year(month),
         stockout_rate = stockout_products / products,
         months_of_cover = safe_div(closing_stock, units_sold))

# Geography -------------------------------------------------------------------
# Revenue and customers by country and region per year
geo_year <- orders_enriched %>%
  filter(!is_cancelled) %>%
  group_by(year = order_year, country, region) %>%
  summarise(orders = n(), revenue = sum(items_subtotal),
            customers = n_distinct(customer_id), .groups = "drop")

# Save ------------------------------------------------------------------------
# All tables go into one list, so later scripts load a single file
analysis <- list(
  orders_enriched = orders_enriched, sales_lines = sales_lines,
  kpi_month = kpi_month, kpi_year = kpi_year,
  category_month = category_month, category_year = category_year,
  product_summary = product_summary, seller_year = seller_year,
  customer_summary = customer_summary, cohort_retention = cohort_retention,
  cohort_year_retention = cohort_year_retention,
  channel_month = channel_month, channel_year = channel_year,
  device_year = device_year, payment_year = payment_year, campaign_perf = campaign_perf,
  logistics_month = logistics_month, warehouse_year = warehouse_year,
  returns_reason_year = returns_reason_year, rating_year = rating_year,
  late_vs_rating = late_vs_rating, inventory_category_month = inventory_category_month,
  geo_year = geo_year, calendar = cl$calendar
)

print(tibble(table = names(analysis), rows = sapply(analysis, nrow)), n = Inf)
saveRDS(analysis, file.path(output_dir, "03_analysis_tables.rds"))
message("Saved ", file.path(output_dir, "03_analysis_tables.rds"))
