# -----------------------------------------------------------------------------
# Script:   02_clean.R
# Purpose:  Clean the raw tables before any analysis: tidy text, set factor
#           levels, add date fields, check that amounts add up, fix or flag
#           bad values, and log how many rows each rule touched.
#
# Input:    Output/01_raw_data.rds
# Output:   Output/02_clean_data.rds, Output/02_cleaning_log.csv
# -----------------------------------------------------------------------------

# Packages --------------------------------------------------------------------
pkgs <- c("dplyr", "purrr", "tibble", "lubridate", "stringr", "readr")
missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing)

library(dplyr)
library(purrr)
library(tibble)
library(lubridate)
library(stringr)
library(readr)

output_dir <- "Output"
raw <- readRDS(file.path(output_dir, "01_raw_data.rds"))

# Cleaning log ----------------------------------------------------------------
# Every rule below adds one row here, so it is easy to see what changed and
# how much. Saved as 02_cleaning_log.csv at the end.
clean_log <- tibble(table = character(), rule = character(), rows_affected = integer())
log_step <- function(table, rule, n) {
  clean_log <<- add_row(clean_log, table = table, rule = rule, rows_affected = as.integer(n))
}

# General tidy-up -------------------------------------------------------------
# Applied to every table: remove extra spaces in text columns and drop exact
# duplicate rows
tidy_table <- function(df, name) {
  df <- df %>% mutate(across(where(is.character), str_squish))
  dups <- sum(duplicated(df))
  if (dups > 0) df <- distinct(df)
  log_step(name, "exact duplicate rows removed", dups)
  df
}
cl <- imap(raw, tidy_table)

# Calendar --------------------------------------------------------------------
# Month and weekday as ordered factors so charts sort Jan-Dec and Mon-Sun.
# Normal days get event_name "None" instead of NA.
cl$calendar <- cl$calendar %>%
  mutate(
    month_name  = factor(month_name, levels = month.name),
    day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                 "Friday", "Saturday", "Sunday")),
    event_name  = coalesce(event_name, "None")
  )

# Categories ------------------------------------------------------------------
# Categories are stored in two levels (top category -> subcategory). I flatten
# them into one lookup so a product gets both names with a single join.
cl$category_lookup <- cl$categories %>%
  filter(level == 2) %>%
  select(category_id, subcategory = category_name, top_category_id = parent_category_id) %>%
  left_join(cl$categories %>% filter(level == 1) %>%
              select(top_category_id = category_id, top_category = category_name),
            by = "top_category_id")

# Customers -------------------------------------------------------------------
# Age is measured at 31 Dec 2025, the end of the data. Birth years outside
# 1920-2010 look like typos and become NA. prime_since is cleared for
# customers who are not Prime members.
ref_date <- as.Date("2025-12-31")

bad_birth <- with(cl$customers, sum(birth_year < 1920 | birth_year > 2010, na.rm = TRUE))
log_step("customers", "birth_year outside 1920-2010 set to NA", bad_birth)

prime_fix <- with(cl$customers, sum(!is_prime_member & !is.na(prime_since)))
log_step("customers", "prime_since cleared for non-members", prime_fix)

cl$customers <- cl$customers %>%
  mutate(
    birth_year  = if_else(birth_year < 1920 | birth_year > 2010, NA_integer_, birth_year),
    prime_since = if_else(is_prime_member, prime_since, as.Date(NA)),
    age         = year(ref_date) - birth_year,
    age_band    = cut(age, breaks = c(17, 24, 34, 44, 54, 64, Inf),
                      labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")),
    gender      = factor(gender, levels = c("F", "M", "Other/Unknown")),
    country     = factor(country, levels = c("US", "CA", "UK", "DE")),
    signup_year = year(signup_date),
    signup_month = floor_date(signup_date, "month")
  )

# Products and sellers --------------------------------------------------------
# Price or cost <= 0 is flagged (price_issue), not removed. Products also get
# their category names, a price band and the 2021 margin.
neg_price <- sum(cl$products$list_price_2021 <= 0 | cl$products$unit_cost_2021 <= 0)
log_step("products", "non-positive price or cost flagged", neg_price)

cl$products <- cl$products %>%
  left_join(cl$category_lookup, by = "category_id") %>%
  mutate(
    is_active    = is.na(discontinued_date),
    launch_year  = year(launch_date),
    price_band   = cut(list_price_2021, breaks = c(0, 25, 50, 100, 250, Inf),
                       labels = c("<25", "25-50", "50-100", "100-250", "250+")),
    margin_2021  = 1 - unit_cost_2021 / list_price_2021,
    price_issue  = list_price_2021 <= 0 | unit_cost_2021 <= 0
  )

cl$sellers <- cl$sellers %>%
  mutate(seller_type = factor(seller_type, levels = c("1P", "3P")))

# Orders ----------------------------------------------------------------------
# Date parts for grouping, fixed factor levels for status, channel, device and
# shipping speed
cl$orders <- cl$orders %>%
  mutate(
    order_date   = as.Date(order_datetime),
    order_year   = year(order_date),
    order_month  = floor_date(order_date, "month"),
    order_hour   = hour(order_datetime),
    order_status = factor(order_status, levels = c("delivered", "delivered_with_return",
                                                   "shipped", "lost", "cancelled")),
    is_cancelled = order_status == "cancelled",
    channel      = factor(channel, levels = c("organic_search", "paid_search", "social_media",
                                              "email", "affiliate", "direct")),
    device       = factor(device, levels = c("desktop", "mobile_web", "mobile_app")),
    shipping_speed = factor(shipping_speed, levels = c("standard", "two_day", "next_day")),
    has_campaign = !is.na(campaign_id)
  )

# Check: subtotal + shipping + tax should equal order_total (1 cent allowed for
# rounding). Mismatches are flagged, not changed.
total_gap <- with(cl$orders, abs(items_subtotal + shipping_fee + tax_amount - order_total) > 0.011)
log_step("orders", "order_total does not reconcile (flagged)", sum(total_gap))
cl$orders$total_mismatch <- total_gap

# Order lines -----------------------------------------------------------------
# Lines with zero or negative quantity are dropped. Then line_total is
# checked against price x quantity - discount.
bad_qty <- sum(cl$order_items$quantity <= 0)
log_step("order_items", "non-positive quantity removed", bad_qty)

cl$order_items <- cl$order_items %>%
  filter(quantity > 0) %>%
  mutate(
    gross_amount = round(unit_price * quantity, 2),
    gross_profit = line_total - line_cost
  )

line_gap <- with(cl$order_items, abs(gross_amount - discount_amount - line_total) > 0.011)
log_step("order_items", "line_total does not reconcile (flagged)", sum(line_gap))
cl$order_items$total_mismatch <- line_gap

# Order subtotal vs the sum of its lines (5 cent tolerance), only logged
sub_check <- cl$order_items %>%
  group_by(order_id) %>%
  summarise(lines_total = sum(line_total), .groups = "drop") %>%
  inner_join(cl$orders %>% select(order_id, items_subtotal), by = "order_id") %>%
  filter(abs(lines_total - items_subtotal) > 0.05)
log_step("orders", "items_subtotal differs from sum of lines", nrow(sub_check))

# Payments --------------------------------------------------------------------
# Failed attempts stay in, they are useful for the payment analysis. Each order
# gets one is_final flag on its last successful payment.
cl$payments <- cl$payments %>%
  mutate(
    payment_method = factor(payment_method, levels = c("credit_card", "debit_card",
                                                       "digital_wallet", "gift_card", "bnpl")),
    is_failed = payment_status == "failed"
  ) %>%
  group_by(order_id) %>%
  mutate(is_final = !is_failed & payment_datetime == max(payment_datetime[!is_failed])) %>%
  ungroup()
log_step("payments", "failed payment attempts (kept, flagged)", sum(cl$payments$is_failed))

# Shipments -------------------------------------------------------------------
# delivery_days counts from the order date, days_late from the promised date.
# A delivery date before the ship date can't be right, so it becomes NA.
cl$shipments <- cl$shipments %>%
  left_join(cl$orders %>% select(order_id, order_date), by = "order_id") %>%
  mutate(
    carrier        = factor(carrier),
    delivery_days  = as.integer(actual_delivery_date - order_date),
    days_late      = as.integer(actual_delivery_date - promised_delivery_date),
    is_late        = !is.na(days_late) & days_late > 0,
    is_delivered   = shipment_status == "delivered"
  ) %>%
  select(-order_date)

bad_dates <- with(cl$shipments, sum(!is.na(actual_delivery_date) & actual_delivery_date < ship_date))
log_step("shipments", "delivered before ship date (set to NA)", bad_dates)
cl$shipments <- cl$shipments %>%
  mutate(actual_delivery_date = if_else(!is.na(actual_delivery_date) & actual_delivery_date < ship_date,
                                        as.Date(NA), actual_delivery_date))

# Returns and reviews ---------------------------------------------------------
# Ratings must be 1-5, anything else is dropped. Sentiment: 4-5 positive,
# 3 neutral, 1-2 negative.
cl$returns <- cl$returns %>%
  mutate(return_reason = factor(return_reason),
         return_status = factor(return_status, levels = c("refunded", "rejected", "pending")))

out_of_range <- sum(cl$reviews$rating < 1 | cl$reviews$rating > 5)
log_step("reviews", "rating outside 1-5 removed", out_of_range)
cl$reviews <- cl$reviews %>%
  filter(between(rating, 1, 5)) %>%
  mutate(sentiment = case_when(rating >= 4 ~ "positive",
                               rating == 3 ~ "neutral",
                               TRUE        ~ "negative"),
         sentiment = factor(sentiment, levels = c("negative", "neutral", "positive")))

# Marketing, inventory, targets -----------------------------------------------
# Month keys for later joins, negative closing stock set to 0, and campaign
# length in days (start and end day both counted)
cl$marketing_daily <- cl$marketing_daily %>%
  mutate(channel = factor(channel, levels = levels(cl$orders$channel)),
         month   = floor_date(date, "month"))

neg_stock <- sum(cl$inventory_monthly$closing_stock < 0)
log_step("inventory_monthly", "negative closing stock set to 0", neg_stock)
cl$inventory_monthly <- cl$inventory_monthly %>%
  mutate(closing_stock = pmax(closing_stock, 0L),
         month = as.Date(paste0(year_month, "-01")))

cl$sales_targets <- cl$sales_targets %>%
  mutate(month = as.Date(paste0(year_month, "-01")))

cl$campaigns <- cl$campaigns %>%
  mutate(duration_days = as.integer(end_date - start_date) + 1L,
         campaign_year = year(start_date))

# Save ------------------------------------------------------------------------
print(clean_log, n = Inf)
write_csv(clean_log, file.path(output_dir, "02_cleaning_log.csv"))
saveRDS(cl, file.path(output_dir, "02_clean_data.rds"))
message("Saved ", file.path(output_dir, "02_clean_data.rds"))
