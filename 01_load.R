# -----------------------------------------------------------------------------
# Script:   01_load.R
# Purpose:  Read the 16 raw CSV files with fixed column types, turn the
#           True/False text flags into logicals, check row counts and keys,
#           and save everything as one list for the cleaning step.
#
# Input:    Data/*.csv
# Output:   Output/01_raw_data.rds
# -----------------------------------------------------------------------------

# Packages --------------------------------------------------------------------
# Installs whatever is missing first, so the script also runs on a new machine
pkgs <- c("readr", "dplyr", "purrr", "tibble")
missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing)

library(readr)
library(dplyr)
library(purrr)
library(tibble)

# Paths -----------------------------------------------------------------------
# Paths are relative to the Amazon project folder
data_dir   <- "Data"
output_dir <- "Output"
if (!dir.exists(data_dir)) {
  stop("Folder 'Data' not found. Open the Amazon RStudio project or setwd() to the Amazon folder.")
}
dir.create(output_dir, showWarnings = FALSE)

# Column types ----------------------------------------------------------------
# Types set by hand for every table instead of letting readr guess, so a bad
# value shows up as a parsing problem. Flag columns are read as "True"/"False"
# text and converted to logical after import.
specs <- list(
  calendar = cols(
    date = col_date(), year = col_integer(), quarter = col_integer(), month = col_integer(),
    month_name = col_character(), year_month = col_character(), iso_week = col_integer(),
    day_of_week = col_character(), is_weekend = col_character(), event_name = col_character(),
    is_peak_season = col_character()
  ),
  categories = cols(
    category_id = col_integer(), category_name = col_character(),
    parent_category_id = col_integer(), level = col_integer()
  ),
  sellers = cols(
    seller_id = col_integer(), seller_name = col_character(), seller_type = col_character(),
    country = col_character(), join_date = col_date(), commission_rate = col_double(),
    fulfilled_by_platform = col_character()
  ),
  products = cols(
    product_id = col_integer(), sku = col_character(), product_name = col_character(),
    category_id = col_integer(), seller_id = col_integer(), brand = col_character(),
    list_price_2021 = col_double(), unit_cost_2021 = col_double(), weight_kg = col_double(),
    launch_date = col_date(), discontinued_date = col_date(), is_prime_eligible = col_character()
  ),
  warehouses = cols(
    warehouse_id = col_integer(), warehouse_name = col_character(), city = col_character(),
    state = col_character(), country = col_character(), region = col_character(),
    open_date = col_date(), capacity_units = col_integer()
  ),
  customers = cols(
    customer_id = col_integer(), customer_code = col_character(), gender = col_character(),
    birth_year = col_integer(), country = col_character(), state = col_character(),
    region = col_character(), signup_date = col_date(), signup_channel = col_character(),
    is_prime_member = col_character(), prime_since = col_date()
  ),
  campaigns = cols(
    campaign_id = col_integer(), campaign_name = col_character(), campaign_type = col_character(),
    channel = col_character(), start_date = col_date(), end_date = col_date(),
    budget_usd = col_double(), target_category = col_character(), discount_pct = col_double()
  ),
  orders = cols(
    order_id = col_integer(), customer_id = col_integer(),
    order_datetime = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
    order_status = col_character(), channel = col_character(), device = col_character(),
    campaign_id = col_integer(), is_prime_order = col_character(), shipping_speed = col_character(),
    n_lines = col_integer(), units = col_integer(), items_subtotal = col_double(),
    discount_total = col_double(), shipping_fee = col_double(), tax_amount = col_double(),
    order_total = col_double()
  ),
  order_items = cols(
    order_item_id = col_integer(), order_id = col_integer(), line_number = col_integer(),
    product_id = col_integer(), seller_id = col_integer(), quantity = col_integer(),
    unit_price = col_double(), discount_pct = col_double(), discount_amount = col_double(),
    line_total = col_double(), unit_cost = col_double(), line_cost = col_double()
  ),
  payments = cols(
    payment_id = col_integer(), order_id = col_integer(),
    payment_datetime = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
    payment_method = col_character(), installments = col_integer(), amount = col_double(),
    payment_status = col_character()
  ),
  shipments = cols(
    shipment_id = col_integer(), order_id = col_integer(), warehouse_id = col_integer(),
    carrier = col_character(), ship_date = col_date(), promised_delivery_date = col_date(),
    actual_delivery_date = col_date(), shipment_status = col_character(),
    shipping_cost = col_double()
  ),
  returns = cols(
    return_id = col_integer(), order_item_id = col_integer(), order_id = col_integer(),
    return_request_date = col_date(), return_reason = col_character(),
    quantity_returned = col_integer(), return_status = col_character(),
    refund_amount = col_double(), restockable = col_character()
  ),
  reviews = cols(
    review_id = col_integer(), order_item_id = col_integer(), product_id = col_integer(),
    customer_id = col_integer(), review_date = col_date(), rating = col_integer(),
    review_title = col_character(), verified_purchase = col_character(),
    helpful_votes = col_integer()
  ),
  inventory_monthly = cols(
    snapshot_id = col_integer(), product_id = col_integer(), year_month = col_character(),
    opening_stock = col_integer(), units_received = col_integer(), units_sold = col_integer(),
    closing_stock = col_integer(), stockout_days = col_integer(), stock_value_usd = col_double()
  ),
  marketing_daily = cols(
    date = col_date(), channel = col_character(), impressions = col_integer(),
    clicks = col_integer(), sessions = col_integer(), spend_usd = col_double(),
    orders = col_integer(), new_customers = col_integer()
  ),
  sales_targets = cols(
    target_id = col_integer(), year_month = col_character(), category_id = col_integer(),
    top_category = col_character(), revenue_target_usd = col_double()
  )
)

# Import ----------------------------------------------------------------------
# to_logical() also takes T/F and 1/0 in case a file uses those
to_logical <- function(x) {
  out <- toupper(trimws(x))
  case_when(out %in% c("TRUE", "T", "1")  ~ TRUE,
            out %in% c("FALSE", "F", "0") ~ FALSE,
            TRUE ~ NA)
}

# Reads one table and warns if readr hit parsing problems (shows the first one)
read_table <- function(name, spec) {
  path <- file.path(data_dir, paste0(name, ".csv"))
  if (!file.exists(path)) stop("Missing file: ", path)
  df <- read_csv(path, col_types = spec, na = c("", "NA"), progress = FALSE)
  p <- problems(df)
  if (nrow(p) > 0) {
    warning(name, ": ", nrow(p), " parsing problem(s). First one: ",
            p$col[1], " row ", p$row[1], " expected ", p$expected[1], " got '", p$actual[1], "'")
  }
  # Only text columns that hold nothing but True/False are converted
  bool_cols <- names(spec$cols)[map_lgl(spec$cols, ~ inherits(.x, "collector_character"))]
  bool_cols <- bool_cols[map_lgl(bool_cols, ~ all(na.omit(unique(df[[.x]])) %in% c("True", "False")))]
  df %>% mutate(across(all_of(bool_cols), to_logical))
}

message("Reading ", length(specs), " tables from '", data_dir, "' ...")
raw <- imap(specs, ~ read_table(.y, .x))

# Data checks -----------------------------------------------------------------
# Rows, columns and NA cells per table, biggest table first
overview <- tibble(
  table   = names(raw),
  rows    = map_int(raw, nrow),
  columns = map_int(raw, ncol),
  na_cells = map_int(raw, ~ sum(is.na(.x)))
) %>% arrange(desc(rows))
print(overview, n = Inf)
message("Total rows: ", format(sum(overview$rows), big.mark = ","))

# Primary keys should be unique. marketing_daily has no single id column, so
# it is not in this list.
pk <- c(calendar = "date", categories = "category_id", sellers = "seller_id",
        products = "product_id", warehouses = "warehouse_id", customers = "customer_id",
        campaigns = "campaign_id", orders = "order_id", order_items = "order_item_id",
        payments = "payment_id", shipments = "shipment_id", returns = "return_id",
        reviews = "review_id", inventory_monthly = "snapshot_id", sales_targets = "target_id")
pk_ok <- imap_lgl(pk, ~ !anyDuplicated(raw[[.y]][[.x]]))
if (!all(pk_ok)) warning("Duplicate primary keys in: ", paste(names(pk_ok)[!pk_ok], collapse = ", "))

# Foreign keys: count child values with no matching parent row (orphans)
fk_check <- function(child, col, parent, pcol) {
  vals <- raw[[child]][[col]]
  vals <- vals[!is.na(vals)]
  tibble(link = paste0(child, ".", col, " -> ", parent, ".", pcol),
         orphans = sum(!vals %in% raw[[parent]][[pcol]]))
}
fk <- bind_rows(
  fk_check("orders", "customer_id", "customers", "customer_id"),
  fk_check("orders", "campaign_id", "campaigns", "campaign_id"),
  fk_check("order_items", "order_id", "orders", "order_id"),
  fk_check("order_items", "product_id", "products", "product_id"),
  fk_check("order_items", "seller_id", "sellers", "seller_id"),
  fk_check("products", "category_id", "categories", "category_id"),
  fk_check("products", "seller_id", "sellers", "seller_id"),
  fk_check("payments", "order_id", "orders", "order_id"),
  fk_check("shipments", "order_id", "orders", "order_id"),
  fk_check("shipments", "warehouse_id", "warehouses", "warehouse_id"),
  fk_check("returns", "order_item_id", "order_items", "order_item_id"),
  fk_check("reviews", "order_item_id", "order_items", "order_item_id"),
  fk_check("reviews", "customer_id", "customers", "customer_id"),
  fk_check("inventory_monthly", "product_id", "products", "product_id"),
  fk_check("sales_targets", "category_id", "categories", "category_id")
)
print(fk, n = Inf)
if (any(fk$orphans > 0)) warning("Some foreign keys do not resolve, see table above.")

# Orders should cover 2021-2025
message("Orders from ", min(as.Date(raw$orders$order_datetime)),
        " to ", max(as.Date(raw$orders$order_datetime)))

# Quick look at the two main sales tables
glimpse(raw$orders)
glimpse(raw$order_items)

# Save ------------------------------------------------------------------------
saveRDS(raw, file.path(output_dir, "01_raw_data.rds"))
message("Saved ", file.path(output_dir, "01_raw_data.rds"))
