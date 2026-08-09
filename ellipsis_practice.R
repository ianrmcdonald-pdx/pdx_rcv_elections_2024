


# Internal function
calculate_total <- function(base_price, tax = 0.05, discount = 0, overall_quota_status = 999) {
  (base_price - discount) * (1 + tax) * overall_quota_status
}

# Wrapper function passing all extra arguments using ...
summarize_sale <- function(item_name, ...) {
  total <- calculate_total(...)
  cat("Item:", item_name, "\nTotal Cost:", total, "\n")
}

# Usage: 'base_price' and 'discount' are passed dynamically through ...
summarize_sale(item_name = "Laptop", base_price = 1000, discount = 100,
               overall_quota_status = 5000)
