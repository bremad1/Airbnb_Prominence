filter(ex_super2 == "t", host_response_rate != "N/A")

# For days_since_last_review filters, create the variable first:
mutate(days_since_last_review = as.numeric(as.Date(Date) - as.Date(last_review)))

filter(
  ex_super2 == "t",
  !is.na(days_since_last_review),
  days_since_last_review >= 0,
  days_since_last_review <= 60
)

filter(
  ex_super2 == "t",
  !is.na(days_since_last_review),
  days_since_last_review >= 0,
  days_since_last_review <= 90
)

filter(
  ex_super2 == "t",
  !is.na(days_since_last_review),
  days_since_last_review >= 0,
  days_since_last_review <= 120
)

filter(
  ex_super2 == "t",
  !is.na(days_since_last_review),
  days_since_last_review >= 0,
  days_since_last_review <= 150
)

filter(
  ex_super2 == "t",
  !is.na(days_since_last_review),
  days_since_last_review >= 0,
  days_since_last_review <= 180
)

# Best minimum_nights mix so far:
mutate(
  minimum_nights_num = suppressWarnings(as.numeric(minimum_nights)),
  ex_quarter_number_of_reviews_num = suppressWarnings(as.numeric(ex_quarter_number_of_reviews))
)

filter(
  ex_super2 == "t",
  !is.na(minimum_nights_num),
  minimum_nights_num >= 30,
  !is.na(ex_quarter_number_of_reviews_num),
  ex_quarter_number_of_reviews_num <= 10
)
