# 리뷰 스크래핑
setwd('C:/Users/sim/Desktop/iCloudDrive/iCloudDrive/4-1/Airbnb/Test/NTA별 ACS')
library(RSelenium)
library(dplyr)
library(lubridate)
library(wdman)
library(purrr)
library(netstat)
library(rvest)
library(jsonlite)
library(httr)
library(tidyverse)
library(xml2)
library(binman)
library(stringr)
library(sf)
library(ggplot2)
# plot 1: location of listings
load('RData/Entire.RData')
point_df <- Entire %>%
  select(id, latitude, longitude) %>%
  distinct(id, .keep_all = TRUE)
points_sf <- st_as_sf(point_df, coords = c("longitude", "latitude"), crs = 4326)
nta <- st_read("NYC NTA2020.geojson")  # 경로를 적절히 수정하세요
ggplot() +
  geom_sf(data = nta, fill = "lightblue", color = "white") +
  geom_sf(data = points_sf, color = "red", size = 1, alpha = 0.6) +
  labs(title = "NYC NTA with Latitude/Longitude Points") +
  theme_minimal()

# generate coordinate -----------------------------------------------------


# 1. 뉴욕시 전체를 덮는 격자 만들기 (기존 방식 유지)
nyc_bbox <- st_bbox(c(xmin = -74.3, xmax = -73.7,
                      ymin = 40.45, ymax = 40.95),
                    crs = st_crs(4326))
nyc_bbox_sf <- st_as_sfc(nyc_bbox)
# 1. 전체 grid 생성
grid <- st_make_grid(nyc_bbox_sf, cellsize = c(0.01, 0.01), square = TRUE)

# 2. grid를 단일 sf 객체로 변환
grid_sf_all <- st_sf(geometry = grid)

# 3. 교차하는 격자만 필터링
grid_in_nyc <- st_filter(grid_sf_all, nta, .predicate = st_intersects)

# 4. grid_id 새로 부여
grid_in_nyc$grid_id <- seq_len(nrow(grid_in_nyc))

div=ggplot() +
  geom_sf(data = nta, fill = "lightblue", color = "white") +
  geom_sf(data = grid_in_nyc, fill = NA, color = "black", size = 0.2) +
  theme_minimal()

ggsave("Figure/Division.eps", plot = div,width = 5, height = 5)

# 꼭짓점 추출 함수
get_ne_sw <- function(polygon, id) {
  coords <- st_coordinates(polygon)[1:5, ]  # 사각형일 경우 5개 (마지막은 첫 점 반복)
  
  # 일반적으로 st_coordinates() 순서:
  # 1 = SW, 2 = NW, 3 = NE, 4 = SE, 5 = SW 다시
  
  data.frame(
    grid_id = id,
    ne_lat = coords[3, "Y"], ne_lng = coords[3, "X"],
    sw_lat = coords[1, "Y"], sw_lng = coords[1, "X"]
  )
}

# 모든 격자에 대해 적용
coord <- map2_dfr(grid_in_nyc$geometry, grid_in_nyc$grid_id, get_ne_sw)
grid_in_nyc$center <- st_centroid(grid_in_nyc$geometry)

ggplot() +
  geom_sf(data = grid_in_nyc, fill = NA, color = "black", size = 0.2) +
  geom_sf(data = nta, fill = "lightblue", color = "white") +
  geom_sf_text(data = grid_in_nyc, aes(geometry = center, label = 1:nrow(grid_in_nyc)),
               size = 3, color = "red") +
  theme_minimal() +
  labs(title = "Grid Over NYC with Row Numbers")

points_with_grid <- st_join(points_sf, grid_in_nyc, join = st_within)

# 2. grid별로 포인트 개수 세기
point_counts <- points_with_grid %>%
  st_drop_geometry() %>%
  count(grid_id, name = "n_points")

# 3. grid 데이터에 개수 붙이기 (없으면 0으로)
grid_in_nyc <- left_join(grid_in_nyc, point_counts, by = "grid_id") %>%
  mutate(n_points = ifelse(is.na(n_points), 0, n_points))
ggplot() +
  geom_sf(data = grid_in_nyc, aes(fill = n_points), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  geom_sf(data = nta, fill = NA, color = "white") +
  theme_minimal() +
  labs(title = "Number of Points per Grid in NYC",
       fill = "Point Count")

# func --------------------------------------------------------------
library(rvest)
extract_listing_info_all <- function(listing_blocks) {
  # 1. id 추출
  ids <- map_chr(listing_blocks, ~ .x %>%
                   html_element("[aria-labelledby^='title_']") %>%
                   html_attr("aria-labelledby") %>%
                   str_extract("\\d+"))
  
  # 2. 가격 추출
  extract_price <- function(node) {
    price <- node %>%
      html_element("span.u1dgw2qm") %>%
      html_text(trim = TRUE)
    
    if (is.na(price)) {
      price <- node %>%
        html_element("span._hb913q") %>%
        html_text(trim = TRUE)
    }
    
    return(price)
  }
  prices <- map_chr(listing_blocks, extract_price)
  
  # 3. Guest favorite 여부
  guest_favorite <- function(node) {
    guest_fav <- node %>%
      html_element("div.t1qa5xaj") %>%
      html_text(trim = TRUE) %>%
      str_detect("Guest favorite") %>%
      as.integer()
    
    if (is.na(guest_fav)) guest_fav <- 0
    return(tibble(guest_fav = guest_fav))
  }
  guest_df <- map_dfr(listing_blocks, guest_favorite)
  
  # 4. Rating, 리뷰 수
  rating_vec <- listing_blocks %>%
    html_element("span.a8jt5op.atm_3f_idpfg4.atm_7h_hxbz6r.atm_7i_ysn8ba.atm_e2_t94yts.atm_ks_zryt35.atm_l8_idpfg4.atm_vv_1q9ccgz.atm_vy_t94yts.au0q88m.atm_mk_stnw88.atm_tk_idpfg4.dir.dir-ltr") %>%
    html_text(trim = TRUE)
  
  rating_num <- str_extract(rating_vec, "\\d\\.\\d+") %>% as.numeric()
  review_count <- str_extract(rating_vec, "\\d+(?= reviews)") %>% as.numeric()
  
  rating_df <- tibble(
    rating = rating_num,
    number_of_reviews = review_count
  )
  
  #5. Flexible Date
  date_info <- map_dfr(listing_blocks, function(block) {
    url_meta <- block %>%
      html_element("meta[itemprop='url']") %>%
      html_attr("content")
    
    Checkin <- str_extract(url_meta, "(?<=check_in=)\\d{4}-\\d{2}-\\d{2}")
    Checkout <- str_extract(url_meta, "(?<=check_out=)\\d{4}-\\d{2}-\\d{2}")
    
    tibble(
      Checkin = Checkin,
      Checkout = Checkout
    )
  })
  
  # 6. 모두 합치기
  combined_df <- tibble(
    id = ids,
    price = prices,
    guest_fav = guest_df$guest_fav,
    rating = rating_df$rating,
    number_of_reviews = rating_df$number_of_reviews,
    Checkin = date_info$Checkin,
    Checkout = date_info$Checkout
  )
  
  return(combined_df)
}

# scrap set up ----------------------------------------------------------------------

binman::list_versions("chromedriver")

eCaps <- list(
  chromeOptions = list(
    args = c(
      "--no-sandbox",
      "--disable-cookies",
      "--disable-extensions",
      "--incognito",              # ✅ 시크릿 모드
      "--start-fullscreen"       # ✅ 전체화면 모드
    ),
    binary = "C:/Users/sim/Downloads/chrome-win64/chrome.exe"  # ✅ binary는 여기!
  )
)

#remDr <- rsDriver(browser = "chrome",chromever = "131.0.6778.85", port =  2681L)
remDr <- rsDriver(browser = "chrome",chromever = "139.0.7258.5",port = 2345L, check=F,
                  extraCapabilities = eCaps)

driver <- remDr$client
driver$setTimeout(type = "script", milliseconds = 2000000000)
overall_start_time <- Sys.time()


# one month test --------------------------------------------------------------------

checkin=as.Date("2025-08-01")
checkout=as.Date("2025-08-31")

ratio <- tibble(
  Checkin = Date(),
  Checkout = Date(),
  obs_total = numeric(),
  id = character(),
  price = character(),
  guest_fav = integer(),
  rating = numeric(),
  number_of_reviews = numeric(),
  page_num = integer(),
  grid_id = integer(),
  timestamp = POSIXct(),
  superhost = integer(),
  ne_lat = numeric(),
  ne_lng = numeric(),
  sw_lat = numeric(),
  sw_lng = numeric()
  
)

#save(ratio,file='ratio.RData')

for (i in 1:nrow(coord)) {
  cat("Processing grid", i, "\n")
  page_num <- 1
  url <- paste0(
    "https://www.airbnb.com/s/New-York-City--New-York--United-States/homes?",
    "refinement_paths%5B%5D=%2Fhomes",
    "&checkin=", checkin,
    "&checkout=", checkout,
    "&room_types%5B%5D=Entire%20home%2Fapt",
    "&query=New%20York%20City%2C%20New%20York%2C%20United%20States",
    "&ne_lat=", coord$ne_lat[i],
    "&ne_lng=", coord$ne_lng[i],
    "&sw_lat=", coord$sw_lat[i],
    "&sw_lng=", coord$sw_lng[i],
    "&search_by_map=true&locale=en&currency=USD&disable_auto_translation=true"
  )
  
  driver$navigate(url)
  Sys.sleep(2.5)
  current_time <- Sys.time()
  
  list <- read_html(driver$getPageSource()[[1]])
  
  span_text <- list %>%
    html_nodes("h1") %>%
    .[1] %>%
    html_elements("span") %>%
    .[2] %>%
    html_text2()
  
  # ▶️ Case 1: 방이 없는 경우 → 바로 0으로 기록 후 skip
  if (length(span_text) == 0) {
    empty_row <- tibble(
      grid_id = i,
      Checkin = checkin,
      Checkout = checkout,
      obs_total = 0,
      sup_total = 0,
      id = NA_character_,
      price = NA_character_,
      guest_fav = NA_integer_,
      rating = NA_real_,
      number_of_reviews = NA_real_,
      page_num = NA_integer_,
      timestamp = current_time,
      superhost = 0,
      ne_lat= coord$ne_lat[i],
      ne_lng= coord$ne_lng[i],
      sw_lat= coord$sw_lat[i],
      sw_lng= coord$sw_lng[i]
    )
    ratio <- bind_rows(ratio, empty_row)
    next  # 슈퍼호스트도 검색하지 않음
  }
  
  # ▶️ Case 2: 숙소가 있는 경우 → obs_total 수집
  obs_total <- as.numeric(str_extract(span_text, "\\d+"))
  max_page <- min(15, ceiling(obs_total / 18))
  
  page_num <- 1
  
  repeat {
    list <- read_html(driver$getPageSource()[[1]])
    
    listing_blocks <- list %>%
      html_node("div[class*='gsgwcjk']") %>%
      html_elements(xpath = "./div")%>%
      discard(~ html_attr(.x, "class") == "f1rykmw3 atm_da_cbdd7d dir dir-ltr") # ✅ "추천 숙소" 제외
    
    listing_info <- extract_listing_info_all(listing_blocks)
    listing_info$grid_id <- i
    listing_info$Checkin <- checkin
    listing_info$Checkout <- checkout
    listing_info$obs_total <- obs_total
    listing_info$page_num <- page_num
    listing_info$timestamp <- current_time
    listing_info$superhost <- 0
    listing_info$ne_lat <- coord$ne_lat[i]
    listing_info$ne_lng <- coord$ne_lng[i]
    listing_info$sw_lat <- coord$sw_lat[i]
    listing_info$sw_lng <- coord$sw_lng[i]
    
    ratio <- bind_rows(ratio, listing_info)
    
    if (page_num >= max_page) break
    
    tryCatch({
      next_btn <- driver$findElement("css selector", "a[aria-label='Next']")
      next_btn$clickElement()
      Sys.sleep(3.5)
      page_num <- page_num + 1
    }, error = function(e) {
      break
    })
  }
  
  # ▶️ 슈퍼호스트 검색
  sup_url <- paste0(url, "&superhost=true")
  sup_ids <- c()
  sup_total <- 0
  page_num_sup <- 1
  
  driver$navigate(sup_url)
  Sys.sleep(2.5)
  
  repeat {
    list <- read_html(driver$getPageSource()[[1]])
    
    if (page_num_sup == 1) {
      span_text_sup <- list %>%
        html_nodes("h1") %>%
        .[1] %>%
        html_elements("span") %>%
        .[2] %>%
        html_text2()
      
      if (length(span_text_sup) > 0) {
        maybe_number <- str_extract(span_text_sup, "\\d+")
        if (!is.na(maybe_number)) {
          sup_total <- as.numeric(maybe_number)
        }
      }
      max_page_sup <- min(15, ceiling(sup_total / 18))
    }
    
    sup_blocks <- list %>%
      html_node("div[class*='gsgwcjk']") %>%
      html_elements(xpath = "./div") %>%
      discard(~ html_attr(.x, "class") == "f1rykmw3 atm_da_cbdd7d dir dir-ltr")
    
    ids <- map_chr(sup_blocks, ~ .x %>%
                     html_element("[aria-labelledby^='title_']") %>%
                     html_attr("aria-labelledby") %>%
                     str_extract("\\d+"))
    
    sup_ids <- c(sup_ids, ids)
    
    if (page_num_sup >= max_page_sup) break
    
    tryCatch({
      next_btn <- driver$findElement("css selector", "a[aria-label='Next']")
      next_btn$clickElement()
      Sys.sleep(3.5)
      page_num_sup <- page_num_sup + 1
    }, error = function(e) {
      break
    })
  }
  
  # ▶️ 슈퍼호스트 ID 반영 및 sup_total 기록
  ratio$superhost[ratio$grid_id == i & ratio$id %in% sup_ids] <- 1
  ratio$sup_total[ratio$grid_id == i] <- sup_total
}

dup_ids <- ratio %>%
  group_by(id) %>%
  filter(n() > 1) %>%
  distinct(id)

# flexible 12 --------------------------------------------------------------------

ratio_flex12 <- tibble(
  Checkin = character(),
  Checkout = character(),
  obs_total = numeric(),
  sup_total= integer(),
  id = character(),
  price = character(),
  guest_fav = integer(),
  rating = numeric(),
  number_of_reviews = numeric(),
  page_num = integer(),
  grid_id = integer(),
  timestamp = POSIXct(),
  superhost = integer(),
  ne_lat = numeric(),
  ne_lng = numeric(),
  sw_lat = numeric(),
  sw_lng = numeric(),
  span_text = character()
  
)

#save(ratio,file='ratio.RData')

for (i in 1:nrow(coord)) {
  cat("Processing grid", i, "\n")
  page_num <- 1
  url <- paste0(
    "https://www.airbnb.com/s/New-York-City--New-York--United-States/homes?",
    "refinement_paths%5B%5D=%2Fhomes",
    "&date_picker_type=flexible_dates&flexible_trip_lengths%5B%5D=one_month",
    "&flexible_trip_dates%5B%5D=january",
    "&flexible_trip_dates%5B%5D=february",
    "&flexible_trip_dates%5B%5D=march",
    "&flexible_trip_dates%5B%5D=april",
    "&flexible_trip_dates%5B%5D=may",
    "&flexible_trip_dates%5B%5D=june",
    "&flexible_trip_dates%5B%5D=july",
    "&flexible_trip_dates%5B%5D=august",
    "&flexible_trip_dates%5B%5D=september",
    "&flexible_trip_dates%5B%5D=october&",
    "&flexible_trip_dates%5B%5D=november",
    "&flexible_trip_dates%5B%5D=december",
    "&room_types%5B%5D=Entire%20home%2Fapt",
    "&query=New%20York%20City%2C%20New%20York%2C%20United%20States",
    "&ne_lat=", coord$ne_lat[i],
    "&ne_lng=", coord$ne_lng[i],
    "&sw_lat=", coord$sw_lat[i],
    "&sw_lng=", coord$sw_lng[i],
    "&search_by_map=true&locale=en&currency=USD&disable_auto_translation=true"
  )
  
  driver$navigate(url)
  Sys.sleep(4.5)
  current_time <- Sys.time()
  
  list <- read_html(driver$getPageSource()[[1]])
  # 정확히 눈에 보이는 텍스트를 가진 span을 Selenium으로 직접 찾기
  #text <- driver$findElement(using = "xpath", "//span[contains(text(), 'home within map area')]")$getElementText()[[1]]
  
  span_text <- suppressMessages(suppressWarnings(
    tryCatch({
      driver$findElement(using = "xpath", "//span[contains(text(), 'home')]")$getElementText()[[1]]
    }, error = function(e) {
      character(0)  # 요소가 없을 때 빈 character(0) 반환
    })
  ))

  # ▶️ Case 1: 방이 없는 경우 → 바로 0으로 기록 후 skip
  if (length(span_text) == 0) {
    empty_row <- tibble(
      grid_id = i,
      Checkin = NA_character_,
      Checkout = NA_character_,
      obs_total = 0,
      sup_total = 0,
      id = NA_character_,
      price = NA_character_,
      guest_fav = NA_integer_,
      rating = NA_real_,
      number_of_reviews = NA_real_,
      page_num = NA_integer_,
      timestamp = current_time,
      superhost = 0,
      ne_lat= coord$ne_lat[i],
      ne_lng= coord$ne_lng[i],
      sw_lat= coord$sw_lat[i],
      sw_lng= coord$sw_lng[i],
      span_text = ""
    )
    ratio_flex12 <- bind_rows(ratio_flex12, empty_row)
    next  # 슈퍼호스트도 검색하지 않음
  }
  
  # ▶️ Case 2: 숙소가 있는 경우 → obs_total 수집
  obs_total <- as.numeric(str_extract(span_text, "\\d+"))
  max_page <- min(15, ceiling(obs_total / 18))
  
  page_num <- 1
  
  repeat {
    list <- read_html(driver$getPageSource()[[1]])
    
    listing_blocks <- list %>%
      html_node("div[class*='gsgwcjk']") %>%
      html_elements(xpath = "./div")%>%
      discard(~ html_attr(.x, "class") == "f1rykmw3 atm_da_cbdd7d dir dir-ltr") # ✅ "추천 숙소" 제외
    
    listing_info <- extract_listing_info_all(listing_blocks)
    listing_info$grid_id <- i
    listing_info$obs_total <- obs_total
    listing_info$page_num <- page_num
    listing_info$timestamp <- current_time
    listing_info$superhost <- 0
    listing_info$ne_lat <- coord$ne_lat[i]
    listing_info$ne_lng <- coord$ne_lng[i]
    listing_info$sw_lat <- coord$sw_lat[i]
    listing_info$sw_lng <- coord$sw_lng[i]
    listing_info$span_text <- span_text
    
    ratio_flex12 <- bind_rows(ratio_flex12, listing_info)
    
    if (page_num >= max_page) break
    
    tryCatch({
      next_btn <- driver$findElement("css selector", "a[aria-label='Next']")
      next_btn$clickElement()
      Sys.sleep(3.5)
      page_num <- page_num + 1
    }, error = function(e) {
      break
    })
  }
  
  # ▶️ 슈퍼호스트 검색
  sup_url <- paste0(url, "&superhost=true")
  sup_ids <- c()
  sup_total <- 0
  page_num_sup <- 1
  
  driver$navigate(sup_url)
  Sys.sleep(4.5)
  
  repeat {
    list <- read_html(driver$getPageSource()[[1]])
    
    if (page_num_sup == 1) {

      span_text_sup <- suppressMessages(suppressWarnings(
        tryCatch({
          driver$findElement(using = "xpath", "//span[contains(text(), 'home')]")$getElementText()[[1]]
        }, error = function(e) {
          character(0)  # 요소가 없을 때 빈 character(0) 반환
        })
      ))
      if (length(span_text_sup) > 0) {
        maybe_number <- str_extract(span_text_sup, "\\d+")
        if (!is.na(maybe_number)) {
          sup_total <- as.numeric(maybe_number)
        }
      }
      max_page_sup <- min(15, ceiling(sup_total / 18))
    }
    
    sup_blocks <- list %>%
      html_node("div[class*='gsgwcjk']") %>%
      html_elements(xpath = "./div") %>%
      discard(~ html_attr(.x, "class") == "f1rykmw3 atm_da_cbdd7d dir dir-ltr")
    
    ids <- map_chr(sup_blocks, ~ .x %>%
                     html_element("[aria-labelledby^='title_']") %>%
                     html_attr("aria-labelledby") %>%
                     str_extract("\\d+"))
    
    sup_ids <- c(sup_ids, ids)
    
    if (page_num_sup >= max_page_sup) break
    
    tryCatch({
      next_btn <- driver$findElement("css selector", "a[aria-label='Next']")
      next_btn$clickElement()
      Sys.sleep(3.5)
      page_num_sup <- page_num_sup + 1
    }, error = function(e) {
      break
    })
  }
  
  # ▶️ 슈퍼호스트 ID 반영 및 sup_total 기록
  ratio_flex12$superhost[ratio_flex12$grid_id == i & ratio_flex12$id %in% sup_ids] <- 1
  ratio_flex12$sup_total[ratio_flex12$grid_id == i] <- sup_total
}

save(ratio_flex12,file='scrapped data/ratio_flex12.RData')

# flexible 3 --------------------------------------------------------------------

ratio_flex3 <- tibble(
  Checkin = character(),
  Checkout = character(),
  obs_total = numeric(),
  sup_total= integer(),
  id = character(),
  price = character(),
  guest_fav = integer(),
  rating = numeric(),
  number_of_reviews = numeric(),
  page_num = integer(),
  grid_id = integer(),
  timestamp = POSIXct(),
  superhost = integer(),
  ne_lat = numeric(),
  ne_lng = numeric(),
  sw_lat = numeric(),
  sw_lng = numeric(),
  span_text = character()
  
)

#save(ratio,file='ratio.RData')

for (i in 1:nrow(coord)) {
  cat("Processing grid", i, "\n")
  page_num <- 1
  url <- paste0(
    "https://www.airbnb.com/s/New-York-City--New-York--United-States/homes?",
    "refinement_paths%5B%5D=%2Fhomes",
    "&date_picker_type=flexible_dates&flexible_trip_lengths%5B%5D=one_month",
    #"&flexible_trip_dates%5B%5D=january",
    #"&flexible_trip_dates%5B%5D=february",
    #"&flexible_trip_dates%5B%5D=march",
    #"&flexible_trip_dates%5B%5D=april",
    #"&flexible_trip_dates%5B%5D=may","
    #&flexible_trip_dates%5B%5D=june",
    "&flexible_trip_dates%5B%5D=july",
    "&flexible_trip_dates%5B%5D=august",
    "&flexible_trip_dates%5B%5D=september",
    #"&flexible_trip_dates%5B%5D=october&",
    #"&flexible_trip_dates%5B%5D=november",
    #"&flexible_trip_dates%5B%5D=december",
    "&room_types%5B%5D=Entire%20home%2Fapt",
    "&query=New%20York%20City%2C%20New%20York%2C%20United%20States",
    "&ne_lat=", coord$ne_lat[i],
    "&ne_lng=", coord$ne_lng[i],
    "&sw_lat=", coord$sw_lat[i],
    "&sw_lng=", coord$sw_lng[i],
    "&search_by_map=true&locale=en&currency=USD&disable_auto_translation=true"
  )
  
  driver$navigate(url)
  Sys.sleep(4.5)
  current_time <- Sys.time()
  
  list <- read_html(driver$getPageSource()[[1]])
  # 정확히 눈에 보이는 텍스트를 가진 span을 Selenium으로 직접 찾기
  #text <- driver$findElement(using = "xpath", "//span[contains(text(), 'home within map area')]")$getElementText()[[1]]
  
  span_text <- suppressMessages(suppressWarnings(
    tryCatch({
      driver$findElement(using = "xpath", "//span[contains(text(), 'home')]")$getElementText()[[1]]
    }, error = function(e) {
      character(0)  # 요소가 없을 때 빈 character(0) 반환
    })
  ))
  
  # ▶️ Case 1: 방이 없는 경우 → 바로 0으로 기록 후 skip
  if (length(span_text) == 0) {
    empty_row <- tibble(
      grid_id = i,
      Checkin = NA_character_,
      Checkout = NA_character_,
      obs_total = 0,
      sup_total = 0,
      id = NA_character_,
      price = NA_character_,
      guest_fav = NA_integer_,
      rating = NA_real_,
      number_of_reviews = NA_real_,
      page_num = NA_integer_,
      timestamp = current_time,
      superhost = 0,
      ne_lat= coord$ne_lat[i],
      ne_lng= coord$ne_lng[i],
      sw_lat= coord$sw_lat[i],
      sw_lng= coord$sw_lng[i],
      span_text = ""
    )
    ratio_flex3 <- bind_rows(ratio_flex3, empty_row)
    next  # 슈퍼호스트도 검색하지 않음
  }
  
  # ▶️ Case 2: 숙소가 있는 경우 → obs_total 수집
  obs_total <- as.numeric(str_extract(span_text, "\\d+"))
  max_page <- min(15, ceiling(obs_total / 18))
  
  page_num <- 1
  
  repeat {
    list <- read_html(driver$getPageSource()[[1]])
    
    listing_blocks <- list %>%
      html_node("div[class*='gsgwcjk']") %>%
      html_elements(xpath = "./div")%>%
      discard(~ html_attr(.x, "class") == "f1rykmw3 atm_da_cbdd7d dir dir-ltr") # ✅ "추천 숙소" 제외
    
    listing_info <- extract_listing_info_all(listing_blocks)
    listing_info$grid_id <- i
    listing_info$obs_total <- obs_total
    listing_info$page_num <- page_num
    listing_info$timestamp <- current_time
    listing_info$superhost <- 0
    listing_info$ne_lat <- coord$ne_lat[i]
    listing_info$ne_lng <- coord$ne_lng[i]
    listing_info$sw_lat <- coord$sw_lat[i]
    listing_info$sw_lng <- coord$sw_lng[i]
    listing_info$span_text <- span_text
    
    ratio_flex3 <- bind_rows(ratio_flex3, listing_info)
    
    if (page_num >= max_page) break
    
    tryCatch({
      next_btn <- driver$findElement("css selector", "a[aria-label='Next']")
      next_btn$clickElement()
      Sys.sleep(3.5)
      page_num <- page_num + 1
    }, error = function(e) {
      break
    })
  }
  
  # ▶️ 슈퍼호스트 검색
  sup_url <- paste0(url, "&superhost=true")
  sup_ids <- c()
  sup_total <- 0
  page_num_sup <- 1
  
  driver$navigate(sup_url)
  Sys.sleep(4.5)
  
  repeat {
    list <- read_html(driver$getPageSource()[[1]])
    
    if (page_num_sup == 1) {
      
      span_text_sup <- suppressMessages(suppressWarnings(
        tryCatch({
          driver$findElement(using = "xpath", "//span[contains(text(), 'home')]")$getElementText()[[1]]
        }, error = function(e) {
          character(0)  # 요소가 없을 때 빈 character(0) 반환
        })
      ))
      if (length(span_text_sup) > 0) {
        maybe_number <- str_extract(span_text_sup, "\\d+")
        if (!is.na(maybe_number)) {
          sup_total <- as.numeric(maybe_number)
        }
      }
      max_page_sup <- min(15, ceiling(sup_total / 18))
    }
    
    sup_blocks <- list %>%
      html_node("div[class*='gsgwcjk']") %>%
      html_elements(xpath = "./div") %>%
      discard(~ html_attr(.x, "class") == "f1rykmw3 atm_da_cbdd7d dir dir-ltr")
    
    ids <- map_chr(sup_blocks, ~ .x %>%
                     html_element("[aria-labelledby^='title_']") %>%
                     html_attr("aria-labelledby") %>%
                     str_extract("\\d+"))
    
    sup_ids <- c(sup_ids, ids)
    
    if (page_num_sup >= max_page_sup) break
    
    tryCatch({
      next_btn <- driver$findElement("css selector", "a[aria-label='Next']")
      next_btn$clickElement()
      Sys.sleep(3.5)
      page_num_sup <- page_num_sup + 1
    }, error = function(e) {
      break
    })
  }
  
  # ▶️ 슈퍼호스트 ID 반영 및 sup_total 기록
  ratio_flex3$superhost[ratio_flex3$grid_id == i & ratio_flex3$id %in% sup_ids] <- 1
  ratio_flex3$sup_total[ratio_flex3$grid_id == i] <- sup_total
}

save(ratio_flex3,file='scrapped data/ratio_flex3.RData')


# flexible 6 --------------------------------------------------------------------

ratio_flex6 <- tibble(
  Checkin = character(),
  Checkout = character(),
  obs_total = numeric(),
  sup_total= integer(),
  id = character(),
  price = character(),
  guest_fav = integer(),
  rating = numeric(),
  number_of_reviews = numeric(),
  page_num = integer(),
  grid_id = integer(),
  timestamp = POSIXct(),
  superhost = integer(),
  ne_lat = numeric(),
  ne_lng = numeric(),
  sw_lat = numeric(),
  sw_lng = numeric(),
  span_text = character()
  
)

#save(ratio,file='ratio.RData')

for (i in 1:nrow(coord)) {
  cat("Processing grid", i, "\n")
  page_num <- 1
  url <- paste0(
    "https://www.airbnb.com/s/New-York-City--New-York--United-States/homes?",
    "refinement_paths%5B%5D=%2Fhomes",
    "&date_picker_type=flexible_dates&flexible_trip_lengths%5B%5D=one_month",
    #"&flexible_trip_dates%5B%5D=january",
    #"&flexible_trip_dates%5B%5D=february",
    #"&flexible_trip_dates%5B%5D=march",
    #"&flexible_trip_dates%5B%5D=april",
    #"&flexible_trip_dates%5B%5D=may","
    #&flexible_trip_dates%5B%5D=june",
    "&flexible_trip_dates%5B%5D=july",
    "&flexible_trip_dates%5B%5D=august",
    "&flexible_trip_dates%5B%5D=september",
    "&flexible_trip_dates%5B%5D=october&",
    "&flexible_trip_dates%5B%5D=november",
    "&flexible_trip_dates%5B%5D=december",
    "&room_types%5B%5D=Entire%20home%2Fapt",
    "&query=New%20York%20City%2C%20New%20York%2C%20United%20States",
    "&ne_lat=", coord$ne_lat[i],
    "&ne_lng=", coord$ne_lng[i],
    "&sw_lat=", coord$sw_lat[i],
    "&sw_lng=", coord$sw_lng[i],
    "&search_by_map=true&locale=en&currency=USD&disable_auto_translation=true"
  )
  
  driver$navigate(url)
  Sys.sleep(4.5)
  current_time <- Sys.time()
  
  list <- read_html(driver$getPageSource()[[1]])
  # 정확히 눈에 보이는 텍스트를 가진 span을 Selenium으로 직접 찾기
  #text <- driver$findElement(using = "xpath", "//span[contains(text(), 'home within map area')]")$getElementText()[[1]]
  
  span_text <- suppressMessages(suppressWarnings(
    tryCatch({
      driver$findElement(using = "xpath", "//span[contains(text(), 'home')]")$getElementText()[[1]]
    }, error = function(e) {
      character(0)  # 요소가 없을 때 빈 character(0) 반환
    })
  ))
  
  # ▶️ Case 1: 방이 없는 경우 → 바로 0으로 기록 후 skip
  if (length(span_text) == 0) {
    empty_row <- tibble(
      grid_id = i,
      Checkin = NA_character_,
      Checkout = NA_character_,
      obs_total = 0,
      sup_total = 0,
      id = NA_character_,
      price = NA_character_,
      guest_fav = NA_integer_,
      rating = NA_real_,
      number_of_reviews = NA_real_,
      page_num = NA_integer_,
      timestamp = current_time,
      superhost = 0,
      ne_lat= coord$ne_lat[i],
      ne_lng= coord$ne_lng[i],
      sw_lat= coord$sw_lat[i],
      sw_lng= coord$sw_lng[i],
      span_text = ""
    )
    ratio_flex6 <- bind_rows(ratio_flex6, empty_row)
    next  # 슈퍼호스트도 검색하지 않음
  }
  
  # ▶️ Case 2: 숙소가 있는 경우 → obs_total 수집
  obs_total <- as.numeric(str_extract(span_text, "\\d+"))
  max_page <- min(15, ceiling(obs_total / 18))
  
  page_num <- 1
  
  repeat {
    list <- read_html(driver$getPageSource()[[1]])
    
    listing_blocks <- list %>%
      html_node("div[class*='gsgwcjk']") %>%
      html_elements(xpath = "./div")%>%
      discard(~ html_attr(.x, "class") == "f1rykmw3 atm_da_cbdd7d dir dir-ltr") # ✅ "추천 숙소" 제외
    
    listing_info <- extract_listing_info_all(listing_blocks)
    listing_info$grid_id <- i
    listing_info$obs_total <- obs_total
    listing_info$page_num <- page_num
    listing_info$timestamp <- current_time
    listing_info$superhost <- 0
    listing_info$ne_lat <- coord$ne_lat[i]
    listing_info$ne_lng <- coord$ne_lng[i]
    listing_info$sw_lat <- coord$sw_lat[i]
    listing_info$sw_lng <- coord$sw_lng[i]
    listing_info$span_text <- span_text
    
    ratio_flex6 <- bind_rows(ratio_flex6, listing_info)
    
    if (page_num >= max_page) break
    
    tryCatch({
      next_btn <- driver$findElement("css selector", "a[aria-label='Next']")
      next_btn$clickElement()
      Sys.sleep(3.5)
      page_num <- page_num + 1
    }, error = function(e) {
      break
    })
  }
  
  # ▶️ 슈퍼호스트 검색
  sup_url <- paste0(url, "&superhost=true")
  sup_ids <- c()
  sup_total <- 0
  page_num_sup <- 1
  
  driver$navigate(sup_url)
  Sys.sleep(4.5)
  
  repeat {
    list <- read_html(driver$getPageSource()[[1]])
    
    if (page_num_sup == 1) {
      
      span_text_sup <- suppressMessages(suppressWarnings(
        tryCatch({
          driver$findElement(using = "xpath", "//span[contains(text(), 'home')]")$getElementText()[[1]]
        }, error = function(e) {
          character(0)  # 요소가 없을 때 빈 character(0) 반환
        })
      ))
      if (length(span_text_sup) > 0) {
        maybe_number <- str_extract(span_text_sup, "\\d+")
        if (!is.na(maybe_number)) {
          sup_total <- as.numeric(maybe_number)
        }
      }
      max_page_sup <- min(15, ceiling(sup_total / 18))
    }
    
    sup_blocks <- list %>%
      html_node("div[class*='gsgwcjk']") %>%
      html_elements(xpath = "./div") %>%
      discard(~ html_attr(.x, "class") == "f1rykmw3 atm_da_cbdd7d dir dir-ltr")
    
    ids <- map_chr(sup_blocks, ~ .x %>%
                     html_element("[aria-labelledby^='title_']") %>%
                     html_attr("aria-labelledby") %>%
                     str_extract("\\d+"))
    
    sup_ids <- c(sup_ids, ids)
    
    if (page_num_sup >= max_page_sup) break
    
    tryCatch({
      next_btn <- driver$findElement("css selector", "a[aria-label='Next']")
      next_btn$clickElement()
      Sys.sleep(3.5)
      page_num_sup <- page_num_sup + 1
    }, error = function(e) {
      break
    })
  }
  
  # ▶️ 슈퍼호스트 ID 반영 및 sup_total 기록
  ratio_flex6$superhost[ratio_flex6$grid_id == i & ratio_flex6$id %in% sup_ids] <- 1
  ratio_flex6$sup_total[ratio_flex6$grid_id == i] <- sup_total
}

save(ratio_flex6,file='scrapped data/ratio_flex6.RData')


# check -------------------------------------------------------------------



z= ratio_flex12 %>%
  group_by(grid_id) %>%
  summarise(
    n_repeat = n(),                    # grid_id가 반복된 횟수
    total_obs_unique = unique(obs_total)  # 해당 grid_id에 있는 total_obs 값
  ) #%>%filter(n_repeat!=total_obs_unique) 
%>%filter(n_repeat!=1)
sum(z$total_obs_unique)
count(z%>%filter(total_obs_unique>=73)) #642
summary(z$total_obs_unique)
# stat --------------------------------------------------------------------
