library(fs)
library(glue)
library(here)
library(tidyverse)

# Get average sunrise and sunset times for vlines in Fires by Hour plot

clean_time <- function(x) {
  x %>% 
    str_extract_all(".+[am|pm]") %>% 
    as_vector() 
}

get_mins <- function(df, col = sunset) {
  q_col <- enquo(col)
  
  df %>% 
    separate(
      !!q_col, c("hr", "mn", "am_pm"),
      sep = "[ :]",
      remove = FALSE
    ) %>% 
    mutate(
      hr = as.numeric(hr),
      hr = 
        case_when(
          am_pm == "pm" ~ hr + 12,
          TRUE ~ hr
        ),
      mn = as.numeric(mn),
      total_mins = (hr*60) + mn
    ) %>% 
    select(-am_pm) %>% 
    drop_na()
}

clean_city_country <- function(x) {
  x %>% 
    str_to_lower() %>% 
    str_replace_all("\\s+", "-")
}

get_sun_rise_set <- function(month = 1,
                             year = 2018,
                             city = "new york",
                             country = "usa",
                             return = "sunrise",
                             base_url = 
                               "https://www.timeanddate.com/sun/usa/new-york?month=",
                             verbose = TRUE) {
  
  city <- city %>% clean_city_country()
  country <- country %>% clean_city_country()
  
  url <- glue("https://www.timeanddate.com/sun/{country}/{city}?month={month}&year={year}") 
  if (verbose) message(url)
  
  df <- url %>% 
    xml2::read_html() %>%
    rvest::html_nodes("table") %>%
    rvest::html_table(fill = TRUE, header = FALSE) %>% 
    purrr::pluck(1) %>% 
    as_tibble(.name_repair = "unique") %>% 
    select(1:3) 
  
  names(df) <- c("day", "sunrise", "sunset")
  
  df <- df %>% 
    slice(4:nrow(.)) %>% 
    mutate(
      sunrise = 
        sunrise %>% 
        map_chr(clean_time),
      sunset = 
        sunset %>% 
        map_chr(clean_time),
      month = month
    ) %>% 
    select(month, day, everything()) 
  
  sunsets <- 
    df %>% 
    select(-sunrise)
  
  sunsets <- 
    sunsets %>% 
    get_mins(col = sunset)
  
  sunrises <- 
    df %>% 
    select(-sunset) %>% 
    get_mins(col = sunrise)
  
  if (return == "sunrise") {
    sunrises
  } else if (return == "sunset") {
    sunsets
  }
}

# Get all 12 months
all_sunrises <- 
  map_df(1:12, .f = get_sun_rise_set, return = "sunrise")

all_sunsets <- 
  map_df(1:12, .f = get_sun_rise_set, return = "sunset")


# Write out
if (!dir_exists("data")) dir_create(here("data"))
write_csv(all_sunrises, here("data", "sunrises_nyc.csv"))
write_csv(all_sunsets, here("data", "sunsets_nyc.csv"))


# Get average sunrises and sunsets
summarise_sun_rise_set <- function(tbl) {
  tbl %>% 
    summarise(
      mean_mins = mean(total_mins, na.rm = TRUE)
    ) %>% 
    mutate(
      hr = (mean_mins/60) %>% floor(), 
      mn = mean_mins - (hr*60)
    )
}

all_sunrises %>% summarise_sun_rise_set()
all_sunsets %>% summarise_sun_rise_set()
