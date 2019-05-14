
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