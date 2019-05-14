


ggplot() +
  geom_smooth(data = all_sunrises, 
              aes(day_of_year, total_mins),
              color = "red") +
  geom_smooth(data = all_sunsets, 
              aes(day_of_year, total_mins)) +
  theme_bw()
