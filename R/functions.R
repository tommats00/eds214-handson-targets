# R/functions.R


# read data
get_data <- function(file) {
  read_csv(file, col_types = cols()) %>%
    drop_na()
}

# compute the the average ice cover across the different lakes
avg_icecover <- function(my_ntl_icecover) {
  ntl_icecover_avg <- my_ntl_icecover %>%
  drop_na(ice_duration) %>% 
  group_by(year) %>%
  summarise(ice_duration = mean(ice_duration)) %>%
  rename(avg_ice_duration = ice_duration)
}

# compute the the average air temp from  Nov to April (next year)
avg_airtemp <- function(my_ntl_airtemp) {
# Add a column to group the Fall and Spring season into a same year, similarly to what is done when defining hydrological year
ntl_airtemp_hydro <- my_ntl_airtemp %>%
  mutate(hydroyear = if_else(month(sampledate) < 10, year-1, year))

# Compute the average air temperature from Nov to April
ntl_airtemp_avg_winter <-  ntl_airtemp_hydro %>%
  filter(month(sampledate) %in% c(11:12,1:4)) %>% # filter the months from Nov to April
  group_by(hydroyear) %>%
  summarise(avg_air_temp_adjusted = mean(ave_air_temp_adjusted))
}


# join the ice cover and air temperatures data sets
join_ntl <- function(ice, air){
  left_join(ice, air, by = c("year" = "hydroyear")) %>%
    drop_na()
}


# plot the scatter plot on the joined data set
scatter_ntl<- function(ntl_all){
  ggplot(data = ntl_all,
         aes(y = avg_ice_duration, x = avg_air_temp_adjusted)) + geom_point(alpha = 0.8) +
    theme_minimal() +
    labs(
      title = "Cold Season Mean Air Temperature and Ice Duration of Lakes",
      y = "Ice Duration (Days)",
      x = "Mean Air Temperature Nov-April (Celsius)",
      subtitle = "North Temperate Lakes LTER"
    ) +
    geom_smooth(
      method = "lm",
      color = "black",
      se = FALSE,
      linewidth = 0.3
    )
}
