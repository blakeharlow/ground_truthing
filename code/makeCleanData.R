# This function takes the tibble directly from the read_excel()
# function. This given tibble should have a laser_position,
# laser_height, target_position, target_height, hypotenuse,
# and vertical_side columns at the minimum.

# This function returns the same tibble except without any aeropoint to
# aeropoint sightings and with the addition of three new columns:
# delta_x, delta_y, and delta_z.

# tib parameter is the tibble that contains the data from the
# sightings. It should have a laser_position,
# laser_height, target_position, target_height, hypotenuse,
# and vertical_side columns at the minimum.




makeCleanData <- function(tib) {
  tib %>% 
    filter(substring(laser_position,1,1) != "a" 
           | substring(target_position,1,1) != "a" ) %>% 
    mutate(delta_x = hypotenuse * sin(bearing * pi / 180), 
           delta_y = hypotenuse * cos(bearing * pi / 180), 
           delta_z = vertical_side + laser_height - target_height) %>%
    return()
}