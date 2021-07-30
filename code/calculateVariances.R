# This function computes the variances for the following columns
# of the given tibble:
    # delta_x
    # delta_y
    # delta_z
# It adds these columns to the given tibble
# and returns this edited tibble

# In order to perform these calculations, the following columns
# must be in the given tibble with data:
    # angle
    # hypotenuse
    # vertical_side
    # horizontal_side
    # bearing

# estimates for the uncertainty (sqrt(variance)) in the:
    # angle
    # hypotenuse
    # bearing
    # laser_ht
    # target_ht
# must be provided to this function. angle and bearing should be 
# entered in degrees.


calculateVariances <- function(tib, angle_unc, hyp_unc, bearing_unc,
                               laser_ht_unc, target_ht_unc) {
  angle_unc_r = angle_unc * pi / 180
  bearing_unc_r = bearing_unc * pi / 180
  
  tib %>%
    mutate(angle_r = angle * pi / 180,
           bearing_r = bearing * pi / 180,
           h_side_var = cos(angle_r)^2 * hyp_unc^2 + hypotenuse^2 * cos(angle_r)^2 * angle_unc_r^2,
           v_side_var = sin(angle_r)^2 * hyp_unc^2 + hypotenuse^2 * cos(angle_r)^2 * angle_unc_r^2,

           delta_x_var = sin(bearing_r)^2 * h_side_var + horizontal_side^2 * cos(bearing_r)^2 * bearing_unc_r^2,
           delta_y_var = cos(bearing_r)^2 * h_side_var + horizontal_side^2 * sin(bearing_r)^2 * bearing_unc_r^2,
           delta_z_var = v_side_var + laser_ht_unc^2 + target_ht_unc^2) %>%
    select(-angle_r, -bearing_r, -h_side_var, -v_side_var) %>%  # these were just intermediate calcs
    return()
}