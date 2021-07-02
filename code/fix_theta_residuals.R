# This function takes a theta_residual (in degrees) and returns
# the corresponding angle that is closest to
# zero, ie between -180 and 180.

# There are a few cases for theta_i that need to be accounted for:
  # CASE 1: if 0 <= theta <= 180:
        # leave theta_i unchanged
  # CASE 2: if 180 < theta < 360:
        # return theta - 360
  # CASE 3: if theta < 0 or theta >= 360
        # perform mod 360 and then use case 1 or case 2

fix_theta_value <- function(theta_value){
  if (theta_value >= 0 & theta_value  <= 180){
    return(theta_value)
  }
  else if(theta_value > 180 & theta_value < 360){
    return(theta_value - 360)
  }
  else{
    return(fix_theta_value(theta_value %% 360))
  }
  
}

# Now make a vectorized version, which takes a residuals vector for v, h, 
# and theta, where the residuals vector is organized as:
# residuals = <v1, h1, theta1, ..., v_m, h_m, theta_m>


# This function does the performs fix_theta_value() on the theta_i
# data of the residuals vector. In other words, it performs this operation 
# only on element 3,6,9,... of the residuals vector.

# This function then returns the edited residuals vector.

fix_theta_residuals <- function(residuals){
  theta_residuals <- residuals[c(FALSE, FALSE, TRUE)]
  theta_residuals <- theta_residuals %>% sapply(fix_theta_value)
  residuals[c(FALSE, FALSE, TRUE)] = theta_residuals
  return(residuals)
}
