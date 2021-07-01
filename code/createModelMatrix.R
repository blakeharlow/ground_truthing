# This function takes a tibble that has a column for both laser_position
# and target position and returns a model matrix (A in Ax=b). The laser
# position has a -1 in A, the target_position has a 1 in A, and every
# other element is 0.

# tib parameter is a tibble that has at least a laser_position column
# and a target_position column.

# levels parameter is a list that orders the levels of the laser_position
# levels to be used in a factor. It should contain every target_position
# and laser_position.


createModelMatrix <- function(tib, levels) {
  # Make a binary pivoted tibble for the laser positions
  A_laser <- tib %>%
    transmute(laser_index = laser_position %>% factor(levels = levels)) %>%
    mutate(rn = row_number()) %>%
    pivot_wider(names_from = laser_index, values_from = laser_index, 
                values_fn = function(x) -length(x), values_fill = 0) %>%
    select(-rn)
  
  
  # And a binary pivoted tibble for the target positions
  A_target <- tib %>%
    transmute(target_index = target_position %>% factor(levels = levels)) %>%
    mutate(rn = row_number()) %>%
    pivot_wider(names_from = target_index, values_from = target_index, 
                values_fn = length, values_fill = 0) %>%
    select(-rn)
  
  
  # Join them and convert to matrix, and finally add to get a final A matrix
  r <- right_join(A_target, A_laser) %>% as.matrix()
  l <- left_join(A_target, A_laser) %>% as.matrix()
  r[is.na(r)] <- 0
  l[is.na(l)] <- 0
  return(r + l)
}