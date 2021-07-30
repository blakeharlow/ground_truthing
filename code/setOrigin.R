# This function takes an input of either a model matrix or
# a response vector. 

# If the input is a model matrix, then it must have column names
# and one column must have a name of "t1". The function than
# returns the same model matrix except with the addition
# of one row. This row has a 1 in the t1 column.

# If the input is a response vector, than this functions simply
# returns the same vector except with a 0 appended to the end.

# When this function is used on both a model matrix (A) and the
# response vector (b), it is essentially fixing the origin
# at the "t1" position.

# If the var_vector parameter is set to True, than the
# appended value is equal to the smallest value in the
# rest of the vector. This is preferred to appending with
# zero, which would result in an infinite weight.



setOrigin <- function(input, var_vector = FALSE) {
  if (is.null(dim(input))){
    # then input is a vector:
    if (var_vector == TRUE){
      return(append(input, min(input)))
    }
    return(append(input, 0))
  }
  else {
    # input is a matrix
    newRow <- vector(mode = "numeric", length = dim(input)[2])
    newRow[which(colnames(input) == "t1")] = 1
    return(rbind(input, newRow))
  }
}