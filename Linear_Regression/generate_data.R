# Generate synthetic data (from uniform distribution between 0 and 1)
# for every independent variable
# present in <x_vector>. It returns

generate_data <- function(x_vector, b_vector, values, sd, output_dir){
  # Store generated x values here # nolint
  x_values <- list()

  # Need a X matrix. First column is 1's # nolint: indentation_linter.
  X <- matrix(rep(1, values), ncol = 1) # nolint

  # Need a noise column # nolint: indentation_linter.
  e <- rnorm(values, 0, sd)

   # iterate through all independent x variables # nolint: indentation_linter.
  for (x in x_vector){

    # generate independent x data points and add to list
    generated_x <- runif(values, 0, 1)
    x_values <- append(x_values, list(generated_x))

    # Add the generated data as a column to the matrix
    X <- cbind(X, generated_x) # nolint
  }

  # generate dependent y data points and add to list
  if (ncol(X) != length(b_vector) || nrow(X) != length(e)) {
    return(-1)
  }

  # Generated y values
  y_values <- (X %*% b_vector) + e

  # Put values in a textfile
  df <- data.frame(do.call(cbind, x_values), y = y_values)
  write.table(df, file = output_dir, sep = ",", row.names = FALSE)

}


generate_data(c(1), c(2, 1), 20, 10, "/Users/nazhussain/Desktop/STA380/STA380/Linear_Regression/output.txt")