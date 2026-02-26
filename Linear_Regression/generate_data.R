#' Generate synthetic data
#' @description
#' 'generate_data' returns generated independent x values, y values, and noise
#' in a csv file or dataframe format.
#' 
#' @param num_x: The number of x values in the equation
#' @param b_vector: A vector containing slope elements
#' @param values: The number of y values to generate
#' @param sd: The standard deviation errors follow
#' @param save: A boolean. If TRUE, save the data in a csv, else, save as 
#' a dataframe
#' 
#' @return This function returns -1 in case vector/matrix multiplication fails.
#' If save is TRUE, it returns 0 and saves the result in a CSV file. If save 
#' is FALSE, it returns a dataframe containing all generated information
#' 
#' @example generate_data(1, c(3,-2), 50, 0.1, save = TRUE)
#' 
generate_data <- function(num_x, b_vector, values, sd, save){    
    # To calculate y=XB + e, we need X and e. Generate random noise
    X <- matrix(nrow=values, ncol=0)
    e <- rnorm(values, 0, sd)

    # iterate through all independent x variables 
    for (i in 1:(num_x+1)){

        # Add a column of 1's
        if (i == 1){
            ones <- rep(1,values)
            X <- cbind(X, ones)
            colnames(X)[i] <- "Ones"
        }
        # generate independent x data points and add to matrix
        else {
            generated_x <- runif(values, 0, 1)
            X <- cbind(X, generated_x) 
            colnames(X)[i] <- paste0("x", i-1)
        }
    }

    # generate dependent y data points and add to list
    if (ncol(X) != length(b_vector) || nrow(X) != length(e)) {
        return(-1)
    }

    # Generated y values
    y_values <- (X %*% b_vector) + e

    # Bind error values and y values
    result <- cbind(X,y_values)
    result <- cbind(result, e)
    colnames(result)[num_x + 2] <- "y"
    colnames(result)[num_x + 3] <- "noise"
    result <- result[, -1]

    # Save as a csv file
    if (save){
        #Get the directory the function is running in and paste the
        #output file in the same directory
        dir <- getwd()
        path <- file.path(dir, "output.csv")

        # Put values in output.csv
        write.csv(result, path, row.names=FALSE)
        return(0)
    } else {
        return(as.data.frame(result))
    }

}
generate_data(num_x = 1, b_vector = c(3, -2), values = 50, sd = 0.1, save = TRUE)


