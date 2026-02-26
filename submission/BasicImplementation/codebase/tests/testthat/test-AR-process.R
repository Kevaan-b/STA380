library(testthat)
source(testthat::test_path("..", "..", "AR-process.R"))
#########################################
# return type and length test           #
#########################################

test_that("Return type is a list, with correct length", {
  dgp <- generate_ar_dgp(m = 2, variance = 2, n = 50, seed = 1)
  
  # return type should be a list
  expect_type(dgp, "list")
  expect_length(dgp$x, 50)
  
  # modify parameter of n, aoso gives the correct length
  dgp <- generate_ar_dgp(m = 2, variance = 2, n = 100, seed = 1)
  expect_length(dgp$x, 100)
})

#########################################
# Error test                            #
#########################################

test_that("Error when phi has invalid length", {
  # expected to return errors when the length of phi doesn't match
  expect_error(generate_ar_dgp(m = 2, variance = 2, n = 50, seed = 1, phi=c(0.2,0.4,0.6)))
})

#########################################
# test of phi value                     #
#########################################

test_that("test of phi are in the range of 1 mod", {
  d <- generate_ar_dgp(m = 10, variance = 2, n = 50, seed = 1, phi=NULL)
  for (i in 1:10){
    expect_true(Mod(d$phi[i]) < 1)
  }
})

#########################################
# reproducibility test                  #
#########################################

test_that("repreducible with same param, including phi",{
  d1 <- generate_ar_dgp(m = 3, variance = 2, n = 50, seed = 1, phi=c(0.2,0.4,0.6))  
  d2 <- generate_ar_dgp(m = 3, variance = 2, n = 50, seed = 1, phi=c(0.2,0.4,0.6))
  
  for (i in 1:50){
    expect_true(d1$x[i] == d2$x[i])
  }
})


#########################################
# mean value test                       #
#########################################

test_that("ar_mean increase",{
  dgp <- generate_ar_dgp(m = 2, variance = 2, n = 50, seed = 1)
  dgp1 <- generate_ar_dgp(m = 2, variance = 2, n = 50, seed = 1, ar_mean = 5)
  
  m1 <- mean(as.numeric(dgp$x))
  m2 <- mean(as.numeric(dgp1$x))
  
  expect_true(m2 > m1 + 4)
  
})


##################################################################################
# test generate_user_ar performs the same as generate_ar_dgp                     #
##################################################################################

test_that("generate_user_ar performs the same as generate_ar_dgp",{
  d1 <- generate_ar_dgp(m = 3, variance = 2, n = 50, seed = 1, phi=c(0.2,0.4,0.6))  
  d2 <- generate_user_ar(phi = c(0.2,0.4,0.6), variance = 2, n = 50, seed = 1)
  for (i in 1:50){
    expect_true(d1$x[i] == d2[i])
  }
})

##################################################################################
# test fit_user_ar fits correctly                                                #
##################################################################################

test_that("test fit_user_ar fits correctly",{
  
  phi_real = c(0.4, -0.2, 0.1)
  d <- generate_ar_dgp(phi = c(0.4, -0.2, 0.1), variance = 2, n = 10, seed = 1, m=3)
  d1 <- generate_ar_dgp(phi = c(0.4, -0.2, 0.1), variance = 2, n = 100, seed = 1, m=3)
  d2 <- generate_ar_dgp(phi = c(0.4, -0.2, 0.1), variance = 2, n = 10000, seed = 1, m=3)
  
  phi_1 <- fit_user_ar(d, 3)
  phi_2 <- fit_user_ar(d1, 3)
  phi_3 <- fit_user_ar(d2, 3)
  
  # test converge
  for (i in 1:3){
    n10 <-Mod(phi_1$fit$coef[i] - phi_real[i]) 
    n10000 <- Mod(phi_3$fit$coef[i] - phi_real[i])
    expect_gt(n10, n10000 - 0.05)
  }
  
  
  # test almost equal
  est <- phi_3$fit$coef
  
  expect_equal(
    unname(est), 
    #── Failure ('test-AR-process.R:110:5'): test fit_user_ar fits correctly
    #phi_3$fit$coef[i] not equal to phi_real[i].
    #names for target but not for current
    #So I asked GPT how to solve it.
    phi_real,
    tolerance = 0.05
  )
  
})
