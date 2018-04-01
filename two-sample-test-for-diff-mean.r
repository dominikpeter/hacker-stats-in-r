
set.seed(3434)

library(magrittr)
library(data.table)
library(boot)

# t-test
x <- sleep$extra[sleep$group==1]
y <- sleep$extra[sleep$group==2]
t.test(x,y, alternative = "less")

xy_c <- c(x, y)
xy_mean <- mean(xy_c)
emp_mean <- mean(x) - mean(y)

shifted_x <- x - mean(x) + xy_mean
shifted_y <- y - mean(y) + xy_mean
calc_mean <- function(x, i){
  xi <- x[i]
  mean(xi)
}

bs_rep_x <- boot(shifted_x, statistic = calc_mean, R = 100000, parallel = "multicore")
bs_rep_y <- boot(shifted_y, statistic = calc_mean, R = 100000, parallel = "multicore")

bs_rep <- bs_rep_x$t - bs_rep_y$t

mean(bs_rep <= emp_mean)



#------------------------------------------------

  
force_a <- c(1.612, 0.605, 0.327, 0.946, 0.541, 1.539, 0.529, 0.628, 1.453,
 0.297, 0.703, 0.269, 0.751, 0.245, 1.182, 0.515, 0.435, 0.383,
 0.457, 0.73 )


force_b <- c(0.172, 0.142, 0.037, 0.453, 0.355, 0.022, 0.502, 0.273, 0.72 ,
            0.582, 0.198, 0.198, 0.597, 0.516, 0.815, 0.402, 0.605, 0.711,
            0.614, 0.468)



forces_concat <- c(force_a,force_b)
mean_force <- mean(forces_concat)
empirical_diff_means <- mean(force_a) - mean(force_b)

force_a_shifted = force_a - mean(force_a) + mean_force
force_b_shifted = force_b - mean(force_b) + mean_force 

# Compute 10,000 bootstrap replicates from shifted arrays
bs_replicates_a = boot(force_a_shifted, calc_mean, R=10000)
bs_replicates_b = boot(force_b_shifted, calc_mean, R=10000)

# Get replicates of difference of means: bs_replicates
bs_replicates = bs_replicates_a$t - bs_replicates_b$t

# Compute and print p-value: p
p = mean(bs_replicates >= empirical_diff_means)
print(paste('p-value =', p))

