library(parallel)
library(doParallel)
# if(!exists("cl"))
  cl <- makeCluster(2)
registerDoParallel(cl)
# library(doRNG)
# registerDoRNG(128640)
library(foreach)

# library(snow)

library(colorspace)

## Is there a better way to do this than passing around an environment?
## I'd rather just assign everything once and take care of it with scoping

# Generate data ----
sample_unif <- function(n_sample) seq(0 + 1/n_sample, 1 - 1/n_sample, 1/n_sample)

generate_data <- function(input_list, n_rep, alpha) {
  d <- new.env(size = 20L)
  
  d$K <- length(input_list)
  names(input_list) <- paste0("G", 1:d$K)
  d$input_list <- input_list

  d$y_pretty <- sapply(input_list, function(item) {
    do.call(item$inv_F, c(list(sample_unif(2000)), item[-(1:3)]))
  }, simplify = FALSE, USE.NAMES = TRUE)
  
  d$N <- sum(vapply(input_list, `[[`, 0, 3))
  d$group_sizes <- vapply(input_list, function(grp) grp$n, 0)
#   group_padding <- max(d$group_sizes) - d$group_sizes
  d$between_df <- d$K - 1
  d$within_df <- sum(d$group_sizes - 1)
  
  d$y_rep <- matrix(foreach(r = 1:n_rep) %:%
              foreach(k = 1:d$K, .combine = c) %dopar% {
    do.call(input_list[[k]][[1]], input_list[[k]][-(1:2)])
  }, n_rep, K)

  d$group_means <- matrix(0, n_rep, K)
  d$grand_mean <- numeric(n_rep)
  d$between_ms <- numeric(n_rep)
  d$within_ms <- numeric(n_rep)
  d$f <- numeric(n_rep)

for(r in 1:n_rep) {
    d$grand_mean[r] <- mean(unlist(d$y_rep[r, k]))
    
    for(k in 1:d$K) {
      n_k <- input_list[[k]]$n
      d$group_means[r, k] <- mean(d$y_rep[r, k])
      
      between_ss <- n_k * (d$group_means[r, k] - d$grand_mean[r])^2
      within_ss <- sum((d$y_rep[r, k] - d$group_means[r, k])^2)
    }
    d$between_ms[r] <- between_ss / d$between_df
    d$within_ms[r] <- within_ss / d$within_df
    d$f[r] <- d$between_ms[r] / d$within_ms[r]
    
    if (!(r %% 250)) {
      cat(intToUtf8(128640))
      if (r == n_rep) cat("\n")
    }
  }

  d$plot_colors <- rainbow_hcl(d$K, start = 30, end = 300)
  d$alpha <- alpha
  
  d
}

# Plot data ----
 
data_boxplot <- function(d) {
  layout(matrix(c(1,1,1,2), nrow = 1))
  
  ## group boxplot
  par(mar = c(3, 2, 1, 0) + .1)
  boxplot(d$y_pretty, col = d$plot_colors, xaxt = "n", las = 1)
  axis(1, at = 1:d$K, labels = names(input_list), tick = FALSE)
  
  ## overall boxplot
  par(mar = c(3, 0, 1, 1) + .1)
  boxplot(unlist(d$y_pretty), xaxt = "n", yaxt = "n")
  axis(1, at = 1, labels = "Overall", tick = FALSE)
}

sampling_distribution_plot <- function(d){
  ## sampling distributions
  # par(mar = c(3, 1, 2.5, 1) + .1)
  plot(density(d$grand_mean), lwd = 2, lty = "dashed", col = "darkgray",
       xlab = "", ylab = "", main = "", yaxt = "n",
       xlim = c(min(d$group_means), max(d$group_means)))
  for(k in 1:d$K) lines(density(d$group_means[, k]),
                        lwd = 2, col = d$plot_colors[k])
}

mean_squares_plot <- function(d) {
  between_den <- density(d$between_ms)
  within_den <- density(d$within_ms)
  ## between MS
  # par(mar = c(1, 0, 2, 1) + .1)
  plot(between_den, lwd = 2, lty = "dashed",
       main = "", xlab = "", ylab = "", yaxt = "n",
       ylim = c(0, max(within_den$y)))
  
  ## within MS
  # par(mar = c(1, 0, 1, 1) + .1)
  lines(within_den, lwd = 2, lty = "dashed",
       main = "", xlab = "", ylab = "", yaxt = "n")
}

f_stat_plot <- function(d) {
  h <- hist(d$f, breaks = 50, plot = FALSE)
  xlim <- c(min(h$breaks), max(h$breaks))
  x <- seq(xlim[1], xlim[2], 0.1)
  x_den <- df(x, d$between_df, d$within_df)
  ylim <- c(0, max(max(x_den), max(h$density)) + 0.1)
  plot(h, freq = FALSE, ylim = ylim, main = "", xlab = "", ylab = "")
  lines(x, x_den, xlim = xlim)
  
  f_alpha <- qf(1 - d$alpha, d$between_df, d$within_df)
  rect(0, xlim[1], f_alpha, ylim[2],
       border = NA,
       col = do.call("rgb", as.list(c(col2rgb("steelblue")/255, 1/4))))
  rect(f_alpha, xlim[1], xlim[2], ylim[2],
       border = NA,
       col = do.call("rgb", as.list(c(col2rgb("wheat")/255, 1/4))))
  
  legend("topright",
         c(sprintf("theoretical coverage: %0.0f%%", pf(f_alpha, d$between_df, d$within_df) * 100),
           sprintf("empirical coverage: %0.0f%%", ecdf(d$f)(f_alpha) * 100)),
         bty = "n", inset = 5/100)
  mtext(
    side = 3,
    text = "100 - coverage%\n=\n% chance of generating an F at least as large, purely by chance",
    cex = 0.8
  )
  title(
    main = "Empirical distribution of ANOVA F statistic",
    line = 3
  )
  title(
    sub = "Curve is theoretical F density",
    line = 2
  )
}

# Timings ----
# 
# il <- list(
#   list(rng = rnorm, inv_F = qnorm, n = 15, mean = 0, sd = 1),
#   list(rng = rnorm, inv_F = qnorm, n = 15, mean = 0, sd = 1),
#   list(rng = rnorm, inv_F = qnorm, n = 15, mean = 0, sd = 1),
#   list(rng = rt, inv_F = qt, n = 15, df = 2, ncp = 1)
# )
