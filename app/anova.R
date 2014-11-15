# Input ----
n_group <- 15
m_group <- 0
sd_group <- 1

n_rep <- 5000
alpha = 0.05

input_list <- list(
  # list(rng = rnorm, inv_F = qnorm, n = n_group, mean = m_group + .6, sd = sd_group),
  list(rng = rnorm, inv_F = qnorm, n = n_group, mean = m_group, sd = sd_group),
  list(rng = rnorm, inv_F = qnorm, n = n_group, mean = m_group, sd = sd_group),
  list(rng = rnorm, inv_F = qnorm, n = n_group, mean = m_group, sd = sd_group),
  list(rng = rt, inv_F = qt, n = n_group, df = 2, ncp = 1)
)

K <- length(input_list)
group_names <- paste0("G", 1:K)

## Generate data ----
library(testthat)
library(dplyr)
library(reshape2)
library(colorspace)

sample_unif <- function(n_sample) seq(0 + 1/n_sample, 1 - 1/n_sample, 1/n_sample)

y_pretty <- lapply(input_list, function(item) {
  do.call(item$inv_F, c(list(sample_unif(2000)), item[-(1:3)]))
})

N <- sum(vapply(input_list, `[[`, 0, 3))
group_sizes <- vapply(input_list, function(grp) grp$n, 0)
group_padding <- max(group_sizes) - group_sizes
between_df <- K - 1
within_df <- sum(group_sizes - 1)

group_means <- matrix(0, n_rep, K)
grand_mean <- numeric(n_rep)
between_ms <- between_ss <- numeric(n_rep)
within_ms <- within_ss <- numeric(n_rep)
f <- numeric(n_rep)

progress <- txtProgressBar(max = n_rep, style = 3)
for(r in 1:n_rep) {
  y_r <- lapply(input_list, function(item) do.call(item[[1]], item[-(1:2)]))
  y_rep[[r]] <- y_r
  names(y_r) <- group_names
  
  grand_mean[r] <- mean(unlist(y_r))
  
  for(k in 1:K) {
  	n_k <- input_list[[k]]$n
  	group_means[r, k] <- mean(y_r[[k]])

  	between_ss[r] <- n_k * (group_means[r, k] - grand_mean[r])^2 + between_ss[r]
  	within_ss[r] <- sum((y_r[[k]] - group_means[r, k])^2) + within_ss[r]
  }
  between_ms[r] <- between_ss[r] / between_df
  within_ms[r] <- within_ss[r] / within_df
  f[r] <- between_ms[r] / within_ms[r]
  
  setTxtProgressBar(progress, r)
}
close(progress)

# plotting ----

plot_colors <- rainbow_hcl(K, start = 30, end = 300)
 
data_boxplot <- function() {
  layout(matrix(c(1,1,1,2), nrow = 1))
  ## group boxplot
  par(mar = c(3, 2, 1, 0) + .1)
  boxplot(y_pretty, col = plot_colors, xaxt = "n", las = 1)
  axis(1, at = 1:K, labels = group_names, tick = FALSE)
  
  ## overall boxplot
  par(mar = c(3, 0, 1, 1) + .1)
  boxplot(unlist(y_pretty), xaxt = "n", yaxt = "n")
  axis(1, at = 1, labels = "Overall", tick = FALSE)
}

sampling_distribution_plot <- function(){
  ## sampling distributions
  # par(mar = c(3, 1, 2.5, 1) + .1)
  plot(density(grand_mean), lwd = 2, lty = "dashed", col = "darkgray",
       xlab = "", ylab = "", main = "", yaxt = "n",
       xlim = c(min(group_means), max(group_means)))
  for(k in 1:K) lines(density(group_means[, k]), lwd = 2, col = plot_colors[k])
}

mean_squares_plot <- function() {
  between_den <- density(between_ms)
  within_den <- density(within_ms)
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

f_stat_plot <- function() {
  h <- hist(f, breaks = 50, plot = FALSE)
  xlim <- c(min(h$breaks), max(h$breaks))
  x <- seq(xlim[1], xlim[2], 0.1)
  x_den <- df(x, between_df, within_df)
  ylim <- c(0, max(max(x_den), max(h$density)) + 0.1)
  plot(h, freq = FALSE, ylim = ylim, main = "", xlab = "", ylab = "")
  lines(x, x_den, xlim = xlim)
  
  f_alpha <- qf(1 - alpha, between_df, within_df)
  rect(0, xlim[1], f_alpha, ylim[2],
       border = NA,
       col = do.call("rgb", as.list(c(col2rgb("steelblue")/255, 1/4))))
  rect(f_alpha, xlim[1], xlim[2], ylim[2],
       border = NA,
       col = do.call("rgb", as.list(c(col2rgb("wheat")/255, 1/4))))
  
  legend("topright",
         c(sprintf("theoretical coverage: %0.0f%%", pf(f_alpha, between_df, within_df) * 100),
           sprintf("empirical coverage: %0.0f%%", ecdf(f)(f_alpha) * 100)),
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
