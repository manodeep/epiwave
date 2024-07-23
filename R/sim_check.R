#' takes a greta array timeseries (can have multiple jurisdictions as columns), plot a few trajectories, either from prior or posterior distributions. This function is intended for diagnostics and troubleshooting purposes.
#'
#' @param greta_array the greta array to visualise
#' @param n_sim number of trajectories to plot
#' @param fitted_values if plotting posteriors, posterior draws
#' @param horizontal_at_one if a horizontal line at y = 1 should be drawn
#' @param title plot title
#'
#' @return plot
#' @export
#'
#' @examples
sim_check <- function(greta_array,
                      n_sim = 1e2,
                      fitted_values = NULL,
                      horizontal_at_one = FALSE,
                      title = NULL) {

  sims <- calculate(greta_array,
                    nsim = n_sim,
                    values = fitted_values)[[1]]

  x_lab <- 1:(dim(sims)[2])
  n_states <- dim(sims)[3]

  y_min <- pmax(min(sims),-1e16)
  y_max <- pmin(max(sims),1e3)
  ylim <- c(y_min, y_max)
  # xlim <- c(10, n_days)

  plot(sims[1, , ] ~ x_lab,
       ylim = ylim,
       # xlim = xlim,
       xlab = "dates",
       ylab = "values",
       type = "n",
       main = title)
  for (i in seq_len(n_sim)) {
    for (j in seq_len(n_states)) {
      lines(sims[i, , j] ~ x_lab,col = scales::alpha("blue",0.09))
    }
  }

  if (horizontal_at_one) {
    abline(h = 1, lty = 2)
  }



}

