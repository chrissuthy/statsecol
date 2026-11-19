#' Simulate data from a dynamic occupancy model
#'
#' Simulates detection/non-detection data from a (non-spatial) dynamic
#' occupancy model with constant colonisation, extinction and detection
#' probabilities. The student ID is used as the random seed, so the same ID
#' always returns the same data, and different IDs return different study
#' designs and different data generating values. The study design and the data
#' generating values are printed when the function is run. Used in MT5767
#' Project 3.
#'
#' @details
#' The study design is drawn first: the number of sites (80 to 140, in steps
#' of 10), the number of seasons (8 to 12) and the number of repeat visits per
#' season (3 or 4). The four data generating values are then drawn from normal
#' distributions on the logit scale and rounded to two decimal places:
#' occupancy in the first season (centred on 0.50, sd 0.80), colonisation
#' probability (0.20, sd 0.45), extinction probability (0.20, sd 0.45) and
#' detection probability per visit (0.45, sd 0.25).
#'
#' The true occupancy state of site \eqn{i} in season \eqn{t} is
#' \deqn{z_{i,1} \sim \mathrm{Bernoulli}(\psi_1),}
#' \deqn{z_{i,t} \sim \mathrm{Bernoulli}(z_{i,t-1}(1-\epsilon) + (1-z_{i,t-1})\gamma), \quad t > 1,}
#' and the detection on visit \eqn{j} is
#' \deqn{y_{i,j,t} \sim \mathrm{Bernoulli}(z_{i,t}\,p).}
#'
#' So that the same ID gives the same data on every computer, the function
#' sets the random number generators with
#' `RNGkind("Mersenne-Twister", "Inversion", "Rejection")` before calling
#' `set.seed()`. Both changes persist after the function returns.
#'
#' @param student_id A matriculation number (e.g. `220012345`), given as a
#'   number or a character string. It is used as the random seed.
#'
#' @return Prints the study design and the data generating values, and
#'   (invisibly) returns a list with the following elements:
#' \describe{
#'   \item{`student_id`}{The ID used as the seed.}
#'   \item{`y`}{The detections (1 = detected, 0 = not detected), as an array
#'     with dimensions site x visit x season.}
#'   \item{`design`}{A named vector with `n_sites`, `n_seasons` and
#'     `n_visits`.}
#'   \item{`truth`}{A list with `parameters` (the values of `psi1`, `gamma`,
#'     `epsilon` and `p` that generated the data) and `prop_occupied` (the
#'     proportion of sites truly occupied in each season).}
#' }
#'
#' @author Chris Sutherland
#'
#' @examples
#' # prints the study design and the data generating values
#' dat <- simulate_dynocc(220012345)
#'
#' # detection histories for the first five sites in season 1
#' dat$y[1:5, , 1]
#'
#' # naive occupancy: proportion of sites with at least one detection per season
#' colMeans(apply(dat$y, c(1, 3), max))
#'
#' # the data generating values
#' dat$truth
#'
#' @importFrom stats plogis qlogis rbinom rnorm setNames
#' @export
simulate_dynocc = function(student_id) {

  # -- the seed is the student ID ----------------------------------------------
  id <- suppressWarnings(as.integer(gsub("[^0-9]", "", as.character(student_id))))
  if (length(id) != 1 || is.na(id) || id < 1) {
    stop("student_id must be your 9-digit matriculation number, e.g. 220012345")
  }
  # fix the generators so every computer gives the same data for the same ID
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(id)

  # -- study design (differs between students) ---------------------------------
  n_sites   <- sample(seq(80, 140, by = 10), 1)   # sites surveyed
  n_seasons <- sample(8:12, 1)                    # seasons (primary periods)
  n_visits  <- sample(3:4, 1)                     # repeat visits per season

  # -- data generating values (differ between students) ------------------------
  # each is drawn from a normal distribution on the logit scale, centred on the
  # value shown, so students get similar but not identical conditions
  draw <- function(centre, sd) round(plogis(rnorm(1, qlogis(centre), sd)), 2)
  psi1    <- draw(0.50, 0.80)   # occupancy in the first season
  gamma   <- draw(0.20, 0.45)   # colonisation probability
  epsilon <- draw(0.20, 0.45)   # extinction probability
  p       <- draw(0.45, 0.25)   # detection probability per visit

  # -- state process: is each site occupied in each season? --------------------
  z <- matrix(NA_integer_, n_sites, n_seasons)
  z[, 1] <- rbinom(n_sites, 1, psi1)
  for (t in 2:n_seasons) {
    z[, t] <- rbinom(n_sites, 1, z[, t - 1] * (1 - epsilon) + (1 - z[, t - 1]) * gamma)
  }

  # -- observation process: was the species detected on each visit? ------------
  y <- array(NA_integer_, dim = c(n_sites, n_visits, n_seasons),
             dimnames = list(site   = paste0("site", seq_len(n_sites)),
                             visit  = paste0("visit", seq_len(n_visits)),
                             season = paste0("season", seq_len(n_seasons))))
  for (t in 1:n_seasons) {
    for (j in 1:n_visits) {
      y[, j, t] <- rbinom(n_sites, 1, z[, t] * p)
    }
  }

  # -- what is returned --------------------------------------------------------
  out <- list(
    student_id = id,
    y          = y,                      # detections: site x visit x season
    design     = c(n_sites = n_sites, n_seasons = n_seasons, n_visits = n_visits),
    truth      = list(
      parameters    = c(psi1 = psi1, gamma = gamma, epsilon = epsilon, p = p),
      # proportion of the surveyed sites that were truly occupied each season
      prop_occupied = setNames(colMeans(z), paste0("season", seq_len(n_seasons)))
    )
  )

  # -- print the design and the data generating values -------------------------
  cat("\nDynamic occupancy data for student ID ", id, "\n\n",
      "Study design\n",
      "  Sites                               ", n_sites, "\n",
      "  Seasons                             ", n_seasons, "\n",
      "  Repeat visits per season            ", n_visits, "\n\n",
      "Data generating (true) values\n",
      "  psi1     occupancy in season 1      ", format(psi1, nsmall = 2), "\n",
      "  gamma    colonisation probability   ", format(gamma, nsmall = 2), "\n",
      "  epsilon  extinction probability     ", format(epsilon, nsmall = 2), "\n",
      "  p        detection probability      ", format(p, nsmall = 2), "\n\n",
      "The detections are in $y, an array with dimensions site x visit x season.\n\n",
      sep = "")

  invisible(out)
}
