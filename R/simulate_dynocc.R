# Simulate detection/non-detection data from a dynamic occupancy model.
# Used in MT5767 Project 3: the student ID sets the seed, so each student gets
# their own study design and their own data generating values.

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
