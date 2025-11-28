#' Calculate Indices Wrapper
#'
#' Performs data normalization (0-1 scale) and applies calculations per group.
#'
#' @param data Joined data frame.
#' @param r Weirdness weight.
#' @param s CoD exponent weight.
#' @param t MED weight.
#'
#' @return Data frame with calculated indices.
#' @noRd
calculate_indices <- function(data, r, s, t) {
  data |>
    dplyr::mutate(
      presence = !is.na(.data$pct_value) & .data$pct_value > 0,
      # Convert percentages (0-100) to fractions (0-1)
      pct_presence = .data$pct_presence / 100,
      # Clamp probabilities to avoid log(0)
      pct_presence_ubound = pmin(0.9999, .data$pct_presence),
      pct_presence_lbound = pmax(0.0001, .data$pct_presence),
      cover_if_present = .data$cover_if_present / 100,
      pct_value = .data$pct_value / 100
    ) |>
    rename(syntaxon = syntaxon_code) |>
    dplyr::group_by(.data$recording, .data$syntaxon) |>
    dplyr::do({
      calc_all_indices(., r = r, s = s, t = t)
    }) |>
    dplyr::ungroup()
}

#' Core Calculation of Indices
#'
#' Computes Weirdness, Incompleteness, Likelihood, MED, and CoD.
#'
#' @param data Subset of data for one recording-syntaxon combination.
#' @param r Weirdness weight.
#' @param s CoD exponent weight.
#' @param t MED weight.
#'
#' @return Data frame of results.
#' @noRd
calc_all_indices <- function(data, r, s, t) {
  # i: species (rows)
  # m: recording (constant in this scope)
  # k: vegetation type (constant in this scope)

  # --- Likelihood Components ---
  # p_present: Prob of species being present (for those actually present)
  p_present <- data$pct_presence[data$presence]
  # q_absent: Prob of species being absent (for those actually absent)
  q_absent <- 1 - data$pct_presence_ubound[!data$presence]

  min2LL <- -2 * sum(log(p_present)) - 2 * sum(log(q_absent))

  # --- Weirdness ---
  # Measures species present in recording but rare in syntaxon
  weirdness <- -2 * sum(log(data$pct_presence_lbound[data$presence]))

  # Normalization factor for weirdness
  # Note: The formula for expectation often sums over ALL species in the syntaxon definition
  expected_weirdness <- sum(data$pct_presence_lbound * (-2 * log(data$pct_presence_lbound)))
  nrm_weirdness <- (weirdness / expected_weirdness) - 1

  # --- Incompleteness ---
  # Measures species absent in recording but frequent in syntaxon
  incompleteness <- -2 * sum(log(1 - data$pct_presence_ubound[!data$presence]))

  # Normalization factor for incompleteness
  expected_incompleteness <- -2 * sum((1 - data$pct_presence_ubound) * log(1 - data$pct_presence_ubound))
  nrm_incompleteness <- (incompleteness / expected_incompleteness) - 1

  # --- Modified Euclidean Distance (MED) ---
  # Distance for present species: |Observed - Expected_if_Present|
  A_minus_CA <- data$pct_value[data$presence] - data$cover_if_present[data$presence]

  # Distance for absent species: |0 - Expected_Mean|
  # Expected mean = cover_if_present * frequency
  A_avg <- data$cover_if_present[!data$presence] * data$pct_presence[!data$presence]

  sum_med_present <- sum(A_minus_CA^2)
  sum_med_absent <- sum((-A_avg)^2) # Squaring handles the negative sign, logic kept for clarity

  med <- sqrt(2 * ((t * sum_med_present) + (1 - t) * sum_med_absent))

  # --- Unlikelihood ---
  unlikelihood <- weirdness + incompleteness
  expected_unlikelihood <- expected_weirdness + expected_incompleteness
  nrm_unlikelihood <- (unlikelihood / expected_unlikelihood) - 1

  # --- Composite Distance (CoD) ---
  # Standard ASSOCIA-like logic:
  # Qualitative part: weighted average of weirdness and incompleteness
  # Quantitative part: MED
  # Combined using power s

  # Note: In some formulas r is defined as the weight, here implemented as (1+r)
  qualitative_part_cod <- ((1 + r) * weirdness + incompleteness) / (1 + r)
  quantitative_part_cod <- pmax(med, 0.0001)

  cod <- (qualitative_part_cod^s) * (quantitative_part_cod^(1 - s))

  # --- Normalized Composite Distance (Alternative) ---
  qualitative_part_cod_nrm <- ((1 + r) * nrm_weirdness + nrm_incompleteness) / (1 + r)

  # Adding 1 before power to handle potentially negative normalized values
  p1 <- (qualitative_part_cod_nrm + 1)^s
  p2 <- (quantitative_part_cod + 1)^(1 - s)
  nrm_cod <- p1 * p2

  # --- Return Result ---
  data.frame(
    weirdness = weirdness,
    incompleteness = incompleteness,
    unlikelihood = min2LL,
    med = med,
    cod = cod,
    wrd = nrm_weirdness,
    inc = nrm_incompleteness,
    llk = nrm_unlikelihood,
    nrm_cod = nrm_cod
  )
}
