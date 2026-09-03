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
# calc_all_indices <- function(data, r, s, t) {
#   # i: species (rows)
#   # m: recording (constant in this scope)
#   # k: vegetation type (constant in this scope)
#
#   # --- Likelihood Components ---
#   # p_present: Prob of species being present (for those actually present)
#   p_present <- data$pct_presence[data$presence]
#   # q_absent: Prob of species being absent (for those actually absent)
#   q_absent <- 1 - data$pct_presence_ubound[!data$presence]
#
#   min2LL <- -2 * sum(log(p_present)) - 2 * sum(log(q_absent))
#
#   # --- Weirdness ---
#   # Measures species present in recording but rare in syntaxon
#   weirdness <- -2 * sum(log(data$pct_presence_lbound[data$presence]))
#
#   # Normalization factor for weirdness
#   # Note: The formula for expectation often sums over ALL species in the syntaxon definition
#   expected_weirdness <- sum(data$pct_presence_lbound * (-2 * log(data$pct_presence_lbound)))
#   nrm_weirdness <- (weirdness / expected_weirdness) - 1
#
#   # --- Incompleteness ---
#   # Measures species absent in recording but frequent in syntaxon
#   incompleteness <- -2 * sum(log(1 - data$pct_presence_ubound[!data$presence]))
#
#   # Normalization factor for incompleteness
#   expected_incompleteness <- -2 * sum((1 - data$pct_presence_ubound) * log(1 - data$pct_presence_ubound))
#   nrm_incompleteness <- (incompleteness / expected_incompleteness) - 1
#
#   # --- Modified Euclidean Distance (MED) ---
#   # Distance for present species: |Observed - Expected_if_Present|
#   A_minus_CA <- data$pct_value[data$presence] - data$cover_if_present[data$presence]
#
#   # Distance for absent species: |0 - Expected_Mean|
#   # Expected mean = cover_if_present * frequency
#   A_avg <- data$cover_if_present[!data$presence] * data$pct_presence[!data$presence]
#
#   sum_med_present <- sum(A_minus_CA^2)
#   sum_med_absent <- sum((-A_avg)^2) # Squaring handles the negative sign, logic kept for clarity
#
#   med <- sqrt(2 * ((t * sum_med_present) + (1 - t) * sum_med_absent))
#
#   # --- Unlikelihood ---
#   unlikelihood <- weirdness + incompleteness
#   expected_unlikelihood <- expected_weirdness + expected_incompleteness
#   nrm_unlikelihood <- (unlikelihood / expected_unlikelihood) - 1
#
#   # --- Composite Distance (CoD) ---
#   # Standard ASSOCIA-like logic:
#   # Qualitative part: weighted average of weirdness and incompleteness
#   # Quantitative part: MED
#   # Combined using power s
#
#   # Note: In some formulas r is defined as the weight, here implemented as (1+r)
#   qualitative_part_cod <- ((1 + r) * weirdness + incompleteness) / (1 + r)
#   quantitative_part_cod <- pmax(med, 0.0001)
#
#   cod <- (qualitative_part_cod^s) * (quantitative_part_cod^(1 - s))
#
#   # --- Normalized Composite Distance (Alternative) ---
#   qualitative_part_cod_nrm <- ((1 + r) * nrm_weirdness + nrm_incompleteness) / (1 + r)
#
#   # Adding 1 before power to handle potentially negative normalized values
#   p1 <- (qualitative_part_cod_nrm + 1)^s
#   p2 <- (quantitative_part_cod + 1)^(1 - s)
#   nrm_cod <- p1 * p2
#
#   # --- Return Result ---
#   data.frame(
#     weirdness = weirdness,
#     incompleteness = incompleteness,
#     unlikelihood = min2LL,
#     med = med,
#     cod = cod,
#     wrd = nrm_weirdness,
#     inc = nrm_incompleteness,
#     llk = nrm_unlikelihood,
#     nrm_cod = nrm_cod
#   )
# }




#' Core Calculation of ASSOCIA Indices
#'
#' Computes Weirdness, Incompleteness, Likelihood, MED, and CoD based on
#' van Tongeren et al. (2008). This version implements normalization to
#' remove richness bias and uses the corrected MED formula from the manual.
#'
#' @param data A data frame containing:
#'   \itemize{
#'     \item \code{presence}: Logical, whether species is present in the relevé.
#'     \item \code{pct_presence}: Frequency of the species in the syntaxon (0-1).
#'     \item \code{pct_value}: Observed abundance in the relevé.
#'     \item \code{cover_if_present}: Characteristic abundance (CA) of the species in the syntaxon.
#'   }
#' @param r Weight for present species in the qualitative index (typically 0.5-1.0).
#' @param s Power exponent (0-1) balancing Qualitative (s) vs Quantitative (1-s) components.
#' @param t Weight for present species in MED (typically 0.5-1.0).
#'
#' @return A data frame with raw and normalized indices. Normalized values of 0
#'   indicate an "average" match, while values > 1 indicate atypical relevés.
#' @export
calc_all_indices <- function(data, r, s, t) {
  # Small epsilon to prevent log(0) errors as suggested in manual [cite: 77, 462]
  eps <- 0.0001
  f <- pmax(pmin(data$pct_presence, 1 - eps), eps)

  # --- 1. Qualitative Components (Raw) ---
  # Weirdness: Surprising presence of rare species [cite: 116, 496]
  weirdness_raw <- -2 * sum(log(f[data$presence]))

  # Incompleteness: Surprising absence of frequent species [cite: 119, 499]
  incompleteness_raw <- -2 * sum(log(1 - f[!data$presence]))

  # Total -2LL (Likelihood): The sum of both surprise components [cite: 76, 460]
  llk_raw <- weirdness_raw + incompleteness_raw

  # --- 2. Normalization (Richness Correction) ---
  # To solve the "high LLK" issue, we compute the 'Average Expected Value'
  # for the specific syntaxon.
  exp_weirdness <- sum(f * (-2 * log(f)))
  exp_incompleteness <- sum((1 - f) * (-2 * log(1 - f)))
  exp_llk <- exp_weirdness + exp_incompleteness

  # Normalized indices: (Observed / Expected) - 1
  # 0 = Average match; -1 = Perfect match; >1 = Atypical [cite: 147, 529]
  nrm_wrd <- (weirdness_raw / exp_weirdness) - 1
  nrm_inc <- (incompleteness_raw / exp_incompleteness) - 1
  nrm_llk <- (llk_raw / exp_llk) - 1

  # --- 3. Modified Euclidean Distance (MED) ---
  # Uses CA (Characteristic Abundance) for present species and
  # Avg Abundance (CA * frequency) for absent species[cite: 130, 509].

  # Distance for present species: (Observed - CA)^2
  diff_present <- (data$pct_value[data$presence] - data$cover_if_present[data$presence])^2

  # Distance for absent species: (0 - Avg_Abundance)^2
  avg_abundance_absent <- data$cover_if_present[!data$presence] * f[!data$presence]
  diff_absent <- (0 - avg_abundance_absent)^2

  # Corrected MED formula from ASSOCIA Manual [cite: 510, 512]
  med <- sqrt(2 * (t * sum(diff_present) + (1 - t) * sum(diff_absent)))

  # --- 4. Composite Distance (CoD) ---
  # Formula: (( (1+r)*W + Inc ) / (1+r) )^s * MED^(1-s) [cite: 517]
  # We use normalized versions of W and Inc to ensure CoD is not
  # dominated by qualitative richness bias. We add 1 to keep values positive.

  qual_part_nrm <- ((1 + r) * nrm_wrd + nrm_inc) / (1 + r)

  # Normalized CoD: Scales both components to a similar magnitude
  nrm_cod <- (qual_part_nrm + 1)^s * (med + 1)^(1 - s)

  # Standard CoD (using raw values for back-compatibility)
  qual_part_raw <- ((1 + r) * weirdness_raw + incompleteness_raw) / (1 + r)
  cod <- (qual_part_raw)^s * (med)^(1 - s)

  return(data.frame(
    weirdness = weirdness_raw,
    incompleteness = incompleteness_raw,
    llk_raw = llk_raw,
    med = med,
    cod = cod,
    nrm_wrd = nrm_wrd,
    nrm_inc = nrm_inc,
    nrm_llk = nrm_llk,
    nrm_cod = nrm_cod
  ))
}
