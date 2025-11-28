# data-raw/generate_example_data.R

library(dplyr)
library(tidyr)

# ... [Your dummy data generation code here] ...

library(inbovegtypering) # Uncomment if installed
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)


# ==============================================================================
# 0. Helper functions
# ==============================================================================

# // Helper to generate random synoptic data for one syntaxon
generate_syntaxon_data <- function(code, favored_species = NULL) {
  # Base probability for all species
  freqs <- runif(20, 0, 20)
  covers <- runif(20, 1, 10)

  # Boost favored species to define the type clearly
  if (!is.null(favored_species)) {
    freqs[favored_species] <- runif(length(favored_species), 80, 100)
    covers[favored_species] <- runif(length(favored_species), 40, 80)
  }

  data.frame(
    syntaxon_code = code,
    species_number = species_ids,
    frequency = round(freqs, 1),
    mean_if_present = round(covers, 1)
  )
}

# // Helper function to create recordings
create_recording <- function(id, species_idx, covers) {
  data.frame(
    recording = id,
    species_number = species_idx,
    pct_value = covers,
    layer_code = "K"
  )
}


# ==============================================================================
# 1. GENERATE DUMMY DATA
# ==============================================================================

set.seed(123) # For reproducibility

# --- Define Species List (20 Species) ---
species_ids <- 1:20
dummy_species_list <- data.frame(
  speciesNumber = species_ids,
  speciesName = paste("Species", LETTERS[1:20]),
  scientificName = paste("Genus", LETTERS[1:20], "species"),
  usageKey = 1000 + species_ids,
  acceptedusageKey = 1000 + species_ids
)

# --- Define Synoptic Table (10 Syntaxa) ---
# We create 10 syntaxa.
# SYN-01 and SYN-02 will be dominated by Species 1-5.
# Other syntaxa will be random mixes.

syntaxa_ids <- paste0("SYN-", sprintf("%02d", 1:10))


# Create the full table
synoptics_list <- list()

# SYN-01: Dominant Sp 1, 2, 3
synoptics_list[[1]] <- generate_syntaxon_data("SYN-01", favored_species = c(1, 2, 3))
# SYN-02: Dominant Sp 2, 3, 4 (Similar to SYN-01)
synoptics_list[[2]] <- generate_syntaxon_data("SYN-02", favored_species = c(2, 3, 4))

# Random others
for (i in 3:10) {
  # Pick 3 random dominant species
  doms <- sample(species_ids, 3)
  synoptics_list[[i]] <- generate_syntaxon_data(syntaxa_ids[i], favored_species = doms)
}

dummy_synoptics <- bind_rows(synoptics_list)

# --- Define Recordings (5 Recordings) ---
# REC-01 and REC-02 will be similar (High cover of Sp 1, 2, 3)
# REC-03, 04, 05 will be random.


# REC-01: Matches SYN-01 well
rec1 <- create_recording("REC-01", c(1, 2, 3, 15, 18), c(70, 60, 50, 5, 2))

# REC-02: Very similar to REC-01 (Matches SYN-01/SYN-02)
# Sp 1 is slightly less, Sp 4 added
rec2 <- create_recording("REC-02", c(1, 2, 3, 4, 15), c(60, 65, 55, 10, 5))

# Random others
rec3 <- create_recording("REC-03", sample(species_ids, 6), runif(6, 10, 80))
rec4 <- create_recording("REC-04", sample(species_ids, 5), runif(5, 5, 50))
rec5 <- create_recording("REC-05", sample(species_ids, 7), runif(7, 10, 40))


dummy_recordings <- bind_rows(rec1, rec2, rec3, rec4, rec5)


# ==============================================================================
# 2. Save the data to the package
# ==============================================================================


usethis::use_data(
  dummy_recordings,
  dummy_synoptics,
  dummy_species_list,
  overwrite = TRUE
)
