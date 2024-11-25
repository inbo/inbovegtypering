library(tidyverse)
synoptic_data_orig <-
  read_csv2("inst/species_list/synoptische_gegevens_2005.csv")
syntaxons_orig <-
  read_csv2("inst/species_list/synoptische_namen_2005.csv")
syn_species_orig <-
  read_csv2("inst/species_list/soortnamen_2005.csv")

# checks (must be TRUE)
!any(duplicated(syn_species_orig$soortnummer))
all(synoptic_data$soortnummer %in% syn_species_orig$soortnummer)
n_species <- nrow(syn_species_orig)
syn_species <- syn_species_orig |>
  mutate(
    rank_custom = "sp-subsp",
    rank_custom = ifelse(str_detect(soortnaam, "\\sspecies"),
      "genus",
      rank_custom
    ),
    soortnaam = str_replace(soortnaam, " species", ""),
    OK = FALSE
  )

websearch <- FALSE
if (websearch) {
  map_gbif <-
    bind_cols(
      syn_species,
      rgbif::name_backbone_checklist(syn_species$soortnaam)
    )
  saveRDS(map_gbif, file = "inst/extdata/syn_species_gbif_mapping.RDS")
}

# Eerste check
#-------------

map_gbif <- readRDS("inst/extdata/syn_species_gbif_mapping.RDS")

syn_species_matched <- map_gbif |>
  mutate(
    rank_custom = ifelse(matchType == "EXACT",
      tolower(rank),
      rank_custom
    ),
    OK = ifelse(matchType == "EXACT", TRUE, OK)
  )

table(syn_species_matched$OK)
saveRDS(syn_species_matched, file = "inst/extdata/syn_species_matched.RDS")

# De L. toevoegen aan soortnamen
#-------------------------------

l_added <- syn_species_matched |>
  filter(!OK) |>
  select(soortnummer, soortnaam, rank_custom, OK) |>
  mutate(soortnaam_custom = paste(soortnaam, "L"))
l_gbif <- rgbif::name_backbone_checklist(l_added$soortnaam_custom)
l_gbif <- bind_cols(l_added, l_gbif) |>
  mutate(
    OK = ifelse(matchType %in% c("EXACT", "FUZZY"), TRUE, OK),
    rank_custom = ifelse(matchType %in% c("EXACT", "FUZZY"),
      tolower(rank),
      rank_custom
    )
  )

syn_species_matched2 <-
  syn_species_matched |>
  filter(OK == TRUE) |>
  bind_rows(l_gbif)

saveRDS(syn_species_matched2, file = "inst/extdata/syn_species_matched2.RDS")

# Probeer Nederlandstalige namen te matchen
#--------------------------------------------

source(file = "inst/development/_fun_taxon_mapping_mbag-mas_ward.R")

not_ok <- syn_species_matched2 |> filter(!OK)



###



syn_names_temp <- syn_species_matched2 |>
  mutate(usageKey = ifelse(OK == FALSE, -1 * soortnummer, usageKey)) |>
  select(soortnummer, usageKey,
    rank = rank_custom, match = matchType, canonicalName
  )





# Voeg de nieuwe namen toe aan de synoptische tabel
#--------------------------------------------------

synoptic_data <- synoptic_data_orig |>
  left_join(
    syn_names_temp |> select(soortnummer, usageKey),
    join_by(soortnummer)
  )

write_excel_csv2(synoptic_data, "inst/resources/synoptic_tabel.csv")
write_excel_csv2(syn_names_temp, "inst/resources/synoptic_species.csv")



