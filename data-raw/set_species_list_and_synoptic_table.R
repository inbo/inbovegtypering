
library(tidyverse)

#usageKey voor accepted species, acceptedUsageKey wel belangrijk voor synoniemen
#Best beide kolommen behouden en op beiden checken
#Voor de niet-eenduidige gbifsoorten, krijgt hetzelfde soortnummer
#verschillende regels met de mogelijk bijhorende namen

soortnamen_orig <-
  read_csv2(file.path("development",
                      "zzz_local_data",
                      "orig",
                      "_originele_soortnamen_2005.csv"))
soortinterpretatie <-
  read_csv2(file.path("development",
                      "zzz_local_data",
                      "working",
                      "soortinterpretatie.csv"))

soortnamen <- soortnamen_orig |>
  mutate(soortnaam = str_replace(soortnaam, " species", ""))

gbif_matches_backbone <- soortnamen |>
  bind_cols(rgbif::name_backbone_checklist(soortnamen$soortnaam,
                                           kingdom = "Plantae"))

no_match <- which(!gbif_matches_backbone$matchType %in% c("EXACT","FUZZY") |
                    gbif_matches_backbone$rank %in%
                    c("FAMILY", "KINGDOM", "ORDER", "PHYLUM"))

gbif_matched <- gbif_matches_backbone |> slice(-no_match)

### after manual curation

gbif_no_matches <- gbif_matches_backbone |> slice(no_match)
not_matched_soortnaam <- gbif_no_matches |>
  select("soortnummer", "soortnaam") |>
  left_join(soortinterpretatie,
            join_by(soortnaam == soortnaam_orig)) |>
  pivot_longer(cols = c(soortnaam_1, soortnaam_2, soortnaam_3),
               names_to = "kandidaat",
               values_to = "soortnaam_kandidaat",
               values_drop_na = TRUE) |>
  mutate(soortnaam_kandidaat = str_replace_all(soortnaam_kandidaat,
                                               "\xa0",
                                               " "))

gbif_new_matches <- not_matched_soortnaam |>
  bind_cols(
    rgbif::name_backbone_checklist(not_matched_soortnaam$soortnaam_kandidaat,
                                   kingdom = "Plantae"))

#non-matches get as usageKey or acceptedUsageKey -soortnaam
gbif_problems <- gbif_new_matches |>
  filter(matchType == "HIGHERRANK" | matchType %in% c("KINGDOM", "PHYLUM")) |>
  mutate(usageKey = -1 * usageKey)

### full list
soortenlijst_volledig <- gbif_matched |>
  bind_rows(gbif_new_matches |>
              filter(!(matchType == "HIGHERRANK" |
                       matchType %in% c("KINGDOM", "PHYLUM")))) |>
  bind_rows(gbif_problems) |>
  select(speciesNumber = "soortnummer",
         speciesName = "soortnaam", "scientificName",
         "usageKey", "acceptedUsageKey",
         "synonym", "rank", "speciesKey", "genusKey", "familyKey",
         "orderKey", "classKey", "phylumKey", "kingdomKey")

write_excel_csv2(
  soortenlijst_volledig,
  file = file.path("development",
                   "zzz_local_data",
                   "interim",
                   "synoptic_species.csv"))
species_list <- soortenlijst_volledig


###########################################################

### SYNOPTIC table

synoptic_table <-
  read_csv2(file.path("development",
                      "zzz_local_data",
                      "orig",
                      "_originele_synoptische_gegevens_2005.csv")) |>
  select(syntaxonCode = "syntaxoncode",
         speciesNumber = "soortnummer",
         frequency = "frequentie",
         mean_if_present = "gem_als_aanwezig")

write_excel_csv2(
  synoptic_table,
  file = file.path("development",
                   "zzz_local_data",
                   "interim",
                   "synoptic_table.csv"))

### Synoptic names

synoptic_names <-
  read_csv2(file.path("development",
                      "zzz_local_data",
                      "orig",
                      "_originele_synoptische_namen_2005.csv")) |>
  select(syntaxonCode = "syntaxoncode",
         scientificSyntaxon = "wet_syntaxonnaam",
         dutchSyntaxon = "nl_syntaxonnaam")

write_excel_csv2(
  synoptic_names,
  file = file.path("development",
                   "zzz_local_data",
                   "interim",
                   "synoptic_names.csv"))

save(species_list, synoptic_table, synoptic_names,
     file =  file.path("development", "R", "sysdata.rda"))

write_official <- FALSE
if (write_official) {
  usethis::use_data(species_list,
                    synoptic_table,
                    synoptic_names,
                    internal = TRUE,
                    overwrite = TRUE)
}

