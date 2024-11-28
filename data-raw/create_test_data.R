#create some test data
  #a few recordings from inboveg (with usageKey added)
  #a small part of the synoptic table
  #only the species_list relevant for the partial synoptic table


### Retrieve some records
###==========================

library("inbodb")
con <- connect_inbo_dbase("D0010_00_Cydonia")
recording_examples_hei <- get_inboveg_recording(
  con,
  survey_name = c("MILKLIM_Heischraal2012"),
  collect = FALSE
)

recording_examples_gb <- get_inboveg_recording(
  con,
  survey_name = c("MILKLIM_W&Z_Geraardsbergen"),
collect = FALSE
)

set.seed(123)
givids <- c(recording_examples_hei |>
              pull(RecordingGivid) |>
              unique() |>
              sample(size = 20),
            recording_examples_gb |>
              pull(RecordingGivid) |>
              unique() |>
              sample(size = 20))


example_recordings <-
  get_inboveg_recording(
    con,
    recording_givid = givids,
  collect = TRUE
)

gbifkeys <- rgbif::name_backbone_checklist(example_recordings$ScientificName)

example_recordings <- example_recordings |>
  bind_cols(gbifkeys |> select(usageKey, acceptedUsageKey,
                               rank, speciesKey, genusKey,
                               familyKey, orderKey, classKey,
                               phylumKey, kingdomKey))


saveRDS(example_recordings,
        file.path("development",
                  "data",
                  "example_recordings.rda"))


### RETRIEVE SOME SYNOPTIC DATA
###=============================

species_list <-
  read_csv2(file.path("development",
                      "zzz_local_data",
                      "interim",
                      "synoptic_species.csv"))

example_recordings <-
  readRDS(file.path("development",
                    "data",
                    "example_recordings.rda"))

synoptic_table_orig <-
  read_csv2(file.path("development",
                      "zzz_local_data",
                      "interim",
                      "synoptic_table.csv"))

synoptic_names <-
  read_csv2(file.path("development",
                      "zzz_local_data",
                      "interim",
                      "synoptic_names.csv"))


synoptic_table <- synoptic_table_orig |>
  mutate(row_id = row_number()) |>
  left_join(species_list |>
              select(speciesNumber, usageKey, acceptedUsageKey),
            join_by(speciesNumber), relationship = "many-to-many") |>
  filter(!is.na(usageKey))

#check good range for example_recordings
chosen_syntaxons <- synoptic_table |>
  group_by(syntaxonCode) |>
  summarise(species_count = sum(usageKey %in% example_recordings$usageKey)) |>
  arrange(desc(species_count)) |>
  slice(c(seq(1, n(), by = 50)[1:15], seq(2, round(n()/2), by = 50)[1:10])) |>
  pull(syntaxonCode)


set.seed(124)
example_synoptics <- synoptic_table |>
  filter(syntaxonCode %in% chosen_syntaxons)

saveRDS(example_synoptics,
        file.path("development", "data", "example_synoptics.rda"))

write_official <- FALSE
if (write_official) {
  usethis::use_data(example_recordings, overwrite = TRUE, internal = FALSE)
  usethis::use_data(example_synoptics, overwrite = TRUE, internal = FALSE)
}
