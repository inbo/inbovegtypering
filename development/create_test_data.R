#create some test data
  #a few recordings from inboveg (with usageKey added)
  #a small part of the synoptic table
  #only the species_list relevant for the partial synoptic table

### Retrieve some records
#########################"

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
                               rank, speciesKey, genusKey))


write_excel_csv2(example_recordings,
                 file.path("development", "extdata", "example_recordings.csv"))


### RETRIEVE SOME SYNOPTIC DATA




### RETRIEVE CORRESPONDING SYNOPTIC SPECIES






table(recording_heischraal2012$RecordingGivid)


