install.packages("inbodb")

library("inbodb")

con <- connect_inbo_dbase("D0010_00_Cydonia")

allsurveys <- get_inboveg_survey(con, collect = TRUE)

partsurveys <- get_inboveg_survey(
  con,
  survey_name = "%MILKLIM%",
  collect = TRUE
)

header_info <- get_inboveg_header(
  con,
  survey_name = c(
    "MILKLIM_Watina",
    "MILKLIM_Heischraal2012",
    "NICHE Vlaanderen"
  ),
  multiple = TRUE,
  rec_type = "Classic",
  collect = TRUE
)

all_header_info <- get_inboveg_header(con) # not collecting data

recording_heischraal2012 <- get_inboveg_recording(
  con,
  survey_name = "MILKLIM_Heischraal2012",
  collect = TRUE
)

recording_specific_givid <- get_inboveg_recording(
  con,
  recording_givid = "IV2012080609161322",
  collect = TRUE
)

recording_specific_givid <- get_inboveg_recording(
  con,
  recording_givid = c("IV2012080609161322", "IV2012081611384756"),
  collect = TRUE
)

allrecordings <- get_inboveg_recording(con)

classif_info <- get_inboveg_classification(
  con,
  survey_name = c("MILKLIM_Heischraal2012", "NICHE Vlaanderen"),
  multiple = TRUE
)
classif_info

all_codes <- get_inboveg_classification(con, collect = TRUE)

# rechtenprobleem voor FUTON DB
qualifiers_heischraal2012 <- get_inboveg_qualifier(
  con,
  survey_name = "MILKLIM_Heischraal2012"
)

layerinfo_severalsurveys <- get_inboveg_layer_cover(
  con,
  survey_name = c("MILKLIM_Heischraal2012", "NICHE Vlaanderen"),
  multiple = TRUE
)

###

con <- connect_inbo_dbase("D0010_00_Cydonia")

recording_heischraal2012 <- get_inboveg_recording(
  con,
  survey_name = "MILKLIM_Heischraal2012",
  collect = TRUE
)

classif_heischraal2012 <-
  get_inboveg_classification(con,
    survey_name = "MILKLIM_Heischraal2012",
    collect = TRUE
  )


###

recording_test <- recording_heischraal2012 |>
  filter(RecordingGivid == "IV2012081611384756")

gbifcodes <- read_csv2("inst/soortenlijst/gbif_inbo.csv")

recording_test_added <- recording_test |>
  mutate(id_key = seq_len(n())) |>
  left_join(
    gbifcodes |> select(scientificName, usageKey),
    join_by(ScientificName == scientificName)
  ) |>
  distinct(id_key, .keep_all = TRUE)



recording_traets <- get_inboveg_recording(
  con,
  survey_name = "Vegetatieopnames_Kalmthout_Traets_1955_1959",
  collect = TRUE
)
