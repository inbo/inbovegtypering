library(tidyverse)
library(readxl)
library(inbovegtypering)

synoptic_table <- read_csv2("synoptic_table.csv") |>
  rename(
    syntaxon_code = syntaxonCode,
    species_number = speciesNumber
  )

data_orig <- read_excel("../test_tubovegopnames (1).xlsx",
  range = "A1:YV451",
  col_names = TRUE
)
colnames(data_orig) <- c(
  "sci_name",
  "laag",
  paste0(
    "opn_",
    sprintf(
      "%03d",
      1:(ncol(data_orig) - 9)
    )
  ),
  "nr", "species_nr", "nl_name",
  "species_nr2", "family", "genus", "soort"
)

metadata <- bind_cols(data_orig[1:11, 1], data_orig[1:11, 3:665]) |> t()

data_obs <- data_orig[12:451, ] |> as.data.frame()
colnames(data_obs) <- colnames(data_orig)

data_ana <- data_obs |>
  pivot_longer(cols = starts_with("opn"), names_to = "opname", values_to = "pct") |>
  mutate(pct = as.numeric(pct)) |>
  filter(!is.na(pct))


data_prep <- data_ana |>
  filter(opname %in% sort(unique(opname))[1:20]) |>
  transmute(
    scientific_name = sci_name,
    layer_code = "K",
    species_number = as.numeric(species_nr),
    recording = opname,
    pct_value = pct
  )


classification <- classify_vegetation(data_prep, synoptic_table)

prepared_data <- prepare_recordings(data_prep, synoptic_table, layer = "K")
prepared_synoptics <- prepare_synoptic_table(synoptic_table)
analysis_data <- join_recordings_synoptics(prepared_data, prepared_synoptics)

calculations <- calculate_indices(analysis_data)

calc_check <- calculate_indices(analysis_data |>
  filter(
    recording == "opn_001",
    syntaxon_code == "01AA02",
    1 == 1
  ))


calculations1 <-
  calculate_indices(
    analysis_data |>
      filter(
        recording == "opn_001",
        syntaxon_code %in% c("03", "30BA02B", "12AA02C", "43AA01", "26AC06")
      )
  )
df_calculations1 <- analysis_data |>
  filter(
    recording == "opn_001",
    syntaxon_code %in% c("03", "30BA02B", "12AA02C", "43AA01", "26AC06"),
    syntaxon_code %in% c("12AA02C")
  ) |>
  view()



data_prep_1 <- data_prep |> filter(RecordingGivid == "opn_001")
data_prep_2 <- data_prep |> filter(RecordingGivid == "opn_002")

#### classification_claude

tmp1 <- calculate_veg_indices(data_prep_1, synoptic_table) |> arrange(CoD)
tmp2 <- calculate_veg_indices(data_prep_2, synoptic_table) |> arrange(CoD)





# Split by RecordingGivid and return as list

my_data <- data.frame(
  RecordingGivid = c("R001", "R001", "R001", "R002", "R002", "R002"),
  species_number = c(437, 463, 640, 437, 640, 723),
  PctValue = c(25, 10, 5, 30, 0, 15),
  LayerCode = "K"
)

tmp <- calculate_veg_indices(my_data, synoptic_table) |> arrange(syntaxonCode)
cls <- post_process_classification(tmp, index_name = "CoD")
print(cls, n = 30)


test <- calculate_veg_indices(data_prep, synoptic_table) |> arrange(syntaxonCode)


classify_vegetation(data_prep,
  synoptics = synoptic_table,
  method = "CoD"
)



# Het werkt, maar niet correct, ligt dat aan speciesNumbers? of is de berekening fout?
# altijd dezelfde info (de eerste opname) wordt weergegeven in summary ongeacht de opname
# altijd dezelfde info ongeacht welke releve (is iets vastgezet ipv de data zelf te gebruiken?)

cl_test_llk <- classify_vegetation(data_prep,
  synoptics = synoptic_table,
  method = "likelihood"
)

cl_test_wrd <- classify_vegetation(data_prep,
  synoptics = synoptic_table,
  method = "weirdness"
)

cl_test_inc <- classify_vegetation(data_prep,
  synoptics = synoptic_table,
  method = "incompleteness"
)

cl_test_med <- classify_vegetation(data_prep,
  synoptics = synoptic_table,
  method = "med"
)

cl_test <- classify_vegetation(data_prep,
  synoptics = synoptic_table,
  method = "cod"
)


tmp <- calculate_veg_indices(data_prep, synoptic_table)
cls001 <- post_process_classification(tmp |> filter(RecordingGivid == "opn_001"),
  index_name = "CoD"
)
cls002 <- post_process_classification(tmp |> filter(RecordingGivid == "opn_002"),
  index_name = "CoD"
)
cls020 <- post_process_classification(tmp |> filter(RecordingGivid == "opn_020"),
  index_name = "CoD"
)
cls001$opn_001[c("syntaxonCode", "CoD")]
cls002$opn_002[c("syntaxonCode", "CoD")]
cls020$opn_020[c("syntaxonCode", "CoD")]




print(cl_test)
summary(cl_test)
plot(cl_test) # hier nog problemen met angle oplossen
ggplot(
  cl_test$opn_001 |> arrange(CoD) |>
    slice(1:10) |>
    mutate(syntaxonCode = factor(syntaxonCode, levels = syntaxonCode)),
  aes(x = syntaxonCode, y = CoD)
) +
  geom_bar(stat = "identity")



view(cl_test$opn_001 |> select(syntaxonCode, CoD))
cl_test$opn_001 |>
  select(syntaxonCode, CoD) |>
  filter(syntaxonCode %in% c("12AA02B", "12AA02C"))
