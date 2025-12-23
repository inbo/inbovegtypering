library(tidyverse)
library(readxl)
library(inbovegtypering)

synoptic_table <- read_csv2("../synoptic_table.csv") |>
  rename(
    syntaxon_code = syntaxonCode,
    species_number = speciesNumber
  )

data_meta <- read_excel("../test_tubovegopnames (2).xlsx",
  range = "A1:YV14",
  col_names = FALSE
)
colnames(data_meta) <- c(
  "sci_name",
  "laag",
  paste0(
    "opn_",
    sprintf(
      "%03d",
      1:(ncol(data_meta) - 9)
    )
  ),
  "nr", "species_nr", "nl_name",
  "species_nr2", "family", "genus", "soort"
)


data <- read_excel("../test_tubovegopnames (2).xlsx",
  range = "A16:YV453",
  col_names = FALSE
)
colnames(data) <- colnames(data_meta)

data <- data |>
  mutate(across(starts_with("opn_"), as.numeric))


data_ana <- data |>
  pivot_longer(cols = starts_with("opn"), names_to = "opname", values_to = "pct") |>
  mutate(pct = as.numeric(pct)) |>
  filter(!is.na(pct))


data_prep <- data_ana |>
  filter(opname %in% sort(unique(opname))[1:100]) |>
  transmute(
    scientific_name = sci_name,
    layer_code = "K",
    species_number = as.numeric(species_nr),
    recording = opname,
    pct_value = pct
  )

data_prep1 <- data_prep |> filter(recording == "opn_001")
data_prep2 <- data_prep |> filter(recording == "opn_002")
data_prep11 <- data_prep |> filter(recording == "opn_011")
data_prep100 <- data_prep |> filter(recording == "opn_100")

classification1 <-
  classify_vegetation(
    data_prep1,
    synoptic_table
  )

classification2 <-
  classify_vegetation(
    data_prep2,
    synoptic_table
  )

classification11 <-
  classify_vegetation(
    data_prep11,
    synoptic_table
  )


classification100 <-
  classify_vegetation(
    data_prep100,
    synoptic_table
  )

summary(classification1)
summary(classification2)
summary(classification11)
summary(classification100)


synt_01AA02B <- synoptic_table |> filter(syntaxon_code == "01AA02B")
data_01AA02B <- data.frame(
  species_number = synt_01AA02B$species_number,
  pct_value = synt_01AA02B$mean_if_present,
  recording = "perfectmatch",
  layer_code = "K"
)

classification_perfect <-
  classify_vegetation(
    data_01AA02B,
    synoptic_table
  )
# onderzoeken waarom llk zo hoog is, de cod en med zijn inderdaad wel heel laag
# med = 0 is zoals verwacht bij een exacte match, med is altijd tss 0 en 1
# 03, 03A, 03AA en veel andere korte namen scoren altijd goed, dus dat onderzoeken


all_perfect_data <- synoptic_table |>
  dplyr::transmute(
    species_number,
    pct_value = mean_if_present,
    recording = syntaxon_code,
    layer_code = "K"
  )

classification_all_perfect <-
  classify_vegetation(all_perfect_data, synoptic_table)

saveRDS(classification_all_perfect, file = "classification_perfect_data.RDS")






# prepared_data <- prepare_recordings(data_prep, synoptic_table, layer = "K")
# prepared_synoptics <- prepare_synoptic_table(synoptic_table)
# analysis_data <- join_recordings_synoptics(prepared_data, prepared_synoptics)
#
# calculations <- calculate_indices(analysis_data)
#
# calc_check <- calculate_indices(analysis_data |>
#   filter(
#     recording == "opn_001",
#     syntaxon_code == "01AA02",
#     1 == 1
#   ))
#
#
# calculations1 <-
#   calculate_indices(
#     analysis_data |>
#       filter(
#         recording == "opn_001",
#         syntaxon_code %in% c("03", "30BA02B", "12AA02C", "43AA01", "26AC06")
#       )
#   )
# df_calculations1 <- analysis_data |>
#   filter(
#     recording == "opn_001",
#     syntaxon_code %in% c("03", "30BA02B", "12AA02C", "43AA01", "26AC06"),
#     syntaxon_code %in% c("12AA02C")
#   ) |>
#   view()
#
#
#
# data_prep_1 <- data_prep |> filter(RecordingGivid == "opn_001")
# data_prep_2 <- data_prep |> filter(RecordingGivid == "opn_002")
#
# #### classification_claude
#
# tmp1 <- calculate_veg_indices(data_prep_1, synoptic_table) |> arrange(CoD)
# tmp2 <- calculate_veg_indices(data_prep_2, synoptic_table) |> arrange(CoD)
#
#
#
#
#
# # Split by RecordingGivid and return as list
#
# my_data <- data.frame(
#   RecordingGivid = c("R001", "R001", "R001", "R002", "R002", "R002"),
#   species_number = c(437, 463, 640, 437, 640, 723),
#   PctValue = c(25, 10, 5, 30, 0, 15),
#   LayerCode = "K"
# )
#
# tmp <- calculate_veg_indices(my_data, synoptic_table) |> arrange(syntaxonCode)
# cls <- post_process_classification(tmp, index_name = "CoD")
# print(cls, n = 30)
#
#
# test <- calculate_veg_indices(data_prep, synoptic_table) |> arrange(syntaxonCode)
#
#
# classify_vegetation(data_prep,
#   synoptics = synoptic_table,
#   method = "CoD"
# )
#
#
#
# # Het werkt, maar niet correct, ligt dat aan speciesNumbers? of is de berekening fout?
# # altijd dezelfde info (de eerste opname) wordt weergegeven in summary ongeacht de opname
# # altijd dezelfde info ongeacht welke releve (is iets vastgezet ipv de data zelf te gebruiken?)
#
# cl_test_llk <- classify_vegetation(data_prep,
#   synoptics = synoptic_table,
#   method = "likelihood"
# )
#
# cl_test_wrd <- classify_vegetation(data_prep,
#   synoptics = synoptic_table,
#   method = "weirdness"
# )
#
# cl_test_inc <- classify_vegetation(data_prep,
#   synoptics = synoptic_table,
#   method = "incompleteness"
# )
#
# cl_test_med <- classify_vegetation(data_prep,
#   synoptics = synoptic_table,
#   method = "med"
# )
#
# cl_test <- classify_vegetation(data_prep,
#   synoptics = synoptic_table,
#   method = "cod"
# )
#
#
# tmp <- calculate_veg_indices(data_prep, synoptic_table)
# cls001 <- post_process_classification(tmp |> filter(RecordingGivid == "opn_001"),
#   index_name = "CoD"
# )
# cls002 <- post_process_classification(tmp |> filter(RecordingGivid == "opn_002"),
#   index_name = "CoD"
# )
# cls020 <- post_process_classification(tmp |> filter(RecordingGivid == "opn_020"),
#   index_name = "CoD"
# )
# cls001$opn_001[c("syntaxonCode", "CoD")]
# cls002$opn_002[c("syntaxonCode", "CoD")]
# cls020$opn_020[c("syntaxonCode", "CoD")]
#
#
#
#
# print(cl_test)
# summary(cl_test)
# plot(cl_test) # hier nog problemen met angle oplossen
# ggplot(
#   cl_test$opn_001 |> arrange(CoD) |>
#     slice(1:10) |>
#     mutate(syntaxonCode = factor(syntaxonCode, levels = syntaxonCode)),
#   aes(x = syntaxonCode, y = CoD)
# ) +
#   geom_bar(stat = "identity")
#
#
#
# view(cl_test$opn_001 |> select(syntaxonCode, CoD))
# cl_test$opn_001 |>
#   select(syntaxonCode, CoD) |>
#   filter(syntaxonCode %in% c("12AA02B", "12AA02C"))
