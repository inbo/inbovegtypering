library(tidyverse)
library(readxl)
#library(inbovegtypering) #nolint

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
  pivot_longer(cols = starts_with("opn"),
               names_to = "opname",
               values_to = "pct") |>
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


synt_01AA02B <- synoptic_table |> filter(syntaxon_code == "01AA02B") #nolint
data_01AA02B <- data.frame(  #nolint
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
# 03, 03A, 03AA en veel andere korte namen scoren altijd goed, dus onderzoeken


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
