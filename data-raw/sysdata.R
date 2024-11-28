#
# # In data-raw/sysdata.R

synoptic_table <-
  read.csv(file.path("development",
                     "zzz_local_data",
                     "interim",
                     "synoptic_table.csv"))

synoptic_names <-
  read.csv(file.path("development",
                     "zzz_local_data",
                     "interim",
                     "synoptic_names.csv"))

species_list <-
  read.csv(file.path("development",
                     "zzz_local_data",
                     "interim",
                     "species_list.csv"))

usethis::use_data(species_list,
                  synoptic_table,
                  synoptic_names,
                  internal = TRUE,
                  overwrite = TRUE)
