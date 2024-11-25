library(tidyverse)
library(inbovegtypering)

# preparation
#------------

conn <- connect_db_inboveg()
conn2 <- connect_db_taxonomy()
synoptic_data <- load_synoptic_data(source = "test")

# observation data
#-----------------
recordings <-
  read_inboveg_recording(
    con_inboveg = conn,
    con_taxa = conn2,
    survey = "MILKLIM_Heischraal2012",
    code = "IV2012081611384756"
  )


# classify
#----------

cls_llk <- classify_likelihood(record, synoptic_data, normalised = TRUE)
cls_wrd <- classify_weirdness(record, synoptic_data, normalised = TRUE)
cls_inc <- classify_incompleteness(record, synoptic_data, normalised = TRUE)
cls_med <- classify_euclideqn(record, synoptic_data, normalised = TRUE)
cls_cod <- classify_composite(record, synoptic_data, normalised = TRUE)

# visualize
#-----------

plot(cls_llk)
plot(cls_wrd)
plot(cls_inc)
plot(cls_med)
plot(cls_cod)
