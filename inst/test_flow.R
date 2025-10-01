library(tidyverse)
library(inbovegtypering)

# preparation
#------------

con_veg <- connect_db_inboveg()
conn_taxa <- connect_db_taxonomy()
conn_flora <- connect_db_flora()

# observation data
#-----------------
single_record <-
  read_inboveg_recording(
    con_inboveg = con_veg,
    con_taxa = conn_taxa,
    survey = "MILKLIM_Heischraal2012",
    code = "IV2012081611384756"
  )

multiple_records <- #>30sec
  read_inboveg_recording(
    con_inboveg = con_veg,
    con_taxa = conn_taxa,
    survey = "MILKLIM_Heischraal2012"
  )

#classify via likelihood
cls_llk <- classify_likelihood(single_record, normalised = TRUE)
summary(cls_llk, n = 50)
plot(cls_llk)

#classify via combibned index

# cls_data <- create_analysis_data(multiple_records, synoptic_table)
cls_cod <- classify_cod(single_record, synoptics = "default")
summary(cls_cod, n = 50)
plot(cls_cod) #werkt nog niet

# classify
#----------

cls_llk <- classify_likelihood(single_record, normalised = TRUE)

cls_wrd <- classify_weirdness(single_record, synoptic_data, normalised = TRUE)
cls_inc <- classify_incompleteness(
  single_record,
  synoptic_data,
  normalised = TRUE
)
cls_med <- classify_euclidean(single_record, synoptic_data, normalised = TRUE)
cls_cod <- classify_composite(single_record, synoptic_data, normalised = TRUE)

# visualize
#-----------

plot(cls_llk)
plot(cls_wrd)
plot(cls_inc)
plot(cls_med)
plot(cls_cod)
