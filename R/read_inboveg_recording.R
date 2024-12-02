#' Read a recording from inboveg
#'
#' Read a recording from Cydonia, specifying the survey and RecordingGivid
#' @param con_inboveg db connection to Cydonia
#' @param con_taxa db connection to "Taxa" db
#' @param survey survey name
#' @param code releve code within survey
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' library(inbovegtypering)
#' con_inboveg <- connect_db_inboveg()
#' con_taxa <- connect_db_taxonomy()
#' record <-
#'   read_inboveg_recording(con_inboveg, con_taxa,
#'     survey = "MILKLIM_Heischraal2012",
#'     code = "IV2012081611384756"
#'   )
#' }
read_inboveg_recording <- function(con_inboveg, con_taxa, survey, code) {

  #step 1: Read records from DB
  #-----------------------------
  records <-
    inbodb::get_inboveg_recording(con_inboveg, survey, collect = TRUE) |>
    dplyr::filter(
      .data$RecordingGivid == code,
      .data$LayerCode == "K"
    ) |>
    dplyr::mutate(coverage = .data$PctValue / 100)

  species_names <- records |> select("OriginalName", "ScientificName")

  #step 2: link Recordings to taxonomy
  #------------------------------------

  #step 2a: find OriginalName in taxa db

  species_def_o <- link_taxa_db(con_taxa, species_names |> pull(OriginalName))
  which_unclassified <- which(is.na(species_def_o$usageKey))
  species_def_s <- link_taxa_db(con_taxa, species_names |> pull(ScientificName))

  #keep all matches op species_def_o and for the unmatched use those matched
  #on scientific name
  species_def <- species_def_o |>
    filter(!is.na(usageKey)) |>
    bind_rows(species_def_s |> slice(which_unclassified))

  if(!("acceptedUsageKey" %in% colnames(species_def)))
    species_def$acceptedUsageKey <- NA

  #step 2b: find species on gbif

  unclassified_names <- species_def |>
    filter(is.na(usageKey)) |>
    pull(name)

  if(length(unclassified_names)) {
    rgbif_found <- rgbif::name_backbone_checklist(unclassified_names) |>
      rename(name = "verbatim_name")

    if(!("acceptedUsageKey" %in% colnames(rgbif_found)))
      rgbif_found$acceptedUsageKey <- NA

    rgbif_found <- rgbif_found |>
      select(all_of(colnames(species_def)))

    species_def <- species_def |>
      filter(!is.na(usageKey)) |>
      bind_rows(rgbif_found)
  }

  #step 2c: find names in with package included species_list

  unclassified_names2 <- species_def |>
    filter(is.na(usageKey)) |>
    pull(name)

  if (length(unclassified_names2)) {
    in_specieslist <-  link_species_list(unclassified_names2)
    species_def <- species_def |>
      filter(!is.na(usageKey)) |>
      bind_rows(in_specieslist)
  }

  #step 3: link synoptic species to species_def
  #--------------------------------------------

   on_usagekey <- species_def |>
    rowwise() |>
    mutate(
      species_number_uk = list(link_synoptic_species(usageKey,
                                                     which = "usage_key")),
      species_number_nm = list(link_synoptic_species(name,
                                                     which = "name")),
      species_number = list(
        unique(c(na.omit(as.numeric(unlist(species_number_uk))),
                na.omit(as.numeric(unlist(species_number_nm)))))))

  #step 4 add species info to record
  #----------------------------------------

  rv <- records |>
    left_join(on_usagekey, join_by(OriginalName == name))

  rv
}
