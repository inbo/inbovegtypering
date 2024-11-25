#' Read species list
#'
#' @param link_gbif use rgbif::name_backbone_checklist
#' @return tibble with 2 columns: 'soortnummer' and 'soortnaam'
#' @export
#' @examples
#' speclist <- read_species_list()
#' gbiflink <- rgbif::name_backbone_checklist(speclist$soortnaam)
#' species_info <- spec_list |> bind_cols(gbiflink)
#' write_excel_csv2(species_info, file = "species_list.csv")
#'
read_species_list <- function(link_gbif = FALSE) {
  # Get the path to the CSV file
  file_path <- system.file("soortenlijst",
    "soortenlijst_gbif.csv",
    package = "inbovegtypering"
  )

  # Check if the file exists
  if (file_path == "") {
    stop("Synoptic data file not found in the package.")
  }

  readr::read_csv2(file_path)
}


###############

link_species_list <- function(file, cleanup = TRUE) {
  read.csv2(file)
}

dothis <- FALSE
if (dothis) {
  soorten <- read.csv2("inst/soortenlijst/soortnamen_2005.csv")
  soorten_first <- rgbif::name_backbone_checklist(soorten$soortnaam)
  soorten_linked <- bind_cols(soorten, soorten_first)
  write_excel_csv2(soorten_linked, "inst/soortenlijst/soortnamen_2005_partially_matched.csv") # nolint

  soorten_found1 <- soorten_linked |> filter(!is.na(usageKey))
  write_excel_csv2(
    soorten_found1,
    "inst/soortenlijst/soortnamen_2005_found_immediately.csv"
  )


  soorten_notfound <- read_csv2("inst/soortenlijst/soortnamen_2005_partially_matched.csv") |> #nolint
    filter(is.na(usageKey)) |>
    select(soortnummer, soortnaam) |>
    mutate(
      soortnaam2 =
        str_replace_all(soortnaam,
          pattern = " \\(wier\\)| species| \\(small\\)| \\(breed\\)| \\(dood\\)", replacement = "")) # nolint


  soorten_linked2 <-
    sapply(paste(soorten_notfound$soortnaam2, "L."),
      FUN = rgbif::name_backbone, rank = "genus"
    ) |>
    bind_rows() |>
    bind_cols(soorten_notfound |> select(soortnummer, soortnaam, soortnaam2))

  soorten_found2 <- soorten_linked2 |> filter(!is.na(usageKey))
  write_excel_csv2(soorten_found2, "inst/soortenlijst/soortnamen_2005_found_genusL.csv") #nolint

  soorten_notfound2 <- soorten_linked2 |>
    filter(is.na(usageKey)) |>
    select(soortnummer, soortnaam, soortnaam2)
  soorten_notfound2 <- soorten_notfound2 |>
    mutate(
      soortnaam3 = soortnaam2,
      soortnaam3 = case_match(
        soortnaam3,
        "Cladophora" ~ "Cladophora K",
        "Vaucheria" ~ "Vaucheria A.P",
        "Zygmales" ~ "custom_Zygmales",
        "Wierflap" ~ "custom_Wierflap",
        "Algenvlokken" ~ "custom_Algenvlokken",
        "Callitriche (smal)" ~ "Callitriche L.",
        "Groenwieren" ~ "custom_Groenwieren",
        "Mossen (overige)" ~ "custom_Mossen (overige)",
        "Pohlia" ~ "Pohlia Hedw",
        "Littorella" ~ "Littorella Bergius",
        "Carex nigra x trinervis" ~ "custom_Carex nigra x trinervis",
        "Campylopus" ~ "Campylopus Brid.",
        "Plagiothecium" ~ "Plagiothecium Schimp.",
        "Ammophila" ~ "Ammophila W.Kirby",
        "Carex hostiana x oederi s. oederi" ~
          "custom_Carex hostiana x oederi s. oederi",
        "Parapholis" ~ "Parapholis C.E.Hubb.",
        "Equisetum hyemale ag. (incl. E. x moorei)" ~ "Equisetum hyemale L.",
        "Omphalina" ~ "Omphalina Q",
        "Dactylorhiza maculata x majalis" ~
          "custom_Dactylorhiza maculata x majalis",
        "Rhinanthus angustifolius x minor" ~
          "custom_Rhinanthus angustifolius x minor",
        "Plagiothecium laetum s.l. Schimp. (incl. P. curvifolium)" ~
          "Plagiothecium laetum Schimp.",
        "Dicranella" ~ "Dicranella (Müll.Hal.) Schimp.",
        "Kurzia" ~ "Kurzia G.Martens",
        "Ceratodon" ~ "Ceratodon Brid.",
        "Rumex cripsus x obtusifolius" ~ "custom_Rumex cripsus x obtusifolius",
        "Korstmossen (overige)" ~ "custom_Korstmossen (overige)",
        "Graan" ~ "custom_Graan",
        "Collema" ~ "Collema P.Browne",
        "Weissia" ~ "Weissia Hedw.",
        "Encalypta" ~ "Encalypta Hedw.",
        "Ammophila" ~ "Ammophila W.Kirby",
        "Ulothrix" ~ "Ulothrix K",
        "Ammophila arenaria" ~ "Ammophila arenaria (L.)",
        "Diplotaxis" ~ "Diplotaxis DC",
        "Digitaria" ~ "Digitaria Haller",
        "Cirsium dissectum x palustre" ~ "custom_Cirsium dissectum x palustre",
        "Carex hostiana x oederi s. oedocarpa" ~
          "custom_Carex hostiana x oederi s. oedocarpa",
        "Mentha aquatica + M. x verticillata" ~
          "custom_Mentha aquatica + M. x verticillata",
        "Viola canina x persicaria" ~ "custom_Viola canina x persicaria",
        "Leucodon" ~ "Leucodon Schw",
        "Amblyodon" ~ "Amblyodon P.Beauv.",
        "Didymodon" ~ "Didymodon Hedw",
        "Setaria" ~ "Setaria P.Beauv.",
        "Graan cultuurgewas" ~ "custom_Graan cultuurgewas",
        "Hakvrucht cultuurgewas" ~ "custom_Hakvrucht cultuurgewas",
        "Zomerpeen cultuurgewas" ~ "custom_Zomerpeen cultuurgewas",
        "Vulpia" ~ "Vulpia C",
        "Grimmia" ~ "Grimmia Hedw",
        "Apera" ~ "Apera Adans",
        "Wintertarwe (cult." ~ "custom_Wintertarwe (cult.",
        "Plagiopus" ~ "Plagiopus Brid.",
        "Cinclidium" ~ "Cinclidium Sw.",
        "Salix caprea x viminalis." ~ "custom_Salix caprea x viminalis.",
        "Athyrium" ~ "Athyrium Roth",
        "Lactarius" ~ "Lactarius Pers",
        .default = soortnaam3 # This keeps all other values unchanged
      )
    )

  soorten_linked3 <-
    sapply(soorten_notfound2$soortnaam3,
      FUN = rgbif::name_backbone
    ) |>
    bind_rows() |>
    bind_cols(soorten_notfound2 |>
                select(soortnummer, soortnaam, soortnaam2, soortnaam3))

  write_excel_csv2(soorten_linked3,
                   "inst/soortenlijst/soortnamen_2005_rest.csv")


  soorten_gbif <- bind_rows(soorten_found1, soorten_found2, soorten_linked3)
  dup_gbif <- soorten_gbif$usageKey[which(duplicated(soorten_gbif$usageKey) &
                                            !is.na(soorten_gbif$usageKey))]
  dup_soorten <- soorten_gbif |>
    filter(usageKey %in% dup_gbif) |>
    arrange(usageKey)



  ### FUTON SOORTENLIJST

  qry <- "
SELECT  [TaxonGIVID]
      ,[TaxonName]
      ,[TaxonLanguageKey]
      ,[TaxonQuickCode]
  FROM [D0013_00_Futon].[dbo].[ftTaxon]
  where TaxonLanguageKey = 'Sci'
"

  conn <- connect_inbo_dbase("D0013_00_Futon")
  inbosoorten <- dbGetQuery(conn, qry)
  run <- FALSE
  if (run) gbifcodes <- rgbif::name_backbone_checklist(inbosoorten$TaxonName)
  gbifcodes <- bind_cols(inbosoorten, gbifcodes)
  write_excel_csv2(gbifcodes, file = "inst/soortenlijst/gbif_inbo.csv")

  to_complete <- gbifcodes |> filter(is.na(usageKey))
  for (i in seq_len(nrow(to_complete))) {
    print(i)
    res <- rgbif::name_suggest(to_complete$TaxonName[i])
    if (!nrow(res$data)) {
      to_complete$usageKey2[i] <- NA
      to_complete$canonicalName2[i] <- NA
    } else {
      to_complete$usageKey2[i] <- res$data$key[i]
      to_complete$canonicalName2[i] <- res$data$canonicalName[i]
    }
  }
}
