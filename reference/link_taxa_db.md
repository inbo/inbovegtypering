# Link species records to GBIF taxonomy via INBO taxonomy database

This function links species records to their corresponding GBIF taxonomy
information by matching species names against the INBO taxonomy database
(FUTON source). It performs a partial string match to accommodate
variations in species name notation.

## Usage

``` r
link_taxa_db(con, species_names)
```

## Arguments

- con:

  A database connection object to the INBO taxonomy database

- species_names:

  A character vector containing species records

## Value

A dataframe containing the matched species with the futondb

## Note

- Species names are matched using a 'LIKE' query with wildcard at the
  end

- Only matches from the FUTON source are considered

- If no match is found, the gbif_usageKey will be NA

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'con' is your database connection
# and 'species_data' is your dataframe with a column 'species_name'
result <- link_to_futondb(con, species_data, "species_name")
} # }

if (FALSE) { # \dontrun{
library(inbovegtypering)
con <- connect_db_taxonomy()
species_names <- c(
  "Dryopteris", "Nardus stricta L.",
  "Lophocolea", "Fraxinus excelsior L."
)
link_futon_db(con, species_names = species_names)
} # }
```
