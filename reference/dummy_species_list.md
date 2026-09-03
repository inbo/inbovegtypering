# Dummy Species List

A reference list linking the internal species numbers used in the dummy
recordings and synoptics to synthetic scientific names and GBIF-style
keys.

## Usage

``` r
data(dummy_species_list)
```

## Format

A data frame with 20 rows and 5 variables:

- speciesNumber:

  Integer. Internal numeric identifier for the species (1-20).

- speciesName:

  Character. Vernacular name (e.g., "Species A").

- scientificName:

  Character. Scientific name (e.g., "Genus A species").

- usageKey:

  Integer. Synthetic GBIF usage key (1001-1020).

- acceptedusageKey:

  Integer. Synthetic GBIF accepted usage key (1001-1020).
