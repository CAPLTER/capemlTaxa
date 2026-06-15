# capemlTaxa

Optional taxonomic mapping tools for
[capeml](https://github.com/CAPLTER/capeml). Provides taxadb-based
workflows for generating a standardized taxa map (`taxa_map.csv`) that
can be handed off to `capeml::create_taxonomicCoverage()` for EML
assembly. Install this package only when you need to generate or refresh
your taxa map via ITIS.

## When do you need capemlTaxa?

| Scenario | Package(s) needed |
|---|---|
| `taxa_map.csv` already exists and taxa have not changed | `capeml` only |
| Building EML from a curated taxonomy table | `capeml` only |
| Querying ITIS to map raw taxon names for the first time | `capeml` + `capemlTaxa` |
| Taxa have changed and `taxa_map.csv` must be refreshed | `capeml` + `capemlTaxa` |

## Installation

```r
pak::pak("caplter/capemlTaxa")
```

## Workflow

**Step 1 — generate the taxa map** (capemlTaxa)

```r
taxa <- data.frame(scientific_name = c("Homo sapiens", "Panthera leo"))

capemlTaxa::write_taxa_map(
  taxa_df  = taxa,
  taxa_col = scientific_name
)
# writes taxa_map.csv to the working directory
```

**Step 2 — assemble EML taxonomic coverage** (capeml)

```r
# Once taxa_map.csv exists, this step does not require capemlTaxa
taxaCoverage <- capeml::create_taxonomicCoverage()
coverage$taxonomicCoverage <- taxaCoverage
```
