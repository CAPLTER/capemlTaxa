# capemlTaxa

Optional taxonomic mapping tools for
[capeml](https://github.com/CAPLTER/capeml). capemlTaxa matches scientific
names or English common names against ITIS through a local `taxadb` database
and writes a standardized `taxa_map.csv` for
`capeml::create_taxonomicCoverage()`.

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

The first lookup may take longer while `taxadb` downloads and caches the ITIS
Darwin Core and common-name data.

## Match scientific names

Each call accepts one name type. Set `match_by` to `"scientific"` or
`"common"`; it defaults to `"scientific"` for compatibility with existing
workflows.

```r
taxa <- data.frame(
  scientific_name = c("Homo sapiens", "Panthera leo")
)

capemlTaxa::write_taxa_map(
  taxa_df = taxa,
  taxa_col = scientific_name,
  match_by = "scientific"
)
```

Scientific synonyms are converted to the accepted ITIS scientific name and
identifier in `taxa_map.csv`.

## Match English common names

Common-name matching is exact and case-insensitive. Only English ITIS records
are considered. Punctuation is preserved, so possessive names can be supplied
normally.

```r
birds <- data.frame(
  common_name = c("Anna's Hummingbird", "Redhead", "Canyon Wren")
)

result <- capemlTaxa::write_taxa_map(
  taxa_df = birds,
  taxa_col = common_name,
  match_by = "common"
)
```

The literal submitted value is retained in `input_name`. For successfully
matched rows, `taxa_clean` is the accepted ITIS scientific name, not the
submitted common name. `vernacularName` is the value stored in the matching
English ITIS common-name record. ITIS capitalization, spelling, or punctuation
can therefore differ from the submitted value. This preserves both an audit
trail to the source data and the scientific-name contract expected by
`capeml`.

For example:

```text
input_name          taxa_clean          vernacularName
Redhead             Aythya americana    Redhead
anna's hummingbird  Calypte anna        Anna's Hummingbird
```

## Resolve ambiguous common names

A common name can identify more than one ITIS taxon. capemlTaxa never chooses
one automatically. It writes a partial `taxa_map.csv`, warns about unresolved
names, and creates two companion files:

- `taxa_duplicates.csv` lists every candidate accepted taxon.
- `taxa_resolutions.csv` is the user-editable selection table.

For example, `taxa_duplicates.csv` may contain:

|input_name |vernacularName |language |update_date |acceptedNameUsageID |taxonID     |scientificName              |taxonRank |taxonomicStatus |kingdom  |phylum   |class     |order        |family   |genus        |specificEpithet |infraspecificEpithet |candidate_taxonID |selected |
|:----------|:--------------|:--------|:-----------|:-------------------|:-----------|:---------------------------|:---------|:---------------|:--------|:--------|:---------|:------------|:--------|:------------|:---------------|:--------------------|:-----------------|:--------|
|Redhead    |redhead        |English  |2004-08-31  |ITIS:172000         |ITIS:172000 |Paragobiodon echinocephalus |species   |accepted        |Animalia |Chordata |Teleostei |Perciformes  |Gobiidae |Paragobiodon |echinocephalus  |NA                   |ITIS:172000       |FALSE    |
|Redhead    |Redhead        |English  |2005-04-25  |ITIS:175125         |ITIS:175125 |Aythya americana            |species   |accepted        |Animalia |Chordata |Aves      |Anseriformes |Anatidae |Aythya       |americana       |NA                   |ITIS:175125       |FALSE    |

Open `taxa_resolutions.csv` and copy the appropriate `candidate_taxonID` into
`selected_taxonID`:

|input_name  |selected_taxonID |comment |
|:-----------|:----------------|:-------|
|Redhead     |ITIS:175125      |bird    |
|Canyon Wren |ITIS:178610      |NA      |

Then rerun the same call:

```r
result <- capemlTaxa::write_taxa_map(
  taxa_df = birds,
  taxa_col = common_name,
  match_by = "common"
)
```

The selected candidate is incorporated into `taxa_map.csv`, and the matching
row in `taxa_duplicates.csv` has `selected = TRUE`. Existing valid selections
and comments are retained on later runs. Invalid selected identifiers cause an
error rather than being silently ignored.

The second call reports the selections it applied, for example:

```text
Applied saved taxonomic resolution(s) to taxa_map.csv: Redhead -> ITIS:175125, Canyon Wren -> ITIS:178610, Green-winged Teal -> ITIS:175081
```

Assign the return value to inspect the same status programmatically:

```r
result <- capemlTaxa::write_taxa_map(
  taxa_df = birds,
  taxa_col = common_name,
  match_by = "common"
)

attr(result, "resolution_status")$resolved
```

Unmatched names remain in `taxa_map.csv` through their literal `input_name`.
Because ITIS supplied no taxon or common-name record, `taxa_clean`,
`vernacularName`, and every other ITIS-derived field are missing.
Unresolved ambiguous names are omitted until selected. If different input
common names resolve to the same accepted taxon, the function warns, keeps the
first input name, and writes only one row for that taxon.

The returned data frame has a `resolution_status` attribute that records
unmatched, unresolved ambiguous, and resolved ambiguous input names:

```r
attr(result, "resolution_status")
```

## Build EML taxonomic coverage

After generating and reviewing `taxa_map.csv`, hand it to `capeml`:

```r
taxaCoverage <- capeml::create_taxonomicCoverage()
coverage$taxonomicCoverage <- taxaCoverage
```

This step does not require capemlTaxa once the map exists.

## Output schema

Each successfully matched row in `taxa_map.csv` is a unique accepted taxon.
The column order is stable:

- input_name
- taxa_clean
- taxonID
- taxonRank
- acceptedNameUsageID
- taxonomicStatus
- update_date
- kingdom
- phylum
- class
- order
- family
- genus
- specificEpithet
- infraspecificEpithet
- vernacularName


The principal name fields have the following provenance:

| Field and condition | Source |
|---|---|
| `input_name` in all output files | Original supplied value |
| `taxa_clean`, matched or resolved | Accepted ITIS `scientificName` |
| `vernacularName`, matched common name | Matching English ITIS common-name record |
| `vernacularName`, matched scientific name | Accepted ITIS taxon record, when available |
| `taxa_clean`, unmatched name | Missing; ITIS supplied no accepted scientific name |
| `vernacularName`, unmatched name | Missing; ITIS supplied no common-name record |

Thus, an input such as `"anna's hummingbird"` may produce
`input_name = "anna's hummingbird"`, `taxa_clean = "Calypte anna"`, and
`vernacularName = "Anna's Hummingbird"` when those are the values supplied by
the accepted and common-name ITIS records.

Use the optional `path` argument to read and write all three workflow files in
an existing directory other than the current working directory.

## DuckDB connection messages

`write_taxa_map()` closes its database connection with
`taxadb::td_disconnect()` on success and error. That function also clears
`taxadb`'s connection cache.

DuckDB may still report that downloaded extensions are being kept in an R
session temporary directory. This is an informational cache-location message,
not an indication that the database connection was left open. To retain those
extensions between R sessions, create a persistent directory and set the
DuckDB option before calling capemlTaxa:

```r
duckdb_extensions <- file.path(
  tools::R_user_dir("capemlTaxa", "cache"),
  "duckdb_extensions"
)
dir.create(duckdb_extensions, recursive = TRUE, showWarnings = FALSE)
options(duckdb.extension_directory = duckdb_extensions)
```
