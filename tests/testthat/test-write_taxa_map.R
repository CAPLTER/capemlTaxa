taxonomy_row <- function(
  taxon_id,
  scientific_name,
  vernacular_name = NA_character_,
  taxon_rank = "species",
  update_date = as.Date("2020-01-01")
) {
  parts <- strsplit(scientific_name, " ", fixed = TRUE)[[1]]

  data.frame(
    vernacularName = vernacular_name,
    language = if (is.na(vernacular_name)) NA_character_ else "English",
    update_date = update_date,
    acceptedNameUsageID = taxon_id,
    taxonID = taxon_id,
    scientificName = scientific_name,
    taxonRank = taxon_rank,
    taxonomicStatus = "accepted",
    kingdom = "Animalia",
    phylum = "Chordata",
    class = "Aves",
    order = "Testiformes",
    family = "Testidae",
    genus = parts[[1]],
    specificEpithet = if (length(parts) > 1L) parts[[2]] else NA_character_,
    infraspecificEpithet = NA_character_,
    stringsAsFactors = FALSE
  )
}

accepted_fixture <- function() {
  rbind(
    taxonomy_row("ITIS:1", "Panthera leo", "lion"),
    taxonomy_row("ITIS:2", "Calypte anna", "Anna's Hummingbird"),
    taxonomy_row("ITIS:3", "Aythya americana", "Redhead"),
    taxonomy_row("ITIS:4", "Paragobiodon echinocephalus", "redhead"),
    taxonomy_row("ITIS:5", "Corvus corax", "Common Raven")
  )
}

raw_scientific_fixture <- function() {
  out <- taxonomy_row("ITIS:old", "Felis leo")
  out$acceptedNameUsageID <- "ITIS:1"
  out$taxonomicStatus <- "synonym"
  out
}

raw_common_fixture <- function(names) {
  fixtures <- list(
    "anna's hummingbird" = rbind(
      taxonomy_row("ITIS:2", "Calypte anna", "Anna's Hummingbird"),
      transform(
        taxonomy_row(
          "ITIS:old-anna", "Ornismya anna", "Anna's Hummingbird"
        ),
        acceptedNameUsageID = "ITIS:2",
        taxonomicStatus = "synonym"
      )
    ),
    "redhead" = rbind(
      taxonomy_row("ITIS:3", "Aythya americana", "Redhead"),
      taxonomy_row(
        "ITIS:4", "Paragobiodon echinocephalus", "redhead"
      )
    ),
    "common raven" = taxonomy_row(
      "ITIS:5", "Corvus corax", "Common Raven"
    ),
    "northern raven" = taxonomy_row(
      "ITIS:5", "Corvus corax", "Northern Raven"
    )
  )

  rows <- fixtures[tolower(names)]
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    return(capemlTaxa:::empty_raw_matches())
  }
  do.call(rbind, rows)
}

mock_taxadb <- function(query_names) {
  state <- new.env(parent = emptyenv())
  state$disconnected <- FALSE

  testthat::local_mocked_bindings(
    get_taxa_db = function() structure(list(), class = "mock_taxadb"),
    provision_taxa_db = function(db) invisible(db),
    disconnect_taxa_db = function(db) {
      state$disconnected <- TRUE
      invisible(NULL)
    },
    query_itis_names = query_names,
    query_accepted_taxa = function(ids, db) {
      accepted_fixture()[accepted_fixture()$taxonID %in% ids, , drop = FALSE]
    },
    .package = "capemlTaxa",
    .env = parent.frame()
  )

  state
}

new_output_dir <- function() {
  path <- tempfile("capemlTaxa-test-")
  dir.create(path)
  path
}

taxa_map_columns <- c(
  "input_name", "taxa_clean", "taxonID", "taxonRank", "acceptedNameUsageID",
  "taxonomicStatus", "update_date", "kingdom", "phylum", "class",
  "order", "family", "genus", "specificEpithet",
  "infraspecificEpithet", "vernacularName"
)

testthat::test_that("scientific matching remains the default and canonicalizes synonyms", {
  path <- new_output_dir()
  state <- mock_taxadb(function(names, match_by, db) raw_scientific_fixture())
  taxa <- data.frame(name = "Felis leo")

  result <- capemlTaxa::write_taxa_map(taxa, name, path = path)

  testthat::expect_identical(result$taxa_clean, "Panthera leo")
  testthat::expect_identical(result$input_name, "Felis leo")
  testthat::expect_identical(result$taxonID, "ITIS:1")
  testthat::expect_identical(names(result), taxa_map_columns)
  testthat::expect_true(file.exists(file.path(path, "taxa_map.csv")))
  testthat::expect_true(state$disconnected)
})

testthat::test_that("common matching preserves apostrophes and canonical scientific names", {
  path <- new_output_dir()
  state <- mock_taxadb(
    function(names, match_by, db) raw_common_fixture(names)
  )
  taxa <- data.frame(name = "  Anna's   Hummingbird ")

  result <- capemlTaxa::write_taxa_map(
    taxa,
    name,
    match_by = "common",
    path = path
  )

  testthat::expect_identical(result$taxa_clean, "Calypte anna")
  testthat::expect_identical(
    result$input_name,
    "  Anna's   Hummingbird "
  )
  testthat::expect_identical(result$vernacularName, "Anna's Hummingbird")
  testthat::expect_true(state$disconnected)
})

testthat::test_that("common queries are case-insensitive and retain only English", {
  query_result <- rbind(
    taxonomy_row("ITIS:2", "Calypte anna", "Anna's Hummingbird"),
    transform(
      taxonomy_row(
        "ITIS:6", "Testus example", "Anna's Hummingbird"
      ),
      language = "Spanish"
    )
  )
  test_db <- suppressMessages(DBI::dbConnect(duckdb::duckdb()))
  DBI::dbWriteTable(test_db, "common_names", query_result)

  testthat::local_mocked_bindings(
    taxa_tbl = function(
      provider,
      schema,
      db
    ) {
      testthat::expect_identical(provider, "itis")
      testthat::expect_identical(schema, "common")
      dplyr::tbl(test_db, "common_names")
    },
    .package = "taxadb"
  )

  result <- capemlTaxa:::query_itis_names(
    names = "anna's hummingbird",
    match_by = "common",
    db = structure(list(), class = "mock_taxadb")
  )
  DBI::dbDisconnect(test_db, shutdown = TRUE)

  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_identical(result$language, "English")
  testthat::expect_identical(result$scientificName, "Calypte anna")
})

testthat::test_that("disconnect closes the connection and clears its cache", {
  db <- suppressMessages(taxadb::td_connect())
  testthat::expect_true(DBI::dbIsValid(db))

  capemlTaxa:::disconnect_taxa_db(db)
  testthat::expect_false(DBI::dbIsValid(db))

  replacement <- suppressMessages(taxadb::td_connect())
  testthat::expect_true(DBI::dbIsValid(replacement))
  capemlTaxa:::disconnect_taxa_db(replacement)
})

testthat::test_that("unresolved ambiguity writes candidates and a partial map", {
  path <- new_output_dir()
  mock_taxadb(function(names, match_by, db) raw_common_fixture(names))
  taxa <- data.frame(name = c("Redhead", "Unknown bird"))

  suppressWarnings(
    result <- capemlTaxa::write_taxa_map(
      taxa,
      name,
      match_by = "common",
      path = path
    )
  )

  duplicates <- readr::read_csv(
    file.path(path, "taxa_duplicates.csv"),
    show_col_types = FALSE
  )
  resolutions <- readr::read_csv(
    file.path(path, "taxa_resolutions.csv"),
    show_col_types = FALSE
  )

  testthat::expect_setequal(
    duplicates$candidate_taxonID,
    c("ITIS:3", "ITIS:4")
  )
  testthat::expect_true(all(!duplicates$selected))
  testthat::expect_identical(
    names(duplicates),
    c(
      "input_name", "vernacularName", "language", "update_date",
      "acceptedNameUsageID", "taxonID", "scientificName", "taxonRank",
      "taxonomicStatus", "kingdom", "phylum", "class", "order", "family",
      "genus", "specificEpithet", "infraspecificEpithet",
      "candidate_taxonID", "selected"
    )
  )
  testthat::expect_identical(
    names(resolutions),
    c("input_name", "selected_taxonID", "comment")
  )
  testthat::expect_identical(result$input_name, "Unknown bird")
  testthat::expect_true(is.na(result$taxa_clean))
  testthat::expect_true(is.na(result$vernacularName))
  testthat::expect_true(is.na(result$taxonID))
  testthat::expect_identical(
    attr(result, "resolution_status")$ambiguous,
    "Redhead"
  )
  testthat::expect_identical(
    attr(result, "resolution_status")$unmatched,
    "Unknown bird"
  )
})

testthat::test_that("unmatched scientific names retain only literal input", {
  path <- new_output_dir()
  mock_taxadb(
    function(names, match_by, db) capemlTaxa:::empty_raw_matches()
  )
  taxa <- data.frame(name = "Unknown species")

  testthat::expect_warning(
    result <- capemlTaxa::write_taxa_map(
      taxa,
      name,
      match_by = "scientific",
      path = path
    ),
    "Unmatched names"
  )

  testthat::expect_identical(result$input_name, "Unknown species")
  testthat::expect_true(is.na(result$taxa_clean))
  testthat::expect_true(is.na(result$vernacularName))
  testthat::expect_true(is.na(result$taxonID))
})

testthat::test_that("a subsequent call incorporates a valid resolution", {
  path <- new_output_dir()
  mock_taxadb(function(names, match_by, db) raw_common_fixture(names))
  taxa <- data.frame(name = "Redhead")

  testthat::expect_warning(
    capemlTaxa::write_taxa_map(
      taxa, name, match_by = "common", path = path
    ),
    "Ambiguous"
  )
  readr::write_csv(
    data.frame(
      input_name = "Redhead",
      selected_taxonID = "ITIS:3",
      comment = "bird"
    ),
    file.path(path, "taxa_resolutions.csv")
  )

  testthat::expect_message(
    result <- capemlTaxa::write_taxa_map(
      taxa,
      name,
      match_by = "common",
      path = path
    ),
    "Redhead -> ITIS:3"
  )
  duplicates <- readr::read_csv(
    file.path(path, "taxa_duplicates.csv"),
    show_col_types = FALSE
  )

  testthat::expect_identical(result$taxa_clean, "Aythya americana")
  testthat::expect_identical(result$input_name, "Redhead")
  testthat::expect_identical(result$taxonID, "ITIS:3")
  testthat::expect_identical(
    duplicates$candidate_taxonID[duplicates$selected],
    "ITIS:3"
  )
  testthat::expect_identical(
    attr(result, "resolution_status")$resolved,
    "Redhead"
  )
})

testthat::test_that("invalid resolutions error and still close the connection", {
  path <- new_output_dir()
  state <- mock_taxadb(function(names, match_by, db) raw_common_fixture(names))
  taxa <- data.frame(name = "Redhead")
  readr::write_csv(
    data.frame(
      input_name = "Redhead",
      selected_taxonID = "ITIS:999",
      comment = NA_character_
    ),
    file.path(path, "taxa_resolutions.csv")
  )

  testthat::expect_error(
    capemlTaxa::write_taxa_map(
      taxa, name, match_by = "common", path = path
    ),
    "Invalid selected_taxonID"
  )
  testthat::expect_true(state$disconnected)
})

testthat::test_that("database provisioning errors still close the connection", {
  path <- new_output_dir()
  state <- new.env(parent = emptyenv())
  state$disconnected <- FALSE

  testthat::local_mocked_bindings(
    get_taxa_db = function() structure(list(), class = "mock_taxadb"),
    provision_taxa_db = function(db) stop("provision failed"),
    disconnect_taxa_db = function(db) {
      state$disconnected <- TRUE
      invisible(NULL)
    },
    .package = "capemlTaxa"
  )

  testthat::expect_error(
    capemlTaxa::write_taxa_map(
      data.frame(name = "Redhead"),
      name,
      match_by = "common",
      path = path
    ),
    "provision failed"
  )
  testthat::expect_true(state$disconnected)
})

testthat::test_that("different aliases for one taxon warn and produce one row", {
  path <- new_output_dir()
  mock_taxadb(function(names, match_by, db) raw_common_fixture(names))
  taxa <- data.frame(name = c("Common Raven", "Northern Raven"))

  testthat::expect_warning(
    result <- capemlTaxa::write_taxa_map(
      taxa, name, match_by = "common", path = path
    ),
    "Multiple input names resolved to the same taxon"
  )

  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_identical(result$vernacularName, "Common Raven")
})

testthat::test_that("inputs and resolution files are validated", {
  path <- new_output_dir()
  mock_taxadb(function(names, match_by, db) raw_common_fixture(names))

  testthat::expect_error(
    capemlTaxa::write_taxa_map(
      data.frame(name = "Redhead"),
      name,
      match_by = "vernacular",
      path = path
    ),
    "arg"
  )

  readr::write_csv(
    data.frame(input_name = "Redhead", selected_taxonID = "ITIS:3"),
    file.path(path, "taxa_resolutions.csv")
  )
  testthat::expect_error(
    capemlTaxa::write_taxa_map(
      data.frame(name = "Redhead"),
      name,
      match_by = "common",
      path = path
    ),
    "must contain"
  )

  readr::write_csv(
    data.frame(
      input_name = c("Redhead", "Redhead"),
      selected_taxonID = c("ITIS:3", "ITIS:4"),
      comment = c("bird", "fish")
    ),
    file.path(path, "taxa_resolutions.csv")
  )
  testthat::expect_error(
    capemlTaxa::write_taxa_map(
      data.frame(name = "Redhead"),
      name,
      match_by = "common",
      path = path
    ),
    "duplicate input_name"
  )
})
