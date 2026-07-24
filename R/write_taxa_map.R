#' Write an ITIS taxa map
#'
#' @description
#' Matches a column of scientific or English common names to ITIS and writes a
#' standardized `taxa_map.csv` for use by `capeml`. Ambiguous matches are never
#' selected automatically. Instead, candidate taxa are written to
#' `taxa_duplicates.csv` and user selections are read from
#' `taxa_resolutions.csv` on a subsequent call.
#'
#' @param taxa_df A data frame containing organism names.
#' @param taxa_col An unquoted column in `taxa_df` containing either scientific
#'   names or common names. A single call cannot mix the two name types.
#' @param match_by Character string indicating how `taxa_col` should be
#'   matched. Must be either `"scientific"` or `"common"`. The default is
#'   `"scientific"` for compatibility with earlier versions of capemlTaxa.
#' @param path Directory in which `taxa_map.csv`, `taxa_duplicates.csv`, and
#'   `taxa_resolutions.csv` are read or written. Defaults to the current working
#'   directory.
#'
#' @return Invisibly returns the taxa map as a data frame. The returned object
#'   has a `resolution_status` attribute containing character vectors named
#'   `unmatched`, `ambiguous`, and `resolved`. The same taxa map, without the
#'   attribute, is written to `taxa_map.csv`.
#'
#' @details
#' Scientific names are standardized with [taxadb::clean_names()]. Common
#' names are matched exactly but without regard to case, and only records whose
#' language is English are considered. Whitespace around common names is
#' normalized, but punctuation is preserved; names such as
#' `"Anna's Hummingbird"` are therefore passed safely and unchanged to
#' `taxadb`.
#'
#' ITIS synonym records are resolved to their accepted taxon. For every matched
#' or user-resolved row, `taxa_clean` comes from the `scientificName` field of
#' the accepted ITIS record. This is true whether the supplied value was a
#' scientific name, a scientific synonym, or a common name. `taxonID` and the
#' remaining taxonomy fields also come from that accepted ITIS record.
#'
#' The literal supplied value is retained in `input_name`, the first column of
#' `taxa_map.csv`. For matched common-name rows, `vernacularName` comes from the
#' matching English ITIS common-name record, so its spelling, capitalization, and
#' punctuation may differ from the supplied value. For matched scientific-name
#' rows, `vernacularName`, when available, comes from the accepted ITIS taxon
#' record. `input_name` is also used in `taxa_duplicates.csv` and
#' `taxa_resolutions.csv`, providing a consistent link among all three files.
#'
#' Unmatched inputs remain in `taxa_map.csv` with their literal `input_name`,
#' while `taxa_clean`, `vernacularName`, and all other ITIS-derived fields are
#' missing. Unresolved ambiguous inputs are excluded from the map until a valid
#' selection is supplied.
#'
#' A candidate is unique by `acceptedNameUsageID`, not by the number of raw
#' ITIS rows. When an input has more than one candidate, all candidates are
#' written to `taxa_duplicates.csv`. The companion `taxa_resolutions.csv`
#' contains one row per ambiguous input. Set `selected_taxonID` to one of that
#' input's `candidate_taxonID` values and call `write_taxa_map()` again.
#' Existing comments and valid selections are preserved. When saved selections
#' are applied, the function reports each `input_name -> selected_taxonID`
#' mapping and includes the selected taxon in `taxa_map.csv`. The selected row
#' is also marked `TRUE` in `taxa_duplicates.csv`.
#'
#' If different input common names resolve to the same accepted taxon, the
#' function warns, retains the first name in input order, and writes one map row
#' for that taxon.
#'
#' The function opens one `taxadb` connection and registers
#' [taxadb::td_disconnect()] immediately so the connection is closed on both
#' success and error. Disconnecting also clears `taxadb`'s connection cache.
#'
#' @examples
#' \dontrun{
#' scientific_taxa <- data.frame(
#'   name = c("Homo sapiens", "Panthera leo")
#' )
#' write_taxa_map(scientific_taxa, name, match_by = "scientific")
#'
#' common_taxa <- data.frame(
#'   name = c("Anna's Hummingbird", "Redhead")
#' )
#' write_taxa_map(common_taxa, name, match_by = "common")
#' # If Redhead is ambiguous, edit taxa_resolutions.csv and call again.
#' }
#'
#' @importFrom dplyr add_count all_of arrange collect distinct filter left_join
#' @importFrom dplyr row_number select transmute
#' @importFrom purrr map_chr map2_lgl
#' @importFrom readr cols col_character read_csv write_csv
#' @importFrom rlang .data
#' @importFrom taxadb clean_names filter_id filter_name taxa_tbl td_connect
#' @importFrom taxadb td_create td_disconnect
#' @export
write_taxa_map <- function(
  taxa_df,
  taxa_col,
  match_by = "scientific",
  path = "."
) {
  match_by <- base::match.arg(match_by, c("scientific", "common"))
  validate_write_taxa_map_inputs(taxa_df = taxa_df, path = path)

  input_names <- taxa_df |>
    dplyr::transmute(input_name = base::as.character({{ taxa_col }})) |>
    dplyr::mutate(
      lookup_name = normalize_taxon_names(.data$input_name, match_by),
      input_index = dplyr::row_number()
    ) |>
    dplyr::filter(!base::is.na(.data$lookup_name), .data$lookup_name != "") |>
    dplyr::distinct(.data$lookup_name, .keep_all = TRUE)

  db <- get_taxa_db()
  base::on.exit(disconnect_taxa_db(db), add = TRUE)
  provision_taxa_db(db)

  raw_matches <- query_itis_names(
    names = input_names$lookup_name,
    match_by = match_by,
    db = db
  )

  candidates <- build_candidates(
    input_names = input_names,
    raw_matches = raw_matches,
    match_by = match_by,
    db = db
  )

  resolved <- resolve_candidates(candidates = candidates, path = path)
  taxa_map <- build_taxa_map(
    input_names = input_names,
    candidates = resolved$candidates,
    ambiguous_names = resolved$ambiguous_names
  )

  status <- list(
    unmatched = input_names$input_name[
      !input_names$lookup_name %in% candidates$lookup_name
    ],
    ambiguous = resolved$unresolved_names,
    resolved = resolved$resolved_names
  )

  if (base::length(status$unmatched) > 0L) {
    base::warning(
      "Unmatched names retained in taxa_map.csv with missing taxonomy: ",
      base::paste(status$unmatched, collapse = ", "),
      call. = FALSE
    )
  }

  if (base::length(status$ambiguous) > 0L) {
    base::warning(
      "Ambiguous names excluded from taxa_map.csv pending resolution: ",
      base::paste(status$ambiguous, collapse = ", "),
      call. = FALSE
    )
  }

  readr::write_csv(
    x = taxa_map,
    file = base::file.path(path, "taxa_map.csv")
  )

  applied_resolutions <- resolved$candidates |>
    dplyr::filter(
      .data$selected,
      .data$lookup_name %in% resolved$ambiguous_names
    )

  if (base::nrow(applied_resolutions) > 0L) {
    resolution_summary <- base::paste0(
      applied_resolutions$input_name,
      " -> ",
      applied_resolutions$candidate_taxonID
    )
    base::message(
      "Applied saved taxonomic resolution(s) to taxa_map.csv: ",
      base::paste(resolution_summary, collapse = ", ")
    )
  }

  base::attr(taxa_map, "resolution_status") <- status
  base::invisible(taxa_map)
}

#' Validate taxa map inputs
#'
#' @param taxa_df A prospective input data frame.
#' @param path Prospective output directory.
#'
#' @return `NULL`, invisibly. Throws an error for invalid inputs.
#' @keywords internal
validate_write_taxa_map_inputs <- function(taxa_df, path) {
  if (!base::is.data.frame(taxa_df)) {
    base::stop("`taxa_df` must be a data frame.", call. = FALSE)
  }

  if (!base::is.character(path) || base::length(path) != 1L ||
      base::is.na(path) || !base::dir.exists(path)) {
    base::stop("`path` must identify an existing directory.", call. = FALSE)
  }

  base::invisible(NULL)
}

#' Normalize names for ITIS lookup
#'
#' @param names Character vector of organism names.
#' @param match_by Either `"scientific"` or `"common"`.
#'
#' @return A normalized character vector of the same length as `names`.
#' @keywords internal
normalize_taxon_names <- function(names, match_by) {
  if (base::identical(match_by, "scientific")) {
    return(
      purrr::map_chr(
        .x = names,
        .f = function(name) {
          if (base::is.na(name)) {
            return(NA_character_)
          }

          taxadb::clean_names(name, lowercase = FALSE)
        }
      )
    )
  }

  base::gsub(
    pattern = "[[:space:]]+",
    replacement = " ",
    x = base::trimws(names)
  )
}

#' Connect to the local taxadb database
#'
#' @return A DBI-compatible taxadb connection.
#' @keywords internal
get_taxa_db <- function() {
  taxadb::td_connect()
}

#' Provision the ITIS database views
#'
#' @param db An open taxadb connection.
#'
#' @return `db`, invisibly, after creating the ITIS Darwin Core and common-name
#'   views.
#' @keywords internal
provision_taxa_db <- function(db) {

  taxadb::td_create(
    provider = "itis",
    schema = c("dwc", "common"),
    db = db
  )

  base::invisible(db)
}

#' Disconnect a taxadb connection
#'
#' @param db A connection returned by [get_taxa_db()].
#'
#' @return `NULL`, invisibly.
#' @keywords internal
disconnect_taxa_db <- function(db) {
  taxadb::td_disconnect(db)
  base::invisible(NULL)
}

#' Query ITIS names
#'
#' @param names Normalized names to query.
#' @param match_by Either `"scientific"` or `"common"`.
#' @param db An open taxadb connection.
#'
#' @return A data frame of raw ITIS matches.
#' @keywords internal
query_itis_names <- function(names, match_by, db) {
  if (base::length(names) == 0L) {
    return(empty_raw_matches())
  }

  if (base::identical(match_by, "common")) {
    lookup_names <- base::tolower(names)

    return(
      taxadb::taxa_tbl(
        provider = "itis",
        schema = "common",
        db = db
      ) |>
        dplyr::filter(
          base::tolower(.data$vernacularName) %in% lookup_names,
          base::tolower(.data$language) == "english"
        ) |>
        dplyr::collect() |>
        base::as.data.frame()
    )
  }

  taxadb::filter_name(
    name = names,
    provider = "itis",
    ignore_case = FALSE,
    db = db
  ) |>
    base::as.data.frame()
}

#' Query accepted ITIS taxa
#'
#' @param ids Accepted ITIS identifiers.
#' @param db An open taxadb connection.
#'
#' @return A data frame containing canonical accepted records.
#' @keywords internal
query_accepted_taxa <- function(ids, db) {
  if (base::length(ids) == 0L) {
    return(empty_taxonomy())
  }

  taxadb::filter_id(
    id = ids,
    provider = "itis",
    db = db
  ) |>
    dplyr::filter(
      .data$taxonID == .data$acceptedNameUsageID,
      .data$taxonomicStatus == "accepted"
    ) |>
    dplyr::distinct(.data$taxonID, .keep_all = TRUE) |>
    base::as.data.frame()
}

#' Build canonical candidate taxa
#'
#' @param input_names Normalized input-name table.
#' @param raw_matches Raw rows returned by [query_itis_names()].
#' @param match_by Either `"scientific"` or `"common"`.
#' @param db An open taxadb connection.
#'
#' @return One candidate row per input name and accepted ITIS identifier.
#' @keywords internal
build_candidates <- function(input_names, raw_matches, match_by, db) {
  if (base::nrow(raw_matches) == 0L) {
    return(empty_candidates())
  }

  raw_matches <- ensure_taxonomy_columns(raw_matches)
  raw_matches <- raw_matches |>
    dplyr::mutate(
      match_key = if (base::identical(match_by, "common")) {
        base::tolower(.data$vernacularName)
      } else {
        .data$scientificName
      }
    )

  keyed_inputs <- input_names |>
    dplyr::mutate(
      match_key = if (base::identical(match_by, "common")) {
        base::tolower(.data$lookup_name)
      } else {
        .data$lookup_name
      }
    )

  matched_inputs <- keyed_inputs |>
    dplyr::left_join(raw_matches, by = "match_key") |>
    dplyr::filter(!base::is.na(.data$acceptedNameUsageID))

  accepted <- query_accepted_taxa(
    ids = base::unique(matched_inputs$acceptedNameUsageID),
    db = db
  ) |>
    ensure_taxonomy_columns()

  canonical <- accepted |>
    dplyr::transmute(
      candidate_taxonID = .data$taxonID,
      taxonID = .data$taxonID,
      acceptedNameUsageID = .data$acceptedNameUsageID,
      scientificName = .data$scientificName,
      taxonRank = .data$taxonRank,
      taxonomicStatus = .data$taxonomicStatus,
      update_date = .data$update_date,
      kingdom = .data$kingdom,
      phylum = .data$phylum,
      class = .data$class,
      order = .data$order,
      family = .data$family,
      genus = .data$genus,
      specificEpithet = .data$specificEpithet,
      infraspecificEpithet = .data$infraspecificEpithet,
      canonical_vernacular = .data$vernacularName
    )

  matched_inputs |>
    dplyr::transmute(
      input_name = .data$input_name,
      lookup_name = .data$lookup_name,
      input_index = .data$input_index,
      vernacularName = if (base::identical(match_by, "common")) {
        .data$vernacularName
      } else {
        NA_character_
      },
      language = if (base::identical(match_by, "common")) {
        .data$language
      } else {
        NA_character_
      },
      candidate_taxonID = .data$acceptedNameUsageID
    ) |>
    dplyr::left_join(canonical, by = "candidate_taxonID") |>
    dplyr::mutate(
      vernacularName = if (base::identical(match_by, "scientific")) {
        .data$canonical_vernacular
      } else {
        .data$vernacularName
      }
    ) |>
    dplyr::arrange(.data$input_index, .data$candidate_taxonID) |>
    dplyr::distinct(
      .data$lookup_name,
      .data$candidate_taxonID,
      .keep_all = TRUE
    ) |>
    dplyr::select(-dplyr::all_of("canonical_vernacular")) |>
    base::as.data.frame()
}

#' Resolve ambiguous candidate taxa
#'
#' @param candidates Canonical candidate table from [build_candidates()].
#' @param path Directory containing resolution files.
#'
#' @return A list containing marked candidates and resolution-status vectors.
#' @keywords internal
resolve_candidates <- function(candidates, path) {
  if (base::nrow(candidates) == 0L) {
    return(list(
      candidates = candidates,
      ambiguous_names = character(),
      unresolved_names = character(),
      resolved_names = character()
    ))
  }

  counted <- candidates |>
    dplyr::add_count(.data$lookup_name, name = "candidate_count")
  ambiguous <- counted |>
    dplyr::filter(.data$candidate_count > 1L)
  ambiguous_names <- base::unique(ambiguous$lookup_name)

  if (base::length(ambiguous_names) == 0L) {
    counted$selected <- TRUE
    return(list(
      candidates = counted,
      ambiguous_names = character(),
      unresolved_names = character(),
      resolved_names = character()
    ))
  }

  resolutions <- prepare_resolutions(
    ambiguous = ambiguous,
    path = path
  )

  selected_lookup <- resolutions |>
    dplyr::filter(
      !base::is.na(.data$selected_taxonID),
      .data$selected_taxonID != ""
    )

  validate_resolutions(
    resolutions = selected_lookup,
    ambiguous = ambiguous
  )

  marked <- counted |>
    dplyr::left_join(
      selected_lookup |>
        dplyr::select(dplyr::all_of(c("input_name", "selected_taxonID"))),
      by = "input_name"
    ) |>
    dplyr::mutate(
      selected = (.data$candidate_count == 1L) |
        (!base::is.na(.data$selected_taxonID) &
           .data$candidate_taxonID == .data$selected_taxonID)
    )

  write_duplicate_candidates(
    candidates = marked |>
      dplyr::filter(.data$candidate_count > 1L),
    path = path
  )

  resolved_names <- selected_lookup$input_name
  unresolved_names <- resolutions$input_name[
    base::is.na(resolutions$selected_taxonID) |
      resolutions$selected_taxonID == ""
  ]

  list(
    candidates = marked,
    ambiguous_names = ambiguous_names,
    unresolved_names = unresolved_names,
    resolved_names = resolved_names
  )
}

#' Prepare the persistent resolution table
#'
#' @param ambiguous Ambiguous candidate rows.
#' @param path Directory containing `taxa_resolutions.csv`.
#'
#' @return A three-column resolution data frame.
#' @keywords internal
prepare_resolutions <- function(ambiguous, path) {
  resolution_path <- base::file.path(path, "taxa_resolutions.csv")
  current <- ambiguous |>
    dplyr::arrange(.data$input_index) |>
    dplyr::distinct(.data$input_name) |>
    dplyr::transmute(
      input_name = .data$input_name,
      selected_taxonID = NA_character_,
      comment = NA_character_
    )

  if (base::file.exists(resolution_path)) {
    existing <- readr::read_csv(
      file = resolution_path,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE
    ) |>
      base::as.data.frame()

    required <- c("input_name", "selected_taxonID", "comment")
    if (!base::all(required %in% base::names(existing))) {
      base::stop(
        "taxa_resolutions.csv must contain: ",
        base::paste(required, collapse = ", "),
        call. = FALSE
      )
    }

    if (base::anyDuplicated(existing$input_name)) {
      base::stop(
        "taxa_resolutions.csv contains duplicate input_name values.",
        call. = FALSE
      )
    }

    current <- current |>
      dplyr::select(dplyr::all_of("input_name")) |>
      dplyr::left_join(
        existing |>
          dplyr::select(dplyr::all_of(required)),
        by = "input_name"
      )
  }

  readr::write_csv(current, resolution_path, na = "NA")
  current
}

#' Validate user-selected candidate identifiers
#'
#' @param resolutions Non-empty user resolutions.
#' @param ambiguous Ambiguous candidate rows.
#'
#' @return `NULL`, invisibly. Throws an error for an invalid selection.
#' @keywords internal
validate_resolutions <- function(resolutions, ambiguous) {
  if (base::nrow(resolutions) == 0L) {
    return(base::invisible(NULL))
  }

  valid <- purrr::map2_lgl(
    .x = resolutions$input_name,
    .y = resolutions$selected_taxonID,
    .f = function(input_name, selected_taxonID) {
      selected_taxonID %in% ambiguous$candidate_taxonID[
        ambiguous$input_name == input_name
      ]
    }
  )

  if (!base::all(valid)) {
    invalid <- resolutions$input_name[!valid]
    base::stop(
      "Invalid selected_taxonID for: ",
      base::paste(invalid, collapse = ", "),
      ". Choose a candidate_taxonID from taxa_duplicates.csv.",
      call. = FALSE
    )
  }

  base::invisible(NULL)
}

#' Write ambiguous candidate taxa
#'
#' @param candidates Marked ambiguous candidate rows.
#' @param path Output directory.
#'
#' @return `NULL`, invisibly.
#' @keywords internal
write_duplicate_candidates <- function(candidates, path) {
  output <- candidates |>
    dplyr::transmute(
      input_name = .data$input_name,
      vernacularName = .data$vernacularName,
      language = .data$language,
      update_date = .data$update_date,
      acceptedNameUsageID = .data$acceptedNameUsageID,
      taxonID = .data$taxonID,
      scientificName = .data$scientificName,
      taxonRank = .data$taxonRank,
      taxonomicStatus = .data$taxonomicStatus,
      kingdom = .data$kingdom,
      phylum = .data$phylum,
      class = .data$class,
      order = .data$order,
      family = .data$family,
      genus = .data$genus,
      specificEpithet = .data$specificEpithet,
      infraspecificEpithet = .data$infraspecificEpithet,
      candidate_taxonID = .data$candidate_taxonID,
      selected = .data$selected
    )

  readr::write_csv(
    output,
    base::file.path(path, "taxa_duplicates.csv")
  )
  base::invisible(NULL)
}

#' Build the capeml-compatible taxa map
#'
#' @param input_names Normalized input-name table.
#' @param candidates Candidate table with a logical `selected` column.
#' @param ambiguous_names Lookup names having multiple candidates.
#'
#' @return A data frame with the stable `taxa_map.csv` schema.
#' @keywords internal
build_taxa_map <- function(
  input_names,
  candidates,
  ambiguous_names
) {
  selected <- candidates |>
    dplyr::filter(.data$selected) |>
    dplyr::arrange(.data$input_index)

  repeated_taxa <- selected$candidate_taxonID[
    base::duplicated(selected$candidate_taxonID) |
      base::duplicated(selected$candidate_taxonID, fromLast = TRUE)
  ] |>
    base::unique()

  if (base::length(repeated_taxa) > 0L) {
    repeated_names <- selected$input_name[
      selected$candidate_taxonID %in% repeated_taxa
    ]
    base::warning(
      "Multiple input names resolved to the same taxon; retaining the first ",
      "input name: ",
      base::paste(base::unique(repeated_names), collapse = ", "),
      call. = FALSE
    )
  }

  selected <- selected |>
    dplyr::distinct(.data$candidate_taxonID, .keep_all = TRUE) |>
    dplyr::transmute(
      input_name = .data$input_name,
      taxa_clean = .data$scientificName,
      taxonID = .data$taxonID,
      taxonRank = .data$taxonRank,
      acceptedNameUsageID = .data$acceptedNameUsageID,
      taxonomicStatus = .data$taxonomicStatus,
      update_date = .data$update_date,
      kingdom = .data$kingdom,
      phylum = .data$phylum,
      class = .data$class,
      order = .data$order,
      family = .data$family,
      genus = .data$genus,
      specificEpithet = .data$specificEpithet,
      infraspecificEpithet = .data$infraspecificEpithet,
      vernacularName = .data$vernacularName,
      input_index = .data$input_index
    )

  unmatched <- input_names |>
    dplyr::filter(
      !.data$lookup_name %in% candidates$lookup_name,
      !.data$lookup_name %in% ambiguous_names
    ) |>
    dplyr::transmute(
      input_name = .data$input_name,
      taxa_clean = NA_character_,
      taxonID = NA_character_,
      taxonRank = NA_character_,
      acceptedNameUsageID = NA_character_,
      taxonomicStatus = NA_character_,
      update_date = base::as.Date(NA),
      kingdom = NA_character_,
      phylum = NA_character_,
      class = NA_character_,
      order = NA_character_,
      family = NA_character_,
      genus = NA_character_,
      specificEpithet = NA_character_,
      infraspecificEpithet = NA_character_,
      vernacularName = NA_character_,
      input_index = .data$input_index
    )

  base::rbind(selected, unmatched) |>
    dplyr::arrange(.data$input_index) |>
    dplyr::select(-dplyr::all_of("input_index")) |>
    base::as.data.frame()
}

#' Ensure the stable taxonomy columns exist
#'
#' @param data A data frame returned by taxadb or a test fixture.
#'
#' @return `data` with any absent taxonomy columns added as missing character
#'   vectors, except `update_date`, which is added as a missing date.
#' @keywords internal
ensure_taxonomy_columns <- function(data) {
  character_columns <- c(
    "vernacularName", "language", "acceptedNameUsageID", "taxonID",
    "scientificName", "taxonRank", "taxonomicStatus", "kingdom", "phylum",
    "class", "order", "family", "genus", "specificEpithet",
    "infraspecificEpithet"
  )

  missing_character <- base::setdiff(character_columns, base::names(data))
  if (base::length(missing_character) > 0L) {
    data[missing_character] <- NA_character_
  }

  if (!"update_date" %in% base::names(data)) {
    data$update_date <- base::as.Date(NA)
  }

  data
}

#' Create an empty raw match table
#'
#' @return A zero-row data frame with taxonomy columns.
#' @keywords internal
empty_raw_matches <- function() {
  empty_taxonomy()
}

#' Create an empty taxonomy table
#'
#' @return A zero-row data frame with the ITIS fields used by capemlTaxa.
#' @keywords internal
empty_taxonomy <- function() {
  base::data.frame(
    vernacularName = character(),
    language = character(),
    update_date = base::as.Date(character()),
    acceptedNameUsageID = character(),
    taxonID = character(),
    scientificName = character(),
    taxonRank = character(),
    taxonomicStatus = character(),
    kingdom = character(),
    phylum = character(),
    class = character(),
    order = character(),
    family = character(),
    genus = character(),
    specificEpithet = character(),
    infraspecificEpithet = character(),
    stringsAsFactors = FALSE
  )
}

#' Create an empty candidate table
#'
#' @return A zero-row candidate data frame.
#' @keywords internal
empty_candidates <- function() {
  base::data.frame(
    input_name = character(),
    lookup_name = character(),
    input_index = integer(),
    vernacularName = character(),
    language = character(),
    candidate_taxonID = character(),
    taxonID = character(),
    acceptedNameUsageID = character(),
    scientificName = character(),
    taxonRank = character(),
    taxonomicStatus = character(),
    update_date = base::as.Date(character()),
    kingdom = character(),
    phylum = character(),
    class = character(),
    order = character(),
    family = character(),
    genus = character(),
    specificEpithet = character(),
    infraspecificEpithet = character(),
    candidate_count = integer(),
    selected = logical(),
    stringsAsFactors = FALSE
  )
}
