`%||%` <- function(x, y) if (is.null(x)) y else x

left_join_outputs <- function(main_df, ..., key = "person_id")
{
  main_dt <- data.table::as.data.table(main_df)
  if (!(key %in% names(main_dt))) stop(sprintf("main_df missing key column: %s", key))
  data.table::setkeyv(main_dt, key)

  others <- list(...)
  for (i in seq_along(others)) {
    dt <- data.table::as.data.table(others[[i]])
    if (nrow(dt) == 0) next
    if (!(key %in% names(dt))) stop(sprintf("join table %d missing key column: %s", i, key))

    if (anyDuplicated(dt[[key]]) > 0) dt <- dt[!duplicated(dt[[key]]), ]

    data.table::setkeyv(dt, key)
    main_dt <- dt[main_dt]
  }

  main_dt[]
}

#' Run selected algorithms with anchor-derivation support, read outputs, and combine
#'
#' Supports workflows where:\n
#' 1) An \"anchor algorithm\" is run and writes a CSV.\n
#' 2) You read that CSV.\n
#' 3) You choose ONE column from it (e.g., \"date_at_age_18\" or \"first_medical_encounter_entry_date\").\n
#' 4) You derive an anchor table: (person_id, anchor_date).\n
#' 5) You run child algorithms using anchor_date_table = that derived anchor table.\n
#'
#' ## Defining algos\n
#' `algos` is a named list; names must equal output base filenames (no .csv).\n
#'
#' Each element can be either:\n
#' - a function (no special config), OR\n
#' - a list with fields:\n
#'   - `fn`: function\n
#'   - `args`: list of args\n
#'   - `anchor_from`: name of anchor algorithm whose output will be used to create anchor_date_table\n
#'   - `anchor_col`: column name (in anchor_from output) to copy into `anchor_date`\n
#'
#' If `anchor_from` is set, this wrapper will:\n
#' - run `anchor_from`\n
#' - read its CSV\n
#' - create `anchor_date_table = df[, c(\"person_id\", \"anchor_date\")]` where\n
#'   `anchor_date := as.Date(df[[anchor_col]])`\n
#' - pass that anchor table into the child algorithm as `anchor_date_table`\n
#'
#' @param output_folder bucket folder where algorithms write outputs (e.g., \"datasets\")\n
#' @param algos named list of algorithms/specs\n
#' @param main_name which element of `algos` is the main dataset to left-join onto\n
#' @param read_fun function(path) to read a CSV from the bucket. Defaults to `read_bucket`\n
#' @param key join key (default \"person_id\")\n
#' @param ... shared args passed to every algorithm (optional)\n
#' @return data.table\n
#' @export
run_algorithms_and_combine <- function(
  output_folder,
  algos,
  main_name,
  read_fun = read_bucket,
  key = "person_id",
  ...
){
  if (!is.function(read_fun)) {
    stop("`read_fun` is not a function. Ensure read_bucket() is available in this session or pass read_fun explicitly.")
  }
  if (is.null(names(algos)) || any(names(algos) == "")) {
    stop("`algos` must be a *named* list where names equal the output base filename (without .csv).")
  }
  if (!(main_name %in% names(algos))) stop("`main_name` must be one of names(algos).")

  shared_args <- list(...)
  ran <- setNames(rep(FALSE, length(algos)), names(algos))
  outputs_cache <- list()       # full outputs read from bucket
  anchor_cache <- list()        # derived anchor_date_table per anchor spec

  read_output <- function(nm) {
    if (!is.null(outputs_cache[[nm]])) return(outputs_cache[[nm]])
    path <- file.path(output_folder, paste0(nm, ".csv"))
    outputs_cache[[nm]] <<- read_fun(path)
    outputs_cache[[nm]]
  }

  make_anchor_table <- function(anchor_algo_name, anchor_col) {
    cache_key <- paste0(anchor_algo_name, "::", anchor_col)
    if (!is.null(anchor_cache[[cache_key]])) return(anchor_cache[[cache_key]])

    df <- data.table::as.data.table(read_output(anchor_algo_name))

    if (!(key %in% names(df))) stop(sprintf("Anchor output '%s' missing key column '%s'", anchor_algo_name, key))
    if (!(anchor_col %in% names(df))) {
      stop(sprintf(
        "Anchor output '%s' does not contain anchor_col '%s'. Available: %s",
        anchor_algo_name, anchor_col, paste(names(df), collapse = ", ")
      ))
    }

    anchor_dt <- df[, .(person_id = get(key), anchor_date = as.Date(get(anchor_col)))]
    # optional: drop rows with missing anchor_date
    anchor_dt <- anchor_dt[!is.na(anchor_date)]
    # enforce 1 row per person_id (keep first)
    if (anyDuplicated(anchor_dt[[key]]) > 0) anchor_dt <- anchor_dt[!duplicated(anchor_dt[[key]]), ]

    anchor_cache[[cache_key]] <<- anchor_dt
    anchor_dt
  }

  run_one <- function(nm) {
    if (isTRUE(ran[[nm]])) return(invisible(TRUE))
    if (!(nm %in% names(algos))) stop(sprintf("Unknown algo referenced: %s", nm))

    spec <- algos[[nm]]

    # normalize
    if (is.function(spec)) {
      spec <- list(fn = spec, args = list(), anchor_from = NULL, anchor_col = NULL)
    } else if (is.list(spec) && is.function(spec$fn)) {
      spec$args <- spec$args %||% list()
      spec$anchor_from <- spec$anchor_from %||% NULL
      spec$anchor_col <- spec$anchor_col %||% NULL
    } else {
      stop(sprintf("algos[[%s]] must be a function or list(fn=<function>, args=<list>, anchor_from=<name>, anchor_col=<col>)", nm))
    }

    # dependency: if this algo needs an anchor derived from another algo output
    if (!is.null(spec$anchor_from)) {
      if (is.null(spec$anchor_col) || !nzchar(spec$anchor_col)) {
        stop(sprintf("Algo '%s' specifies anchor_from='%s' but no anchor_col was provided.", nm, spec$anchor_from))
      }

      # ensure parent ran and wrote its file
      run_one(spec$anchor_from)

      # derive anchor_date_table and inject, unless user explicitly set anchor_date_table
      if (is.null(spec$args$anchor_date_table)) {
        spec$args$anchor_date_table <- make_anchor_table(spec$anchor_from, spec$anchor_col)
      }
    }

    algo_args <- c(shared_args, spec$args)
    do.call(spec$fn, c(list(output_folder = output_folder), algo_args))

    ran[[nm]] <<- TRUE
    invisible(TRUE)
  }

  # 1) run all (dependency-aware)
  for (nm in names(algos)) run_one(nm)

  # 2) read all outputs
  for (nm in names(algos)) {
    if (is.null(outputs_cache[[nm]])) outputs_cache[[nm]] <- read_output(nm)
  }

  # 3) combine
  main_df <- outputs_cache[[main_name]]
  join_others <- outputs_cache[names(outputs_cache) != main_name]
  do.call(left_join_outputs, c(list(main_df), join_others, list(key = key)))
}

# -----------------------
# Example usage:
# -----------------------
# output_folder <- "datasets"
#
# algos_to_run <- list(
#   demographics = demographics,
#
#   # Anchor-producing algorithm (writes datasets/first_medical_encounter.csv)
#   first_medical_encounter = list(
#     fn = first_medical_encounter,
#     args = list(suffix = "_baseline")  # optional
#   ),
#
#   # Child algorithm: use anchor_date_table derived from the above output column
#   max_ldl = list(
#     fn = max_ldl,
#     anchor_from = "first_medical_encounter",
#     anchor_col  = "first_medical_encounter_entry_date_baseline",  # column to copy into anchor_date
#     args = list(before = 0, after = 10000000, suffix = "_after_anchor")
#   )
# )
#
# combined <- run_algorithms_and_combine(
#   output_folder = output_folder,
#   algos = algos_to_run,
#   main_name = "demographics"
# )