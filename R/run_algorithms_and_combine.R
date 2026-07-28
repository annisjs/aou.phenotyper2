`%||%` <- function(x, y) if (is.null(x)) y else x

#' Left join multiple tables onto a main table by person_id
#'
#' @param main_df main data.frame/data.table defining the row universe
#' @param ... additional tables to left join onto main_df
#' @param key join key (default "person_id")
#' @return data.table
#' @export
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

#' Run selected algorithms with dependency support, read outputs, and combine
#'
#' This version supports cases where an algorithm (child) needs an anchor table that
#' is produced by another algorithm (parent) and therefore must be run FIRST, then
#' read into memory, then passed to the child as `anchor_date_table`.
#'
#' ## How to define `algos`
#' `algos` is a named list. Names MUST match each algorithm's output base filename (no .csv).
#'
#' Each element can be either:\n
#' - a function, OR\n
#' - a list with fields:\n
#'   - `fn`: the function\n
#'   - `args`: list of args passed to fn\n
#'   - `anchor_from`: OPTIONAL. Name of another algo in `algos` whose output should be read and used as `anchor_date_table`.\n
#'
#' If `anchor_from` is provided, the wrapper will:\n
#' 1) run `anchor_from`\n
#' 2) read `<output_folder>/<anchor_from>.csv` into a data.frame\n
#' 3) inject it into the child's args as `anchor_date_table = <that df>` (unless you already supplied anchor_date_table)\n
#'
#' @param output_folder bucket folder where algorithms write outputs (e.g., "datasets")
#' @param algos named list of algorithms/specs (see above)
#' @param main_name which element of `algos` is the main dataset to left-join onto
#' @param read_fun function(path) to read a CSV from the bucket. Defaults to `read_bucket`
#'   (expected to be available in the notebook/session).
#' @param key join key (default "person_id")
#' @param ... shared args passed to every algorithm (optional). Per-algorithm args override/add.
#' @return data.table
#' @export
#'
#' @examples
#' # output_folder <- "datasets"
#' #
#' # algos_to_run <- list(
#' #   demographics = demographics,
#' #
#' #   # This algo produces an anchor table (must have person_id, anchor_date)
#' #   date_at_age_18 = date_at_age_18,
#' #
#' #   # Child algo uses the anchor table produced above
#' #   max_ldl = list(
#' #     fn = max_ldl,
#' #     anchor_from = "date_at_age_18",
#' #     args = list(before = 0, after = 10000000, suffix = "_after_age_18")
#' #   )
#' # )
#' #
#' # combined <- run_algorithms_and_combine(
#' #   output_folder = output_folder,
#' #   algos = algos_to_run,
#' #   main_name = "demographics"
#' # )
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
  outputs_cache <- list()  # read outputs as needed (esp. anchors)

  # read helper with cache
  read_output <- function(nm) {
    if (!is.null(outputs_cache[[nm]])) return(outputs_cache[[nm]])
    path <- file.path(output_folder, paste0(nm, ".csv"))
    outputs_cache[[nm]] <<- read_fun(path)
    outputs_cache[[nm]]
  }

  # run helper (recursive for dependencies)
  run_one <- function(nm) {
    if (isTRUE(ran[[nm]])) return(invisible(TRUE))
    if (!(nm %in% names(algos))) stop(sprintf("Unknown algo referenced: %s", nm))

    spec <- algos[[nm]]

    # normalize spec to list(fn=, args=, anchor_from=)
    if (is.function(spec)) {
      spec <- list(fn = spec, args = list(), anchor_from = NULL)
    } else if (is.list(spec) && is.function(spec$fn)) {
      spec$args <- spec$args %||% list()
    } else {
      stop(sprintf("algos[[%s]] must be a function or list(fn=<function>, args=<list>, anchor_from=<name>)", nm))
    }

    # ensure anchor dependency has run + is read before running child
    if (!is.null(spec$anchor_from)) {
      parent <- spec$anchor_from
      run_one(parent)

      # only inject anchor_date_table if user didn't supply it explicitly
      if (is.null(spec$args$anchor_date_table)) {
        spec$args$anchor_date_table <- read_output(parent)
      }
    }

    # run the algorithm
    algo_args <- c(shared_args, spec$args)
    do.call(spec$fn, c(list(output_folder = output_folder), algo_args))

    ran[[nm]] <<- TRUE
    invisible(TRUE)
  }

  # 1) run all (will respect dependencies via run_one recursion)
  for (nm in names(algos)) run_one(nm)

  # 2) read all outputs (including ones already cached)
  for (nm in names(algos)) {
    if (is.null(outputs_cache[[nm]])) outputs_cache[[nm]] <- read_output(nm)
  }

  # 3) combine (left join onto main)
  main_df <- outputs_cache[[main_name]]
  join_others <- outputs_cache[names(outputs_cache) != main_name]
  do.call(left_join_outputs, c(list(main_df), join_others, list(key = key)))
}