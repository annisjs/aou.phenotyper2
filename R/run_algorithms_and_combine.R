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

    if (anyDuplicated(dt[[key]]) > 0) {
      dt <- dt[!duplicated(dt[[key]]), ]
    }

    data.table::setkeyv(dt, key)
    main_dt <- dt[main_dt]
  }

  main_dt[]
}

#' Run selected algorithms, read their outputs, and combine into one table
#'
#' Names of `algos` MUST match the output base filename written by the algorithm
#' (i.e., the third argument to `.write_to_bucket`), without the ".csv".
#'
#' @param output_folder bucket folder where algorithms write outputs (e.g., "datasets")
#' @param algos named list of algorithms to run. Each element is either a function or
#'   list(fn = <function>, args = <list>).
#' @param main_name which element of `algos` is the main dataset to left-join onto
#' @param read_fun function(path) to read a CSV from the bucket. Defaults to `read_bucket`
#'   (expected to be available in the notebook/session).
#' @param key join key (default "person_id")
#' @param ... shared args passed to every algorithm
#' @return data.table
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
  if (!(main_name %in% names(algos))) {
    stop("`main_name` must be one of names(algos).")
  }

  shared_args <- list(...)

  # 1) run algorithms
  for (nm in names(algos)) {
    spec <- algos[[nm]]

    if (is.function(spec)) {
      fn <- spec
      algo_args <- shared_args
    } else if (is.list(spec) && is.function(spec$fn)) {
      fn <- spec$fn
      algo_args <- c(shared_args, spec$args %||% list())
    } else {
      stop(sprintf("algos[[%s]] must be a function or list(fn=<function>, args=<list>)", nm))
    }

    do.call(fn, c(list(output_folder = output_folder), algo_args))
  }

  # 2) read outputs
  outputs <- lapply(names(algos), function(nm) {
    read_fun(file.path(output_folder, paste0(nm, ".csv")))
  })
  names(outputs) <- names(algos)

  # 3) combine
  main_df <- outputs[[main_name]]
  join_others <- outputs[names(outputs) != main_name]
  do.call(left_join_outputs, c(list(main_df), join_others, list(key = key)))
}