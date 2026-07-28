`%||%` <- function(x, y) if (is.null(x)) y else x

#' Left join multiple tables onto a main table by person_id
#'
#' @param main_df main data.frame/data.table defining the row universe
#' @param ... additional tables to left join onto main_df
#' @param key join key (default "person_id")
#' @return data.table
#' @export
#'
#' @examples
#' # dems <- read_bucket("datasets/demographics.csv")
#' # max_ldl <- read_bucket("datasets/max_ldl.csv")
#' # combined <- left_join_outputs(dems, max_ldl)
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

    # Enforce 1 row per person_id on RHS (wide merge expectation)
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
#' ## Key idea\n
#' You pass a named list `algos` describing WHICH algorithms to run and HOW to run them.\n
#' The **names** of `algos` MUST match the output base filename written by each algorithm\n
#' (i.e., the third argument to `.write_to_bucket`), without the \".csv\".\n
#' Example: `max_ldl()` writes using `.write_to_bucket(..., \"max_ldl\")`, so the name must be \"max_ldl\".\n
#' \n
#' ## Two ways to specify algorithms\n
#' ### (A) Simple algorithms (no special args)\n
#' Provide the function directly:\n
#' `algos <- list(demographics = demographics, max_ldl = max_ldl)`\n
#' \n
#' ### (B) Algorithms that need anchors / suffix / custom args\n
#' Provide a list with `fn` and `args`:\n
#' `algos <- list(max_ldl = list(fn = max_ldl, args = list(anchor_date_table=anchor, before=0, after=1e7, suffix=\"_after_18\")))`\n
#' \n
#' This exactly mirrors how you would run it manually in the notebook:\n
#' `max_ldl(output_folder, anchor_date_table=anchor, before=0, after=1e7, suffix=\"_after_18\")`\n
#'
#' @param output_folder bucket folder where algorithms write outputs (e.g., \"datasets\")\n
#' @param algos named list of algorithms to run. Each element is either:\n
#' - a function, OR\n
#' - `list(fn = <function>, args = <list of arguments>)`\n
#' \n
#' @param main_name which element of `algos` is the main dataset to left-join onto\n
#' @param read_fun function(path) to read a CSV from the bucket. Defaults to `read_bucket`\n
#'   (expected to be available in the notebook/session from your other library).\n
#' @param key join key (default \"person_id\")\n
#' @param ... shared args passed to every algorithm (optional). Useful if many algorithms share the same\n
#'   `anchor_date_table`, `before`, `after`, `suffix`, etc. Per-algorithm `args` override/add to these.\n
#'
#' @return data.table\n
#' @export
#'
#' @examples
#' # output_folder <- "datasets"
#' # anchor <- read_bucket("datasets/anchor.csv")  # must have person_id, anchor_date
#' #
#' # algos_to_run <- list(
#' #   demographics = demographics,  # writes datasets/demographics.csv
#' #   max_ldl = list(               # writes datasets/max_ldl.csv
#' #     fn = max_ldl,
#' #     args = list(
#' #       anchor_date_table = anchor_algorithm,
#' #       before = 0,
#' #       after = 10000000,
#' #       suffix = "_after_age_18"
#' #     )
#' #   ),
#' #   max_hgba1c = list(
#' #     fn = max_hgba1c,
#' #     args = list(
#' #       anchor_date_table = anchor_algorithm,
#' #       before = 0,
#' #       after = 10000000,
#' #       suffix = "_after_age_18"
#' #     )
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

  # 2) read outputs (expects <output_folder>/<name>.csv)
  outputs <- lapply(names(algos), function(nm) {
    read_fun(file.path(output_folder, paste0(nm, ".csv")))
  })
  names(outputs) <- names(algos)

  # 3) combine
  main_df <- outputs[[main_name]]
  join_others <- outputs[names(outputs) != main_name]
  do.call(left_join_outputs, c(list(main_df), join_others, list(key = key)))
}