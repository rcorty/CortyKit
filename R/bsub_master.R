#' @title bsub_master Function
#' @name bsub_master
#' @author Robert Corty
#'
#' @description my great function
#'
#'
#'
bsub_master <- function(f,
                        constant.params,
                        parallel.params,
                        crossed.params,
                        q = 'day',
                        M = '5',
                        testing = FALSE) {

  set.seed(Sys.time())

  # validate input -------------------------------------------------------------
  bsub_validate_input_(constant.params = constant.params,
                       parallel.params = parallel.params,
                       crossed.params = crossed.params)

  # make a batch code for use in naming directories and files ------------------
  batch.code <- bsub_make_batch_code_(constant.params = constant.params)

  # make directories: one for results and one for LSF output -------------------
  bsub_make_dirs_(batch.code)

  # begin writing batch summary file -------------------------------------------
  batch.summary.file <- file(description = file.path(results.dir,
                                                     'batch_summary.txt'),
                             open = 'w')
  writeLines(text = paste('Batch submission started at', Sys.time()),
             con = batch.summary.file)


  # make a data.frame of all the parameter combinations to be run --------------
  param.combo.df <-
    bsub_make_param_combo_df_(constant.params = constant.params,
                              parallel.params = parallel.params,
                              crossed.params = crossed.params)

  # send off each job
  for (job.num in 1:nrow(param.df)) {

    function.def.file <- file(description = file.path(results.dir,
                                                      'function_def.R'),
                              open = 'w')
    writeLines(text = )
    job.params <- paste(names(param.df), param.df[job.num,], sep = '=', collapse = '_')
    results.file.path <- file.path(results.dir, job.params)


    args <- paste('-J', paste0(batch.code, '_', job.params),
                  '-o', file.path(lsf.out.dir, job.params),
                  '-q', q,
                  '-M', M,
                  'Rscript',
                  script.file.name,
                  results.file.path,
                  paste(param.df[job.num, ], collapse = ' '))

    writeLines(text = paste('Sending job', job.num, ':', job.params),
               con = description.file)
    system2(command = if (testing) { 'echo' } else { 'bsub'} ,
            args = args)  # TODO: why no results being saved to directory?

  }


  # write closing of description file
  writeLines(text = 'grid-expanded job data.frame is',
             con = description.file)
  # TODO: figure out how to pretty-print a df
  write.table(x = param.df,
              file = description.file,
              quote = FALSE,
              row.names = FALSE)
  writeLines(text = paste('Batch submission completed at', Sys.time()),
             con = batch.summary.file)
  close(con = batch.summary.file)

}



bsub_validate_input_ <- function(constant.params,
                                 parallel.params,
                                 crossed.params) {

  if (any(sapply(X = constant.params, FUN = length) != 1)) {
    stop('Constant params is not a list of length-one vectors')
  }

  parallel.lengths <- sapply(X = parallel.params, FUN = length)
  if (any(parallel.lengths < 2)) {
    stop('Parallel params is not a list of length-2+ vectors')
  }
  if (!(all(parallel.lengths == length(parallel.params[[1]])))) {
    stop('Parallel params is not a list of equal-length vectors')
  }

  if (any(sapply(X = crossed.params, FUN = length) < 2)) {
    stop('Crossed params is not a list of length-2+ vectors')
  }
}


bsub_make_batch_code_ <- function(constant.params = NULL) {

  if (is.null(constant.params)) {
    return(paste(sample(x = LETTERS, size = 8, replace = TRUE),
                 collapse = ''))
  } else {
    return(paste(names(constant.params), unlist(constant.params),
                 sep = '=', collapse = '_'))
  }
}


bsub_make_dirs_ <- function(batch.code) {

  results.dir <- paste('Results', batch.code, sep = '_')
  results.dir.created <- dir.create(path = results.dir)
  if (!results.dir.created) {
    stop('Problem creating results directory: ', results.dir)
  }

  lsf.out.dir <- paste('lsf_out', batch.code, sep = '_')
  lsf.out.dir.created <- dir.create(path = lsf.out.dir)
  if (!lsf.out.dir.created) {
    stop('Problem creating LSF output directory: ', lsf.out.dir)
  }

}


bsub_make_param_combo_df_ <- function(constant.params,
                                      parallel.params,
                                      crossed.params) {

  # TODO, deal with missingness

  # first just the crossed parameters ------------------------------------------
  to.expand <- cbind(data.frame(crossed.params),
                     parallel.idx = 1:length(parallel.params[[1]]))
  crossed.param.df <- rev(expand.grid(rev(to.expand),
                                      stringsAsFactors = FALSE))
  num.param.sets <- nrow(crossed.param.df)
  par.param.selector <- crossed.param.df[['parallel.idx']]
  crossed.param.df[['parallel.idx']] <- NULL

  # then add the parallel parameters -------------------------------------------
  crossed.and.parallel.df <-
    bind_cols(param.df,
              data.frame(parallel.params)[par.param.selector,])

  # finally add in the constant parameters -------------------------------------
  all.params.df <-
    bind_cols(crossed.and.parallel.df,
              data.frame(constant.params)[rep(1, num.param.sets),])

  return(all.params.df
         )

}
