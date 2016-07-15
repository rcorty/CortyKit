#' @title bsub_master Function
#' @name bsub_master
#' @author Robert Corty
#'
#' @description
#'
#' @param f the function to be evaluated with potentially many different
#' parameter sets
#' @param param.list A named list of vectors of parameters.
#' By default, all combinations will be used.
#'
#' @return t
#'
#' @details
#'
#' @examples
#'
#'
#' @export
#'
bsub_master <- function(f, param.list,
                        combination.type = 'hypercube', q = 'day', M = '5') {

  # validation that params provided in param.list
  # are the entire parameter list for f
  if(!identical(formalArgs(f), names(param.list))) {
    stop(paste('function', substitute(f), 'has parameters [',
               paste(formalArgs(f), collapse = ', '),
               '], while the parameters provided in argument param.list are [',
               paste(names(param.list), collapse = ', '), '].',
               'They must be identical.'))
  }

  set.seed(Sys.time())

  # set up directories for results and info on the lsf jobs
  batch.code <- paste(sample(x = LETTERS, size = 8, replace = TRUE),
                      collapse = '')
  results.dir <- paste('Results', batch.code, sep = '_')
  lsf.out.dir <- paste('lsf_out', batch.code, sep = '_')
  system2(command = 'mkdir',
          args = paste(results.dir, lsf.out.dir))

  # start a description file for the batch
  description.file <- file(description = file.path(results.dir,
                                                   'batch_description.txt'),
                           open = 'w')
  writeLines(text = paste('Batch submission started at', Sys.time()),
             con = description.file)


  # send off a bsub job for each parameter combination
  if (combination.type == 'hypercube') {
    param.df <- rev(expand.grid(rev(param.list)))
  } else {
    stop('combination.type other than hypercube not yet implemented')
  }

  for (job.num in 1:nrow(param.df)) {
    job.params <- paste(names(param.df), param.df[job.num,], sep = '=', collapse = ',')

    writeLines(text = paste('Job', job.num, ':', job.params),
               con = description.file)

    set.results.dir <- paste0("-e \"options(results.dir = '", results.dir, "')\"")
    set.param.list <- paste0("-e \"options(param.list =", job.params, ")\"")
    define.f <- paste("-e \"f <-", paste(deparse(f), collapse = ''), "\"")
    run.f <- paste("-e \"f(", job.params, ")\"")

    args <- paste('-J', paste0(batch.code, '_', job.params),
                  '-o', file.path(lsf.out.dir, job.params),
                  '-q', q,
                  '-M', M,
                  'Rscript',
                  set.results.dir,
                  set.param.list,
                  define.f,
                  run.f)

    system2(command = "bsub",
            args = shQuote(args))  # TODO: why no results being saved to directory?

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
             con = description.file)
  close(description.file)

}
