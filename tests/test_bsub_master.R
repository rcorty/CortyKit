library(CortyKit)

# bsub_master(script.file.name = 'test_bsub_worker.R',
#             param.list = list(height = c(3, 4), width = c(7,8), color='blue', depth=3))

CalcCrazy <- function(a, b) {
  c <- a + 10
  # a long long comment
  # gets longer and longer
  d <- 100 * 82
  # longer and longer
  # whaaaaat
  return(a + getOption(x = 'deparse.cutoff') * b);
}


bsub_master()
