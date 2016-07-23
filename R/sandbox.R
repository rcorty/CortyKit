CalcCrazy <- function(a, b) {
  c <- a + 10
  # a long long comment
  # gets longer and longer
  d <- 100 * 82
  # longer and longer
  # whaaaaat
  return(a + getOption(x = 'deparse.cutoff') * b);
}


function.strings <- str_trim(deparse(expr = CalcCrazy))
middle.idxs <- 3:(length(function.strings) - 1)
middle.strings.semicoloned <- paste(function.strings[middle.idxs], ';', sep = '', collapse = '  ')


to.file <- paste('f <- ', paste(function.strings[1],
                                function.strings[2],
                                middle.strings.semicoloned,
                                function.strings[length(function.strings)],
                                collapse = ''))

to.file <- c('f <- ',
             function.strings[1],
             function.strings[2],
             middle.strings.semicoloned,
             function.strings[length(function.strings)])


fileConn<-file("test.file.R")
writeLines(to.file, fileConn)
close(fileConn)

eval(parse(file = 'test.file.R'))

f(1, 2)
