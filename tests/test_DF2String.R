n <- 10

x <- data.frame(a = 6:(5+n),
                b = runif(n),
                c = sapply(X = sample(1:10, size = n),
                           FUN = function(i) paste(sample(x = LETTERS, size = i),
                                                   collapse = '')))


DF2String(DF = x, signif = 3)
