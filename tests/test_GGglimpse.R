n <- 100

df1 <- data_frame(a = rnorm(n = n),
                  b = sample(x = round(n/10), size = n, replace = TRUE))

GGglimpse(df = df1)
