args <- commandArgs(trailingOnly = TRUE)
results.file.path <- args[1]
height <- as.numeric(args[2])
width <- as.numeric(args[3])

a <- height*width

saveRDS(object = a,
        file = results.file.path)

