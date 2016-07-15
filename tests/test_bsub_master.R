library(CortyKit)
library(methods)

area.computer <- function(height, width) {

  a <- height*width

  saveRDS(object = a,
          file = file.path(getOption(x = 'results.dir'),
                           getOption(x = 'param.list')))
}


bsub_master(f = area.computer,
            param.list = list(height = 3, width = 7))
