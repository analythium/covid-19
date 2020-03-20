x <- readLines("update.R")
x <- x[-1]
i <- rev(grep("---", x))[1L]
x <- c(x[seq_len(i)],
    "#+ include=FALSE",
    "knitr::opts_chunk$set(eval=FALSE)",
    x[-seq_len(i)])
dir.create("www/methods")
writeLines(x, "www/methods/index.R")
rmarkdown::render("www/methods/index.R")
unlink("www/methods/index.R")
