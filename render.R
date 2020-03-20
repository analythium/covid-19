x <- readLines("update.R")
x <- x[-1]
i <- rev(grep("---", x))[1L]
x <- c(x[seq_len(i)],
    "#+ include=FALSE",
    "knitr::opts_chunk$set(eval=FALSE)",
    x[-seq_len(i)])
writeLines(x, "www/methods.R")
rmarkdown::render("www/methods.R")
unlink("www/methods.R")
