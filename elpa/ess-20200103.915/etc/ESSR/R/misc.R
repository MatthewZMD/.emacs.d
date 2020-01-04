.ess_weave <- function(command, file, encoding = NULL){
    cmd_symb <- substitute(command)
    if (grepl('knit|purl', deparse(cmd_symb))) require(knitr)
    od <- getwd()
    on.exit(setwd(od))
    setwd(dirname(file))
    frame <- parent.frame()
    if (is.null(encoding))
        eval(bquote(.(cmd_symb)(.(file))), envir = frame)
    else
        eval(bquote(.(cmd_symb)(.(file), encoding = .(encoding))), envir = frame)
}

.ess_knit <- function(file, output = NULL){
    library(knitr)
    frame <- parent.frame()
    od <- getwd()
    on.exit(setwd(od))
    setwd(dirname(file))
    ## this bquote is really needed for data.table := operator to work correctly
    eval(bquote(knit(.(file), output = .(output))), envir = frame)
}


.ess_sweave <- function(file, output = NULL){
    od <- getwd()
    frame <- parent.frame()
    on.exit(setwd(od))
    setwd(dirname(file))
    eval(bquote(Sweave(.(file), output = .(output))), envir = frame)
}

## Users might find it useful. So don't prefix with .ess.
.ess_htsummary <- function(x, hlength = 4, tlength = 4, digits = 3) {
    ## fixme: simplify and generalize
    snames <- c("mean", "sd", "min", "max", "nlev", "NAs")
    d <- " "
    num_sumr <- function(x){
        c(f(mean(x, na.rm = TRUE)),
          f(sd(x, na.rm = TRUE)),
          f(min(x, na.rm = TRUE)),
          f(max(x, na.rm = TRUE)),
          d,
          f(sum(is.na(x), na.rm = TRUE)))
    }
    f <- function(x) format(x, digits = digits)

    if (is.data.frame(x) | is.matrix(x)) {
        if (nrow(x) <= tlength + hlength){
            print(x)
        } else {
            if (is.matrix(x))
                x <- data.frame(unclass(x))
            ## conversion needed, to avoid problems with derived classes suchs
            ## as data.table
            h <- as.data.frame(head(x, hlength))
            t <- as.data.frame(tail(x, tlength))
            for (i in 1:ncol(x)) {
                h[[i]] <- f(h[[i]])
                t[[i]] <- f(t[[i]])
            }
            ## summaries
            sumr <- sapply(x, function(c){
                if (is.logical(c))
                    ## treat logical as numeric; it's harmless
                    c <- as.integer(c)
                if (is.numeric(c))
                    num_sumr(c)
                else if (is.factor(c)) c(d, d, d, d, nlevels(c), sum(is.na(c)))
                else rep.int(d, length(snames))
            })
            sumr <- as.data.frame(sumr)
            row.names(sumr) <- snames
            dots <- rep("...", ncol(x))
            empty <- rep.int(" ", ncol(x))
            lines <- rep.int(" ", ncol(x))
            df <- rbind(h, ... = dots, t, `_____` = lines, sumr, ` ` = empty)
            print(df)
        }
    } else {
        cat("head(", hlength, "):\n", sep = "")
        print(head(x, hlength))
        if (length(x) > tlength + hlength){
            cat("\ntail(", tlength, "):\n", sep = "")
            print(tail(x, tlength))
        }
        cat("_____\n")
        if (is.numeric(x) || is.logical(x))
            print(structure(num_sumr(x), names = snames), quote = FALSE)
        else if (is.factor(x)){
            cat("NAs: ", sum(is.na(x), na.rm = TRUE), "\n")
            cat("levels: \n")
            print(levels(x))
        }
    }
    invisible(NULL)
}


.ess_vignettes <- function(all=FALSE) {
    vs <- unclass(browseVignettes(all = all))
    vs <- vs[sapply(vs, length) > 0]

    mat2elist <- function(mat) {
        if (!is.null(dim(mat))){
            apply(mat, 1, function(r)
                sprintf("(list \"%s\")",
                        paste0(gsub("\"", "\\\\\"",
                                    as.vector(r[c("Title", "Dir", "PDF",
                                                  "File", "R")])),
                               collapse = "\" \"")))
        }
    }
    cat("(list \n",
        paste0(mapply(function(el, name) {
            sprintf("(list \"%s\"  %s)",
                    name, paste0(mat2elist(el), collapse = "\n"))
        },
        vs, names(vs)), collapse = "\n"), ")\n")
}

.ess_Rd2txt <- function(rd) {
    fun <- tools::Rd2txt
    if (length(formals(fun)["stages"]))# newer R version
        fun(rd, stages = c("build", "install", "render"))
    else
        fun(rd)
}

## Hacked help.start() to use with ess-rutils.el
.ess_help_start <- function(update=FALSE, remote=NULL) {
    home <- if (is.null(remote)) {
                port <- tools::startDynamicHelp(NA)
                if (port > 0L) {
                    if (update)
                        make.packages.html(temp=TRUE)
                    paste0("http://127.0.0.1:", port)
                }
                else stop(".ess_help_start() requires the HTTP server to be running",
                          call.=FALSE)
            } else remote
    paste0(home, "/doc/html/index.html")
}
