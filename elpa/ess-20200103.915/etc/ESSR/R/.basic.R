#### Essential functionality needed by ESS

## Should work on *all* vesions of R.
## Do not use _ in names, nor :: , nor 1L etc, as they
## cannot be parsed in old R versions

.ess.getRversion <- function() {
    if(exists("getRversion", mode="function")) getRversion()
    else paste(R.version$major, R.version$minor, sep=".")
}

## loading ESSR.rda might fail, so re-assign here:
.ess.Rversion <- .ess.getRversion()

.ess.R.has.utils <- (.ess.Rversion >= "1.9.0")
.ess.utils.name <- paste("package",
                         if(.ess.Rversion >= "1.9.0") "utils" else "base",
                         sep = ":")

## Instead of modern  utils::help use one that works in R 1.0.0:
.ess.findFUN   <- get("find", .ess.utils.name)


### HELP
.ess.help <- function(..., help.type = getOption("help_type")) {
    if (is.null(help.type)) {
        help.type <- "text"
    }

    ## - get("help", ..) searching in global env works with devtools redefines
    ## - Redefining to .ess.help this way is necessary because
    ##   utils:::print.help_files_with_topic (used internally when there's
    ##   more than one a package) uses the quoted call
    ##   MM: don't understand; more specifically?
    .ess.help <- function(...) {
        do.call(get("help", envir = .GlobalEnv), list(...))
    }

    if (.ess.Rversion > "2.10") {
        ## Abbreviating help_type to avoid underscore
        .ess.help(..., help = help.type)
    } else {
        .ess.help(..., htmlhelp = help.type == "html")
    }
}

.ess.getHelpAliases <- function(){
    readrds <-
        if(.ess.Rversion >= '2.13.0') readRDS
        else .readRDS
    rds.files <- paste(searchpaths(), "/help/aliases.rds", sep = "")
    unlist(lapply(rds.files,
                  function(f){
                      if( file.exists(f) )
                          try(names(readrds(f)))
                  }),
           use.names = FALSE)
}

### SOURCING
.ess.eval <- function(string, visibly = TRUE, output = FALSE,
                      max.deparse.length = 300,
                      file = tempfile("ESS"), local = NULL)
{
    if (is.null(local)) {
        local <- if (.ess.Rversion > '2.13') parent.frame() else FALSE
    }

    ## create FILE, put string into it. Then source.
    ## arguments are like in source and .ess.source
    cat(string, file = file)
    ## The following on.exit infloops in R 3.3.0
    ## https://github.com/emacs-ess/ESS/issues/334
    ## https://bugs.r-project.org/bugzilla/show_bug.cgi?id=16971
    ## So we are cleanning it in .ess.source instead.
    ## on.exit(file.remove(file))
    .ess.source(file, visibly = visibly, output = output,
                max.deparse.length = max.deparse.length,
                local = local, fake.source = TRUE)
}

.ess.strip.error <- function(msg, srcfile) {
    pattern <- paste0(srcfile, ":[0-9]+:[0-9]+: ")
    sub(pattern, "", msg)
}

.ess.file.remove <- function(file){
    if (base::file.exists(file)) base::file.remove(file)
    else FALSE
}

.ess.source <- function(file, visibly = TRUE, output = FALSE,
                        max.deparse.length = 300, local = NULL,
                        fake.source = FALSE, keep.source = TRUE,
                        message.prefix = "") {
    if (is.null(local)) {
        local <- if (.ess.Rversion > "2.13")
            parent.frame()
        else FALSE
    }

    ss <-
        if (.ess.Rversion >= "3.4")
            base::source
        else if (.ess.Rversion >= "2.8")
            function(..., spaced) base::source(...)
        else function(..., spaced, keep.source) base::source(...)

    on.exit({
        if (fake.source)
            .ess.file.remove(file)
    })

    out <- ss(file, echo = visibly, local = local, print.eval = output, spaced = FALSE,
              max.deparse.length = max.deparse.length, keep.source = keep.source)

    if(!fake.source)
        cat(sprintf("%sSourced file %s\n", message.prefix, file))

    ## Return value for org-babel
    invisible(out$value)
}

if(.ess.Rversion < "1.8")
    ## (works for "1.7.2"): bquote() was new in 1.8.0
    bquote <- function(expr, where=parent.frame()){
        unquote <- function(e)
            if (is.pairlist(e)) as.pairlist(lapply(e, unquote))
            else if (length(e) <= 1) e
            else if (e[[1]] == as.name(".")) eval(e[[2]], where)
            else as.call(lapply(e, unquote))

        unquote(substitute(expr))
    }
