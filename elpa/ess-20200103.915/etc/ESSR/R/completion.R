## Do *NOT* use  1L -- it gives  parse errors in historical versions of R

## Try a setup working in as old R as possible.
## ===>
##    1)  do not use "_" in names!   --- seems impossible for the Millenials ..
##    2)  use our own simplified definition of  '::' and ':::' ?
##
if(!exists("local"))
    local <- function(expr, envir = environment()) { invisible(eval(expr, envir=envir)) }

##' Robust version of
##'    utils:::.addFunctionInfo(c = c("recursive", "use.names"))
local({
    U <- asNamespace("utils"); fn <- ".addFunctionInfo"
    EX <- exists(fn, envir=U)
    if(EX && is.function(FN <- get(fn, envir=U))) {
        FN(c = c("recursive", "use.names")); ##dbg: cat("Calling utils:::",fn,"(c = ...)\n")
    }
})


.ess_eval <- function(str, env = globalenv()) {
    ## don't remove; really need eval(parse(  here!!
    tryCatch(base::eval(base::parse(text=str), envir = env),
             error=function(e) NULL) ## also works for special objects containing @:$ etc
}

.ess_nonull <- function(x, default = "") {
    if (is.null(x)) default
    else x
}

.ess_srcref <- function(name, pkg) {
    if (!is.null(pkg) && requireNamespace(pkg)) {
        env <- asNamespace(pkg)
    } else {
        env <- globalenv()
    }
    fn <- .ess_eval(name, env)
    out <- "()\n"
    if (is.function(fn) && !is.null(utils::getSrcref(fn))) {
        file <- utils::getSrcFilename(fn, full.names = TRUE)
        if (file != "") {
            line <- .ess_nonull(utils::getSrcLocation(fn, "line"), 1)
            col <- .ess_nonull(utils::getSrcLocation(fn, "column"), 1)
            out <- sprintf("(\"%s\" %d %d)\n", file, line, col - 1)
        }
    }
    cat(out)
}


.ess_fn_pkg <- function(fn_name) {
    fn <- .ess_eval(fn_name)
    env_name <- base::environmentName(base::environment(fn))
    out <- if (base::is.primitive(fn)) { # environment() does not work on primitives.
               "base"
           } else if (base::is.function(fn) && env_name != "R_GlobalEnv") {
               env_name
           } else {
               ""
           }
    base::cat(base::sprintf("%s\n", out))
}

.ess_funargs <- function(funname) {
    if(.ess.Rversion > '2.14.1') {
        ## temporarily disable JIT compilation and errors
        comp <- compiler::enableJIT(0)
        op <- options(error=NULL)
        on.exit({ options(op); compiler::enableJIT(comp) })
    }
    fun <- .ess_eval(funname)
    if(is.function(fun)) {
        special <- grepl('[:$@[]', funname)
        args <- if(!special){
                    fundef <- paste(funname, '.default',sep='')
                    do.call('argsAnywhere', list(fundef))
                }

        if(is.null(args))
            args <- args(fun)
        if(is.null(args))
            args <- do.call('argsAnywhere', list(funname))

        fmls <- formals(args)
        fmls_names <- names(fmls)
        fmls <- gsub('\"', '\\\"',
                     gsub("\\", "\\\\", as.character(fmls), fixed = TRUE),
                     fixed=TRUE)
        args_alist <-
            sprintf("'(%s)",
                    paste("(\"", fmls_names, "\" . \"", fmls, "\")",
                          sep = '', collapse = ' '))
        allargs <-
            if (special) fmls_names
            else tryCatch(gsub(' ?= ?', '', utils:::functionArgs(funname, ''), fixed = FALSE),
                          error=function(e) NULL)
        allargs <- sprintf("'(\"%s\")",
                           paste(allargs, collapse = '\" "'))
        envname <-
            if (is.primitive(fun)) "base"
            else environmentName(environment(fun))
        if (envname == "R_GlobalEnv") envname <- ""
        cat(sprintf('(list \"%s\" %s %s)\n',
                    envname, args_alist, allargs))
    }
}


.ess_get_completions <- function(string, end, suffix = " = ") {
    oldopts <- utils::rc.options(funarg.suffix = suffix)
    on.exit(utils::rc.options(oldopts))
    if(.ess.Rversion > '2.14.1'){
        comp <- compiler::enableJIT(0)
        op <- options(error=NULL)
        on.exit({ options(op); compiler::enableJIT(comp)}, add = TRUE)
    }
    utils:::.assignLinebuffer(string)
    utils:::.assignEnd(end)
    utils:::.guessTokenFromLine()
    utils:::.completeToken()
    c(get('token', envir=utils:::.CompletionEnv),
      utils:::.retrieveCompletions())
}

.ess_arg_help <- function(arg, func){
    op <- options(error=NULL)
    on.exit(options(op))
    fguess <-
        if(is.null(func)) get('fguess', envir=utils:::.CompletionEnv)
        else func
    findArgHelp <- function(fun, arg){
        file <- help(fun, try.all.packages=FALSE)[[1]]
        hlp <- utils:::.getHelpFile(file)
        id <- grep('arguments', tools:::RdTags(hlp), fixed=TRUE)
        if(length(id)){
            arg_section <- hlp[[id[[1]]]]
            items <- grep('item', tools:::RdTags(arg_section), fixed=TRUE)
            ## cat('items:', items, fill=TRUE)
            if(length(items)){
                arg_section <- arg_section[items]
                args <- unlist(lapply(arg_section,
                                      function(el) paste(unlist(el[[1]][[1]], TRUE, FALSE), collapse='')))
                fits <- grep(arg, args, fixed=TRUE)
                ## cat('args', args, 'fits', fill=TRUE)
                if(length(fits))
                    paste(unlist(arg_section[[fits[1]]][[2]], TRUE, FALSE), collapse='')
            }
        }
    }
    funcs <- c(fguess, tryCatch(methods(fguess),
                                warning=function(w) {NULL},
                                error=function(e) {NULL}))
    if(length(funcs) > 1 && length(pos <- grep('default', funcs))){
        funcs <- c(funcs[[pos[[1]]]], funcs[-pos[[1]]])
    }
    i <- 1; found <- FALSE
    out <- 'No help found'
    while(i <= length(funcs) && is.null(out <-
                                            tryCatch(findArgHelp(funcs[[i]], arg),
                                                     warning=function(w) {NULL},
                                                     error=function(e) {NULL})
                                        ))
        i <- i + 1
    cat('\n\n', as.character(out), '\n')
}
