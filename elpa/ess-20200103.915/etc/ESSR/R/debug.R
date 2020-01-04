### BREAKPOINTS
.ESSBP. <- new.env()

### DEBUG/UNDEBUG
.ess_find_funcs <- function(env) {
    objs <- ls(envir = env, all.names = TRUE)
    if (length(objs) > 0)
        objs <- objs[sapply(objs, exists, envir = env,
                            mode = 'function', inherits = FALSE)]
    objs
}

.ess_all_functions <- function(packages = c(), env = NULL) {
    if(is.null(env))
        env <- parent.frame()
    empty <- emptyenv()
    coll <- list()
    for(p in packages){
        ## package might not be attached
        try(
        {
            objNS <- .ess_find_funcs(asNamespace(p))
            objPKG <- .ess_find_funcs(as.environment(paste0('package:', p)))
            objNS <- setdiff(objNS, objPKG)
            if(length(objPKG))
                coll[[length(coll) + 1]] <- paste0(p, ':::', objNS)
        }, silent = TRUE)
    }
    while(!identical(empty, env)){
        coll[[length(coll) + 1]] <- .ess_find_funcs(env)
        env <- parent.env(env)
    }
    grep('^\\.ess', unlist(coll, use.names = FALSE),
         invert = TRUE, value = TRUE)
}


.ess_dbg_flag_for_debuging <- function(fname){
    all <- utils::getAnywhere(fname)
    if(length(all$obj) == 0){
        msg <- sprintf("No functions names '%s' found", fname)
    } else {
        msg <- sprintf("Flagged '%s' for debugging", fname)
        tryCatch(lapply(all$obj, debug),
                 error = function(e){
                     msg <- paste0("Error: ", e$message)
                 })
    }
    cat(msg)
    .ess_mpi_message(msg)
}

.ess_dbg_getTracedAndDebugged <- function()
{
    packages <- base::.packages()
    tr_state <- tracingState(FALSE)
    on.exit(tracingState(tr_state))
    generics <- methods::getGenerics()
    all_traced <- c()
    for(i in seq_along(generics)){
        genf <- methods::getGeneric(generics[[i]],
                                    package=generics@package[[i]])
        if(!is.null(genf)){ ## might happen !! v.2.13
            menv <- methods::getMethodsForDispatch(genf)
            traced <- unlist(eapply(menv, is, 'traceable', all.names=TRUE))
            if(length(traced) && any(traced))
                all_traced <- c(paste(generics[[i]],':',
                                      names(traced)[traced],sep=''), all_traced)
            tfn <- getFunction(generics[[i]], mustFind=FALSE, where = .GlobalEnv)
            if(!is.null(tfn ) && is(tfn,  'traceable')) # if the default is traced,  it does not appear in the menv :()
                all_traced <- c(generics[[i]], all_traced)
        }
    }
    debugged_pkg <- unlist(lapply(packages, function(pkgname){
        ns <- asNamespace(pkgname)
        funcs <- .ess_find_funcs(ns)
        dbged <- funcs[unlist(lapply(funcs,
                                     function(f){
                                         isdebugged(get(f, envir = ns, inherits = FALSE))
                                     }))]
        if(length(dbged))
            paste0(pkgname, ':::`', dbged, '`')
    }))
    env <- parent.frame()
    ## traced function don't appear here. Not realy needed and would affect performance.
    all <- .ess_all_functions(packages = packages, env = env)
    which_deb <- lapply(all, function(nm){
        ## if isdebugged is called with string it doess find
        tryCatch(isdebugged(get(nm, envir = env)),
                 error = function(e) FALSE)
        ## try(eval(substitute(isdebugged(nm), list(nm = as.name(nm)))), silent = T)
    })
    debugged <- all[which(unlist(which_deb, recursive=FALSE, use.names=FALSE))]
    unique(c(debugged_pkg, debugged, all_traced))
}


.ess_dbg_UntraceOrUndebug <- function(name, env = parent.frame()) {
    tr_state <- tracingState(FALSE)
    on.exit(tracingState(tr_state))
    if( grepl('::', name) ){
        ## foo:::bar name
        eval(parse(text = sprintf('undebug(%s)', name)))
    }else{
        ## name is a name of a function to be undebugged or has a form
        ## name:Class1#Class2#Class3 for traced methods
        name <- strsplit(name, ':', fixed = TRUE)[[1]]
        if( length(name)>1 ){
            ## a method
            fun <- name[[1]]
            sig <- strsplit(paste(name[-1], collapse=''), '#', fixed=TRUE)[[1]]
            untrace(fun, signature = sig)
        }else{
            ## function
            if( is(getFunction(name, where = parent.frame()), 'traceable') )
                untrace(name)
            else if(grepl(":", name))
                undebug(name)
            else
                undebug(get(name, envir = env))
        }}
}

.ess_dbg_UndebugALL <- function(funcs)
{
    tr_state <- tracingState(FALSE)
    on.exit(tracingState(tr_state))
    env <- parent.frame()
    invisible(lapply(funcs, function( nm ) {
        ## ugly tryCatch, but there might be several names pointing to the
        ## same function, like foo:::bar and bar. An alternative would be
        ## to call .ess_dbg_getTracedAndDebugged each time but that might
        ## be ery slow
        try(.ess_dbg_UntraceOrUndebug(nm, env = env), TRUE)
    }))
}


### WATCH
.ess_watch_expressions <- list()

.ess_watch_eval <- function()
{
    env <- as.environment("ESSR")
    exps <- get('.ess_watch_expressions', envir = env)
    if(length(exps) == 0) {
        ## using old style so this can be parsed by R 1.9.1 (e.g):
        cat('\n# Watch list is empty!\n',
            '# a       append new expression',
            '# i       insert new expression',
            '# k       kill',
            '# e       edit the expression',
            '# r       rename',
            '# n/p     navigate',
            '# u/d,U   move the expression up/down',
            '# q       kill the buffer',
            sep="\n")
    } else {
        .parent_frame <- parent.frame()
        .essWEnames <- allNames(exps)
        len0p <- !nzchar(.essWEnames)
        .essWEnames[len0p] <- seq_along(len0p)[len0p]
        for(i in seq_along(exps)) {
            cat('\n@---- ', .essWEnames[[i]], ' ',
                rep.int('-', max(0, 35 - nchar(.essWEnames[[i]]))), '-@\n', sep = '')
            cat(paste('@---:', deparse(exps[[i]][[1]])), ' \n', sep = '')
            tryCatch(print(eval(exps[[i]],
                                envir = .parent_frame)),
                     error = function(e) cat('Error:', e$message, '\n' ),
                     warning = function(w) cat('warning: ', w$message, '\n' ))
        }
    }
}


.ess_watch_assign_expressions <- function(elist) {
    assign(".ess_watch_expressions", elist, envir = as.environment("ESSR"))
}

.ess_log_eval <- function(log_name) {
    env <- as.environment("ESSR")
    if(!exists(log_name, envir = env, inherits = FALSE))
        assign(log_name, list(), envir = env)
    log <- get(log_name, envir = env, inherits = FALSE)
    .essWEnames <- allNames(.ess_watch_expressions)
    cur_log <- list()
    .parent_frame <- parent.frame()
    for(i in seq_along(.ess_watch_expressions)) {
        capture.output( {
            cur_log[[i]] <-
                tryCatch(eval(.ess_watch_expressions[[i]]),
                         envir = .parent_frame,
                         error = function(e) paste('Error:', e$message, '\n'),
                         warning = function(w) paste('warning: ', w$message, '\n'))
            if(is.null(cur_log[i][[1]]))
                cur_log[i] <- list(NULL)
        })
    }
    names(cur_log) <- .essWEnames
    assign(log_name, c(log, list(cur_log)), envir = env)
    invisible(NULL)
}


.ess_package_attached <- function(pack_name){
    as.logical(match(paste0("package:", pack_name), search()))
}

## magrittr debug_pipe
.ess_pipe_browser <- function(x){
    if(is.list(x))
        evalq({
            browser(skipCalls = 2)
            x
        }, envir = x)
    else if(is.environment(x))
        ## enclos argumentn has no effect for unclear reason, need to hack
        eval(bquote({
            x <- .(environment())
            browser(skipCalls = 2)
            x
        }), envir = x)
    else {
        browser(skipCalls = 0)
        x
    }
}
