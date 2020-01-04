## NOTE ON S3 METHODS: New S3 methods are not automatically registered. You can
## register them manually after you have inserted method_name.my_class into your
## package environment using ess-developer, like follows:
##
##    registerS3method("method_name", "my_class", my_package:::method_name.my_class)
##
## If an S3 methods already exists in a package, ESS-developer will do the right
## thing.

## evaluate the STRING by saving into a file and calling .ess.ns_source
.ess.ns_eval <- function(string, visibly, output, package,
                         file = tempfile("ESSDev"), verbose = FALSE,
                         fallback_env = NULL, local_env = parent.frame()) {
    cat(string, file = file)
    on.exit(.ess.file.remove(file))
    .ess.ns_source(file, visibly, output, package = package,
                   verbose = verbose, fake.source = TRUE,
                   fallback_env = fallback_env, local_env = local_env)
}


##' Source FILE into an environment. After having a look at each new object in
##' the environment, decide what to do with it. Handles plain objects,
##' functions, existing S3 methods, S4 classes and methods.
##' @param fallback_env environment to assign objects which don't exist in the
##'     package namespace
.ess.ns_source <- function(file, visibly, output, expr,
                           package = "", verbose = FALSE,
                           fake.source = FALSE,
                           fallback_env = NULL,
                           local_env = NULL) {
    oldopts <- options(warn = 2)
    on.exit(options(oldopts))
    pname <- paste("package:", package, sep = "")
    envpkg <- tryCatch(as.environment(pname), error = function(cond) NULL)
    if (is.null(envpkg))
        if (require(package, quietly = TRUE, character.only = TRUE)) {
            envpkg <- tryCatch(as.environment(pname), error = function(cond) NULL)
        } else {
            ## no such package; source in current (local) user environment
            return(.ess.source(file, visibly = visibly,
                               output = output, local = local_env,
                               fake.source = fake.source))
        }

    envns <- tryCatch(asNamespace(package), error = function(cond) NULL)
    if (is.null(envns))
        stop(gettextf("Can't find a namespace environment corresponding to package name '%s\"",
                      package), domain = NA)

    ## Here we know that both envns and envpkg exists and are environments
    if (is.null(fallback_env))
        fallback_env <- .ess.ns_insert_essenv(envns)

    ## Get all Imports envs where we propagate objects
    pkgEnvNames <- Filter(.ess.is_package, search())
    packages <- lapply(pkgEnvNames, function(envName) substring(envName, 9))
    importsEnvs <- lapply(packages, function(pkgName) parent.env(asNamespace(pkgName)))

    ## Evaluate the FILE into new ENV
    env <- .ess.ns_evalSource(file, visibly, output, substitute(expr), package, fake.source)
    envPackage <- getPackageName(env, FALSE)
    if (nzchar(envPackage) && envPackage != package)
        warning(gettextf("Supplied package, %s, differs from package inferred from source, %s",
                         sQuote(package), sQuote(envPackage)), domain = NA)

    ## Get all sourced objects, methods and classes
    allObjects <- objects(envir = env, all.names = TRUE)
    allObjects <- allObjects[!(allObjects %in% c(".cacheOnAssign", ".packageName"))]
    MetaPattern <- methods:::.TableMetaPattern()
    ClassPattern <- methods:::.ClassMetaPattern()
    allPlainObjects <- allObjects[!(grepl(MetaPattern, allObjects) |
                                    grepl(ClassPattern, allObjects))]
    allMethodTables <- allObjects[grepl(MetaPattern, allObjects)]
    allClassDefs <- allObjects[grepl(ClassPattern, allObjects)]

    ## PLAIN OBJECTS and FUNCTIONS:
    funcNs <- funcPkg <- newFunc <- newNs <- newObjects <- newPkg <- objectsNs <- objectsPkg <- character()
    dependentPkgs <- list()

    for (this in allPlainObjects) {
        thisEnv <- get(this, envir = env)
        thisNs <- NULL

        ## NS
        if (exists(this, envir = envns, inherits = FALSE)){
            thisNs <- get(this, envir = envns)
            if(is.function(thisNs) || is.function(thisEnv)){
                if(is.function(thisNs) && is.function(thisEnv)){
                    if(.ess.differs(thisEnv, thisNs)){
                        environment(thisEnv) <- environment(thisNs)
                        .ess.assign(this, thisEnv, envns)
                        funcNs <- c(funcNs, this)
                        if(exists(".__S3MethodsTable__.", envir = envns, inherits = FALSE)){
                            S3_table <- get(".__S3MethodsTable__.", envir = envns)
                            if(exists(this, envir = S3_table, inherits = FALSE))
                                .ess.assign(this, thisEnv, S3_table)
                        }
                    }
                }else{
                    newNs <- c(newNs, this)
                }
            }else{
                if(!identical(thisEnv, thisNs)){
                    .ess.assign(this, thisEnv, envns)
                    objectsNs <- c(objectsNs, this)
                }
            }
        }else{
            newNs <- c(newNs, this)
        }

        ## PKG
        if (exists(this, envir = envpkg, inherits = FALSE)){
            thisPkg <- get(this, envir = envpkg)
            if(is.function(thisPkg) || is.function(thisEnv)){
                if(is.function(thisPkg) && is.function(thisEnv)){
                    if(.ess.differs(thisPkg, thisEnv)){
                        environment(thisEnv) <- environment(thisPkg)
                        .ess.assign(this, thisEnv, envpkg)
                        funcPkg <- c(funcPkg, this)
                    }
                }else{
                    newPkg <- c(newPkg, this)
                }
            }else{
                if(!identical(thisPkg, thisEnv)){
                    .ess.assign(this, thisEnv, envpkg)
                    objectsPkg <- c(objectsPkg, this)
                }
            }
        }else{
            newPkg <- c(newPkg, this)
        }

        if (!is.null(thisNs)) {
            isDependent <- .ess.ns_propagate(thisEnv, this, importsEnvs)
            newDeps <- stats::setNames(list(packages[isDependent]), this)
            dependentPkgs <- c(dependentPkgs, newDeps)
        }
    }

    ## deal with new plain objects and functions
    for (this in intersect(newPkg, newNs)) {
        thisEnv <- get(this, envir = env, inherits = FALSE)
        if (exists(this, envir = fallback_env, inherits = FALSE)){
            thisGl <- get(this, envir = fallback_env)
            if (.ess.differs(thisEnv, thisGl)) {
                if (is.function(thisEnv)) {
                    environment(thisEnv) <- envns
                    newFunc <- c(newFunc, this)
                } else {
                    newObjects <- c(newObjects, this)
                }
                .ess.assign(this, thisEnv, fallback_env)
                if (.is.essenv(fallback_env))
                    .ess.assign(this, thisEnv, .GlobalEnv)
            }
        } else {
            if (is.function(thisEnv)) {
                environment(thisEnv) <- envns
                newFunc <- c(newFunc, this)
            } else {
                newObjects <- c(newObjects, this)
            }
            .ess.assign(this, thisEnv, fallback_env)
            if (.is.essenv(fallback_env))
                .ess.assign(this, thisEnv, .GlobalEnv)
        }
    }

    if(length(funcNs))
        objectsNs <- c(objectsNs, sprintf("FUN[%s]", paste(funcNs, collapse = ", ")))
    if(length(funcPkg))
        objectsPkg <- c(objectsPkg, sprintf("FUN[%s]", paste(funcPkg, collapse = ", ")))
    if(length(newFunc))
        newObjects <- c(newObjects, sprintf("FUN[%s]", paste(newFunc, collapse = ", ")))

    ## CLASSES
    classesPkg <- classesNs <- newClasses <- character()
    for(this in allClassDefs){
        newPkg <- newNs <- FALSE
        thisEnv <- get(this, envir = env)
        if(exists(this, envir = envpkg, inherits = FALSE)){
            if(!.ess.identicalClass(thisEnv, get(this, envir = envpkg))){
                .ess.assign(this, thisEnv, envir = envpkg)
                classesPkg <- c(classesPkg, this)
            }
        }else{
            newPkg <- TRUE
        }
        if(exists(this, envir = envns, inherits = FALSE)){
            if(!.ess.identicalClass(thisEnv, get(this, envir = envns))){
                .ess.assign(this, thisEnv, envir = envns)
                classesNs <- c(classesNs, this)
            }
        }else{
            newNs <- TRUE
        }
        if(newNs && newPkg){
            if(exists(this, envir = fallback_env, inherits = FALSE)){
                if(!.ess.identicalClass(thisEnv, get(this, envir = fallback_env))){
                    .ess.assign(this, thisEnv, envir = fallback_env)
                    newClasses <- c(newClasses, this)
                }
            }else{
                .ess.assign(this, thisEnv, envir = fallback_env)
                newClasses <- c(newClasses, this)
            }
        }
    }
    if(length(classesPkg))
        objectsPkg <- gettextf("CLS[%s]", sub(ClassPattern, "", paste(classesPkg, collapse = ", ")))
    if(length(classesNs))
        objectsNs <- gettextf("CLS[%s]", sub(ClassPattern, "", paste(classesNs, collapse = ", ")))
    if(length(newClasses))
        newObjects <- gettextf("CLS[%s]", sub(ClassPattern, "", paste(newClasses, collapse = ", ")))

    ## METHODS:
    ## Method internals: For efficiency reasons setMethod() caches
    ## method definition into a global table which you can get with
    ## 'getMethodsForDispatch' function, and when a method is dispatched that
    ## table is used. When ess-developer is used to source method definitions the
    ## two copies of the functions are identical up to the environment. The
    ## environment of the cached object has namespace:foo as it's parent but the
    ## environment of the object in local table is precisely namspace:foo. This
    ## does not cause any difference in evaluation.
    methodNames <- allMethodTables
    methods <- sub(methods:::.TableMetaPrefix(), "", methodNames)
    methods <- sub(":.*", "", methods)
    methodsNs <- newMethods <- character()
    for (i in seq_along(methods)){
        table <- methodNames[[i]]
        tableEnv <- get(table,  envir = env)
        if(exists(table,  envir = envns, inherits = FALSE)){
            inserted <- .ess.ns_insertMethods(tableEnv, get(table, envir = envns), envns)
            if(length(inserted))
                methodsNs <- c(methodsNs,  gettextf("%s{%s}", methods[[i]], paste(inserted, collapse = ", ")))
        }else if(exists(table,  envir = fallback_env, inherits = FALSE)){
            inserted <- .ess.ns_insertMethods(tableEnv, get(table, envir = fallback_env), envns)
            if(length(inserted))
                newMethods <- c(newMethods,  gettextf("%s{%s}", methods[[i]], paste(inserted, collapse = ", ")))
        }else{
            .ess.assign(table, tableEnv, envir = fallback_env)
            newMethods <- c(newMethods,  gettextf("%s{%s}", methods[[i]], paste(objects(envir = tableEnv, all.names = T), collapse = ", ")))
        }
    }
    if(length(methodsNs))
        objectsNs <- c(objectsNs, gettextf("METH[%s]", paste(methodsNs, collapse = ", ")))
    if(length(newMethods))
        newObjects <- c(newObjects, gettextf("METH[%s]", paste(newMethods, collapse = ", ")))

    if (verbose) {
        msgs <- unlist(list(
            if(length(objectsPkg))
                sprintf("PKG: %s", paste(objectsPkg, collapse = ", ")),
            if(length(objectsNs))
                sprintf("NS: %s", paste(objectsNs, collapse = ", ")),
            if(length(dependentPkgs))
                .ess.ns_format_deps(dependentPkgs),
            if(length(newObjects)) {
                env_name <- .ess.ns_env_name(fallback_env)
                sprintf("%s: %s", env_name, paste(newObjects, collapse = ", "))
            }))
        if(length(msgs))
            .ess_mpi_message(paste(msgs, collapse = "  "))

    }

    invisible(env)
}


.ess.ns_insertMethods <- function(tableEnv,  tablePkg, envns) {
    inserted <- character()
    for(m in ls(envir = tableEnv, all.names = T)){
        if(exists(m, envir = tablePkg, inherits = FALSE)){
            thisEnv <- get(m, envir = tableEnv)
            thisPkg <- get(m, envir = tablePkg)
            if(is(thisEnv, "MethodDefinition") && is(thisPkg, "MethodDefinition") &&
               .ess.differs(thisEnv@.Data, thisPkg@.Data)){
                environment(thisEnv@.Data) <- envns
                ## environment of cached method in getMethodsForDispatch table is still env
                ## not a problem as such,  but might confuse users
                .ess.assign(m, thisEnv, tablePkg)
                inserted <- c(inserted, m)
            }}}
    inserted
}

## our version of R's evalSource
.ess.ns_evalSource <- function(file, visibly, output, expr, package = "",
                               fake.source = FALSE) {
    envns <- tryCatch(asNamespace(package), error = function(cond) NULL)
    if(is.null(envns))
        stop(gettextf("Package \"%s\" is not attached and no namespace found for it",
                      package), domain = NA)
    env <- new.env(parent = envns)
    env[[".packageName"]] <- package
    methods:::setCacheOnAssign(env, TRUE)
    if (missing(file))
        eval(expr, envir = env)
    else  if (is(file, "character"))
        for (f in file) {
            .ess.source(f, local = env, visibly = visibly,
                        output = output, keep.source = TRUE,
                        max.deparse.length = 300,
                        fake.source = fake.source,
                        message.prefix = sprintf("[%s] ", package))
        }
    else stop(gettextf("Invalid file argument:  got an object of class \"%s\"",
                       class(file)[[1]]), domain = NA)
    env
}


.ess.assign <- function(x, value, envir) {
    ## Cannot add bindings to locked environments
    exists <- exists(x, envir = envir, inherits = FALSE)
    if (exists && bindingIsLocked(x, envir)) {
        unlockBinding(x, envir)
        assign(x, value, envir = envir, inherits = FALSE)
        op <- options(warn = -1)
        on.exit(options(op))
        lockBinding(x, envir)
    } else if (exists || !environmentIsLocked(envir)) {
        assign(x, value, envir = envir, inherits = FALSE)
    } else {
        warning(sprintf("Cannot assign `%s` in locked environment", x),
                call. = FALSE)
    }
    invisible(NULL)
}

.ess.identicalClass <- function(cls1, cls2, printInfo = FALSE) {
    slots1 <- slotNames(class(cls1))
    slots2 <- slotNames(class(cls2))
    if(identical(slots1, slots2)){
        vK <- grep("versionKey", slots1)
        if(length(vK))
            slots1 <- slots2 <- slots1[-vK]
        out <- sapply(slots1, function(nm) identical(slot(cls1, nm), slot(cls2, nm)))
        if(printInfo) print(out)
        all(out)
    }
}

.ess.differs <- function(f1, f2) {
    if (is.function(f1) && is.function(f2)){
        !(identical(body(f1), body(f2)) && identical(args(f1), args(f2)))
    }else
        !identical(f1, f2)
}


.ess.is_package <- function(envName) {
  isPkg <- identical(substring(envName, 0, 8), "package:")
  isPkg && (envName != "package:base")
}

.ess.ns_propagate <- function(obj, name, importsEnvs) {
  containsObj <- vapply(importsEnvs, logical(1), FUN = function(envs) {
    name %in% names(envs)
  })

  lapply(importsEnvs[containsObj], .ess.assign,
         x = name, value = obj)

  containsObj
}


.ess.ns_format_deps <- function(dependentPkgs) {
    pkgs <- unique(unlist(dependentPkgs, use.names = FALSE))

    lapply(pkgs, function(pkg) {
        isDep <- vapply(dependentPkgs, function(deps) pkg %in% deps, logical(1))
        pkgDependentObjs <- names(dependentPkgs[isDep])
        sprintf("DEP:%s [%s]   ", pkg, paste(pkgDependentObjs, collapse = ", "))
    })
}

.ess.ns_env_name <- function(env) {
    name <- environmentName(env)
    name <-
        if (name == "") "Local"
        else if (grepl("^essenv:", name)) "NEW"
        else name
    name
}


.ess.ns_insert_essenv <- function(nsenv) {
    if (is.character(nsenv))
        nsenv <- base::asNamespace(nsenv)
    stopifnot(isNamespace(nsenv))
    if (identical(nsenv, .BaseNamespaceEnv))
        return(.GlobalEnv)
    essenv_name <- sprintf("essenv:%s", environmentName(nsenv))
    nsenv_parent <- parent.env(nsenv)
    if (environmentName(nsenv_parent) == essenv_name) {
        return(nsenv_parent)
    }
    essenv <- new.env(parent = nsenv_parent)
    essenv[[".__ESSENV__."]] <- TRUE
    attr(essenv, "name") <- essenv_name
    nssym <- ".__NAMESPACE__."
    nssym_val <- get(nssym, envir = nsenv, inherits = FALSE)
    unlockBinding(nssym, nsenv)
    nsenv[[nssym]] <- NULL
    on.exit({
        nsenv[[nssym]] <- nssym_val
        lockBinding(nssym, nsenv)
    })
    parent.env(nsenv) <- essenv
    essenv
}

.is.essenv <- function(env) {
    exists(".__ESSENV__.", envir = env, inherits = FALSE)
}
