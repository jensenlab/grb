
# ========== utility function ==========
`%<>%` <- magrittr::`%<>%`
`%>%` <- magrittr::`%>%`

expand <- function(x, n) {
  if (!is.null(x)) {
    x <- rep(x, length.out=n)
  }
  x
}

# just like as.XXX, but keeps NULL
# inputs NULL
nullable_assert_factory <- function(as_f) {
  function (x) {
    if (is.null(x)) NULL else as_f(x)
  }
}

assert_integer <- nullable_assert_factory(as.integer)
assert_numeric <- nullable_assert_factory(as.numeric)

as_char <- function(x) {
  if (is.null(x)) {
    NULL
  } else {
    paste0(x, collapse="")
  }
}

names_to_ind <- function(nms, all_names) {
  # remember that GRB models are 0-indexed
  if (!is.character(nms)) {
    return(as.integer(nms - 1))
  }
  sapply(nms, function(x) which(x == all_names) - 1) %>%
    unname() %>%
    as.integer()
}

#' @export GRBenv
GRBenv <- setRefClass(
  "GRBenv",
  fields = list(exptr = "externalptr"),
  methods = list(
    initialize = function(logfilename="") {
      exptr <<- .Call("GRB_loadenv", logfilename)
    },

    loadenv = function(logfilename="") {
      exptr <<- .Call("GRB_loadenv", logfilename)
    }
  )
)

#' @export GRBmodel
GRBmodel <- setRefClass(
  "GRBmodel",
  fields = list(exptr = "externalptr", envptr = "externalptr", auto_update = "logical"),
  methods = list(
    initialize = function(env=GRBenv$new(), name="", numvars=0,
                          obj=NULL, lb=NULL, ub=NULL,
                          vtype=NULL, vnames=NULL, auto_update = TRUE) {
      exptr <<- .Call(
        "GRB_newmodel",
        env$exptr,
        name,
        as.integer(numvars),
        obj,
        lb,
        ub,
        vtype,
        vnames)
      envptr <<- env$exptr
      auto_update <<- auto_update
    },

    update = function() invisible(.Call("GRB_updatemodel", exptr)),
    optimize = function() invisible(.Call("GRB_optimize", exptr)),

    addvar = function(name="", vtype='C',
                      obj=NULL, lb=NULL, ub=NULL,
                      vind=NULL, vval=NULL) {
      error <- .Call(
        "GRB_addvar",
        exptr,
        length(vind),
        as.integer(vind),
        as.numeric(vval),
        assert_numeric(obj),
        assert_numeric(lb),
        assert_numeric(ub),
        vtype,
        name
      )

      if (auto_update) update()
      invisible(error)
    },

    addvars = function(name, vtype='C',
                       obj=NULL, lb=NULL, ub=NULL,
                       vind=NULL, vval=NULL) {
      n <- length(name)
      vtype %<>% expand(n)
      obj %<>% expand(n)
      lb %<>% expand(n)
      ub %<>% expand(n)
      vind %<>% expand(n)
      vval %<>% expand(n)

      errors <- integer(n)
      for (i in 1:n) {
        errors[i] <- addvar(
          name[i], vtype[i], obj[i], lb[i], ub[i], vind[i], vval[i]
        )
      }

      if (auto_update) update()
      invisible(errors)
    },

    addconstr = function(values, sense, rhs, name="", ind=NULL) {
      varnames <- get_names("VarName")
      error <- .Call(
        "GRB_addconstr",
        exptr,
        length(values),
        names_to_ind(names(values), varnames),
        unname(values),
        sense,
        rhs,
        name
      )

      if (auto_update) update()
      invisible(error)
    },

    addqpterms = function(values, var1, var2) {
      varnames <- get_names("VarName")
      error <- .Call(
        "GRB_addqpterms",
        exptr,
        length(values),
        names_to_ind(var1, varnames),
        names_to_ind(var2, varnames),
        values
      )

      if (auto_update) update()
      invisible(error)
    },

    delq = function() invisible(.Call("GRB_delq", exptr)),

    setpwlobj = function(var, x, y) {
      var <- names_to_ind(var, get_names("VarName"))
      error <- .Call("GRB_setpwlobj", exptr, var, length(x), x, y)

      if (auto_update) update()
      invisible(error)
    },

    getattr = function(attrname, named=TRUE) {
      info <- .Call("GRB_getattrinfo", exptr, attrname)
      if (info$datatype == -1) stop("Invalid attribute name: ", attrname)

      if (info$attrtype == 0) { # model attribute
        callname <- switch(info$datatype+1,
                           NULL,  # should not happen; no model char attributes
                           "GRB_getintattr",
                           "GRB_getdblattr",
                           "GRB_getstrattr")
        value <- .Call(callname, exptr, attrname)
        return(value)
      } else { # variable or constraint attribute
        callname <- switch(info$datatype+1,
                           "GRB_getcharattrarray",
                           "GRB_getintattrarray",
                           "GRB_getdblattrarray",
                           "GRB_getstrattrarray")
        n <- get_sizes(
          switch(info$attrtype,
                 "NumVars",
                 "NumConstrs",
                 "NumSOS",
                 "NumQConstrs",
                 "NumGenConstrs")
        )

        values <- .Call(callname, exptr, attrname, 0L, n)
        if (named && info$datatype != 3) { # SOS constraints do not have names
          names(values) <- get_names(
            switch(info$attrtype,
                   "VarName",
                   "ConstrName",
                   NULL,
                   "QCName",
                   "GenConstrName")
          )
        }
        return(values)
      }
    },

    setattr = function(attrname, values, ind=NULL) {
      info <- .Call("GRB_getattrinfo", exptr, attrname)
      if (info$datatype == -1) stop("Invalid attribute name: ", attrname)
      if (info$settable != 1) stop("Attribute ", attrname, " is not settable.")

      if (info$attrtype == 0) { # model attribute
        callname <- switch(info$datatype+1,
                           NULL,  # should not happen; no model char attributes
                           "GRB_setintattr",
                           "GRB_setdblattr",
                           "GRB_setstrattr")
        error <- .Call(callname, exptr, attrname, values)
      } else {
        callname <- switch(info$datatype+1,
                           "GRB_setcharattrlist",
                           "GRB_setintattrlist",
                           "GRB_setdblattrlist",
                           "GRB_setstrattrlist")

        all_names <- get_names(
          switch(info$attrtype,
                 "VarName",
                 "ConstrName",
                 NULL,
                 "QCName",
                 "GenConstrName")
        )
        if (!is.null(names(values))) {
          ind <- names_to_ind(names(values), all_names)
        }

        error <- .Call(callname, exptr, attrname, length(values), ind, values)
      }

      if (auto_update) update()
      invisible(error)
    },

    # ======= convenience functions =======
    get_sizes = function(attrname=NULL) {
      if (is.null(attrname)) {
        to_get <- c("NumVars",
                    "NumConstrs", "NumSOS", "NumQConstrs", "NumGenConstrs",
                    "NumNZs", "NumQNZs", "NumQCNZs",
                    "NumIntVars", "NumBinVars", "NumPWLObjVars")
      } else {
        stopifnot(is.character(attrname))
        to_get <- attrname
      }

      vals <- setNames(
        lapply(to_get, function(x) .Call("GRB_getintattr", exptr, x)),
        to_get
      )

      if (length(to_get) == 1) {
        return(vals[[1]])
      } else {
        return(vals)
      }
    },

    get_names = function(attrname=NULL) {
      sizes <- get_sizes()
      name_lengths <- list(
        VarName = sizes$NumVars,
        ConstrName = sizes$NumConstrs,
        QCName = sizes$NumQConstrs,
        GenConstrName = sizes$NumGenConstrs,
        ObjNName = sizes$NumPWLObjVars
      )
      if (!is.null(attrname)) {
        name_lengths <- name_lengths[attrname]
      }
      all_names <- lapply(names(name_lengths), function(name) {
        .Call("GRB_getstrattrarray", exptr, name, 0L, name_lengths[[name]])
      }) %>% setNames(names(name_lengths))

      if (length(attrname) == 1) {
        return(all_names[[1]])
      } else {
        return(all_names)
      }
    },

    set_model_sense = function(sense=NULL, minimize=FALSE, maximize=FALSE) {
      stopifnot(!is.null(sense) || minimize || maximize)
      if (!is.null(sense)) {
        if (is.numeric(sense)) {
          sense <- as.integer(sense)
        } else if (is.character(sense)) {
          sense <- if (grepl(tolower(sense), "minimize")) 1L else -1L
        } else {
          stop("sense must be numeric or character")
        }
      } else {
        sense <- if (minimize) 1L else -1L
      }
      setattr("ModelSense", sense)
    },

    get_solution = function(named=TRUE) {
      sol_fields <- c("ObjVal", "X", "Runtime", "Status")
      sol <- lapply(sol_fields, function(x) getattr(x, named=named)) %>%
        setNames(sol_fields)
      return(sol)
    },

    show_output = function(tf=TRUE) {
      error <- .Call("GRB_setintparam", exptr, "OutputFlag", as.integer(tf))
      if (auto_update) update()
      invisible(error)
    },

    show = function(tempfile="MODEL_SHOW_TEMP.lp") {
      .Call("GRB_write", exptr, tempfile)
      writeLines(readLines(tempfile))
      file.remove(tempfile)
      invisible(NULL)
    }
  )
)
