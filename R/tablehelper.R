#' @name matrify
#' @title take a data.table/frame, shave first column into rownames, make a matrix
#'
#' @description
#' convenience function to convert to matrix
#' and optionally filter out the first column
#' which may be rownames that are not relevant to further data analysis
#'
#' @param obj a data.frame or matrix
#' @param rm_col1 a logical vector specifying if the 1st column should be removed
#' @return a matrix
#' @export
matrify = function(obj, rm_col1 = TRUE, use.c1.rownames = TRUE) {
    if (rm_col1) {
        if (use.c1.rownames) {
            rn = as.matrix(obj[,1])[,1, drop = TRUE]
        } else {
            rn = NULL
        }
        setRownames(as.matrix(obj[,-1]), rn)
    } else {
        as.matrix(obj)
    }
}


#' @name eNROW
#' @title does vapply NROW
#'
#' @description
#'
#' @export eNROW
eNROW <- function(x) {
    return(vapply(x, NROW, integer(1)))
}




#' @name row.sort
#' @title sort rows of integer matrix
#'
#' @description
#' matrix(a[order(row(a), a)], ncol = ncol(a), byrow = TRUE)
#' ^^ came from stackoverflow
#' https://stackoverflow.com/questions/9506442/fastest-way-to-sort-each-row-of-a-large-matrix-in-r
#'
#' Rfast::rowsort is faster than the base function but it is still very fast
#'
#' @export row.sort
row.sort <- function(a, use_rfast = TRUE) {
    out = tryCatch(Rfast::rowSort(a),
                   error = function(e) matrix(a[order(row(a), a)], ncol = ncol(a), byrow = TRUE))
    return(out)
}


#' @name rows.all
#' @title test whether all row entries are TRUE
#'
#' @description
#'
#' @export rows.all
rows.all <- function(mat) {
    vec = logical(NROW(mat))
    for (i in seq_len(NROW(mat))) {
        vec[i] = all(mat[i,])
    }
    return(vec)
}

#' @name rows.any
#' @title test whether any row entries are TRUE
#'
#' @description
#'
#' @export rows.any
rows.any <- function(mat) {
    vec = logical(NROW(mat))
    for (i in seq_len(NROW(mat))) {
        vec[i] = any(mat[i,])
    }
    return(vec)
}


#' @name copydt
#' @title copy data frame/table columns to a new data table with forced column structure
#'
#' @description
#' Ensure that all columns in out data table possess the specified columns
#' in which default values for missing columns will be NA valuess
#'
#' @export
copydt = function(dt, columns, as.data.table = FALSE) {
    out = data.frame()[seq_len(max(NROW(dt), 1)),]
    ix = seq_len(NROW(columns))
    outname = names(columns)
    badnames = !nzchar(outname) | is.na(outname)
    if (is.null(outname)) outname = columns
    if (any(badnames)) outname[badnames] = columns[badnames]
    for (i in ix) {
        cn = columns[i]
        nm = outname[i]
        if (is.null(dt[[cn]]))
            out[[nm]] = NA
        else
            out[[nm]] = rep_len(dt[[cn]], NROW(out))
    }
    if (NROW(dt) == 0) {
        out = out[0,,drop=F]
    }
    if (as.data.table) {
        setDT(out)
    }
    return(out)
}


#' @name dedup.cols
#' @title applies dedup to colnames
#'
#' dedup the column names of a data.frame/data.table
#'
#' @return A data.table or data.frame
#' @export dedup.cols
dedup.cols = function(tbl, remove = FALSE) {
    if (remove) {
        if (!inherits(tbl, "data.table"))
            return(tbl[, match(unique(colnames(tbl)), colnames(tbl))])
        else
            return(tbl[, match(unique(colnames(tbl)), colnames(tbl)), with = FALSE])
    } else {
            colnames(tbl) = make.unique(colnames(tbl))
            return(tbl)
    }
}



#' @name do.assign
#' @title assign columns or list elements
#'
#'
#'
#' @author Kevin Hadi
#' @export do.assign
do.assign = function(x, ..., pf = parent.frame()) {
  mc_2340873450987 = match.call(expand.dots = FALSE)
  ddd = as.list(mc_2340873450987)$`...`
  if (is.null(names(ddd))) names(ddd) = paste0(rep_len("V", length(ddd)), seq_along(ddd))
  for (i in seq_along(ddd)) {
    d = ddd[[i]]
    nml = names(ddd[i])
    if (is.call(d) || is.name(d)) {
      ev = BiocGenerics::eval(d, envir = parent.frame())
      nm = names(ev)
      .DIM = DIM2(ev)
      .dim = dim(ev)
      nr = .DIM[1L]
      nc = .DIM[2L]
      if (inherits(ev, c("list")) && nc == 1L) {
        if (is.null(nm)) nm = rep_len(nml, nr)
        for (ii in seq_len(nr)) {
          x[[nm[ii]]] = ev[[ii]]
        }
      } else if (length(.dim) > 0L) {
        if (is.null(nm)) nm = rep_len(nml, nc)
        for (ii in seq_len(nc)) {
          x[[nm[ii]]] = ev[[ii]]
        }
      } else {
        x[[nml]] = ev
      }
    }
  }
  return(x)
}

#' @name complete.cases2
#' @title complete.cases wrapper
#'
#' @description
#' can use this within data.table
#'
#' @export complete.cases2
complete.cases2 = function(...) {
    complete.cases(as.data.frame(list(...)))
}
