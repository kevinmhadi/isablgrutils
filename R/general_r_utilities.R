#' @name NCOL2
#' @title extending NCOL
#'
#' @description
#' NCOL = 1 for NULL, or any vector with length == 0
#' seems counterintuitive so this is the fix
#' 
#'
#' @export
NCOL2 <- function(x) {
  d = dim(x)
  ln = length(d)
  lx = length(x)
  if (ln > 1L) {
    d[2L]
  } else if (lx == 0L) {
    0L
  } else {
    1L
  }
}

#' @name DIM
#' @title extending NROW and NCOL
#'
#' @description
#' 
#'
#' @export
DIM = function(x) {
    return(c(NROW(x), NCOL(x)))
}

#' @name DIM2
#' @title extending NROW and NCOL2
#'
#' @description
#' 
#'
#' @export
DIM2 <- function(x) {
    return(c(NROW(x), NCOL2(x)))
}


#' @name match3
#' @title similar to setkey except a general use utility
#'
#' very slow version of keying a la data.table
#' but for general/interactive use
#'
#' @export
match3 = function(x, table, nomatch = NA_integer_, use.data.table = TRUE) {
    out = if (use.data.table) {
              tryCatch({
                  dx = data.table(x = x)[, id.x := seq_len(.N)]
                  dtb = data.table(table = table)[, id.tb := seq_len(.N)]
                  ## setkey(dx, x)[list(dtb$table)]$id.x
                  setkey(dtb, table)[list(dx$x)]$id.tb
              }, error = function(e) structure("err", class = "err"))
          }
    if (!is.null(out) && !inherits(out, "err")) return(out)
    dx = within(data.frame(x = x), {id.x = seq_along(x)})
    dtb = within(data.frame(table = table), {id.tb = seq_along(table)})
    res = merge(dx, dtb, by.x = "x", by.y = "table", all.x = TRUE,
                allow.cartesian = TRUE)
    return(res$id.tb[order(res$id.x)])
}


#' @name names2
#' @title robust name()
#'
#' gives back character vector same length of input regardless whether named or not
#'
#' @param str a path string
#' @return a string with multiple parentheses replaced with a single parenthesis
#' @export
names2 = function(x) {
    nm = names(x)
    if (is.null(nm))
        return(rep("", length.out = length(x)))
    else
        return(nm)
}

#' @name rownames2
#' @title robust rownames
#'
#' gives back character vector same number of rows of input regardless whether named or not
#'
#' @param str a path string
#' @return a string with multiple parentheses replaced with a single parenthesis
#' @export
rownames2 = function(x) {
    if (!is.null(dim(x)))
        nm = rownames(x)
    else
        nm = names(x)
    if (is.null(nm))
        return(rep("", length.out = len(x)))
    else
        return(rep(nm, length.out = len(x)))
}

#' @name colnames2
#' @title robust colnames
#'
#' gives back character vector same number of columns of input regardless whether named or not
#'
#' @param str a path string
#' @return a string with multiple parentheses replaced with a single parenthesis
#' @export
colnames2 = function(x) {
    nm = colnames(x)
    if (is.null(nm))
        return(rep("", length.out = NCOL(x)))
    else
        return(nm)
}


#' @name column_to_rownames
#' @title making column into rownames
#'
#' internal version that doesn't require library(tibble)
#'
#' @param .data a data frame/table
#' @return a data frame/table with rownames from a column
#' @export
column_to_rownames = function(.data, var = "rowname", force = T, sep = " ") {
  ## if (inherits(.data, c("data.frame", "DFrame"))) {
  if (!is.null(dim(.data))) {
    tmpfun = function(...) paste(..., sep = sep)
    if (!is.null(rownames(.data)) || force) {
      ## rn = .data[[var]]
      if (is.numeric(var)) {
        eva = eval(parse(text = paste(".data[,", paste("c(", paste0(var, collapse = ", "), ")"), ",drop=FALSE]")))
        if (ncol(eva) > 1) eva = dodo.call2(dg(tmpfun), eva)
        rn = unname(unlist(eva))
        colix = setdiff(seq_len(ncol(.data)), var)
      } else if (is.character(var)) {
        eva = eval(parse(text = paste(".data[,", paste("c(", paste0(paste0("\"", var, "\""), collapse = ", "), ")"), ",drop=FALSE]")))
        if (ncol(eva) > 1) eva = dodo.call2(dg(tmpfun), eva)
        rn = unname(unlist(eva))
        colix = setdiff(seq_len(ncol(.data)), match3(var,colnames(.data)))
      }
      eval(parse(text = paste(".data = .data[,", paste("c(", paste0(colix, collapse = ", "), ")"), ", drop = FALSE]")))
      ## .data = .data[, colix,drop = FALSE]
      if (inherits(.data, "tbl"))
        .data = as.data.frame(.data)
      if (inherits(.data, "data.frame")) {
        rownames(.data) = make.unique(replace(as.character(rn), is.na(rn), "NA"))
      } else {
        rownames(.data) = replace(as.character(rn), is.na(rn), "NA")
      }
      return(.data)
    } else
      return(.data)
  } else
    stop("must be a data frame-like object")
}

#' @name col2rn
#' @title alias for column_to_rownames
#'
#' internal version that doesn't require library(tibble)
#'
#' @param .data a data frame/table
#' @return a data frame/table with rownames from a column
#' @export
col2rn = column_to_rownames


#' @name rownames_to_column
#' @title making column out of rownames
#'
#' internal version that doesn't require library(tibble)
#'
#' @param .data a data frame/table
#' @return a data frame/table with the rownames as an additional column
#' @export
rownames_to_column = function(.data, var = "rowname", keep.rownames = FALSE,
                              asdf = as.data.frame, as.data.frame = FALSE) {
    ## if (inherits(.data, c("data.frame", "DFrame"))) {
    as.data.frame = asdf
    if (!is.null(dim(.data))) {
        if (!is.null(rownames(.data))) {
            rn = rownames(.data)
            if (as.data.frame)
                .data = cbind(u.var5912349879872349876 = rn, as.data.frame(.data, row.names = make.unique(rn)))
            else
                .data = cbind(u.var5912349879872349876 = rn, .data)
            colnames(.data)[1] = var
            if (keep.rownames)
                rownames(.data) = rn
            return(.data)
        } else
            return(.data)
    } else
        stop("must be a data frame-like object")
}

#' @name rn2col
#' @title alias for rownames_to_column
#'
#' internal version that doesn't require library(tibble)
#'
#' @param .data a data frame/table
#' @return a data frame/table with rownames from a column
#' @export
rn2col = rownames_to_column



#' @name setColnames
#' @title convenience function to set column names
#'
#' @param object tabled object
#' @param nm names of the new columns
#' @return colnamed object
#' @export
setColnames = function(object = nm, nm = NULL, pattern = NULL, replacement = "") {
    if (!is.null(nm)) {
        if (is.null(names(nm)))
            colnames2(object)  = nm
        else {
            ix = match3(names(nm), colnames(object))
            colnames2(object)[ix] = nm
        }
    } else if (!is.null(pattern)) {
        colnames2(object) = gsub(pattern, replacement, colnames2(object))
    }
    return(object)
}

#' @name setcolnames
#' @title convenience function to set column names
#'
#' alias of setColnames
#'
#' @param object tabled object
#' @param nm names of the new columns
#' @return colnamed object
#' @export
setcolnames = setColnames


#' @name setRownames
#' @title convenience function to set row names
#'
#' sets rownames of an object
#'
#' @param object tabled object
#' @param nm names of the new columns
#' @return rownamed object
#' @export
setRownames = function(object = nm, nm) {
    rownames2(object) = nm
    object
}


#' @name setrownaes
#' @title convenience function to set row names
#'
#' sets rownames of an object
#'
#' @param object tabled object
#' @param nm names of the new columns
#' @return rownamed object
#' @export
setrownames = setRownames


#' @name setcols
#' @title convenience function to set columns
#'
#' sets columns of an object
#'
#' @param dt data frame/table or matrix
#' @param old integer or character or logical vector corresponding to current colnames in dt
#' @param new character vector for new column names
#' @return colnamed object
#' @export
setcols = function(dt, old, new) {
  if (inherits(dt, c("GRanges", "GRangesList"))) {
    mcols(dt) = setcols(mcols(dt), old, new)
    return(dt)
  }
  cnames = colnames2(dt)
  if (missing(new) || missing(old)) {
    if (missing(old)) {
      old = new
    }
    if (is.character(old) && length(old) == length(cnames)) {
      colnames(dt) = old
      return(dt)
    } else {
      stop("names provided must be same length as ncol(dt)")
    }
  }
  if (is.character(old)) {
    out = merge(data.frame(cnames, seq_along(cnames)), data.frame(cnames = old, new = new),
      allow.cartesian = T)
    cnames[out[[2]]] = out[[3]]
    colnames(dt) = cnames
    return(dt)
  }
  if (is.logical(old)) {
    if (! length(old) == length(cnames)) stop("logical vector must be same length as ncol(dt)")
    old = which(old)
  }
  cnames[old] = new
  colnames(dt) = cnames
  return(dt)
}

#' @name names2<-
#' @title robust name() assignment
#'
#' similar to rlang::names2
#'
#' @param x vector
#' @return x a vector with all names
#' @export
`names2<-` = function(x, value, useempty = FALSE) {
    names(x) = if (!is.null(value))
                   rep(value, length.out = length(x))
               else {
                   if (useempty)
                       rep("", length.out = length(x))
               }
    return(x)
}

#' @name rownames2<-
#' @title robust rownames() assignment
#'
#'
#' @param x vector or matrix
#' @return x a vector fully named or rownamed
#' @export
`rownames2<-` = function(x, value, useempty = FALSE) {
    if (!is.null(dim(x))) {
        rownames(x) = if (!is.null(value))
                       rep(value, length.out = nrow(x))
                   else {
                       if (useempty)
                           rep("", length.out = nrow(x))
                   }
    } else {
        names(x) = if (!is.null(value))
                       rep(value, length.out = length(x))
                   else {
                       if (useempty)
                           rep("", length.out = length(x))
                   }
    }
    return(x)
}

#' @name colnames2<-
#' @title robust colnames() assignment
#'
#'
#' @param x data with dimensions
#' @return x data fully colnamed
#' @export
`colnames2<-` = function(x, value, useempty = FALSE) {
    colnames(x) = if (!is.null(value))
                      rep(value, length.out = ncol(x))
                  else {
                      if (useempty)
                          rep("", length.out = ncol(x))
                  }
    return(x)
}




#' @name AND
#' @title test boolean AND across multiple vectors
#'
#' @description
#'
#' @export
AND = function(FUN = identity, ...) {
    lst = lapply(list(...), FUN)
    Reduce(function(x,y) {x & y}, lst)
}

#' @name OR
#' @title test boolean OR across multiple vectors
#'
#' @description
#'
#' @export
OR = function(FUN = identity, ...) {
    lst = lapply(list(...), FUN)
    Reduce(function(x,y) {x | y}, lst)
}

#' @name transp
#' @title transpose a list
#'
#' @description
#'
#' @export
transp = function (lst, ffun = list) 
{
    do.call(Map, c(f = ffun, lst))
}

#' @name frac
#' @title fraction from a vector of numeric values
#'
#' @description
#'
#' @export frac
frac = function(x) {
    x / sum(x)
}


#' @name setAllNames
#' @title setAllNames
#'
#' convenience function to set all names of a vector
#'
#' @param vec vector
#' @param nm character
#' @return named vector
#' @export
setAllNames = function(vec, nm) {
    if (is.null(nm)) {
        return(setNames(vec, NULL))
    } else {
        if (length(nm) < length(vec)) {
            nm = rep_len2(nm, vec)
        }
    }
    return(setNames(vec, nm))
}

#' @name setNames2
#' @title setNames2
#'
#' convenience function to set all names of a vector
#'
#' @param vec vector
#' @param nm character
#' @return named vector
#' @export
setNames2 = function(vec, nm, useempty = FALSE) {
    names2(vec, useempty = useempty) = nm
    return(vec)
}

#' @name selfname
#'
#' @title name a character vector to itself
#' @param char A character vector
#' @return A named character vector
#' @export
selfname = function(char) {setNames2(char, char)}


#' @name intercalate
#' @title collate two vectors together
#'
#' interleave vectors together
#'
#' @param ... A set of vectors to collate
#' @return a vector with values of inputs collated together
#' @examples
#' intercalate(c("a","d","f"), c("b", "e", "g", "z"))
#' @export
intercalate = function(..., fillin = FALSE) {
    isNested <- function(x) {
        if (class(x) != "list") {
            stop("Expecting 'x' to be a list")
        }
        out <- any(sapply(x, is.list))
        return(out)
    }
    args = list(...)
    if (isNested(args)) {
        args = unlist(args, recursive = F)
    }
    if (fillin) {
      mx = max(lengths(args))
      args = lapply(args, function(x) x[rep_len(seq_along(x), mx)])
    }
    s_along = lapply(args, seq_along)
    ord = order(do.call(c, s_along))
    conc = do.call(c, args)
    return(conc[ord])
}

#' @name intercalate_lst
#' @title collate lists together
#'
#' @description
#' interleave multiple vectors
#'
#' @param ... A set of lists to collate
#' @return a lists with elements collated together
#' @examples
#' intercalate(list(paste0(1:5, "_A")), list(paste0(1:3, "_B")), list(paste0(1:6, "_C")))
#' @export
intercalate_lst = function(...) {
    args = list(...)
    s_along = lapply(args, seq_along)
    ord = order(do.call(c, s_along))
    conc = do.call(c, args)
    return(conc[ord])
}



#' @name rand.string
#' @title make a random string
#'
#' @return random string
#' @author Someone from Stackoverflow
#' @export rand.string
rand.string = function(n=1, length=12)
{
    randomString <- c(1:n)                  # initialize vector
    for (i in 1:n)
    {
        randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                        length, replace=TRUE),
                                 collapse="")
    }
    return(randomString)
}

#' @name mkst
#' @title MaKe STring
#'
#' making string out of vector for eval(parse(text = ...))
#'
#'
#' @export
mkst = function(v, f = "c", po = "(", pc = ")", collapse = ",", asnull = FALSE) {
    if (identical(NROW(v), 0L)) {
        if (asnull) return(NULL) else return("")
    }
    out = paste0(f, po, paste0(v, collapse = collapse), pc)
    return(out)
}

#' @name et
#' @title shortcut for eval(parse(text = <string>))
#'
#' @param txt string to evaluate
#' @author Kevin Hadi
#' @export
et = function(txt, eval = TRUE, envir = parent.frame(), enclos = parent.frame(2)) {
    out = parse(text = txt)
    ## enclos = stackenv2()
    if (eval) {
        return(eval(out, envir = envir, enclos = enclos))
    } else {
        return(out)
    }
}


#' @name dodo.call2
#' @title dodo.call+
#'
#' FUN can be an anonymous function call
#' dodo.call2({function(...) paste(..., collapse = " ")}, list(bstring1, bstring2))
#' can also omit the brackets
#'
#'
#'
#' @param FUN function
#' @author Marcin Imielinski
#' @export dodo.call2
dodo.call2 = function (FUN, args, use.names = T)
{
    if (!is.character(FUN))
      FUN = substitute(FUN)
    if (use.names && !is.null(names(args)))
      argn = paste0("\"", names(args), "\"", "=")
    else
        argn = NULL
    if (!is.matrix(args)) {
        cmd = paste(
            paste0("{", as.character(as.expression(FUN)), "}"),
            "(",
            paste0(argn, "args[[", 1:length(args), "]]", collapse = ","),
            ")",
            sep = "")
    } else {
        cmd = paste(
            paste0("{", as.character(as.expression(FUN)), "}"),
            "(",
            paste0(argn, "args[,", 1:ncol(args), "]", collapse = ","),
            ")",
            sep = "")
    }
    return(et(cmd))
}

#' @name duped
#' @title duped
#'
#'
#'
#' @param ... vectors to paste by
#' @author Kevin Hadi
#' @export
duped = function(..., binder = "data.table") {
    duplicated(tryCatch(et(sprintf("%s(...)", binder)),
                        ## error = function(e) do.call(cbind, list(...))))
                        error = function(e) paste(...)))
}



#' @name rleseq
#' @title numbers up within repeating elements of a vector
#'
#' @description
#' returns unique id within each unique element of a vector or set of provided vectors
#' and also a running id within each unique element
#'
#' @param ... Vector(s) to identify with unique id and a running id within each unique id
#' @param clump a logical specifying if duplicates are to be counted together
#' @param recurs a logical that is meant to only be set by the function when using clump = TRUE
#' @return a list of idx and seq
#' @author Kevin Hadi
#' @export
rleseq = function (..., clump = TRUE, recurs = FALSE, na.clump = TRUE, 
                   na.ignore = FALSE, sep = paste0(" ", rand.string(length = 6), 
                     " "), use.data.table = FALSE) 
{
    rand.string <- function(n = 1, length = 12) {
        randomString <- c(1:n)
        for (i in 1:n) {
            randomString[i] <- paste(sample(c(0:9, letters, LETTERS), 
                                            length, replace = TRUE), collapse = "")
        }
        return(randomString)
    }
    force(sep)
    out = if (use.data.table) {
              tryCatch(
              {
                  dt = data.table(...)
                  setnames(dt, make.names(rep("", ncol(dt)), unique = T))
                  ## make.unique
                  cmd = sprintf("dt[, I := .I][, .(idx = .GRP, seq = seq_len(.N), lns = .N, I), by = %s]", mkst(colnames(dt), "list"))
                  dt = eval(parse(text = cmd))
                  setkey(dt, I)[, .(idx, seq, lns)]
              }, error = function(e) structure("data table didn't work...", class = "err"))
          }
    if (!(is.null(out) || class(out)[1] == "err"))
        return(as.list(out))


    lns = base::lengths(list(...))
    if (!all(lns == lns[1])) 
        warning("not all vectors provided have same length")
    fulllens = max(lns, na.rm = T)
    vec = setNames(paste(..., sep = sep), seq_len(fulllens))
    if (length(vec) == 0) {
        out = list(idx = integer(0), seq = integer(0), lns = integer(0))
        return(out)
    }
    if (na.clump) {
        paste = function(..., sep) base::paste(..., sep = sep)
    } else {
        ## paste = function(..., sep) base::paste(stringr::str_c(..., sep = sep))
        paste = function(..., sep) {
            comp = complete.cases(list(...))
            out = base::paste(..., sep)
            out[!comp] = NA_character_
            return(out)

        }
    }
    if (na.ignore) {
        isnotna = which(rowSums(as.data.frame(lapply(list(...), 
                                                     is.na))) == 0)
        out = list(idx = rep(NA, fulllens), seq = rep(NA, fulllens), 
                   lns = rep(NA, fulllens))
        if (length(isnotna)) 
            vec = vec[isnotna]
        tmpout = do.call(rleseq, c(alist(... = vec), alist(clump = clump, 
                                                           recurs = recurs, na.clump = na.clump, na.ignore = FALSE, use.data.table = FALSE)))
        for (i in seq_along(out)) out[[i]][isnotna] = tmpout[[i]]
        return(out)
    }
    if (!clump) {
        rlev = rle(vec)
        if (recurs) {
            ## return(unlist(unname(lapply(rlev$lengths, seq_len))))
            return(sequence(rlev$lengths))
        }
        else {
            out = list(idx = rep(seq_along(rlev$lengths), times = rlev$lengths), 
                       seq = unlist(unname(lapply(rlev$lengths, seq_len))))
            out$lns = ave(out[[1]], out[[1]], FUN = length)
            return(out)
        }
    }
    else {
        if (!na.clump) {
            vec = replace2(vec, which(x == "NA"), dedup(dg(x)[dg(x) == 
                                                              "NA"]))
        }
        vec = setNames(vec, seq_along(vec))
        lst = split(vec, factor(vec, levels = unique(vec)))
        ord = as.integer(names(unlist(unname(lst))))
        idx = rep(seq_along(lst), times = base::lengths(lst))
        out = list(idx = idx[order(ord)], seq = rleseq(idx, clump = FALSE, 
                                                       recurs = TRUE, use.data.table = FALSE)[order(ord)])
        ## out$lns = ave(out[[1]], out[[1]], FUN = length)
        out$lns = unname(rep(base::lengths(lst), times = base::lengths(lst)))
        return(out)
    }
}


#' @name clobber
#' @title same as dplyr::coalesce
#'
#' clobber NA, or some value between multiple vectors
#' bads can be a function that returns a logical
#'
#'
#' @param ... vectors to merge together
#' @param bads a set of values to clobber, or a function that returns a logical
#' @param r2l merge from left to right per pair of vectors
#' @param fromLast if TRUE, merge from last vector to first
#' @param comparefun A 2 argument function (i.e. function(x,y) x < y), if r2l = FALSE, then the greater value will be chosen as y is on the right, for function(x,y) x < y. if r2l = TRUE, then the lesser value will be chosen
#' @export
clobber = function(..., bads = NA, bads.x = NULL, bads.y = NULL, r2l = FALSE, fromLast = FALSE, opposite = TRUE, comparefun = NULL, remove.empty = TRUE) {
    lst = list(...)
    lens = eNROW(lst)
    maxlen = max(lens)
    if (length(unique(lens)) > 1)
        lst = lapply(lst, function(x) rep(x, length.out = maxlen))
    if (remove.empty)
        lst = lst[eNROW(lst) > 0]
    if ( !length(bads) && !length(bads.x) && !length(bads.y))
        stop("You gotta set one of bads, bads.x, or bads.y")
    if ({ length(bads.x) && length(bads.y) }) {
        message("bads.x and bads.y both set explicitly")
        message("setting opposite to FALSE")
        opposite = FALSE
    }
    anytrue = function(vec) rep(TRUE, length.out = length(vec))
    if (bads || !length(bads)) {
        message("bads set to NULL or TRUE")
        message("setting opposite to FALSE")
        bads = anytrue
        opposite = FALSE
    }
    if (opposite) {
        yfun = get("!", mode = "function")
    } else {
        yfun = get("identity", mode = "function")
    }
    if (!length(bads.x)) bads.x = bads
    if (!length(bads.y)) bads.y = bads
    dofun = function(x,y) {
        if (is.function(bads.x))
            badsx = which(bads.x(x))
        else
            badsx = which(x %in% bads.x)
        if (is.function(bads.y))
            nbadsy = which(yfun(bads.y(y)))
        else
            nbadsy = which(yfun(y %in% bads.y))
        ix = intersect(badsx, nbadsy)
        return(replace(x, ix, rep(y[ix], length.out = length(ix))))
    }
    if (is.null(comparefun)) {
        if (!r2l)
            return(Reduce(function(x,y) dofun(x,y), lst, right = fromLast))
        else
            return(Reduce(function(x,y) dofun(y,x), lst, right = fromLast))
    } else {
        yfun = get("identity", mode = "function")
        if (!r2l) {
            return(Reduce(function(x,y) {
                if (is.function(bads.x))
                    badsx = which(bads.x(x))
                else
                    badsx = which(x %in% bads.x)
                if (is.function(bads.y))
                    nbadsy = which(yfun(bads.y(y)))
                else
                    nbadsy = which(yfun(y %in% bads.y))
                lg = which(comparefun(x,y))
                lg = setdiff(lg, nbadsy)
                out = x
                out[badsx] = y[badsx]
                out[lg] = y[lg]
                out
            }, lst, right = fromLast))
        } else {
            return(Reduce(function(x,y) {
                if (is.function(bads.x))
                    badsx = which(bads.x(x))
                else
                    badsx = which(x %in% bads.x)
                if (is.function(bads.y))
                    nbadsy = which(yfun(bads.y(y)))
                else
                    nbadsy = which(yfun(y %in% bads.y))
                lg = which(comparefun(x,y))
                lg = setdiff(lg, nbadsy)
                out = y
                out[nbadsy] = x[nbadsy]
                out[lg] = x[lg]
                out
            }, lst, right = fromLast))
        }
    }
}



#' @name coalesce
#' @title same as dplyr::coalesce, khtools:coalesce is an alias for khtools::clobber
#'
#' clobber NA, or some value between multiple vectors
#' bads can be a function that returns a logical
#'
#' @export
coalesce = clobber




#' @name uniqf
#' @title Unique factor
#'
#' @description
#' Make a unique factor based on one or more vectors
#' in parallel.
#'
#' @export uniqf
uniqf = function (..., sep = " ")
{
    set.seed(10)
    lst = as.list(match.call()[-1])
    force(sep)
    nm = names(lst)
    if (is.null(nm)) {
        nm = rep_len("", length(lst))
    }
    ix = which(nm != "sep")
    tmpix = do.call(paste, c(lst[ix], alist(sep = sep)))
    tmpix = factor(tmpix, levels = unique(tmpix))
    tmpix
}


#' @name false2na
#' @title replace FALSE with NA
#'
#' @description
#' A convenience function to set a logical vector with NAs to false
#'
#' @return A logical vector with NAs set to FALSE
#' @export
false2na = function(x) {
    if (is.logical(x))
        x[x %in% FALSE] = NA
    else
        stop("x is not logical")
    x
}

#' @name nonzero2na
#' @title replace 0 to with NA
#'
#' @description
#' A convenience function to set a logical vector with NAs to false
#'
#' @return A logical vector with NAs set to FALSE
#' @export
nonzero2na = function(x) {
    if (is.integer(x))
        naval = NA_integer_
    else if (is.double(x))
        naval = NA_real_
    else
        stop("x is not double or integer")
    x[x > 0] = naval
    x
}

#' @name na2false
#' @title replace logical vector with NA to FALSE
#'
#' @description
#' A convenience function to set a logical vector with NAs to false
#'
#' @return A logical vector with NAs set to FALSE
#' @export
na2false = function(v)
{
    ## v = ifelse(is.na(v), v, FALSE)
    ## v[is.na(v)] = FALSE
    v[isNA(v)] = FALSE
    ## mode(v) = "logical"
    v
}


#' @name na2true
#' @title replace logical vector with NA to TRUE
#'
#' @description
#' A convenience function to set a logical vector with NAs to TRUE
#'
#' @return A logical vector with NAs set to TRUE
#' @export
na2true = function(v)
{
    ## v = ifelse(is.na(v), v, FALSE)
    ## v[is.na(v)] = TRUE
    v[isNA(v)] = TRUE
    ## as.logical(v)
    ## mode(v) = "logical"
    v
}

#' @name na2zero
#' @title na2zero
#'
#' A convenience function to set a numeric vector with NAs to zero
#'
#' @return A numeric vector with NAs set to zero
#' @export
na2zero = function(v) {
    ## v = ifelse(is.na(v), v, FALSE)
    ## v[is.na(v)] = 0
    v[isNA(v)] = 0
    return(v)
}

#' @name nan2zero
#' @title nan2zero
#'
#' A convenience function to set a numeric vector with NaNs to zero
#'
#' @return A numeric vector with NaNs set to zero
#' @export
nan2zero = function(v) {
    v[is.nan(v)] = 0
    return(v)
}


#' @name na2empty
#' @title na2empty
#'
#' A convenience function to set a character vector with NAs to an
#' empty character
#'
#' @return A character vector
#' @export
na2empty = function(v) {
    ## v = ifelse(is.na(v), v, FALSE)
    ## v[is.na(v)] = ""
    v[isNA(v)] = ""
    as.character(v)
}


#' @name empty2na
#' @title A convenience function to set a character vector with NAs to an
#' empty character
#'
#' @return A character vector
#' @export
empty2na = function(v) {
    ## v = ifelse(is.na(v), v, FALSE)
    v[nchar(v) == 0] = as.character(NA)
    v
}


#' @name find_dups
#' @title find all duplicates in a vector
#'
#' @param vec A vector
#' @return a logical vector with all positions marked TRUE being duplicates
#' @examples
#' find_dups(c(1,1,1,3,5))
#' find_dups(c(1,3,1,3,1))
#' find_dups(c(3,1,5,4,4))
#'
#' @export
find_dups = function(..., re_sort = FALSE, sep = " ", as.logical = FALSE) {
  lst = as.list(match.call())[-1]
  ix = setdiff(seq_along(lst), which(names(lst) %in% c("re_sort", "sep", "as.logical")))
  ## cl = sapply(lst[ix], class)
  if (length(ix) > 1)
    vec = do.call(function(...) paste(..., sep = sep), list(...))
  else
      vec = unlist(list(...))
  duplg = duplicated(vec)
  if (as.logical) return(duplg)
  dupix = which(duplg); rm(duplg)
  if (!re_sort) {
    return(which(vec %in% vec[dupix]))
  } else {
    matching_idx = match2(sort(vec[dupix]), vec)
    return(which(!is.na(matching_idx))[order(na.omit(matching_idx))])
  }
}

#' @name pivotlist
#' @title Pivot a list
#'
#' @description
#' Pivoting a list
#'
#' @export
pivotlist = function(x) {
    countup = unlist(lapply(lengths(x), seq_len))
    split(unlist(x), countup)
}
