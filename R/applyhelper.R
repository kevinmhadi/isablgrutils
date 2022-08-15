#' @name dtapply
#' @title mclapply on a table split by a column
#'
#' @description
#'
#' @export
dtapply = function (tbl,  split_col = "system_id", FUN, mc.cores = 1, mc.strict = TRUE, split_col_sort = FALSE, mclapply = parallel::mclapply, ...) 
{
    ## spl = tbl[[split_col]]
    ## dups = logical(NROW(tbl))
    dups = 0
    for (x in split_col) {
        dups = dups + anyDuplicated(tbl[[x]])
    }
    if (dups > 0) {
        if (mc.strict) errfun = stop else errfun = warning
        errfun("split column contains duplicates - some entries will have multiple paths")
    }
    ## if (!split_col_sort) {
    ##     spl = factor(spl, unique(spl))
    ## }
    ## lst = split(tbl, spl)
    lst = split_by(tbl, split_col, split_col_sort = split_col_sort)
    mclapply(lst, mc.cores = mc.cores, FUN, ...)
}

#' @name split_by
#' @title split data frame-like object based on columns
#'
#' @description
#' 
#'
#' @export
split_by = function(dt, fields, do.unname = FALSE, split_col_sort = FALSE) {
    in_type_is_granges = inherits(dt, c("GRanges", "IRanges", "GRangesList", "IRangesList"))
    if (in_type_is_granges) {
        out = dt
        dt = mcols(dt)
        si = seqinfo(out)
        if (!NROW(out))
            return(gr.fix(GRangesList(), si))
    }
    if (!NROW(dt)) return(list())
    colix = match3(fields, names(dt))
    ## colix = which(names(dt) %in% fields)
    expr = parse(text = sprintf("dt[,%s,drop=FALSE]", mkst(colix)))
    cols = eval(expr)
    if (!split_col_sort) 
        uf = dodo.call2(FUN = function(...) uniqf(..., sep = " "), as.list(cols))
    else
        uf = dodo.call2(FUN = function(...) paste(..., sep = " "), as.list(cols))
    ## rles = dodo.call2(FUN = rleseq, as.list(cols))
    if (in_type_is_granges) {
        out = split(out, uf)
        mcols(out) = dt[!duplicated(uf), fields]
        if (do.unname) out = unname(out)
        return(out)
    }
    out = split(dt, uf)
    if (do.unname) out = unname(out)
    return(out)
}

#' @name printerr
#' @title print error message from tryCatch
#'
#' @description
#' useful for tryCatch(..., error = function(e) printerr(<some_custom_msg>))
#'
#' @export
printerr = function(msg = "", e) {
    if (missing(e)) {
        e = dg(e)
    }
    cm = as.character(conditionMessage(e))
    cc = as.character(conditionCall(e))
    eval(quote(print(structure(paste("error: ", msg, cm, cc), class = "err"))))
}


#' @name check_lst
#' @title checking list for elements that are errors
#'
#' checking a list for any elements that are try-errors
#' usually from an lapply(..., function(x) try({})) call
#'
#' @param lst A list
#' @return a logical vector marking which elements are try-errors"
#' @export
check_lst = function(lst, class_condition = c("simpleError", "try-error", "error", "errored", "err"))
{
    ## unlist(lapply(lst, function(x) class(x)[1])) %in% class_condition
    return(vapply(lst, function(x) class(x)[1], "") %in% class_condition)
}

#' @name iderr
#' @title returns ids of list elements that are errors
#'
#' checking a list for any elements that are try-errors
#' usually from an lapply(..., function(x) try({})) call
#'
#' @param lst A list
#' @return a logical vector marking which elements are try-errors"
#' @export
iderr = function(lst, class_condition = c("simpleError", "try-error", "error", "errored", "err")) {
  which(check_lst(lst))
}

#' @name whicherr
#' @title iderr
#'
#' checking a list for any elements that are try-errors
#' usually from an lapply(..., function(x) try({})) call
#'
#' @param lst A list
#' @return a logical vector marking which elements are try-errors"
#' @export
whicherr = iderr


#' @name ret_no_err
#' @title a wrapper around check_lst
#'
#' 
#'
#' @param lst A list (usually the output of lapply(... , function(x) try({}))
#' @return only returns the non-errors in the list
#' @export
ret_no_err = function(lst, class_condition = c("simpleError", "try-error", "error", "errored", "err"))
{
    return(lst[!check_lst(lst, class_condition = class_condition)])
}

#' @name ret_err
#' @title a wrapper around check_lst
#'
#'
#' @param lst A list (usually the output of lapply(... , function(x) try({}))
#' @return only returns the errors in the list
#' @export
ret_err = function(lst, class_condition = c("simpleError", "try-error", "error", "errored", "err"))
{
    return(lst[check_lst(lst, class_condition = class_condition)])
}

#' using check_lst to return
#'
#' @param lst A list (usually the output of lapply(... , function(x) try({}))
#' @return returns full length list with errored elements changed to NA
#' @export
ret_na_err = function(lst, class_condition = c("try-error", "error", "errored", "err"))
{
    lst[check_lst(lst, class_condition = class_condition)] = NA
    return(lst)
}



#' @name try2
#' @title wrapper around tryCatch - robust to parallel:: functions
#'
#' A slightly more robust version of try that works within the parallel:: set of functions
#' that pre-deploy a cluster.
#'
#' @export
try2 = function(expr, ..., finally) {
    tryCatch(expr,
             error = function(e) {
                 msg = structure(paste(conditionMessage(e), conditionCall(e), sep = "\n"), class = "err")
                 cat("Error: ", msg, "\n\n")
                 return(msg)
             },
             finally = finally,
             ... = ...)
}
