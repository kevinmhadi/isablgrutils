#' @name gr.sort
#' @title sort granges, grangeslist
#' @description
#'
#' sort granges or grangeslist by seqlevels
#' also reorders seqlevels into 1:22, X, Y format
#'
#' @return GRanges
#' @author Kevin Hadi
#' @export gr.sort
gr.sort = function(gr, ignore.strand = TRUE) {
    ## tmp = GenomeInfoDb::sortSeqlevels(gr)
    ## if (ignore.strand)
    ##     strand(tmp) = "*"
    ## GenomicRanges::order(grl)
    ## return(GenomicRanges::sort(tmp))
    return(gr[gr.order(gr)])
}

#' @name gr.order
#' @title order granges, grangeslist
#' @description
#'
#' order granges or grangeslist by seqlevels
#' also reorders seqlevels into 1:22, X, Y format
#'
#' @return GRanges
#' @author Kevin Hadi
#' @export gr.order
gr.order = function(gr, ignore.strand = T) {
    sgr = GenomeInfoDb::sortSeqlevels(gr)
    if (ignore.strand)
        sgr = gr.stripstrand(sgr)
    return(GenomicRanges::order(sgr))
}

#' @name gr.stripstrand
#' @title Strip Strand
#'
#' @description
#'
#' Remove strand information from genomicranges
#'
#' @return GRanges
#' @author Kevin Hadi
#' @export gr.stripstrand
gr.stripstrand = function(gr) {
    strand(gr) = "*"
    return(gr)
    
}


#' @name gr.resize
#' @title Resize granges without running into negative width error
#' @description
#'
#' lower size limit of window is 0
#'
#' @return GRanges
#' @author Kevin Hadi
#' @export gr.resize
gr.resize = function (gr, width, pad = TRUE, minwid = 0, each = TRUE, ignore.strand = FALSE,
    fix = "center", reduce = FALSE)
{
    wid = width
    if (pad) {
        if (each) {
            wid = wid * 2
        }
        width.arg = pmax(width(gr) + wid, minwid)
    }
    else width.arg = pmax(wid, minwid)
    if (reduce) {
        ## gr = GenomicRanges::reduce(gr + width.arg, ignore.strand = ignore.strand) -
        ##     width.arg
        out = gr.resize(gr, width, pad = pad, minwid = minwid, each = each, ignore.strand = ignore.strand, fix = fix, reduce = FALSE)
        out = GenomicRanges::reduce(out, ignore.strand = ignore.strand)
        out = gr.resize(out, width, pad = pad, minwid = minwid, each = each, ignore.strand = ignore.strand, fix = fix, reduce = FALSE)
        return(out)
    }
    return(GenomicRanges::resize(gr, width = width.arg, fix = fix,
        ignore.strand = ignore.strand))
}


#' @name grl.pivot
#' @title Pivot GRangesList
#' 
#'
#' @export grl.pivot
grl.pivot = function (x) 
{
    if (length(x) == 0) {
        return(GRangesList(GRanges(seqlengths = seqlengths(x)), 
            GRanges(seqlengths = seqlengths(x))))
    }
    return(GenomicRanges::split(BiocGenerics::unlist(x), rep(1:length(x[[1]]), 
        length(x))))
}

#' @name gr2bed
#' @title GRanges to Bed
#'
#' @description
#' converting gr to bed like table
#' also shifts coordinates to half closed 0 based
#'
#' @export
gr2bed = function(gr) {
    df = as(gr, "data.frame")
    colnames(df)[1:3] = c("chrom", "chromStart", "chromEnd")
    df$width = NULL
    out = cbind(df[,1:3,drop=F], name = as.character(seq_len(NROW(df))),
                score = rep_len(0, NROW(df)), df[,-c(1:3),drop=F])
    out$chromStart = out$chromStart - 1
    return(out)
}



#' @name grl2bedpe
#' @title GenomicRangesList to bedpe
#'
#' @description
#' converting grl to bedpe-like table
#' also shifts coordinates to half closed 0 based
#'
#' @export
grl2bedpe = function(grl, add_breakend_mcol = FALSE, flip = FALSE, as.data.table = TRUE, zerobased = TRUE) {
    grpiv = grl.pivot(grl)
    if (zerobased) {
        grpiv[[1]] = gr.resize(grpiv[[1]], width = 2, pad = FALSE, fix = "end")
        grpiv[[2]] = gr.resize(grpiv[[2]], width = 2, pad = FALSE, fix = "end")
    }


    mcgrl = as(mcols(grl), "data.frame")

    df1 = as(grpiv[[1]], "data.frame")[, c(1:3, 5), drop=F]
    df2 = as(grpiv[[2]], "data.frame")[, c(1:3, 5), drop=F]
    colnames(df1) = c("chrom1", "start1", "end1", "strand1")
    colnames(df2) = c("chrom2", "start2", "end2", "strand2")

    mc1 = data.frame()[seq_len(NROW(df1)),,drop=F]
    mc2 = data.frame()[seq_len(NROW(df2)),,drop=F]

    if (add_breakend_mcol) {
        mc1 = as(grpiv[[1]], "data.frame")[,-c(1:5),drop=F]
        mc2 = as(grpiv[[2]], "data.frame")[,-c(1:5),drop=F]

        colnames(mc1) = paste0("first.", colnames(mc1))
        colnames(mc2) = paste0("second.", colnames(mc2))
    }

    out = cbind(df1, df2, name = as.character(seq_len(NROW(df1))), score = rep_len(0, NROW(df1)), mcgrl, mc1, mc2)

    canon_col = c(1, 2, 3, 5, 6, 7, 9, 10, 4, 8)
    ## out[, canon_col, drop = F]
    nix = seq_len(ncol(out))

    ## reorder
    out = out[, c(canon_col, nix[!nix %in% canon_col]), drop = F]

    if (flip) {
        out$strand1 = c("+" = "-", "-" = "+")[out$strand1]
        out$strand2 = c("+" = "-", "-" = "+")[out$strand2]
    }

    if (as.data.table)
        return(tryCatch(as.data.table(out), error = function(e) return(out)))
    else
        return(out)
    
}

#' @name bedpe2grl
#' @title Convert Bedpe Table to GRangesList
#'
#' @description
#' convert a bedpe in data frame format to a GRangesList
#'
#' @export
bedpe2grl = function(bedpe, flip = FALSE, trim = TRUE, genome = NULL, sort = TRUE) {
    if (!NROW(bedpe)) return(GRangesList())
    bedpe$chrom1 = as.character(bedpe$chrom1)
    bedpe$chrom2 = as.character(bedpe$chrom2)
    st1 = bedpe$strand1
    st2 = bedpe$strand2
    if (isTRUE(flip)) {
        st1 = c("+" = "-", "-" = "+")[st1]
        st2 = c("+" = "-", "-" = "+")[st2]
    }
    gr1 = data.frame(seqnames = bedpe$chrom1, start = bedpe$start1,
                     end = bedpe$end1, strand = st1)
    gr1 = makeGRangesFromDataFrame(gr1)
    gr1 = gr.resize(gr1, 1, pad = FALSE, fix = "end")
    gr2 = data.frame(seqnames = bedpe$chrom2, start = bedpe$start2,
                end = bedpe$end2, strand = st2)
    gr2 = makeGRangesFromDataFrame(gr2)
    gr2 = gr.resize(gr2, 1, pad = FALSE, fix = "end")
    d1.cols = intersect(c("name", "score"), colnames(bedpe))
    if (length(d1.cols))
        d1 = tryCatch(bedpe[, d1.cols, drop = F, with = F], error = function(e) bedpe[, d1.cols, drop = F])
    else
        d1 = tryCatch(bedpe[, 0, drop = F, with = F], error = function(e) bedpe[, 0, drop = F])
    ## d1 = bedpe[, c("name", "score"), drop=F]
    d2 = bedpe[, -c(1:10), drop=F]    
    grl = grl.pivot(GRangesList(gr1, gr2))
    mcols(grl) = cbind(d1, d2)
    if (sort) grl = gr.sort(grl)
    return(grl)
}



#' @name gr_construct_by
#' @title adding on by field to seqnames for more efficient by queries
#' 
#' @description
#'
#' Uses by field from metadata column to insert into seqnames
#' This is useful for more efficient queries findoverlaps queries between 2 ranges
#' when we want to stratify the query with a "by" field.
#' This feeds into the gr.findoverlaps family of gUtils tools.
#'
#' @return A GRanges with the by metadata field attached to the seqnames
#' @author Kevin Hadi
#' @export gr_construct_by
gr_construct_by = function(x, by = NULL, na.seql = TRUE) {
    if (is.null(by) || length(x) == 0) return(x)
    ## this.sep1 = {set.seed(10); paste0(" ", rand.string(), " ")}
    this.sep1 = " G89LbS7RCine "
    ## this.sep2 = {set.seed(11); paste0(" ", rand.string(), " ")}
    this.sep2 = " VxTofMAXRbkl "
    ## ans = copy2(x)
    ans = x
    thisp = function(...) paste(..., sep = this.sep1)
    f1 = do.call(paste, c(as.list(mcols(x)[, by, drop = FALSE]), sep = this.sep1))
    f2 = as.character(seqnames(x))
    f2i = as.integer(seqnames(x))
    f12 = paste(f1, f2, sep = this.sep2)
    ui = which(!duplicated(f12))
    ans_seqlevels = f12[ui]
    x_seqinfo <- seqinfo(x)
    ans_seqlengths = unname(seqlengths(x_seqinfo)[f2i[ui]])
    if (na.seql)
        ans_seqlengths[] = NA_integer_
    ans_isCircular <- unname(isCircular(x_seqinfo))[f2i[ui]]
    ans_seqinfo <- Seqinfo(ans_seqlevels, ans_seqlengths, ans_isCircular)
    ans@seqnames <- Rle(factor(f12, ans_seqlevels))
    ans@seqinfo <- ans_seqinfo
    return(ans)
}

#' @name na.seql
#' @title NA out seqlevels
#' 
#' @description
#'
#' @return A GRanges with NA in all seqlevels
#' @author Kevin Hadi
#' @export na.seql
na.seql = function (x) 
{
    x_seqinfo = seqinfo(x)
    ans = x
    ans_seqlengths = seqlengths(x_seqinfo)
    ans_seqlevels = seqlevels(x_seqinfo)
    ans_isCircular = isCircular(x_seqinfo)
    ans_seqlengths[] = NA_integer_
    ans_seqinfo = Seqinfo(ans_seqlevels, ans_seqlengths, ans_isCircular)
    ans@seqinfo = ans_seqinfo
    return(ans)
}


#' @name gr_deconstruct_by
#' @title removing by field and random string barcode to seqnames for more efficient by queries
#' 
#' @description
#'
#' to be used with gr_construct_by
#'
#' @return A GRanges with the by metadata field attached to the seqnames
#' @author Kevin Hadi
#' @export gr_deconstruct_by
gr_deconstruct_by = function (x, by = NULL, meta = FALSE) 
{
    if (is.null(by) || length(x) == 0) 
        return(x)
    this.sep1 = " G89LbS7RCine "
    this.sep2 = " VxTofMAXRbkl "
    ans = x
    f1 = as.character(seqnames(x))
    f2 = sub(paste0(".*", this.sep2), "", f1)
    ## f2 = trimws(gsub(paste0(".*", this.sep2), "", f1))
    ## f2 = trimws(gsub(paste0(".*", this.sep1), "", f2))
    ui = which(!duplicated(f1))
    x_seqinfo <- seqinfo(x)
    seql = rleseq(f2[ui], clump = T)
    lst = lapply(split(seqlengths(x_seqinfo)[f1[ui]], seql$idx), 
        function(x) max(x))
    uii = which(!duplicated(f2[ui]))
    ans_seqlevels = f2[ui][uii]
    ans_seqlengths = setNames(unlist(lst), ans_seqlevels)
    ans_isCircular <- unname(isCircular(x_seqinfo))[ans_seqlevels]
    ans_seqinfo <- Seqinfo(ans_seqlevels, ans_seqlengths, ans_isCircular)
    ans@seqnames <- Rle(factor(f2, ans_seqlevels))
    ans@seqinfo <- ans_seqinfo
    if (meta) {
        f0 = sub(paste0(this.sep2, ".*"), "", f1)
        ## f0 = data.table::tstrsplit(f0, this.sep1, fixed = T)
        ## f0 = strsplit(f0, this.sep1)
        ## f0 = trans(f0, c)
        ## mc = as.data.table(unname(f0))
        mc = as.data.frame(unname(pivotlist(strsplit(f0, this.sep1, fixed = T))))
        if (!(identical(by, "") | identical(by, NA) | identical(by, "NA")) &&
            length(by) == NCOL(mc)) {
            colnames(mc) = by
        }
        ## debugonce(do.assign)
        mcols(ans) = do.assign(mcols(ans), mc)
    }
    return(ans)
}


#' @name shift_up
#' @title shift_upstream from plyranges package
#' @description
#'
#' works on genomicranges
#'
#' @return granges
#' @author Stuart Lee
#' @author Michael Lawrence
#' @author Dianne Cook
#' @export
shift_up = function (x, shift = 0L)
{
    strand = function(bla) {as.character(BiocGenerics::strand(bla))}
    neg <- strand(x) == "-"
    pos <- strand(x) %in% c("+", "*")
    if (length(x) == length(shift)) {
        shift_neg <- shift[which(neg)]
        shift_pos <- shift[which(pos)]
        x[neg] <- shift_right(x[neg], shift_neg)
        x[pos] <- shift_left(x[pos], shift_pos)
    }
    else {
        x[neg] <- shift_right(x[neg], shift)
        x[pos] <- shift_left(x[pos], shift)
    }
    return(x)
}


#' @name shift_down
#' @title shift_downstream from plyranges package
#' @description
#'
#' works on genomicranges
#'
#' @return granges
#' @author Stuart Lee
#' @author Michael Lawrence
#' @author Dianne Cook
#' @export
shift_down = function (x, shift = 0L)
{
    strand = function(bla) {as.character(BiocGenerics::strand(x))}
    neg <- strand(x) == "-"
    pos <- strand(x) %in% c("+", "*")
    if (length(x) == length(shift)) {
        shift_neg <- shift[which(neg)]
        shift_pos <- shift[which(pos)]
        x[neg] <- shift_left(x[neg], shift_neg)
        x[pos] <- shift_right(x[pos], shift_pos)
    }
    else {
        x[neg] <- shift_left(x[neg], shift)
        x[pos] <- shift_right(x[pos], shift)
    }
    return(x)
}

#' @name shift_left
#' @title shift_left from plyranges package
#' @description
#'
#' works on genomicranges
#'
#' @return granges
#' @author Stuart Lee
#' @author Michael Lawrence
#' @author Dianne Cook
#' @export
shift_left = function (x, shift = 0L)
{
    shift_l <- -1L * shift
    return(GenomicRanges::shift(x, shift_l))
}

#' @name shift_right
#' @title shift_right from plyranges package
#' @description
#'
#' works on genomicranges
#'
#' @return granges
#' @author Stuart Lee
#' @author Michael Lawrence
#' @author Dianne Cook
#' @export
shift_right = function (x, shift = 0L)
{
    return(GenomicRanges::shift(x, shift))
}




#' @name gr2df
#' @title granges to datatable via dataframe
#' @description
#'
#' GRangesList
#'
#' @return GRangesList
#' @author Kevin Hadi
#' @export gr2df
gr2df = function(gr, var = "rowname") {
    sf = options()$stringsAsFactors
    on.exit({options(stringsAsFactors = sf)})
    options(stringsAsFactors = FALSE)
    rn = names(gr)
    if (!is.null(rn))
        rn2 = make.unique(rn)
    else
        rn2 = as.character(seq_along(gr))
    if (inherits(gr, "GRanges")) {
        df = GenomicRanges::as.data.frame(gr, row.names = rn2)
        cmd = sprintf("cbind(%s = rn2, df)", var)
        df = et(cmd)
    } else if (inherits(gr, "GRangesList"))
        df = GenomicRanges::as.data.frame(gr)
    setDT(df)
    return(dt_f2char(df,c("seqnames", "strand")))
}


#' @name df2gr
#' @title data frame to GRanges
#' @description
#'
#' data frame to GRanges
#'
#' @return GRanges
#' @author Kevin Hadi
#' @export df2gr
df2gr = function (df, seqnames.field = "seqnames", start.field = "start", 
    end.field = "end", strand.field = "strand", ignore.strand = FALSE, 
    keep.extra.columns = TRUE, starts.in.df.are.0based = FALSE) {
    if (!inherits(df, "data.frame")) {
        df = as.data.frame(df)
    }
    if (inherits(seqnames.field, c("numeric", "integer"))) {
        seqnames.field = colnames(df)[seqnames.field]
    }
    if (inherits(start.field, c("numeric", "integer"))) {
        start.field = colnames(df)[start.field]
    }
    if (inherits(end.field, c("numeric", "integer"))) {
        end.field = colnames(df)[end.field]
    }
    if (inherits(strand.field, c("numeric", "integer"))) {
        strand.field = colnames(df)[strand.field]
    }
    badcols = c("seqnames", "start", "end", "strand", "ranges", 
        "width", "element", "seqlengths", "seqlevels", "isCircular")
    badcols = setdiff(badcols, c(seqnames.field, start.field, 
        end.field, strand.field))
    ix = which(colnames(df) %in% badcols)
    if (length(ix) > 0) 
        df = et(sprintf("df[, -%s]", mkst(ix)))
    cnames = colnames(df)
    names(cnames) = cnames
    relevant_cols = match(c(seqnames.field, start.field, end.field, 
        strand.field), cnames)
    if (is.na(relevant_cols[4])) {
        relevant_cols = relevant_cols[-4]
        ignore.strand = TRUE
    }
    addon = rand.string()
    cnames[relevant_cols] = paste(cnames[relevant_cols], addon, 
        sep = "_")
    colnames(df)[relevant_cols] = cnames[relevant_cols]
    seqnames.field = paste(seqnames.field, addon, sep = "_")
    start.field = paste(start.field, addon, sep = "_")
    end.field = paste(end.field, addon, sep = "_")
    strand.field = paste(strand.field, addon, sep = "_")
    makeGRangesFromDataFrame(df, seqnames.field = seqnames.field, 
        start.field = start.field, end.field = end.field, strand.field = strand.field, 
        ignore.strand = ignore.strand, keep.extra.columns = keep.extra.columns, 
        starts.in.df.are.0based = starts.in.df.are.0based)
}


#' @name gr.flipstrand
#' @title works with GRangesLists
#' 
#' @description
#'
#' to be used with gr_construct_by
#'
#' @return A GRanges with the by metadata field attached to the seqnames
#' @author Kevin Hadi
#' @export gr.flipstrand
gr.flipstrand = function(gr) {
    if (!inherits(gr, c("GRanges" ,"GRangesList"))) {
        stop("not a GRanges / GRangesList")
    }
    if (is(gr, "GRangesList")) {
        this.strand = gr@unlistData@strand
        gr@unlistData@strand = S4Vectors::Rle(factor(c("*" = "*", "+" = "-", "-" = "+")[as.character(this.strand)], levels(this.strand)))
    } else {
        this.strand = gr@strand
        gr@strand = S4Vectors::Rle(factor(c("*" = "*", "+" = "-", "-" = "+")[as.character(this.strand)], levels(this.strand)))
    }
    return(gr)
}


#' @name grl.flipstrand
#' @title flip strand of grangeslist
#' @description
#'
#' GRangesList
#'
#' @return GRangesList
#' @author Kevin Hadi
#' @export grl.flipstrand
grl.flipstrand = function(grl) {
    tmp_vals = mcols(grl)
    tmp_gr = unlist(grl, use.names = FALSE)
    tmp_gr = gr.flipstrand(tmp_gr)
    new_grl = relist(tmp_gr, grl)
    mcols(new_grl) = tmp_vals
    return(new_grl)
}



#' @name write_bed
#' @title write bed or bedpe into canonical formatted table
#'
#' Write a table into  a bed oe bedpe formatted file
#' Ensures the header is commented out
#'
#' @return A data.table
#' @export write_bed
write_bed = function(bed, outpath) {
    cn = colnames(bed)
    cn[1] = paste0("#", cn[1])
    bedhead = paste0(cn, collapse = "\t")
    writeLines(bedhead, outpath)
    tryCatch(fwrite(bed, outpath, sep = "\t", col.names = FALSE, row.names = FALSE, append = TRUE),
             error = function(e) {
                 write.table(bed, outpath, sep = "\t", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
             })
}

#' @name read_bed
#' @title read bed or bedpe as a table
#'
#' Read in a bed oe bedpe formatted file into tabular format
#'
#' @return A data.table
#' @export read_bed
read_bed = function(bedpath) {
    f = file(bedpath, open = "r")
    thisline = readLines(f, 1)
    headers = character(0)
    while (length(grep("^((#)|(chrom)|(chr))", thisline, ignore.case = T))) {
        headers = c(headers, thisline)
        thisline = readLines(f, 1)
    }    
    lastheader = tail(headers, 1)
    ## ln = sum(length(headers), length(thisline))
    ## while (length(thisline) > 0) {
    ##     ## thisline = readBin(f, "raw", n = 50000)
    ##     ## sum(thisline == as.raw(10L))
    ##     thisline = readLines(f, n = 50000)
    ##     ln = length(thisline) + ln
    ## }
    ## fread(bedpath, skip = length(headers))
    ## bed = tryCatch(fread(bedpath, skip = NROW(headers), header = F),
    ##                error = function(e) {
    ##                    read.table(bedpath, comment.char = "", skip = NROW(headers), header = F)
    ##                })
    bedhead = gsub("^#", "", unlist(strsplit(lastheader, "\t")))
    bed = tryCatch(fread(bedpath, skip = NROW(headers), header = F), 
                   error = function(e) NULL)
    if (is.null(bed)) 
        bed = tryCatch(read.table(bedpath, comment.char = "", skip = NROW(headers), 
                                  header = F), error = function(e) NULL)
    
    if (is.null(bed)) {
        bed = matrix(integer(0), ncol = length(bedhead))
        bed = as.data.table(bed)
    }

    if (identical(NROW(bedhead), ncol(bed))) {
        colnames(bed) = bedhead
    }
    return(bed)
}

#' @name read.bam.header
#' @title read.bam.header
#'
#' Read in a bam header into tabular format
#'
#' @return A data.table
#' @export read.bam.header
read.bam.header = function(bam, trim = FALSE) {
    cmd = sprintf("samtools view -H %s", bam)
    ## tb = fread(text = system(cmd, intern = TRUE), fill = TRUE, sep = "\t", header = F)
    tb = setDT(read.table(text = system(cmd, intern = TRUE), fill = TRUE, sep = "\t", header = F))
    if (!trim) {
        return(as.data.table(tb))
    } else {
        tb = tb[grepl("^SN", V2)][, V2 := gsub("SN:", "", V2)]
        return(tb)
    }
}




#' @name bcfindex
#' @title bcfindex
#'
#' index a bcf/vcf file
#'
#' @return vcf path
#' @export
bcfindex = function(vcf, force = TRUE) {
    ## if (!force) {
    if (!grepl(".[bv]cf(.gz)?$", vcf)) {
        stop("check if you have a valid bcf/vcf file")
    }
    if (!file.exists(paste0(vcf, ".tbi")) & !file.exists(paste0(vcf, ".csi")) || force) {
        system(sprintf("bcftools index --tbi %s", vcf))
    }
    vcf
}



#' @name gr.noval
#' @title get rid of mcols on GRanges/GRangesLists
#' 
#' @description
#' remove all metadata from GRanges or GRangesList
#'
#' @return GRanges or GRangesList
#' @author Kevin Hadi
#' @export gr.noval
gr.noval = function(gr, keep.col = NULL, drop.col = NULL) {
    if (is.null(keep.col) & is.null(drop.col)) {
        select_col = NULL
    } else {
        all_col = colnames(gr@elementMetadata)
        if (inherits(gr, "GRangesList")) {
            all_col = c(all_col, colnames(gr@unlistData@elementMetadata))
        }

        if (!is.null(keep.col) & is.null(drop.col)) {
            select_col = intersect(all_col, keep.col)
        } else if (is.null(keep.col) & !is.null(drop.col)) {
            select_col = setdiff(all_col, drop.col)
        } else if (!is.null(keep.col) && !is.null(drop.col)) {
            if (intersect(keep.col, drop.col) > 0) {
                warning("drop.col and keep.col args have overlapping elements\nkeeping the columns that overlap")
                select_col = intersect(setdiff(all_col, setdiff(drop.col, keep.col)), keep.col)
            }
        }
    }
    if (inherits(gr, "GRangesList")) {
        tmp_query = intersect(select_col, colnames(gr@unlistData@elementMetadata))
        gr@unlistData@elementMetadata = gr@unlistData@elementMetadata[,c(tmp_query), drop = FALSE]
    }
    tmp_query = intersect(select_col, colnames(gr@elementMetadata))
    gr@elementMetadata = gr@elementMetadata[,c(tmp_query),drop = FALSE]
    return(gr)
}


#' @name gr.spreduce
#' @title reduce based on a field(s) to split by in elementMetadata of GRanges, or given vector
#' @description
#'
#' split and reduce GRanges by field(s)
#' if providing a variable not already within the GRanges,
#' may need to use dynget(variable_name)
#'
#' @return GRanges
#' @author Kevin Hadi
#' @export gr.spreduce
gr.spreduce = function(gr,  ..., ignore.strand = FALSE, pad = 0, return.grl = FALSE, sep = paste0(" ", rand.string(length = 8), " ")) {
  lst = as.list(match.call())[-1]
  ix = which(!names(lst) %in% c("gr", "sep", "pad", "ignore.strand", "return.grl"))
  vars = unlist(sapply(lst[ix], function(x) unlist(sapply(x, toString))))
  if (length(vars) == 1) {
    if (!vars %in% colnames(mcols(gr)))
      vars = tryCatch(unlist(list(...)), error = function(e) vars)
  }
  if (!all(vars %in% colnames(mcols(gr))))
    stop("Must specify valid metadata columns in gr")
  tmpix = do.call(
    function(...) paste(..., sep = sep),
    as.list(mcols(gr)[,vars, drop = F]))
  unix = which(!duplicated(tmpix))
  tmpix = factor(tmpix, levels = tmpix[unix])
  grl = unname(gr.noval(gr) %>% GenomicRanges::split(tmpix))
  grl = GenomicRanges::reduce(grl + pad, ignore.strand = ignore.strand)
  if (return.grl) {
    mcols(grl) = mcols(gr)[unix,vars,drop = F]
    return(grl)
  } else {
    out = unlist(grl)
    mcols(out) = mcols(gr)[rep(unix, times = IRanges::width(grl@partitioning)),
      vars,drop = F]
    return(out)
  }
}

#' @name gr.sprange
#' @title get range based on a field(s) to split by in elementMetadata of GRanges, or given vector
#' @description
#'
#' split and get range of GRanges by field(s)
#' if providing a variable not already within the GRanges,
#' may need to use dynget(variable_name)
#'
#' @return GRanges
#' @author Kevin Hadi
#' @export gr.sprange
gr.sprange = function (gr, ..., ignore.strand = FALSE, pad = 0, return.grl = FALSE, 
    sep = paste0(" ", rand.string(length = 8), " ")) 
{
    lst = as.list(match.call())[-1]
    ix = which(!names(lst) %in% c("gr", "sep", "pad", "ignore.strand", 
        "return.grl"))
    vars = unlist(sapply(lst[ix], function(x) unlist(sapply(x, 
        toString))))
    if (length(vars) == 1) {
        if (!vars %in% colnames(mcols(gr))) 
            vars = tryCatch(unlist(list(...)), error = function(e) vars)
    }
    if (!all(vars %in% colnames(mcols(gr)))) 
        stop("Must specify valid metadata columns in gr")
    tmpix = do.call(function(...) paste(..., sep = sep), as.list(mcols(gr)[, 
        vars, drop = F]))
    unix = which(!duplicated(tmpix))
    tmpix = factor(tmpix, levels = tmpix[unix])
    grl = unname(gr.noval(gr) %>% GenomicRanges::split(tmpix))
    grl = range(grl + pad, ignore.strand = ignore.strand)
    if (return.grl) {
        mcols(grl) = mcols(gr)[unix, vars, drop = F]
        return(grl)
    }
    else {
        out = unlist(grl)
        mcols(out) = mcols(gr)[rep(unix, times = IRanges::width(grl@partitioning)), 
            vars, drop = F]
        return(out)
    }
}
