#' Get Template
#'
#' @param files Path to file on S3 Bucket
#' @param outdir output directory for files to download
#' @param verbose should progress be added to downloading?
#' @param overwrite overwrite files if they already exist
#' @param ... additional arguments to pass to other methods
#'
#' @return Output filename character vector that was downloaded
#' @export
#' @importFrom httr stop_for_status write_disk progress
#' @examples
#' temps = templates()
#' list_template_files(temps[1])
#' all_files = list_template_files(temps)
#' out = list_template_images(temps[1])
#' data = get_template_files(out[1:3], outdir = tempfile())
get_template_files = function(
  files,
  outdir = NULL,
  verbose = TRUE,
  overwrite = FALSE,
  ...) {

  if (is.null(outdir)) {
    outdir = tf_home()
  }
  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
  }
  all_sub_dirs = file.path(outdir, dirname(files))
  lapply(all_sub_dirs, dir.create, showWarnings = FALSE, recursive = TRUE)
  outfiles = file.path(outdir, files)
  results = lapply(files, function(path) {
    outfile = file.path(outdir, path)
    if (!file.exists(outfile) || file.size(outfile) == 0 || overwrite) {
      try({s3_get(path, verbose = verbose, outfile = outfile)})
    } else {
      outfile
    }
  })
  names(outfiles) = files
  outfiles[!file.exists(outfiles)] = NA
  outfiles[file.size(outfiles) == 0] = NA
  return(outfiles)
}

#' @export
#' @rdname get_template_files
get_template = function(
  template,
  outdir = NULL,
  verbose = TRUE,
  overwrite = FALSE,
  ...) {
  all_files = list_template_files(template)
  result = lapply(template, get_template_files,
                  outdir = outdir, verbose = verbose,
                  overwrite = overwrite,
                  ...)
  result
}

#' @export
#' @rdname get_template_files
#' @param template A template identifier
list_template_files = function(
  template
) {
  path = get_template_path()
  path = file.path(path,
                   sprintf("tpl-%s", template))
  files = lapply(path, function(x) {
    file.path(basename(x),
              list.files(x, recursive = TRUE,
                         full.names = FALSE)
    )
  })
  names(files) = template
  files = unlist(files)
  files
}

#' @export
#' @rdname get_template_files
list_template_images = function(
  template
) {
  path = get_template_path()
  path = file.path(path,
                   sprintf("tpl-%s", template))
  files = lapply(path, function(x) {
    file.path(basename(x),
              list.files(x, recursive = TRUE,
                         pattern = "[.](nii|gii)",
                         full.names = FALSE)
    )
  })
  names(files) = template
  files = unlist(files)
  files
}


#' @export
#' @rdname get_template_files
templates = function() {
  sort(get_templates())
}


get_templates = function() {
  path = get_template_path()
  dirs = list.dirs(path, recursive = FALSE)
  templates = basename(dirs)
  templates = sub("tpl-", "", templates)
}

get_template_nii = function() {
  path = get_template_path()
  files = list.files(pattern = "[.]nii", path = path, recursive = TRUE)
  files
}

get_template_gii = function() {
  path = get_template_path()
  files = list.files(pattern = "[.]gii", path = path, recursive = TRUE)
  files
}


s3_get = function(path, verbose = TRUE, outfile = NULL) {
  url = paste(TF_S3_ROOT, path, sep = "/")
  url = gsub("//", "/", url)

  if (verbose) {
    message(sprintf("Downloading %s", url))
  }

  tfile = tempfile()
  res = httr::GET(url,
                  if (!is.null(outfile)) httr::write_disk(outfile,
                                                          overwrite = TRUE))
  httr::stop_for_status(res)

  return(res)
}



#' Fetch template citations
#'
#' @param template A template identifier
#' @param as_bibtex The elements are returned as \code{\link{bibentry}}
#'
#' @return A list of references
#' @export
#'
#' @examples
#' get_metadata("Fischer344")
#' get_citations("Fischer344")
get_citations = function(template, as_bibtex = TRUE) {
  data = get_metadata(template)
  refs = data$ReferencesAndLinks
  names(refs) = refs
  if (length(refs) > 0) {
    bib = lapply(refs, function(doi) {
      if (!grepl("doi.org", doi)) {
        return(NA)
      }
      res = httr::GET(doi, httr::accept("application/x-bibtex"))
      if (httr::status_code(res) <= 400) {
        res = httr::content(res, as = "text")
        if (as_bibtex) {
          if (!requireNamespace("bibtex", quietly = TRUE)) {
            warning("bibtex package required to turn to bibtex, returning text")
          } else {
            tfile = tempfile()
            writeLines(res, tfile)
            res = bibtex::read.bib(tfile)
          }
        }
      } else {
        res = NA
      }
      res
    })
  } else {
    bib = NULL
  }
  bib
}


#' @export
#' @rdname get_citations
get_metadata = function(template) {
  stopifnot(length(template) == 1,
            is.character(template),
            nchar(template) > 0)
  tdesc = file.path(tf_home(),
                    sprintf("tpl-%s", template),
                    "template_description.json")
  if (!file.exists(tdesc)) {
    exdir = tempfile()
    dir.create(exdir)
    skel_file = get_skeleton()
    out = utils::unzip(skel_file, exdir = exdir)
    tdesc = file.path(
      exdir,
      sprintf("tpl-%s", template),
      "template_description.json")
  }
  stopifnot(file.exists(tdesc))
  jsonlite::fromJSON(tdesc)
}
