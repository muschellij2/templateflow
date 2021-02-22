#' Get Skeleton File
#'
#' @return A character file name
#' @export
#'
#' @examples
#' res = get_skeleton()
get_skeleton = function() {

  TF_SKEL_URL = paste0(
    "https://raw.githubusercontent.com/templateflow/python-client/",
    "{release}/templateflow/conf/templateflow-skel.{ext}"
  )

  getter = function(ext, verbose = TRUE, type = "warn") {
    tfile = tempfile(fileext = paste0(".", ext))
    url = glue::glue(TF_SKEL_URL, release="master", ext = ext)
    res = httr::GET(
      url,
      httr::write_disk(tfile, overwrite = TRUE),
      if (verbose) httr::progress()
    )
    if (type == "warn") {
      httr::warn_for_status(res)
    }
    if (type == "stop") {
      httr::stop_for_status(res)
    }
    return(tfile)
  }

  md5file = getter("md5", type = "warn")
  zipfile =  getter("zip", type = "warn")
  md5 = strsplit(readLines(md5file), " ")[[1]][1]
  return(zipfile)
}

#' @export
#' @rdname get_skeleton
#' @param outdir output directory for skeleton
install_skeleton = function(outdir = tf_home()) {
  skel_file = get_skeleton()
  unzip(skel_file, overwrite = TRUE, exdir = outdir)
  return(outdir)
}

#' @export
#' @rdname get_skeleton
get_template_path = function() {
  dl_skeleton = TRUE
  if (file.exists(tf_home())) {
    dl_skeleton = FALSE
    dirs = list.dirs(tf_home(), recursive = FALSE)
    if (length(dirs) == 0) {
      dl_skeleton = TRUE
    } else {
      path = tf_home()
    }
  }
  if (dl_skeleton) {
    exdir = tempfile()
    dir.create(exdir)
    path = install_skeleton(outdir = exdir)
  }
  path
}