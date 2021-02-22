#' TemplateFlow Home Folder
#'
#' @return A character path
#' @export
#'
#' @examples
#' tf_home()
tf_home = function() {
  TF_DEFAULT_HOME = "~/.cache/templateflow"
  Sys.getenv("TEMPLATEFLOW_HOME", unset = TF_DEFAULT_HOME)
}