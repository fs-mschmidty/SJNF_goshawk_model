init_ee <- function() {
  suppressWarnings(
    rgee::ee_Initialize(
      auth_mode = "notebook",
      drive = "TRUE"
    )
  )
}
