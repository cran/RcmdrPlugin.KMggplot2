.onAttach <- function(lib, pkg) {

  options(
    kmg2FontSize   = "16",
    kmg2FontFamily = 0,
    kmg2ColourSet  = 0,
    kmg2SaveGraph  = 0,
    kmg2Theme      = 1
  )

  packageStartupMessage()

}
