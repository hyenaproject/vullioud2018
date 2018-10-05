.onAttach <- function(libname, pkgname) {
  ## This function should not be called by the user.
  ## It displays a message when the package is being loaded.
  packageStartupMessage(## display message
    "\n Welcome to vullioud2018,",
    "\n ",
    "\n This package has not been conceived for general use.",
    "\n It has only been created to document the analysis of the paper:",
    "\n ",
    "\n Social support drives female dominance in the spotted hyena",
    "\n ",
    "\n by Vullioud, Davidian, Wachter, Rousset, Courtiol and H\u00F6ner,",
    "\n published in Nature Ecology Evolution (2018).",
    "\n ",
    "\n Type ?vullioud2018 for information about how to use this package!",
    "\n"
  )
}