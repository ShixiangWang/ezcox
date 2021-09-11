.onAttach <- function(libname, pkgname) {
  version <- packageDescription(pkgname, fields = "Version")

  msg <- paste0("Welcome to 'ezcox' package!
=======================================================================
", "You are using ", pkgname, " version ", version, "

Project home : https://github.com/ShixiangWang/ezcox
Documentation: https://shixiangwang.github.io/ezcox

Run citation(\"ezcox\") to see how to cite 'ezcox'.
=======================================================================
                 ")
  packageStartupMessage(msg)
}
