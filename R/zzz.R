.onAttach <- function(libname, pkgname) {
  version <- packageDescription(pkgname, fields = "Version")

  msg <- paste0("Welcome to 'ezcox' package!
=======================================================================
", "You are using ", pkgname, " version ", version, "

Project home : https://github.com/ShixiangWang/ezcox
Documentation: https://shixiangwang.github.io/ezcox
Cite as      : arXiv:2110.14232
=======================================================================
                 ")
  packageStartupMessage(msg)
}
