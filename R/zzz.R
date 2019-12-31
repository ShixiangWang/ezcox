.onAttach <- function(libname, pkgname) {
  version <- packageDescription(pkgname, fields = "Version")

  msg <- paste0("Welcome to 'ezcox' package!
=======================================================================
", "You are using ", pkgname, " version ", version, "

Github page  : https://github.com/ShixiangWang/ezcox
Documentation: https://shixiangwang.github.io/ezcox/articles/ezcox.html

Run citation(\"ezcox\") to see how to cite 'ezcox'.
=======================================================================
                 ")
  packageStartupMessage(msg)
}
