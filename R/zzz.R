.onAttach <- function(libname, pkgname) {
  version <- packageDescription(pkgname, fields = "Version")

  msg <- paste0("Welcome to 'ezcox' package!
=======================================================================
", "You are using ", pkgname, " version ", version, "

Github page  : https://github.com/ShixiangWang/ezcox
Documentation: https://shixiangwang.github.io/ezcox/articles/ezcox.html

If you use it in academic field, please cite:

  Wang, Shixiang, et al. “The predictive power of tumor
mutational burden in lung cancer immunotherapy response is influenced
by patients’ sex.” International journal of cancer (2019).
=======================================================================
                 ")
  packageStartupMessage(msg)
}
