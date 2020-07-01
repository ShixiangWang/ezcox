#' @import magrittr forestmodel
cox_panel <- function(headings = list(variable = "Group", n = "N", measure = "Hazard ratio", ci = NULL, p = "p")) {

  panels <- list(
    forest_panel(width = 0.03),
    forest_panel(width = 0.1, display = variable, fontface = "bold", heading = headings$variable),
    forest_panel(width = 0.1, display = level),
    forest_panel(width = 0.05, display = n, hjust = 1, heading = headings$n),
    forest_panel(width = 0.03, item = "vline", hjust = 0.5),
    forest_panel(
      width = 0.55, item = "forest", hjust = 0.5, heading = headings$measure,
      linetype = "dashed", line_x = 0
    ),
    forest_panel(width = 0.03, item = "vline", hjust = 0.5),
    forest_panel(
      width = 0.12,
      display = dplyr::if_else(reference, "Reference",
                        sprintf("%0.2f (%0.2f, %0.2f)", trans(estimate), trans(conf.low), trans(conf.high))
      ),
      heading = headings$ci,
      display_na = NA
    ),
    forest_panel(
      width = 0.05, display = dplyr::if_else(reference, "", format.pval(p.value, digits = 1, eps = 1e-3)), display_na = NA,
      hjust = 1, heading = headings$p
    ),
    forest_panel(width = 0.03)
  )

  panels
}

utils::globalVariables(
  c("variable", "level", "n", "reference", "estimate", "conf.low", "conf.high", "p.value", "trans",
    "forest_panel")
)
