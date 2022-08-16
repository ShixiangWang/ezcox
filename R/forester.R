# This is a modified version of forester from https://github.com/rdboyes/forester/blob/master/R/forester.R
# The original code is released under MIT license.
#
# MIT License
#
# Copyright (c) 2020 Randy Boyes
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


#' Create a forest plot for simple data
#'
#' @param data Data frame (required). The information to be displayed as the forest plot.
#' @param display_cols 4 columns stand for axis text and the forest data,
#' default using `c("term", "HR", "conf.low", "conf.high")`.
#' @param estimate_precision Integer. The number of decimal places on the estimate (default 2).
#' @param null_line_at Numeric. Default 0. Change to 1 if using relative measures such as OR, RR.
#' @param font_family String. The font to use for the ggplot. Default "mono".
#' @param x_scale_linear Logical. Default TRUE, change to FALSE for log scale
#' @param xlim Vector. Manually specify limits for the x axis as a vector length 2, i.e. c(low, high)
#' @param xbreaks Vector. X axis breaks to label. Specify limits in xlim if using this option.
#' @param point_sizes Vector. Length should be equal to 1 or nrow(left_side_data).
#' The sizes of the points in the center plot, where 3.25 is the default.
#' @param point_shape Vector. Length should be equal to 1 or nrow(left_side_data).
#' The shapes of the points in the center plot, where 16 (a filled circle) is the default.
#' @param label_hjust,label_vjust,label_color,label_size hjust, vjust
#' color and size for the label text.
#' @return a `ggplot` object.
#' @export
#' @examples
#' library(survival)
#'
#' t1 <- ezcox(lung, covariates = c(
#'   "age", "sex",
#'   "ph.karno", "pat.karno"
#' ))
#' p <- forester(t1, xlim = c(0, 1.5))
#' p
#' p2 <- forester(t1, xlim = c(0.5, 1.5))
#' p2
#' @testexamples
#' expect_s3_class(p, "ggplot")
#' expect_s3_class(p2, "ggplot")
forester <- function(data,
                     display_cols = c("Variable", "HR", "lower_95", "upper_95"),
                     estimate_precision = 2,
                     null_line_at = 1,
                     font_family = "mono",
                     x_scale_linear = TRUE,
                     xlim = NULL,
                     xbreaks = NULL,
                     point_sizes = 3,
                     point_shape = 16,
                     label_hjust = 0, label_vjust = -1,
                     label_color = "blue", label_size = 3) {
  tdata <- data[, display_cols]
  colnames(tdata) <- c("term", "estimate", "conf.low", "conf.high")

  tdata <- dplyr::mutate_if(
    tdata, is.numeric,
    ~ sprintf(., fmt = paste0("%#.", estimate_precision, "f"))
  )

  tdata[tdata == "NA"] <- " "
  # pretty formatting for confidence intervals
  tdata$Estimate <- ifelse(
    tdata$estimate == " ", " ",
    paste0(tdata$estimate, " (", tdata$conf.low, " to ", tdata$conf.high, ")")
  )
  tdata$estimate <- as.numeric(tdata$estimate)
  tdata$conf.low <- as.numeric(tdata$conf.low)
  tdata$conf.high <- as.numeric(tdata$conf.high)


  ######## calculations for the top and bottom of the plot
  tdata$row_num <- (nrow(tdata) - 1):0

  h_adj <- dplyr::case_when(
    font_family == "mono" ~ 0.2,
    font_family == "serif" ~ .43,
    font_family == "sans" ~ .37,
    TRUE ~ 0
  )

  slope_adj <- dplyr::case_when(
    font_family == "mono" ~ -0.175,
    font_family == "serif" ~ -.19,
    font_family == "sans" ~ -.16,
    TRUE ~ 0
  )

  font_adj <- 0.3 + h_adj + log(nrow(tdata)) * slope_adj

  y_low <- -.5 + font_adj + -.1381 * log(nrow(tdata))
  y_high <- 1.017 * nrow(tdata) - 0.6

  #### add shapes and sizes to tdata ########
  tdata$shape <- point_shape
  tdata$sizes <- point_sizes

  #### if a ci will be out of bounds, add arrow on the oob side  ###############
  g_oob <- tibble::tibble()

  if (!is.null(xlim)) {
    oob_arrows <- tdata

    oob_arrows$x_low <- xlim[1]
    oob_arrows$x_high <- xlim[2]

    ra <- sum(oob_arrows$conf.high > oob_arrows$x_high, na.rm = T) > 0
    la <- sum(oob_arrows$conf.low < oob_arrows$x_low, na.rm = T) > 0

    if (ra) {
      right_arrows <- dplyr::select(
        dplyr::filter(oob_arrows, conf.high > .data$x_high),
        start = .data$conf.low, end = .data$x_high, y = .data$row_num
      )
    }
    if (la) {
      left_arrows <- dplyr::select(
        dplyr::filter(oob_arrows, conf.low < .data$x_low),
        start = .data$conf.high, end = .data$x_low, y = .data$row_num
      )
    }

    if (ra && !la) {
      g_oob <- right_arrows
    } else if (!ra && la) {
      g_oob <- left_arrows
    } else if (ra && la) {
      g_oob <- rbind.data.frame(right_arrows, left_arrows)
    }
  }

  ########## the main figure - this will be overlaid on the table ##############
  center <- ggplot2::ggplot(data = tdata) +
    ggplot2::geom_point(
      ggplot2::aes(
        y = .data$row_num, x = .data$estimate,
        size = .data$sizes, shape = .data$shape
      ),
      na.rm = TRUE
    ) +
    ggplot2::scale_shape_identity() +
    ggplot2::scale_size_identity() +
    ggplot2::geom_errorbarh(
      data = tdata, ggplot2::aes(
        y = .data$row_num,
        xmin = .data$conf.low,
        xmax = .data$conf.high
      ),
      height = .1,
      na.rm = TRUE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(y = .data$row_num, x = if (nrow(g_oob) > 0) {
        dplyr::mutate(
          oob_arrows,
          estimate2 = dplyr::case_when(
            conf.high > x_high & conf.low < x_low ~ (x_low + x_high) / 2.1,
            conf.high > x_high ~ (conf.low + x_high) / 2.1,
            conf.low < x_low ~ (x_low + conf.high) / 2.1,
            TRUE ~ .data$estimate
          )
        ) %>% dplyr::pull(estimate2)
      } else {
        .data$estimate
      }, label = .data$Estimate),
      hjust = label_hjust, vjust = label_vjust,
      color = label_color, size = label_size
    ) +
    ggplot2::theme_classic() + # base theme
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(), # remove axis, make bg transparent
      axis.line.y = ggplot2::element_blank(),
      text = ggplot2::element_text(family = font_family, size = 12),
      panel.background = ggplot2::element_rect(fill = "transparent"),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = "transparent"),
      legend.box.background = ggplot2::element_rect(fill = "transparent")
    ) +
    ggplot2::geom_vline(xintercept = null_line_at, linetype = "dashed") +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      breaks = tdata$row_num,
      labels = tdata$term
    ) +
    ggplot2::xlab(display_cols[2])

  ### add oob arrows if required ###
  if (nrow(g_oob) > 0) {
    center <- center +
      ggplot2::geom_segment(
        data = g_oob,
        ggplot2::aes(
          x = .data$start,
          xend = .data$end,
          y = .data$y,
          yend = .data$y
        ),
        arrow = ggplot2::arrow(
          angle = 15,
          type = "closed",
          length = grid::unit(0.05, "npc")
        )
      )
  }

  ####### fix plot zoom ######
  if (is.null(xlim)) {
    center <- center + ggplot2::coord_cartesian(ylim = c(y_low, y_high))
  } else {
    center <- center + ggplot2::coord_cartesian(ylim = c(y_low, y_high), xlim = xlim)
  }

  ######## handle breaks, log vs linear scales ########
  if (x_scale_linear) {
    if (is.null(xbreaks)) {
      center <- center + ggplot2::scale_x_continuous(
        labels = scales::number_format(accuracy = 0.1),
        expand = c(0, 0)
      )
    } else {
      center <- center + ggplot2::scale_x_continuous(
        labels = scales::number_format(accuracy = 0.1),
        breaks = xbreaks,
        expand = c(0, 0)
      )
    }
  } else {
    if (is.null(xbreaks)) {
      center <- center + ggplot2::scale_x_log10(
        labels = scales::number_format(accuracy = 0.1),
        expand = c(0, 0)
      )
    } else {
      center <- center + ggplot2::scale_x_log10(
        labels = scales::number_format(accuracy = 0.1),
        breaks = xbreaks,
        expand = c(0, 0)
      )
    }
  }

  center
}