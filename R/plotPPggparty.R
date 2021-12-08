#' Plot using ggparty
#'
#' Put Indipendent Variable
#' @usage pp_ggparty(PPTreeregOBJ,ind_variable,...)
#' @param PPTreeregOBJ PPTreereg object
#' @param ind_variable independent varible to show
#' @param ... arguments to be passed to methods
#' @export
#' @keywords tree

pp_ggparty <- function(PPTreeregOBJ,ind_variable,...){
  predict = origclass = id = nodesize = 1
  partyOBJ <- as_party(PPTreeregOBJ)
  p <- ggparty::ggparty(partyOBJ, terminal_space = 0.7 )+
    ggparty::geom_edge() +
    ggparty::geom_node_splitvar()  +
    ggparty::geom_node_plot(gglist = list(ggplot2::geom_point(ggplot2::aes(x = eval(parse(text = ind_variable)),
                                                                           y = predict,
                                                                           col = origclass),
                                                              alpha = 0.4),
                                          ggplot2::xlab(ind_variable),
                                          ggplot2::ylab("                            predict"),
                                          ggplot2::theme_bw(base_size = 10)),
                            height = 0.5,
                            shared_axis_labels = TRUE
    )+
    ggparty::geom_node_plot(gglist = list(ggplot2::geom_point(ggplot2::aes(x = eval(parse(text = ind_variable)),
                                                                           y = origclass,
                                                                           col = origclass),
                                                              alpha = 0.4),
                                          ggplot2::xlab(" "),
                                          ggplot2::theme_bw(base_size = 10)),
                            height = 0.5,
                            shared_axis_labels = TRUE,
                            nudge_y = -0.3,
                            legend_separator = TRUE)+

    ggparty::geom_node_label(
      ggplot2::aes(label = paste0("Node ", id, ", N = ", nodesize)),
      ids = "terminal",
      size = 3,
      nudge_y = 0.01
    )

  p
}
