#' @title Graph conditional means and effects from a regression with interactions
#' @description Wrapper around ggplot that graphs all conditional means and effects from the output of `int_conditions`
#'
#' @param data output of a `int_conditions` call
#' @param facet A formula of the form X~Y with all interaction terms specified. Must be supplied.
#' @param digits How many decimal digits should be displayed?
#' @param width Maximum number of characters before wrapping the strip.
#' @param col_effect Colour of background panel for each effect type. The default colors are taken from `RColorBrewer`'s Pastel1 palette. Length of vector must be either 1 (in which case all faceted effects have the same color) or n, where n is the number of faceted variables in the dataset.
#' @param col_level Colour of background panel for all levels.
#' @param col_label Background colour of label strip.
#' @param eff_var Which variable should the color for the effect panel be faceted on? Defaults to the variable used for the outer facet.
#' @param alpha_e The alpha level for the effect panels. Defaults to 1.
#' @param alpha_l The alpha level for the level panels. Defaults to 1.
#'
#' @return A ggplot object that plots all conditional means and effects from the output of `int_conditions`. Values in red are negative estimates while bold represents estimates with p<0.05.
#' @examples
#' library(ggplot2)
#'
#' set.seed(1)
#' dat <- data.frame(X1 = sample(0:1, 100, replace=TRUE), X2 = sample(0:1, 100, replace=TRUE), X3 = sample(0:1, 100, replace=TRUE))
#' dat <- dat |> transform(Y = X1 + 2*X2 + 3*X1*X2*X3 + rnorm(1))
#' mod <- lm(Y~X1*X2*X3, dat)
#' cond_tab <- int_conditions(mod, data = dat, .names = c(A1 = "X1", A2 = "X2", A3 = "X3"))
#' plot <- int_graph(cond_tab, facet = A1~A2+A3)
#'
#' # Output is a ggplot object that can be manually manipulated further
#' plot + ggtitle("testing")
#'
#' @export
#' @importFrom ggplot2 ggplot theme_void theme element_rect element_text margin rel aes geom_rect scale_fill_manual geom_text scale_color_manual
#' @importFrom ggh4x facet_nested



int_graph <-
  function(data, # a data frame of the form returned by Rinteract::int_conditions
           facet = NULL, # no default, must be supplied
           digits = 3, # number of digits to display after decimal point
           width = 10, # width for wrapping
           col_effect = c("#FBB4AE",
                          "#B3CDE3",
                          "#CCEBC5",
                          "#DECBE4"),
           col_level = "#F0F0F0",
           col_label = "#DEEBF7",
           eff_var = NULL,
           alpha_e = 1,
           alpha_l = 1
  ){

    if(is.null(facet)) stop("facet argument cannot be empty!")

    # get number of variables

    facet_char <- setdiff(as.character(facet), "~")

    lhs <- trimws(strsplit_vec(facet_char[1], "\\+"))[1]
    rhs <- trimws(strsplit_vec(facet_char[2], "\\+"))[1]

    vars <-
      trimws(unlist(strsplit(facet_char, "\\+")))

    # which variable to set effect colours

    # if not specified, it is by default the outer facet(s)

    if(is.null(eff_var)) eff_var <- c(rhs, lhs)[grepl("\\+", facet_char)][1]
    if(length(eff_var)==0) eff_var <- vars[1]

    # now set colours

    names_col_effect <- c("base", "1", "all", "effect")

    eff_var_vals <- unique(data[,eff_var])

    names_col_effect[names_col_effect=="base"] <-
      setdiff(eff_var_vals, names_col_effect)

    names(col_effect) <- names_col_effect


    # first round digits

    data[,c("estimate", "std.error", "p.value")] <-
      apply(data[,c("estimate", "std.error", "p.value")], 2, function(x) round(x, digits))

    # add some vars for plotting later

    data <-
      transform(data,
                label = ifelse(!is.na(std.error),
                               paste0(estimate, "\n(", std.error, ")"), estimate),
                x = 1,
                y = 1,
                sign = ifelse(estimate > 0, "pos", "neg"),
                sig = ifelse(p.value < .05, TRUE, FALSE))

    # factorise all vars

    for (i in vars){
      vals <- data[,i]

      var_levels <- c(setdiff(unique(vals), names_col_effect[-1]),
                      names_col_effect[-1])

      data[,i] <- factor(vals, levels = var_levels)

    }


    # Now set the background

    p <-
      ggplot(data, aes(x, y, label = label)) +
      facet_nested(facet,
                   labeller = label_wrap_gen_both(width),
                   switch = "y") +
      theme_void() +
      theme(legend.position = "none",
            strip.background = element_rect(fill = col_label,
                                            colour = "white",
                                            linewidth = 0.1,
                                            linetype = 1),
            strip.text = element_text(colour = "black",
                                      size = rel(1),
                                      margin = margin(5, 5, 5, 5))
      )

    p <- p +
      # first fill with effect colors
      geom_rect(aes(fill = eval(parse(text = eff_var))), xmin = -Inf, xmax = Inf,
                ymin = -Inf, ymax = Inf, alpha = alpha_e) +
      scale_fill_manual(values=col_effect) +

      # now layer over the colors for the levels; if no fill enter NA
      geom_rect(data = subset(data, value=="Level"), fill = col_level, xmin = -Inf, xmax = Inf,
                ymin = -Inf, ymax = Inf, alpha = alpha_l) +

      # now add colors for signs and significance
      geom_text(aes(color = sign,
                    fontface = ifelse(sig, 2, 1))) +
      scale_color_manual(values = c("pos" = "black", "neg" = "red"))

  }
