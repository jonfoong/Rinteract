#' @title Graph conditional means and effects from a regression with interactions
#'
#'
#' @description Wrapper around ggplot that graphs all conditional means and effects from the output of `int_conditions`
#'
#' @param data output of a `int_conditions` call
#' @param facet A formula of the form X~Y with all interaction terms specified. Must be supplied.
#' @param digits How many decimal digits should be displayed?
#' @param width Maximum number of characters before wrapping the strip.
#' @param col_level Colour of background panel for all levels. Different colours should be used to differentiate between conditional levels and effects.
#' @param col_effect Colour of background panel for each effect type. Must be a named vector with names 0, 1, all, effect. Only needed if running 3 and 4 way interactions
#' @param alpha Alpha value for each facet row in a fourway interaction. The purpose of this is to create a dark, then light contrast between rows that are faceted.
#' @param col_label Background colour of label strip.
#'
#' @return A ggplot object that plots all conditional means and effects from the output of `int_conditions`
#' @examples
#' set.seed(1)
#' dat <- data.frame(X1 = sample(0:1, 100, replace=TRUE), X2 = sample(0:1, 100, replace=TRUE))
#' dat <- dat |> mutate(Y = X1 + 2*X2 + 3*X1*X2 + rnorm(1))
#' mod <- lm(Y~X1*X2, dat)
#' cond_tab <- int_conditions(mod, data = dat, main_vars = c("X1", "X2"), names = c(A1 = "X1", A2 = "X2"))
#' plot <- int_graph(cond_tab, facet = X1~X2)
#'
#' # Output is a ggplot object that can be manually manipulated further
#' plot + ggtitle("testing")
#'
#' @export
#' @importFrom dplyr %>% mutate
#' @importFrom ggplot2 ggplot theme_void theme element_rect element_text margin rel aes geom_rect scale_fill_manual geom_text
#' @importFrom ggh4x facet_nested



int_graph <-
  function(data, # a data frame of the form returned by Rinteract::int_conditions
           facet = NULL, # no default, must be supplied
           digits = 3, # number of digits to display after decimal point
           width = 10, # width for wrapping
           col_level = "gray95",
           col_effect = c("0" = "dodgerblue", # must be named vector
                          "1" = "chartreuse",
                          "all" ="tomato",
                          "effect" = "gold"),
           alpha = c(1, 0.5, 1, 0.5), # when plotting a 4way interaction, what is the alpha value for each facet row?
           col_label = "wheat"
  ){

    if(is.null(facet)) stop("facet argument cannot be empty!")

    data <-
      data %>%
      mutate(label = paste0(round(estimate, digits), "\n(", round(std.error, digits), ")"),
             x = 1, y = 1) %>%
      mutate(sign = ifelse(estimate>0, "pos", "neg"),
             sig = ifelse(p.value<.05, TRUE, FALSE))

    p <-
      data |>
      ggplot(aes(x, y, label = label)) +
      facet_nested(facet,
                   labeller = label_wrap_gen_both(width),
                   switch="y") +
      theme_void() +
      theme(legend.position = "none",
            strip.background = element_rect(fill=col_label, colour = "white",
                                            linewidth = 0.8,
                                            linetype = 1),
            strip.text = element_text(colour = "black", size = rel(1),
                                      margin = margin(5, 5, 5, 5))
      )

    # get number of variables

    facet_char <- setdiff(as.character(facet), "~")

    lhs <- trimws(unlist(strsplit(facet_char[1], "\\+")))[1]
    rhs <- trimws(unlist(strsplit(facet_char[2], "\\+")))[1]

    vars <-
      trimws(unlist(strsplit(facet_char, "\\+")))

    if(length(vars)==2){

      p <-
        p +
        geom_rect(aes(fill = value), xmin = -Inf, xmax = Inf,
                  ymin = -Inf, ymax = Inf, alpha = 1) +
        scale_fill_manual(values = c("Level" = "gray95", "Causal effect" = "cadetblue1"))

    } else if(length(vars)==3){

      # which variable to set effect colours

      eff_var <-
        c(lhs, rhs)[grepl("\\+", facet_char)]

      p <-
        p +
        geom_rect(aes(fill = eval(parse(text = eff_var))), xmin = -Inf, xmax = Inf,
                  ymin = -Inf, ymax = Inf, alpha = 1) +
        geom_rect(data = subset(data, value=="Level"), fill = col_level, xmin = -Inf, xmax = Inf,
                  ymin = -Inf, ymax = Inf) +
        scale_fill_manual(values=col_effect)


    } else if(length(vars)==4){

      loop_df <-
        data.frame(vals = names(col_effect), alpha = alpha)

      for (i in 1:4){

        p <-
          p +
          geom_rect(data = subset(data, eval(parse(text = lhs[1]))==loop_df$vals[i] & value!="Level"),
                    aes(fill = eval(parse(text = rhs[1]))),
                    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = loop_df$alpha[i])

      }

      p <-
        p +
        scale_fill_manual(values = col_effect) +
        geom_rect(data = subset(data, value=="Level"), fill = col_level, xmin = -Inf, xmax = Inf,
                  ymin = -Inf, ymax = Inf)

    }

    p +
      geom_text(aes(color = sign,
                    fontface = ifelse(sig, 2, 1))) +
      scale_color_manual(values = c("pos" = "black", "neg" = "red"))

  }
