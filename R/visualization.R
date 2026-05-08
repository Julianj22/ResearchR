#' @title Scatter Plot Builder
#' @description Builds a basic scatter plot with the ability to add a third and fourth dimension.
#' @param data This is the data set containing the variables you want to visualize.
#' @param x Variable for your x-axis
#' @param y Variable for your y-axis
#' @param color_by Another variable you could use to conditionally change the color of your points. If numeric, the variable will manifest as a color gradient—if categorical the variable will manifest as a discrete set of colors.
#' @param size_by Another variable you could use to conditionally change the size of your points. This will only work with a numeric variable.
#' @param title A string containing the title of your graph
#' @param subtitle A string containing the subtitle of your graph
#' @param x_name The name of your x-variable. Ex: your variable might be `b_birds`, but you may want it to appear as "Brown Birds" on the graph.
#' @param y_name The name of your y-variable.
#' @param color_name The name of your color_by variable.
#' @param size_name The name of your size_by variable.
#' @param line Set to TRUE or FALSE if you would or would not like a line of best fit drawn through your points.
#' @param se Set to TRUE or FALSE if you would or would not like the standard error visualized with your line of best fit.
#' @return A scatter plot made to the user's specifications
#' @export

scatter_plot <- function(data,
                         x,
                         y,
                         color_by = NULL,
                         size_by = NULL,
                         title = "I don't like nice titles",
                         subtitle = "And I HATE good subtitles",
                         x_name = rlang::as_label(rlang::enquo(x)),
                         y_name = rlang::as_label(rlang::enquo(y)),
                         color_name = rlang::as_label(rlang::enquo(color_by)),
                         size_name = rlang::as_label(rlang::enquo(size_by)),
                         line = TRUE,
                         se=FALSE) {

  x_quo     <- rlang::enquo(x)
  y_quo     <- rlang::enquo(y)
  color_quo <- rlang::enquo(color_by)
  size_quo  <- rlang::enquo(size_by)

  # --- input checks ---
  if (!is.data.frame(data)) stop("Woah now! Data must be a data frame.")

  x_col <- data[[rlang::as_label(x_quo)]]
  y_col <- data[[rlang::as_label(y_quo)]]

  if (!is.numeric(x_col) || !is.numeric(y_col)) {
    stop("Hold up! Your x and y variables must both be numeric.")
  }

  if (!rlang::quo_is_null(size_quo)) {
    size_col <- data[[rlang::as_label(size_quo)]]
    if (!is.numeric(size_col)) stop("Wait a sec! Your size_by variable must be numeric.")
  }
  # --- build plot ---
  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_quo, y = !!y_quo,
                                          color = !!color_quo,
                                          size = !!size_quo)) +
    ggplot2::geom_point(alpha = 0.75, shape = 16) +

    # clean, professional theme base
    ggplot2::theme_minimal(base_size = 13) +

    ggplot2::theme(
      # typography
      text = ggplot2::element_text(family = "Times New Roman", color = "#2c2c2c"),
      plot.title = ggplot2::element_text(face = "bold", size = 15, hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 11, color = "#666666", hjust = 0),

      # axes
      axis.title = ggplot2::element_text(face = "bold", size = 11),
      axis.text = ggplot2::element_text(size = 10, color = "#444444"),
      axis.line = ggplot2::element_line(color = "#cccccc"),

      # grid — keep horizontal only for cleanliness
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#eeeeee"),

      # legend
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold", size = 10),
      legend.text = ggplot2::element_text(size = 9),

      # padding
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    )+
    ggplot2::theme(text = ggplot2::element_text(family = "Times New Roman")) +
    ggplot2::labs(title = title, subtitle = subtitle,
                  x = x_name, y = y_name,
                  color = color_name, size = size_name)

  if (line) {
    p <- p + ggplot2::geom_smooth(method = "lm", se = se)
  }

  return(p)
}



#' @title Box plot Builder
#' @description Builds a basic box plot with the ability to add a second dimension.
#' @param data This is the data set containing the variables you want to visualize.
#' @param x Variable you want to visualize the distribution of. This must be a numeric variable.
#' @param y Another variable you could use to observe the distribution of your original variable by level. Ex: Heights of squirrels by borough—here, "borough" would be the y variable. This variable must be categorical.
#' @param title A string containing the title of your graph
#' @param subtitle A string containing the subtitle of your graph
#' @param x_name The name of your x-variable. Ex: your variable might be `b_birds`, but you may want it to appear as "Brown Birds" on the graph.
#' @param y_name The name of your y-variable.
#' @return A box plot made to the user's specifications
#' @export

box_plot <- function(data,
                         x,
                         y=NULL,
                         title = "I don't like nice titles",
                         subtitle = "And I HATE good subtitles",
                         x_name = rlang::as_label(rlang::enquo(x)),
                         y_name = rlang::as_label(rlang::enquo(y))
                     ) {

  x_quo     <- rlang::enquo(x)
  y_quo     <- rlang::enquo(y)

  # --- input checks ---
  if (!is.data.frame(data)) stop("Woah now! Data must be a data frame.")

  x_col <- data[[rlang::as_label(x_quo)]]
  y_col <- data[[rlang::as_label(y_quo)]]

  if (!is.numeric(x_col)) {
    stop("Hold up! Your x variable must be numeric.")
  }

  if (!rlang::quo_is_null(y_quo)) {
    y_col <- data[[rlang::as_label(y_quo)]]
    if (!is.character(y_col) && !is.factor(y_col)) stop("Wait a sec! Your y variable must be categorical.")
  }

  # --- build plot ---
  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!y_quo, y = !!x_quo)) +
    ggplot2::geom_boxplot(outlier.colour = "red", outlier.shape = 1) +

    # clean, professional theme base
    ggplot2::theme_minimal(base_size = 13) +

    ggplot2::theme(
      # typography
      text = ggplot2::element_text(family = "Times New Roman", color = "#2c2c2c"),
      plot.title = ggplot2::element_text(face = "bold", size = 15, hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 11, color = "#666666", hjust = 0),

      # axes
      axis.title = ggplot2::element_text(face = "bold", size = 11),
      axis.text = ggplot2::element_text(size = 10, color = "#444444"),
      axis.line = ggplot2::element_line(color = "#cccccc"),

      # grid — keep horizontal only for cleanliness
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#eeeeee"),

      # legend
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold", size = 10),
      legend.text = ggplot2::element_text(size = 9),

      # padding
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    )+
    ggplot2::theme(text = ggplot2::element_text(family = "Times New Roman")) +
    ggplot2::labs(title = title, subtitle = subtitle,
                  x = ifelse(y_name=="NULL","",y_name), y = x_name)

  return(p)
}


#' @title Density plot Builder
#' @description Builds a basic density plot with the ability to add a second dimension.
#' @param data This is the data set containing the variables you want to visualize.
#' @param x Variable you want to visualize the distribution of. This must be a numeric variable.
#' @param color_by Another variable you could use to observe the distribution of your original variable by level. Ex: Heights of squirrels by borough—here, "borough" would be the color_by variable. This variable must be categorical.
#' @param facet_by Another variable you could use to observe the distribution of your original variable by level. Must be categorical.
#' @param title A string containing the title of your graph
#' @param subtitle A string containing the subtitle of your graph
#' @param x_name The name of your x-variable. Ex: your variable might be `b_birds`, but you may want it to appear as "Brown Birds" on the graph.
#' @param color_name The name of your color variable.
#' @return A density plot made to the user's specifications
#' @export

density_plot <- function(data,
                         x,
                         color_by = NULL,
                         facet_by=NULL,
                         title = "I don't like nice titles",
                         subtitle = "And I HATE good subtitles",
                         x_name = rlang::as_label(rlang::enquo(x)),
                         color_name = rlang::as_label(rlang::enquo(color_by))
) {
  x_quo     <- rlang::enquo(x)
  color_quo <- rlang::enquo(color_by)
  facet_quo <- rlang::enquo(facet_by)

  # --- input checks ---
  if (!is.data.frame(data)) stop("Woah now! Data must be a data frame.")

  x_col <- data[[rlang::as_label(x_quo)]]
  if (!is.numeric(x_col)) stop("Hold up! Your x variable must be numeric.")

  if (!rlang::quo_is_null(color_quo)) {
    color_col <- data[[rlang::as_label(color_quo)]]
    if (!is.character(color_col) && !is.factor(color_col)) {
      stop("Wait a sec! Your color_by variable must be categorical.")
    }
  }

  if (!rlang::quo_is_null(facet_quo)) {
    facet_col <- data[[rlang::as_label(facet_quo)]]
    if (!is.character(facet_col) && !is.factor(facet_col)) {
      stop("Wait a sec! Your facet_by variable must be categorical.")
    }
  }

  # --- build plot ---
  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_quo, fill=!!color_quo)) +
    ggplot2::geom_density(alpha=0.75) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Times New Roman", color = "#2c2c2c"),
      plot.title = ggplot2::element_text(face = "bold", size = 15, hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 11, color = "#666666", hjust = 0),
      axis.title = ggplot2::element_text(face = "bold", size = 11),
      axis.text = ggplot2::element_text(size = 10, color = "#444444"),
      axis.line = ggplot2::element_line(color = "#cccccc"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#eeeeee"),
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold", size = 10),
      legend.text = ggplot2::element_text(size = 9),
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, x = x_name, y="Density")

  if (!rlang::quo_is_null(facet_quo)) {
    p <- p +
      ggplot2::facet_wrap(ggplot2::vars(!!facet_quo), ncol = 1, strip.position = "top") +
      ggplot2::theme(
        strip.text = ggplot2::element_text(size = 8, vjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = ggplot2::element_text(hjust = 0)
      ) +
      ggplot2::labs(title = title, subtitle = subtitle, y = "Density",
                     x= x_name)
  }

  return(p)
}



#' @title Bar plot Builder
#' @description Builds a basic bar plot with the ability to add a second dimension.
#' @param data This is the data set containing the variables you want to visualize.
#' @param x Variable you want to see the count of by level. Ex: Number of brown squirrels vs. gray squirrels vs. black squirrels. The x-variable here would be "fur color". Must be categorical
#' @param y Another variable you could use to observe proportions. Ex: Number of brown squirrels that are from Brooklyn vs. Queens vs. Manhattan. The y-variable here would be "borough". Must be categorical.
#' @param title A string containing the title of your graph
#' @param subtitle A string containing the subtitle of your graph
#' @param x_name The name of your x-variable. Ex: your variable might be `b_birds`, but you may want it to appear as "Brown Birds" on the graph.
#' @param y_name The name of your y-variable.
#' @param flip Set to TRUE or FALSE if you would like to flip the plot's axes.
#' @return A bar plot made to the user's specifications
#' @export

bar_plot <- function(data,
                     x,
                     y=NULL,
                     title = "I don't like nice titles",
                     subtitle = "And I HATE good subtitles",
                     x_name = rlang::as_label(rlang::enquo(x)),
                     y_name = rlang::as_label(rlang::enquo(y)),
                     flip=FALSE
) {

  x_quo     <- rlang::enquo(x)
  y_quo     <- rlang::enquo(y)

  # --- input checks ---
  if (!is.data.frame(data)) stop("Woah now! Data must be a data frame.")

  x_col <- data[[rlang::as_label(x_quo)]]


  if (!is.character(x_col) && !is.factor(x_col)) {
    stop("Hold up! Your x variable must be categorical.")
  }

  if (!rlang::quo_is_null(y_quo)) {
    y_col <- data[[rlang::as_label(y_quo)]]
    if (!is.character(y_col) && !is.factor(y_col)) stop("Wait a sec! Your y variable must be categorical.")
  }



  # --- build plot ---


    p<-ggplot2::ggplot(data, ggplot2::aes(x = !!x_quo, fill = !!y_quo))+
      geom_bar()+
      ggplot2::theme_minimal(base_size = 13) +

    ggplot2::theme(
      # typography
      text = ggplot2::element_text(family = "Times New Roman", color = "#2c2c2c"),
      plot.title = ggplot2::element_text(face = "bold", size = 15, hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 11, color = "#666666", hjust = 0),

      # axes
      axis.title = ggplot2::element_text(face = "bold", size = 11),
      axis.text = ggplot2::element_text(size = 10, color = "#444444"),
      axis.line = ggplot2::element_line(color = "#cccccc"),

      # grid — keep horizontal only for cleanliness
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#eeeeee"),

      # legend
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold", size = 10),
      legend.text = ggplot2::element_text(size = 9),

      # padding
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    )+
    ggplot2::theme(text = ggplot2::element_text(family = "Times New Roman")) +
    ggplot2::labs(title = title, subtitle = subtitle,
                  fill = ifelse(y_name=="NULL","",y_name), x = x_name)

    if(flip){
      p<-p+coord_flip()
    }

  return(p)
}



#' @title Density plot Builder
#' @description Builds a basic density plot with the ability to add a second dimension.
#' @param data This is the data set containing the variables you want to visualize.
#' @param x Variable you want to visualize the distribution of. This must be a numeric variable.
#' @param color_by Another variable you could use to observe the distribution of your original variable by level. Ex: Heights of squirrels by borough—here, "borough" would be the color_by variable. This variable must be categorical.
#' @param facet_by Another variable you could use to observe the distribution of your original variable by level. Must be categorical.
#' @param title A string containing the title of your graph
#' @param subtitle A string containing the subtitle of your graph
#' @param x_name The name of your x-variable. Ex: your variable might be `b_birds`, but you may want it to appear as "Brown Birds" on the graph.
#' @param color_name The name of your color variable.
#' @return A density plot made to the user's specifications
#' @export

density_plot <- function(data,
                         x,
                         color_by = NULL,
                         facet_by=NULL,
                         title = "I don't like nice titles",
                         subtitle = "And I HATE good subtitles",
                         x_name = rlang::as_label(rlang::enquo(x)),
                         color_name = rlang::as_label(rlang::enquo(color_by))
) {
  x_quo     <- rlang::enquo(x)
  color_quo <- rlang::enquo(color_by)
  facet_quo <- rlang::enquo(facet_by)

  # --- input checks ---
  if (!is.data.frame(data)) stop("Woah now! Data must be a data frame.")

  x_col <- data[[rlang::as_label(x_quo)]]
  if (!is.numeric(x_col)) stop("Hold up! Your x variable must be numeric.")

  if (!rlang::quo_is_null(color_quo)) {
    color_col <- data[[rlang::as_label(color_quo)]]
    if (!is.character(color_col) && !is.factor(color_col)) {
      stop("Wait a sec! Your color_by variable must be categorical.")
    }
  }

  if (!rlang::quo_is_null(facet_quo)) {
    facet_col <- data[[rlang::as_label(facet_quo)]]
    if (!is.character(facet_col) && !is.factor(facet_col)) {
      stop("Wait a sec! Your facet_by variable must be categorical.")
    }
  }

  # --- build plot ---
  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_quo, fill=!!color_quo)) +
    ggplot2::geom_density(alpha=0.75) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Times New Roman", color = "#2c2c2c"),
      plot.title = ggplot2::element_text(face = "bold", size = 15, hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 11, color = "#666666", hjust = 0),
      axis.title = ggplot2::element_text(face = "bold", size = 11),
      axis.text = ggplot2::element_text(size = 10, color = "#444444"),
      axis.line = ggplot2::element_line(color = "#cccccc"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#eeeeee"),
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold", size = 10),
      legend.text = ggplot2::element_text(size = 9),
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, x = x_name, y="Density")

  if (!rlang::quo_is_null(facet_quo)) {
    p <- p +
      ggplot2::facet_wrap(ggplot2::vars(!!facet_quo), ncol = 1, strip.position = "top") +
      ggplot2::theme(
        strip.text = ggplot2::element_text(size = 8, vjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = ggplot2::element_text(hjust = 0)
      ) +
      ggplot2::labs(title = title, subtitle = subtitle, y = "Density",
                     x= x_name)
  }

  return(p)
}


#' @title Column plot Builder
#' @description Builds a basic column plot with the ability to add a third dimension.
#' @param data This is the data set containing the variables you want to visualize.
#' @param x Variable for your x-axis. Must be categorical
#' @param y Variable for your y-axis. Must be numeric.
#' @param color_by Another variable you can use to observe proportion. Must be categorical
#' @param title A string containing the title of your graph
#' @param subtitle A string containing the subtitle of your graph
#' @param x_name The name of your x-variable. Ex: your variable might be `b_birds`, but you may want it to appear as "Brown Birds" on the graph.
#' @param y_name The name of your y-variable.
#' @param color_name The name of your color_by variable.
#' @param flip Set to TRUE or FALSE if you would like to flip the plot's axes.
#' @return A column plot made to the user's specifications
#' @export

column_plot <- function(data,
                     x,
                     y,
                     color_by=NULL,
                     title = "I don't like nice titles",
                     subtitle = "And I HATE good subtitles",
                     x_name = rlang::as_label(rlang::enquo(x)),
                     y_name = rlang::as_label(rlang::enquo(y)),
                     color_name = rlang::as_label(rlang::enquo(color_by)),
                     flip=FALSE
) {

  x_quo     <- rlang::enquo(x)
  y_quo     <- rlang::enquo(y)
  color_quo     <- rlang::enquo(color_by)

  # --- input checks ---
  if (!is.data.frame(data)) stop("Woah now! Data must be a data frame.")

  x_col <- data[[rlang::as_label(x_quo)]]
  y_col <- data[[rlang::as_label(y_quo)]]


  if (!is.character(x_col) && !is.factor(x_col)) {
    stop("Hold up! Your x variable must be categorical.")
  }

  if (!is.numeric(y_col)) stop("Wait a sec! Your y variable must be numeric.")

  if (!rlang::quo_is_null(color_quo)) {
    color_col <- data[[rlang::as_label(color_quo)]]
    if (!is.character(color_col) && !is.factor(color_col)) {
      stop("Wait a sec! Your color_by variable must be categorical.")
    }
  }


  # --- build plot ---


  p<-ggplot2::ggplot(data, ggplot2::aes(x = !!x_quo, y=!!y_quo, fill = !!color_quo))


  if (!rlang::quo_is_null(color_quo)) {
    p <- p + ggplot2::geom_col(position = "dodge")
  } else {
    p <- p + ggplot2::geom_col()
  }

    p<-p+ggplot2::theme_minimal(base_size = 13) +

    ggplot2::theme(
      # typography
      text = ggplot2::element_text(family = "Times New Roman", color = "#2c2c2c"),
      plot.title = ggplot2::element_text(face = "bold", size = 15, hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 11, color = "#666666", hjust = 0),

      # axes
      axis.title = ggplot2::element_text(face = "bold", size = 11),
      axis.text = ggplot2::element_text(size = 10, color = "#444444"),
      axis.line = ggplot2::element_line(color = "#cccccc"),

      # grid — keep horizontal only for cleanliness
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#eeeeee"),

      # legend
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold", size = 10),
      legend.text = ggplot2::element_text(size = 9),

      # padding
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    )+
    ggplot2::theme(text = ggplot2::element_text(family = "Times New Roman")) +
    ggplot2::labs(title = title, subtitle = subtitle,
                  fill = ifelse(color_name=="NULL","",color_name), x = x_name, y=y_name)

  if(flip){
    p<-p+coord_flip()
  }

  return(p)
}


#' @title Line Plot Builder
#' @description Builds a basic line plot with the ability to add a third dimension and fourth dimension.
#' @param data This is the data set containing the variables you want to visualize.
#' @param x Variable for your x-axis. This must be numeric AND continuous. Ex: Time is often a variable used here.
#' @param y Variable for your y-axis. This must be numeric.
#' @param color_by Another variable you could use to conditionally change the color of your points. Must be categorical.
#' @param line_type Another variable you could use to conditionally change the type of your line. This will only work with a categorical variable.
#' @param title A string containing the title of your graph
#' @param subtitle A string containing the subtitle of your graph
#' @param x_name The name of your x-variable. Ex: your variable might be `b_birds`, but you may want it to appear as "Brown Birds" on the graph.
#' @param y_name The name of your y-variable.
#' @param color_name The name of your color_by variable.
#' @param line_name The name of your size_by variable.
#' @return A line plot made to the user's specifications
#' @export

line_plot <- function(data,
                         x,
                         y,
                         color_by = NULL,
                         line_type = NULL,
                         title = "I don't like nice titles",
                         subtitle = "And I HATE good subtitles",
                         x_name = rlang::as_label(rlang::enquo(x)),
                         y_name = rlang::as_label(rlang::enquo(y)),
                         color_name = rlang::as_label(rlang::enquo(color_by)),
                         line_name = rlang::as_label(rlang::enquo(line_type))
                      ) {

  x_quo     <- rlang::enquo(x)
  y_quo     <- rlang::enquo(y)
  color_quo <- rlang::enquo(color_by)
  line_quo  <- rlang::enquo(line_type)

  # --- input checks ---
  if (!is.data.frame(data)) stop("Woah now! Data must be a data frame.")

  x_col <- data[[rlang::as_label(x_quo)]]
  y_col <- data[[rlang::as_label(y_quo)]]

  if (!is.numeric(x_col) || !is.numeric(y_col)) {
    stop("Hold up! Your x and y variables must both be numeric.")
  }

  if (!rlang::quo_is_null(line_quo)) {
    line_col <- data[[rlang::as_label(line_quo)]]
    if (!is.character(line_col) && !is.factor(line_col)) stop("Wait a sec! Your line_type variable must be categorical.")
  }


  if (!rlang::quo_is_null(color_quo)) {
    color_col <- data[[rlang::as_label(color_quo)]]
    if (!is.character(color_col) && !is.factor(color_col)) stop("Wait a sec! Your color_by variable must be categorical.")
  }
  # --- build plot ---
  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_quo, y = !!y_quo,
                                          color = !!color_quo,
                                          linetype = !!line_quo)) +
    ggplot2::geom_line(alpha = 0.8) +

    # clean, professional theme base
    ggplot2::theme_minimal(base_size = 13) +

    ggplot2::theme(
      # typography
      text = ggplot2::element_text(family = "Times New Roman", color = "#2c2c2c"),
      plot.title = ggplot2::element_text(face = "bold", size = 15, hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 11, color = "#666666", hjust = 0),

      # axes
      axis.title = ggplot2::element_text(face = "bold", size = 11),
      axis.text = ggplot2::element_text(size = 10, color = "#444444"),
      axis.line = ggplot2::element_line(color = "#cccccc"),

      # grid — keep horizontal only for cleanliness
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#eeeeee"),

      # legend
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold", size = 10),
      legend.text = ggplot2::element_text(size = 9),

      # padding
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    )+
    ggplot2::theme(text = ggplot2::element_text(family = "Times New Roman")) +
    ggplot2::labs(title = title, subtitle = subtitle,
                  x = x_name, y = y_name,
                  color = color_name, linetype = line_name)


  return(p)
}

