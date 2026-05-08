data(mtcars)
data(iris)

test_that("stops when data is not a data frame", {
  expect_error(scatter_plot(data = list(x = 1, y = 2), x = x, y = y),
               "data must be a data frame")
})

test_that("stops when x or y are not numeric", {
  expect_error(scatter_plot(data = iris, x = Species, y = Sepal.Length),
               "x and y variables must both be numeric")
})

test_that("stops when size_by is not numeric", {
  expect_error(scatter_plot(data = iris, x = Sepal.Length, y = Sepal.Width,
                            size_by = Species),
               "size_by variable must be numeric")
})

test_that("returns a ggplot object", {
  p <- scatter_plot(data = mtcars, x = hp, y = mpg)
  expect_true(ggplot2::is.ggplot(p))
})

test_that("returns a ggplot object with color_by", {
  p <- scatter_plot(data = mtcars, x = hp, y = mpg, color_by = cyl)
  expect_true(ggplot2::is.ggplot(p))
})

test_that("works without optional arguments", {
  expect_no_error(scatter_plot(data = mtcars, x = hp, y = mpg))
})

test_that("works with line = FALSE", {
  expect_no_error(scatter_plot(data = mtcars, x = hp, y = mpg, line = FALSE))
})

test_that("works with all optional arguments provided", {
  expect_no_error(scatter_plot(data = mtcars, x = hp, y = mpg,
                               color_by = cyl,
                               size_by = cyl,
                               title = "Test",
                               subtitle = "Subtitle",
                               line = FALSE))
})

test_that("auto labels match column names", {
  p <- scatter_plot(data = mtcars, x = hp, y = mpg)
  expect_equal(p$labels$x, "hp")
  expect_equal(p$labels$y, "mpg")
})

test_that("custom labels override defaults", {
  p <- scatter_plot(data = mtcars, x = hp, y = mpg,
                    x_name = "Horsepower", y_name = "Mileage")
  expect_equal(p$labels$x, "Horsepower")
  expect_equal(p$labels$y, "Mileage")
})

test_that("custom title and subtitle are applied", {
  p <- scatter_plot(data = mtcars, x = hp, y = mpg,
                    title = "My Title", subtitle = "My Subtitle")
  expect_equal(p$labels$title, "My Title")
  expect_equal(p$labels$subtitle, "My Subtitle")
})

test_that("handles data frame with only two rows", {
  small_df <- data.frame(x = c(1, 2), y = c(3, 4))
  expect_no_error(scatter_plot(data = small_df, x = x, y = y))
})

test_that("handles NAs in x and y without crashing", {
  na_df <- data.frame(x = c(1, NA, 3), y = c(4, 5, NA))
  expect_no_error(scatter_plot(data = na_df, x = x, y = y))
})

test_that("handles single column data frame gracefully", {
  expect_error(scatter_plot(data = data.frame(x = 1:5), x = x, y = y))
})


########################BOXPLOT#######################

data(mtcars)
data(iris)

# convert to character for y variable testing
mtcars$cyl_char <- as.character(mtcars$cyl)
iris$Species_char <- as.character(iris$Species)

# --- input validation ---

test_that("stops when data is not a data frame", {
  expect_error(box_plot(data = list(x = 1:5), x = x),
               "data must be a data frame")
})

test_that("stops when x is not numeric", {
  expect_error(box_plot(data = mtcars, x = cyl_char),
               "x variable must be numeric")
})

test_that("stops when y is not categorical", {
  expect_error(box_plot(data = mtcars, x = mpg, y = wt),
               "y variable must be categorical")
})

# --- output type ---

test_that("returns a ggplot object without y", {
  p <- box_plot(data = mtcars, x = mpg)
  expect_true(ggplot2::is.ggplot(p))
})

test_that("returns a ggplot object with y", {
  p <- box_plot(data = mtcars, x = mpg, y = cyl_char)
  expect_true(ggplot2::is.ggplot(p))
})

# --- optional arguments ---

test_that("works with only required arguments", {
  expect_no_error(box_plot(data = mtcars, x = mpg))
})

test_that("works with all arguments provided", {
  expect_no_error(box_plot(data = mtcars, x = mpg, y = cyl_char,
                           title = "Test", subtitle = "Subtitle",
                           x_name = "Mileage", y_name = "Cylinders"))
})

# --- labels ---

test_that("auto labels match column names", {
  p <- box_plot(data = mtcars, x = mpg, y = cyl_char)
  expect_equal(p$labels$y, "mpg")
  expect_equal(p$labels$x, "cyl_char")
})

test_that("custom labels override defaults", {
  p <- box_plot(data = mtcars, x = mpg, y = cyl_char,
                x_name = "Mileage", y_name = "Cylinders")
  expect_equal(p$labels$y, "Mileage")
  expect_equal(p$labels$x, "Cylinders")
})

test_that("x axis label is empty string when y is not provided", {
  p <- box_plot(data = mtcars, x = mpg)
  expect_equal(p$labels$x, "")
})

test_that("custom title and subtitle are applied", {
  p <- box_plot(data = mtcars, x = mpg, title = "My Title", subtitle = "My Subtitle")
  expect_equal(p$labels$title, "My Title")
  expect_equal(p$labels$subtitle, "My Subtitle")
})

# --- edge cases ---

test_that("handles NAs in x without crashing", {
  na_df <- data.frame(x = c(1, NA, 3, 4, 5))
  expect_no_error(box_plot(data = na_df, x = x))
})

test_that("handles single unique value in x", {
  flat_df <- data.frame(x = rep(5, 10))
  expect_no_error(box_plot(data = flat_df, x = x))
})

test_that("handles single unique category in y", {
  one_group <- data.frame(x = c(1,2,3,4,5), y = rep("a", 5))
  expect_no_error(box_plot(data = one_group, x = x, y = y))
})

######################DENSITY#######################

data(mtcars)
data(iris)

mtcars$cyl_char <- as.character(mtcars$cyl)
mtcars$gear_char <- as.character(mtcars$gear)
iris$Species_char <- as.character(iris$Species)

# --- input validation ---

test_that("stops when data is not a data frame", {
  expect_error(density_plot(data = list(x = 1:5), x = x),
               "data must be a data frame")
})

test_that("stops when x is not numeric", {
  expect_error(density_plot(data = mtcars, x = cyl_char),
               "x variable must be numeric")
})

test_that("stops when color_by is not categorical", {
  expect_error(density_plot(data = mtcars, x = mpg, color_by = wt),
               "color_by variable must be categorical")
})

test_that("stops when facet_by is not categorical", {
  expect_error(density_plot(data = mtcars, x = mpg, facet_by = wt),
               "facet_by variable must be categorical")
})

# --- output type ---

test_that("returns a ggplot object with only x", {
  p <- density_plot(data = mtcars, x = mpg)
  expect_true(ggplot2::is.ggplot(p))
})

test_that("returns a ggplot object with color_by", {
  p <- density_plot(data = mtcars, x = mpg, color_by = cyl_char)
  expect_true(ggplot2::is.ggplot(p))
})

test_that("returns a ggplot object with facet_by", {
  p <- density_plot(data = mtcars, x = mpg, facet_by = cyl_char)
  expect_true(ggplot2::is.ggplot(p))
})

test_that("returns a ggplot object with both color_by and facet_by", {
  p <- density_plot(data = mtcars, x = mpg, color_by = cyl_char, facet_by = gear_char)
  expect_true(ggplot2::is.ggplot(p))
})

# --- optional arguments ---

test_that("works with only required arguments", {
  expect_no_error(density_plot(data = mtcars, x = mpg))
})

test_that("works with all arguments provided", {
  expect_no_error(density_plot(data = mtcars, x = mpg,
                               color_by = cyl_char,
                               facet_by = gear_char,
                               title = "Test", subtitle = "Subtitle",
                               x_name = "Mileage", color_name = "Cylinders"))
})

# --- labels ---

test_that("auto x label matches column name", {
  p <- density_plot(data = mtcars, x = mpg)
  expect_equal(p$labels$x, "mpg")
})

test_that("y label is always Density", {
  p <- density_plot(data = mtcars, x = mpg)
  expect_equal(p$labels$y, "Density")
})

test_that("custom x label overrides default", {
  p <- density_plot(data = mtcars, x = mpg, x_name = "Mileage")
  expect_equal(p$labels$x, "Mileage")
})

test_that("custom title and subtitle are applied", {
  p <- density_plot(data = mtcars, x = mpg,
                    title = "My Title", subtitle = "My Subtitle")
  expect_equal(p$labels$title, "My Title")
  expect_equal(p$labels$subtitle, "My Subtitle")
})

# --- edge cases ---

test_that("handles NAs in x without crashing", {
  na_df <- data.frame(x = c(1, NA, 3, 4, 5))
  expect_no_error(density_plot(data = na_df, x = x))
})

test_that("handles factor color_by without crashing", {
  expect_no_error(density_plot(data = iris, x = Sepal.Length, color_by = Species))
})

test_that("handles factor facet_by without crashing", {
  expect_no_error(density_plot(data = iris, x = Sepal.Length, facet_by = Species))
})

test_that("handles single unique value in x", {
  flat_df <- data.frame(x = rep(5, 10))
  expect_no_error(density_plot(data = flat_df, x = x))
})

####################BARPLOT###########################

data(mtcars)
data(iris)

mtcars$cyl_char  <- as.character(mtcars$cyl)
mtcars$gear_char <- as.character(mtcars$gear)
iris$Species_char <- as.character(iris$Species)

# --- input validation ---

test_that("stops when data is not a data frame", {
  expect_error(bar_plot(data = list(x = letters[1:5]), x = x),
               "data must be a data frame")
})

test_that("stops when x is not categorical", {
  expect_error(bar_plot(data = mtcars, x = mpg),
               "x variable must be categorical")
})

test_that("stops when y is not categorical", {
  expect_error(bar_plot(data = mtcars, x = cyl_char, y = mpg),
               "y variable must be categorical")
})

# --- output type ---

test_that("returns a ggplot object with only x", {
  p <- bar_plot(data = mtcars, x = cyl_char)
  expect_true(ggplot2::is.ggplot(p))
})

test_that("returns a ggplot object with x and y", {
  p <- bar_plot(data = mtcars, x = cyl_char, y = gear_char)
  expect_true(ggplot2::is.ggplot(p))
})

# --- optional arguments ---

test_that("works with only required arguments", {
  expect_no_error(bar_plot(data = mtcars, x = cyl_char))
})

test_that("works with all arguments provided", {
  expect_no_error(bar_plot(data = mtcars, x = cyl_char, y = gear_char,
                           title = "Test", subtitle = "Subtitle",
                           x_name = "Cylinders", y_name = "Gears",
                           flip = TRUE))
})

test_that("works with flip = TRUE", {
  expect_no_error(bar_plot(data = mtcars, x = cyl_char, flip = TRUE))
})

test_that("works with flip = FALSE", {
  expect_no_error(bar_plot(data = mtcars, x = cyl_char, flip = FALSE))
})

test_that("works with factor x variable", {
  expect_no_error(bar_plot(data = iris, x = Species))
})

test_that("works with factor y variable", {
  expect_no_error(bar_plot(data = iris, x = Species_char, y = Species))
})

# --- labels ---

test_that("auto x label matches column name", {
  p <- bar_plot(data = mtcars, x = cyl_char)
  expect_equal(p$labels$x, "cyl_char")
})

test_that("fill label is empty string when y is not provided", {
  p <- bar_plot(data = mtcars, x = cyl_char)
  expect_equal(p$labels$fill, "")
})

test_that("fill label matches y column name when y is provided", {
  p <- bar_plot(data = mtcars, x = cyl_char, y = gear_char)
  expect_equal(p$labels$fill, "gear_char")
})

test_that("custom x label overrides default", {
  p <- bar_plot(data = mtcars, x = cyl_char, x_name = "Cylinders")
  expect_equal(p$labels$x, "Cylinders")
})

test_that("custom y label overrides default", {
  p <- bar_plot(data = mtcars, x = cyl_char, y = gear_char, y_name = "Gears")
  expect_equal(p$labels$fill, "Gears")
})

test_that("custom title and subtitle are applied", {
  p <- bar_plot(data = mtcars, x = cyl_char,
                title = "My Title", subtitle = "My Subtitle")
  expect_equal(p$labels$title, "My Title")
  expect_equal(p$labels$subtitle, "My Subtitle")
})

# --- edge cases ---

test_that("handles single level in x", {
  one_level <- data.frame(x = rep("a", 10))
  expect_no_error(bar_plot(data = one_level, x = x))
})

test_that("handles many levels in x", {
  many_levels <- data.frame(x = letters)
  expect_no_error(bar_plot(data = many_levels, x = x))
})

test_that("handles single row data frame", {
  single_row <- data.frame(x = "a")
  expect_no_error(bar_plot(data = single_row, x = x))
})

# ==============================================================================
# COLUMN PLOT TESTS
# ==============================================================================

# --- input validation ---

test_that("column_plot stops when data is not a data frame", {
  expect_error(column_plot(data = list(x = letters[1:5], y = 1:5), x = x, y = y),
               "data must be a data frame")
})

test_that("column_plot stops when x is not categorical", {
  expect_error(column_plot(data = mtcars, x = mpg, y = hp),
               "x variable must be categorical")
})

test_that("column_plot stops when y is not numeric", {
  expect_error(column_plot(data = mtcars, x = cyl_char, y = gear_char),
               "y variable must be numeric")
})

test_that("column_plot stops when color_by is not categorical", {
  expect_error(column_plot(data = mtcars, x = cyl_char, y = mpg, color_by = hp),
               "color_by variable must be categorical")
})

# --- output type ---

test_that("column_plot returns a ggplot object with required args only", {
  p <- column_plot(data = mtcars, x = cyl_char, y = mpg)
  expect_true(ggplot2::is.ggplot(p))
})

test_that("column_plot returns a ggplot object with color_by", {
  p <- column_plot(data = mtcars, x = cyl_char, y = mpg, color_by = gear_char)
  expect_true(ggplot2::is.ggplot(p))
})

# --- optional arguments ---

test_that("column_plot works with only required arguments", {
  expect_no_error(column_plot(data = mtcars, x = cyl_char, y = mpg))
})

test_that("column_plot works with all arguments provided", {
  expect_no_error(column_plot(data = mtcars, x = cyl_char, y = mpg,
                              color_by = gear_char,
                              title = "Test", subtitle = "Subtitle",
                              x_name = "Cylinders", y_name = "Mileage",
                              color_name = "Gears", flip = TRUE))
})

test_that("column_plot works with flip = TRUE", {
  expect_no_error(column_plot(data = mtcars, x = cyl_char, y = mpg, flip = TRUE))
})

test_that("column_plot works with factor x variable", {
  expect_no_error(column_plot(data = iris, x = Species, y = Sepal.Length))
})

# --- labels ---

test_that("column_plot auto labels match column names", {
  p <- column_plot(data = mtcars, x = cyl_char, y = mpg)
  expect_equal(p$labels$x, "cyl_char")
  expect_equal(p$labels$y, "mpg")
})

test_that("column_plot fill label is empty string when color_by is not provided", {
  p <- column_plot(data = mtcars, x = cyl_char, y = mpg)
  expect_equal(p$labels$fill, "")
})

test_that("column_plot fill label matches color_by column name", {
  p <- column_plot(data = mtcars, x = cyl_char, y = mpg, color_by = gear_char)
  expect_equal(p$labels$fill, "gear_char")
})

test_that("column_plot custom labels override defaults", {
  p <- column_plot(data = mtcars, x = cyl_char, y = mpg,
                   x_name = "Cylinders", y_name = "Mileage")
  expect_equal(p$labels$x, "Cylinders")
  expect_equal(p$labels$y, "Mileage")
})

test_that("column_plot custom title and subtitle are applied", {
  p <- column_plot(data = mtcars, x = cyl_char, y = mpg,
                   title = "My Title", subtitle = "My Subtitle")
  expect_equal(p$labels$title, "My Title")
  expect_equal(p$labels$subtitle, "My Subtitle")
})

# --- edge cases ---

test_that("column_plot handles single level in x", {
  one_level <- data.frame(x = rep("a", 5), y = 1:5)
  expect_no_error(column_plot(data = one_level, x = x, y = y))
})

test_that("column_plot handles NAs in y without crashing", {
  na_df <- data.frame(x = c("a","b","c"), y = c(1, NA, 3))
  expect_no_error(column_plot(data = na_df, x = x, y = y))
})

# ==============================================================================
# LINE PLOT TESTS
# ==============================================================================

# --- input validation ---

test_that("line_plot stops when data is not a data frame", {
  expect_error(line_plot(data = list(x = 1:5, y = 1:5), x = x, y = y),
               "data must be a data frame")
})

test_that("line_plot stops when x is not numeric", {
  expect_error(line_plot(data = test_df, x = grade, y = score),
               "x and y variables must both be numeric")
})

test_that("line_plot stops when y is not numeric", {
  expect_error(line_plot(data = test_df, x = year, y = grade),
               "x and y variables must both be numeric")
})

test_that("line_plot stops when color_by is not categorical", {
  expect_error(line_plot(data = test_df, x = year, y = score, color_by = score),
               "must be numeric")
})

test_that("line_plot stops when line_type is not categorical", {
  expect_error(line_plot(data = test_df, x = year, y = score, line_type = score),
               "must be numeric")
})

# --- output type ---

test_that("line_plot returns a ggplot object with required args only", {
  p <- line_plot(data = test_df, x = year, y = score)
  expect_true(ggplot2::is.ggplot(p))
})

test_that("line_plot returns a ggplot object with color_by", {
  p <- line_plot(data = test_df, x = year, y = score, color_by = grade)
  expect_true(ggplot2::is.ggplot(p))
})

test_that("line_plot returns a ggplot object with line_type", {
  p <- line_plot(data = test_df, x = year, y = score, line_type = grade)
  expect_true(ggplot2::is.ggplot(p))
})

test_that("line_plot returns a ggplot object with both color_by and line_type", {
  p <- line_plot(data = test_df, x = year, y = score,
                 color_by = grade, line_type = school)
  expect_true(ggplot2::is.ggplot(p))
})

# --- optional arguments ---

test_that("line_plot works with only required arguments", {
  expect_no_error(line_plot(data = test_df, x = year, y = score))
})

test_that("line_plot works with all arguments provided", {
  expect_no_error(line_plot(data = test_df, x = year, y = score,
                            color_by = grade, line_type = school,
                            title = "Test", subtitle = "Subtitle",
                            x_name = "Year", y_name = "Score",
                            color_name = "Grade", line_name = "School"))
})

test_that("line_plot works with factor color_by", {
  expect_no_error(line_plot(data = iris, x = Sepal.Length, y = Sepal.Width,
                            color_by = Species))
})

# --- labels ---

test_that("line_plot auto labels match column names", {
  p <- line_plot(data = test_df, x = year, y = score)
  expect_equal(p$labels$x, "year")
  expect_equal(p$labels$y, "score")
})

test_that("line_plot custom labels override defaults", {
  p <- line_plot(data = test_df, x = year, y = score,
                 x_name = "Year", y_name = "Score")
  expect_equal(p$labels$x, "Year")
  expect_equal(p$labels$y, "Score")
})

test_that("line_plot color label matches color_by column name", {
  p <- line_plot(data = test_df, x = year, y = score, color_by = grade)
  expect_equal(p$labels$colour, "grade")
})

test_that("line_plot linetype label matches line_type column name", {
  p <- line_plot(data = test_df, x = year, y = score, line_type = grade)
  expect_equal(p$labels$linetype, "grade")
})

test_that("line_plot custom title and subtitle are applied", {
  p <- line_plot(data = test_df, x = year, y = score,
                 title = "My Title", subtitle = "My Subtitle")
  expect_equal(p$labels$title, "My Title")
  expect_equal(p$labels$subtitle, "My Subtitle")
})

# --- edge cases ---

test_that("line_plot handles NAs in y without crashing", {
  na_df <- data.frame(x = 1:5, y = c(1, NA, 3, 4, 5))
  expect_no_error(line_plot(data = na_df, x = x, y = y))
})

test_that("line_plot handles two row data frame", {
  small_df <- data.frame(x = c(1, 2), y = c(3, 4))
  expect_no_error(line_plot(data = small_df, x = x, y = y))
})
