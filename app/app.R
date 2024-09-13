.gen_grid <- function(dim = 16, fill_val = 0) {
  matrix(rep(fill_val, dim ^ 2), dim)
}

.gen_image <- function(
    mat = pixel_matrix(),
    col_0 = "grey95",
    col_1 = "grey10"
) {

  graphics::par(mar = rep(0, 4))

  graphics::image(
    t(mat[seq(nrow(mat), 1), ]),  # invert to line up click with matrix
    zlim = c(0, 1),
    col  = c(col_0, col_1),
    axes = FALSE,
    xlab = "",
    ylab = ""
  )

  graphics::abline(v = .gen_ablines(mat, "x"), col = "white")
  graphics::abline(h = .gen_ablines(mat, "y"), col = "white")

}

.gen_ablines <- function(mat = pixel_matrix(), dim = c("x", "y")) {

  dim_n <- ncol(mat)
  if (dim == "y") dim_n <- nrow(mat)

  if (dim_n > 1) {
    dim_unit  <- 1 / (dim_n - 1)  # width of one pixel
    dim_lines <- seq(  # min/max pixel centres at -1,1, but there's 'overhang'
      0 - dim_unit - (dim_unit / 2),  # add half a pixel overhang at min
      1 + dim_unit + (dim_unit / 2),  # add half a pixel overhang at max
      dim_unit
    )
  }

  dim_lines

}

.get_pixel_coords <- function(mat = pixel_matrix(), coords = point_coords()) {

  # Pixel centres
  x_n    <- ncol(mat)  # number of pixels in the x dimension
  x_unit <- 1 / (x_n - 1)  # x width of pixels
  x_mids <- seq(0, 1, x_unit)  # full set of pixel centres on x axes
  y_n    <- nrow(mat)
  y_unit <- 1 / (y_n - 1)
  y_mids <- seq(0, 1, y_unit)

  # Calculate distances xy from clicked point to pixel centres
  x_diffs <- abs(coords[["x"]] - x_mids)
  y_diffs <- rev(abs(coords[["y"]] - y_mids))

  # Identify pixel closest to click
  pixel_coords <- list(x = which.min(x_diffs), y = which.min(y_diffs))

  pixel_coords  # list with values 'x' and 'y' giving pixel location on grid

}

.gen_updated_pixel_matrix <- function(
    mat = pixel_matrix(),
    coords = pixel_coords()
) {
  matrix_updated <- mat
  new_value <- matrix_updated[coords[["y"]], coords[["x"]]] + 1
  if (new_value > 1) new_value <- 0
  matrix_updated[coords[["y"]], coords[["x"]]] <- new_value
  matrix_updated
}

ui <- shiny::fluidPage(
  htmltools::h1("little pixel fun zone"),
  htmltools::p("by", htmltools::a("matt", href = "https://www.matt-dray.com", target = "_blank")),
  shiny::plotOutput("pixel_grid", 400, 400, shiny::clickOpts("clicked_point", TRUE)),
  htmltools::br(),
  shiny::actionButton("button_clear", shiny::icon("broom")),
  shiny::actionButton("button_fill", shiny::icon("fill-drip")),
  shiny::actionButton("button_robot", shiny::icon("robot")),
  shiny::downloadButton("button_download", NULL, icon = shiny::icon("floppy-disk"))
)

server <- function(input, output, session) {

  # Reactives

  pixel_matrix <- shiny::reactiveVal(.gen_grid(16, 0))

  point_coords <- shiny::reactive({
    list(x = input$clicked_point[["x"]], y = input$clicked_point[["y"]])
  })

  pixel_coords <- shiny::reactive({
    .get_pixel_coords(pixel_matrix(), point_coords())
  })

  # Observers

  shiny::observeEvent(input$clicked_point, {
    matrix_updated <- .gen_updated_pixel_matrix(pixel_matrix(), pixel_coords())
    pixel_matrix(matrix_updated)
  })

  shiny::observeEvent(input$button_clear, pixel_matrix(.gen_grid(16, 0)))

  shiny::observeEvent(input$button_fill, pixel_matrix(.gen_grid(16, 1)))

  shiny::observeEvent(input$button_robot, {
    img <- matrix(sample(0:1, 16 ^ 2, TRUE), 16)
    if (sample(c(TRUE, FALSE), 1, prob = c(0.1, 0.9))) {
      img <- matrix(
        c(
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
          1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
          0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1,
          0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0,
          1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1,
          1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
          0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0,
          0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0,
          1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
          0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1,
          1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0
        ),
        16
      )
    }
    pixel_matrix(img)
  })

  # Outputs

  output$pixel_grid <- shiny::renderPlot({
    .gen_image(pixel_matrix(), "grey95", "grey10")
  })

  output$button_download <- downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d-%H%M%S"), "_treasured-art.png")
    },
    content = function(file) {
      ppi <- 300
      png(file, width = 4 * ppi, height = 4 * ppi, res = ppi)
      .gen_image(pixel_matrix(), "grey95", "grey10")
      dev.off()
    }
  )

}

shiny::shinyApp(ui, server)
