.gen_grid <- function(dim = 16, fill_val = "#E5E5E5") {
  matrix(rep(fill_val, dim ^ 2), dim)
}

.convert_hex2pxltrx <- function(mat) {
  unique_colours <- unique(as.character(mat))
  colour_lookup <- setNames(seq(length(unique_colours)) - 1, unique_colours)
  new_mat <- matrix(colour_lookup[mat], ncol(mat))
  attr(new_mat, "colours") <- unique_colours
  class(new_mat) <- "pixeltrix"
  new_mat
}

.gen_image <- function(mat) {

  mat_pxltrx <- .convert_hex2pxltrx(mat)
  mat_colours <- attributes(mat_pxltrx)[["colours"]]
  n_colours <- length(mat_colours)
  mat_invert <- t(mat_pxltrx[seq(nrow(mat_pxltrx), 1), ])

  graphics::par(mar = rep(0, 4))

  graphics::image(
    mat_invert,
    zlim = c(0, n_colours - 1),
    col  = mat_colours,
    axes = FALSE,
    xlab = "",
    ylab = ""
  )

  graphics::abline(v = .gen_ablines(mat, "x"), col = "white")
  graphics::abline(h = .gen_ablines(mat, "y"), col = "white")

}

.gen_ablines <- function(mat, dim = c("x", "y")) {

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

.get_pixel_coords <- function(mat, point_coords) {

  # Pixel centres
  x_n    <- ncol(mat)  # number of pixels in the x dimension
  x_unit <- 1 / (x_n - 1)  # x width of pixels
  x_mids <- seq(0, 1, x_unit)  # full set of pixel centres on x axes
  y_n    <- nrow(mat)
  y_unit <- 1 / (y_n - 1)
  y_mids <- seq(0, 1, y_unit)

  # Calculate distances xy from clicked point to pixel centres
  x_diffs <- abs(point_coords[["x"]] - x_mids)
  y_diffs <- rev(abs(point_coords[["y"]] - y_mids))

  # Identify pixel closest to click
  pixel_coords <- list(x = which.min(x_diffs), y = which.min(y_diffs))

  pixel_coords  # list with values 'x' and 'y' giving pixel location on grid

}

.gen_updated_pixel_matrix <- function(mat, pixel_coords, selected_colour) {
  new_value <- selected_colour
  mat[pixel_coords[["y"]], pixel_coords[["x"]]] <- new_value
  mat
}

.gen_bot_pixel_matrix <- function() {

  rand_colour <- sample(c("#FF0000", "#00FF00", "#0000FF", "#000000"), 1)
  colour_lookup <- setNames(c("#E5E5E5", rand_colour), 0:1)

  vec <- sample(c(0, 1), 16 ^ 2, TRUE)

  if (sample(c(TRUE, FALSE), 1, prob = c(0.1, 0.9))) {
    vec <- c(
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0,
      0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0,
      0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0,
      0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0,
      0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0,
      0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0,
      0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0,
      0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0,
      0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0,
      0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0,
      0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0,
      0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0,
      0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    )
  }

  matrix(colour_lookup[as.character(vec)], 16)

}

ui <- shiny::fluidPage(
  htmltools::h1("little pixel fun zone"),
  htmltools::p(
    "by",
    htmltools::a("matt", href = "https://www.matt-dray.com", target = "_blank")
  ),
  shiny::plotOutput(
    "pixel_grid",
    380,
    380,
    shiny::clickOpts("clicked_point", TRUE)
  ),
  htmltools::br(),
  colourpicker::colourInput(
    "selected_colour",
    NULL,
    "black",
    palette = "limited",
    returnName = TRUE,
    width = 120
  ),
  shiny::actionButton("button_undo", shiny::icon("rotate-left")),
  shiny::actionButton("button_fill", shiny::icon("fill-drip")),
  shiny::actionButton("button_robot", shiny::icon("robot")),
  shiny::downloadButton(
    "button_download_matrix",
    NULL,
    icon = shiny::icon("file-code")
  ),
  shiny::downloadButton(
    "button_download_img",
    NULL,
    icon = shiny::icon("file-image")
  )
)

server <- function(input, output, session) {

  # Reactives

  pixel_matrices <- shiny::reactiveValues(slot1 = .gen_grid(16, "#E5E5E5"))

  point_coords <- shiny::reactive({
    list(x = input$clicked_point[["x"]], y = input$clicked_point[["y"]])
  })

  pixel_coords <- shiny::reactive({
    .get_pixel_coords(pixel_matrices[["slot1"]], point_coords())
  })

  undo_button_icon <- shiny::reactiveVal("rotate-left")

  # Observers

  shiny::observeEvent(input$clicked_point, {
    matrix_updated <- .gen_updated_pixel_matrix(
      shiny::isolate(pixel_matrices[["slot1"]]),
      pixel_coords(),
      input$selected_colour
    )
    pixel_matrices[["slot2"]] <- pixel_matrices[["slot1"]]
    pixel_matrices[["slot1"]] <- matrix_updated
  })

  shiny::observeEvent(
    input$button_undo, {

      # Switch 'memory' slots
      slot1 <- pixel_matrices[["slot1"]]
      slot2 <- pixel_matrices[["slot2"]]
      pixel_matrices[["slot2"]] <- slot1
      pixel_matrices[["slot1"]] <- slot2

      # Invert undo/redo icon

      current_icon <- undo_button_icon()
      if (current_icon == "rotate-left") undo_button_icon("rotate-right")
      if (current_icon == "rotate-right") undo_button_icon("rotate-left")

      shiny::updateActionButton(
        inputId = "button_undo",
        icon = shiny::icon(undo_button_icon())
      )

    })

  shiny::observeEvent(input$button_fill, {
    matrix_filled <- .gen_grid(16, input$selected_colour)
    pixel_matrices[["slot2"]] <- pixel_matrices[["slot1"]]
    pixel_matrices[["slot1"]] <- matrix_filled
  })

  shiny::observeEvent(input$button_robot, {
    matrix_by_bot <- .gen_bot_pixel_matrix()
    pixel_matrices[["slot2"]] <- pixel_matrices[["slot1"]]
    pixel_matrices[["slot1"]] <- matrix_by_bot
  })

  # Outputs

  output$pixel_grid <- shiny::renderPlot({
    .gen_image(pixel_matrices[["slot1"]])
  })

  output$button_download_matrix <- downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d-%H%M%S"), "_treasured-art_matrix.rds")
    },
    content = function(file) {
      readr::write_rds(.convert_hex2pxltrx(pixel_matrices[["slot1"]]), file)
    }
  )

  output$button_download_img <- downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y-%m-%d-%H%M%S"), "_treasured-art.png")
    },
    content = function(file) {
      ppi <- 300
      png(file, width = 4 * ppi, height = 4 * ppi, res = ppi)
      .gen_image(pixel_matrices[["slot1"]])
      dev.off()
    }
  )

}

shiny::shinyApp(ui, server)
