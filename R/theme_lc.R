#' Theme based off of Lauren Condie's work
#'
#'
#' @export
#'
#'
theme_lc <- function(){

  theme_minimal() %+replace%

  theme(
    plot.title = element_text(color = "#B07E63"),
    axis.title.y = element_text(color = "#D3B593"),
    axis.title.x = element_text(color = "#D3B593"),
    panel.grid.major = element_line(color = "#D3B593", linewidth = .06),
    axis.text.x = element_text(color = "#78776C"),
    axis.text.y = element_text(color = "#78776C"),
    panel.grid.minor = element_blank()
  )
}
