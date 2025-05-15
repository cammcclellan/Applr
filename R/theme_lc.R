#' Theme based off of Lauren Condie's work
#'
#'
#' @export
#'
#'
theme_lc <- function(){

  update_geom_defaults('point', list(color = "#78776C"))
  update_geom_defaults('smooth', list(color= '#B7B799', fill = '#D3B593', alpha = 0.3))
  update_geom_defaults('line', list(color='#B7B799'))
  update_geom_defaults('segment', list(color='#B7B799'))
  update_geom_defaults('hline', list(color='#B7B799'))

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

ggplot(KidsFeet, aes(x=length,
                     y=width))+
  geom_point()+
  geom_smooth(method='lm', formula = y~x, se=T)+
  theme_lc()
