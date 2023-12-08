def_pol <- function(x, y, pol){
  as.logical(sp::point.in.polygon(point.x = x,
                                  point.y = y,
                                  pol.x = pol[,1],
                                  pol.y = pol[,2]))
}

my_df_generator <- function(dfm, year, col_name){
  col_name <- rlang::sym(col_name)
  df <- dfm %>%
    filter(ano == year) %>%
    group_by(x,y) %>%
    summarise(z = mean({{col_name}},na.rm = TRUE)) %>%
    drop_na()
  return(df)
}

map_theme <- function(){
  list(
    ggplot2::theme(
      panel.background = ggplot2::element_rect(color="black",fill = "white"),
      panel.grid.major = ggplot2::element_line(color="gray",linetype = 3)),
    ggspatial::annotation_scale(
      location="bl",
      height = ggplot2::unit(0.2,"cm")),
    ggspatial::annotation_north_arrow(
      location="top",
      style = ggspatial::north_arrow_nautical,
      height = ggplot2::unit(1.5,"cm"),
      width =  ggplot2::unit(1.5,"cm"))
  )
}

my_xlsx_reader <- function(my_way){
  df <- readxl::read_xlsx(my_way)
  my_string <- str_split(my_way,pattern = "[/.-]",simplify = TRUE)

  df <- df %>%
    mutate(variable = my_string[1,2],
           year = my_string[1,3])
  names(df) <- c("x","y","z","variable","year")
  df
}
