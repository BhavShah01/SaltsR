graph_salt_balance <- function(ECOS_output) {
  ECOS_output |>
    ggplot(aes(X, Y, col = Salt, fill = Salt)) +
    geom_line(alpha = 0.7, size = 1.5) +
    geom_point(alpha = 0.7, size = 1) +
    ggrepel::geom_text_repel(aes(label = Crystallisation)) +
    labs(x = "Humidity (%RH)", y = "Amount of substance (mol)",
         title = NULL,
         subtitle = paste0("Temperature ", unique(ECOS_output$Temp), "C"),
         caption = "Price (2000) and Bionda (2005)") +
    theme_classic(base_size = 16)
}
