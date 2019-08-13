plot_bar <- function(df, col){
  p <- df %>% 
    count_(col) %>% 
    ggplot(aes(x = reorder(!! sym(col), -n), y = n / sum(n))) +
    geom_bar(stat = "identity")
  return(p)
}

grouped_tfidf <- function(df, tfidf_var, group, ngrams,
                          plot_len = 20, color = "grey40"){
  columns <- paste0("w", seq(ngrams))
  
  tokens <- df %>%
    unnest_tokens(bigrams, !! sym(tfidf_var), token = "ngrams", n = ngrams) %>% 
    separate(bigrams, columns, sep = " ")
  
  for (col in columns){
    tokens <- tokens %>% 
      filter(!(!! sym(col) %in% stop_words$word))
  }
  
  plots <- function(categorie, color){
    tokens %>% 
      unite_("bigram", columns, sep = " ") %>% 
      filter(!str_detect(bigram, "[:digit:]")) %>% 
      count(!! sym(group), bigram) %>% 
      bind_tf_idf(bigram, !! sym(group), n) %>% 
      filter(!! sym(group) == categorie) %>%
      arrange(desc(tf_idf)) %>%
      slice(1:plot_len) %>% 
      ggplot(aes(x = reorder(bigram, tf_idf), y = tf_idf)) +
      geom_bar(stat = "identity", fill = color, width = .8) +
      labs(title = categorie, x = NULL) +
      theme_bw() +
      coord_flip()
  }
  
  categories <- unique(df[[group]])
  
  if (length(categories) > 8 | length(categories) < 3){
    p <- purrr::map2(categories, color, ~plots(.x, .y))
  } else{
    colors <- RColorBrewer::brewer.pal(n = length(categories), name = "Set2")
    p <- purrr::map2(categories, colors, ~plots(.x, .y))
  }
  
  return(p)
}


plot_cor <- function(df, group_var, group, cor_limit = 0.5, n_counts = 10){
  jobs_asia_words <- df %>% 
    filter(!! enquo(group_var) == group) %>%
    mutate(vaga = row_number() %/% 10) %>%
    filter(vaga > 0) %>%
    unnest_tokens(word, Minimum_Qual) %>%
    anti_join(stop_words) %>% 
    filter(!str_detect(word, "[:digit:]")) %>% 
    select(!! enquo(group_var), vaga, word)
  
  word_cors <- jobs_asia_words %>%
    group_by(word) %>%
    filter(n() >= n_counts) %>%
    pairwise_cor(word, vaga, sort = TRUE)
  
  p <- word_cors %>%
    filter(correlation > cor_limit) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = correlation), color = "grey",
                   show.legend = FALSE) +
    geom_node_point(size = 4) + 
    geom_node_text(aes(label = name), repel = TRUE, 
                   point.padding = unit(0.2, "lines")) +
    labs(title = paste("Rede de correlações mais fortes:", group)) +
    theme_void()
  
  return(p)
}