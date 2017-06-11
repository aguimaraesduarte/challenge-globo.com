# @author: Andre Duarte
# @date: June 1st, 2016
# @purpose: Globo.com presentation
# @confidential: True

# Evaluate all <algorithms> according to the <scheme>, for topN recommendations, with parameter <n>.
# Plot ROC and precision/recall curves to determine the best algorithms to use.
# <leg_pos> determines the position of the legend.
evaluate_and_plot <- function(scheme, algorithms, n = c(1, 3, 5, 10, 20, 50), leg_pos = "topleft"){
  # Run algorithms, predict next n movies
  results <- evaluate(scheme, algorithms, n = n)
  
  # Draw ROC curve
  plot(results, annotate = TRUE, legend = leg_pos)
  
  # See precision / recall
  plot(results, "prec/rec", annotate = TRUE, legend = leg_pos)
}

# Test four algorithms (Random, Popular, user-based, item-based) to predict the topN recommendations.
# <scheme> is a necessary parameter.
compare_algos <- function(scheme){
  # Algorithms to test
  algorithms <- list(
    "random items" = list(name = "RANDOM", param = NULL),
    "popular items" = list(name = "POPULAR", param = NULL),
    "user-based CF" = list(name = "UBCF", param = list(method = "jaccard",
                                                       nn = 50)),
    "item-based CF" = list(name = "IBCF", param = list(method = 'jaccard',
                                                       k = 50))
  )
  
  evaluate_and_plot(scheme, algorithms)
}

# Compare similarity methods for user-based and item-based CF.
# <scheme> is a necessary parameter.
compare_similarity <- function(scheme){
  # Algorithms to test
  algorithms <- list(
    "IBCF Jaccard" = list(name = "IBCF", param = list(method = "jaccard",
                                                      k = 50)),
    "IBCF Cosine" = list(name = "IBCF", param = list(method = "cosine",
                                                     k = 50)),
    "IBCF Pearson" = list(name = "IBCF", param = list(method = "pearson",
                                                      k = 50))
  )
  
  evaluate_and_plot(scheme, algorithms)
}

# Compare different values for k set through <seq_k> (item-based CF).
# <scheme> is a necessary parameter.
compare_k <- function(scheme, seq_k = seq(10, 100, 10)){
  vector_k <- seq_k
  
  algorithms <- lapply(vector_k, function(k){
    list(name = "IBCF", param = list(method = "jaccard",
                                     k = k))
  })
  names(algorithms) <- paste0("IBCF_k_", vector_k)
  
  evaluate_and_plot(scheme, algorithms)
}