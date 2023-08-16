library(graphstats)

omni <- svd(gs.omni(original$Y, mixed$Y))

pcs <- 2
omni_plot <- omni$u[,1:pcs] %>% as_tibble()
colnames(omni_plot) <- as.character(1:pcs)
omni_plot <- cbind( omni_plot,groups = as.factor(groups), discipline = rep(c(1,2), each = nrow(omni_plot)/2))

distances_omni <- apply(as.matrix(1:12, nrow = 1), MARGIN = 1, FUN = function(ii){
  distance_moved(omni$u, d = ii, scale = FALSE, eigenvals = omni$d)
})