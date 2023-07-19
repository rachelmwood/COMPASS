source("distances.R")
require(ggplot2)
require(ROCit)

roc_data <- function(score, classes, groups = NULL) {
    if (is.null(groups)) {
        roc_fit <- rocit(score, classes)
    return(data.frame(
        FPR = roc_fit$FPR,
        TPR = roc_fit$TPR))
    }
    groups <- as.factor(groups)
    levels <- levels(groups)
    fits <- vector("list", length(levels))
    names(fits) <- levels
    for (i in 1:length(levels)) {
        current_scores <- score[groups == levels[i]]
        current_classes <- classes[groups == levels[i]]
        current_roc <- rocit(current_scores, current_classes)
        fits[[i]] <- current_roc
    }
    return(fits)
}

plot_rocs <- function(
    roc_list, together = TRUE, layout = NULL, pallete = NULL) {
    if (together) {
        if (is.null(pallete)) {
            pallete <- rainbow(length(roc_list))
        }
        plot(roc_list[[1]], col = c(pallete[1], "#BEBEBE"), main = "ROC Curves",
         legend = FALSE, YIndex = FALSE)
        for (i in 2:length(roc_list)) {
            lines(roc_list[[i]]$FPR, roc_list[[i]]$TPR, col = pallete[i])
        }
        legend("bottomright", col = pallete,
            names(roc_list), lwd = 2)
        return(NULL)
    } else {
        if (is.null(layout)) {
            layout <- matrix(1:length(roc_list), ncol = 2)
        }
        par(mfrow = c(nrow(layout), ncol(layout)))
        for (i in 1:length(roc_list)) {
            plot(roc_list[[i]], main = names(roc_list)[i])
        }
        return(NULL)
    }
}