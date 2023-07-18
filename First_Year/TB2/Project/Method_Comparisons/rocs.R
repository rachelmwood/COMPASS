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
    
    levels <- levels(groups)
    return_data <- data.frame(
        FPR = numeric(),
        TPR = numeric(),
        grouping = factor()
    )
    for (i in levels) {
        current_scores <- score[groups == i]
        current_classes <- classes[groups == i]
        current_roc <- rocit(current_scores, current_classes)
        return_data <- rbind(return_data, data.frame(
            FPR = current_roc$FPR,
            TPR = current_roc$TPR,
            grouping = factor(i)
        ))
    }
    return(return_data)
}

plot_rocs <- function(data, grouping) {
    plot <- ggplot(data, aes(x = FPR, y = TPR)) + # nolint
        geom_line(aes(color = grouping)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        scale_color_brewer(palette = "Set1") +
        theme_bw()
    return(plot)
}