#' manhattan_plot
#'
#' Produce a manhattan plot of GWAS results.
#'
#' @param df A data frame returned from 'run_gwas'
#''
#' @return ggplot2 plot.
#' @export
#'
#' @examples
#' manhattan_plot(df)
manhattan_plot <- function(df, genome_size=2814816, alpha=0.05,
                           correction="bonferroni",
                           xtitle="Position along chromosome") {

    if (correction == "bonferroni") {
        alpha <- bonferroni_correction(alpha, nrow(df))
    }

    p <- ggplot2::ggplot(df, ggplot2::aes(x=position, y=logp)) +
        xlab(xtitle) +
        ylab(expression(-log[10](P))) +
        geom_point() +
        scale_x_continuous(
            breaks = round(seq(0, genome_size, by = 250000), 1),
            labels = round(seq(0, genome_size, by = 250000)/1000000,1)) +
        geom_hline(yintercept=alpha, linetype="dashed") +
        theme_bw() +
        theme(text = element_text(size=20))

    return(p)
}
#' qq_plot
#'
#' Produce a QQ plot of GWAS results.
#'
#' @param df A data frame returned from 'run_gwas'
#''
#' @return ggplot2 plot.
#' @export
#'
#' @examples
#' qq_plot(df)
qq_plot <- function(df, alpha=0.05, correction="bonferroni") {
    if (correction == "bonferroni") {
        alpha <- bonferroni_correction(alpha, nrow(df))
    }

    df$x <- sort(-1*log10(1:nrow(df) / nrow(df)))

    p <- ggplot(df, aes(x=x, y=logp)) +
        xlab(expression(Expected~~-log[10](P))) +
        ylab(expression(Observed~~-log[10](P))) +
        geom_point() +
        geom_line() +
        geom_abline(intercept = 0, slope = 1) +
        geom_hline(yintercept=alpha, linetype="dashed") +
        theme_bw() +
        theme(text = element_text(size=16))

    return(p)
}

#' bonferroni_correction
#'
#' Returns a Bonferroni corrected alpha value..
#'
#' @param alpha A significant p-value threshold
#'
#' @param n Total number of independent tests.
#'
#' @return A -log10 float value.
#' @export
#'
#' @examples
#' bonferroni_correction(0.05, 100)
bonferroni_correction <- function(alpha, n) {
    return(-1 * log10(alpha / n))
}
