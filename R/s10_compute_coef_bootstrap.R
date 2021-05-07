# s10_compute_coef_bootstrap.R


# Step 10 : Compute standard effort of regression coefficient with bootstrap.


# Project : habitat_suitability_curves_fda
# Author  : Jeremie Boudreault
# Email   : Jeremie.Boudreault [at] ete.inrs [dot] ca
# Depends : R (v3.6.3)
# License : CC BY-NC-ND 4.0



# Library ----------------------------------------------------------------------


library(data.table)
library(ggplot2)
library(FDboost)


# Functions --------------------------------------------------------------------


source(file.path("R", "functions", "globals.R"))
source(file.path("R", "functions", "internals.R"))
source(file.path("R", "functions", "plot_helpers.R"))
source(file.path("R", "functions", "fdboost_helpers.R"))


# Parameters -------------------------------------------------------------------


# Number of bootstrap.
n_bs <- 1000L


# Imports ----------------------------------------------------------------------


# Fitted functional models.
frm_results <- qs::qread(
    file = file.path("out", "tmp", "s08_frm_results_full.qs")
)

# Functional observations.
fd_curves_list <- qs::qread(
    file = file.path("out", "tmp", "s06_fd_curves_list.qs")
)

# Plot of regression coefficient.
plot_frm_coef <- qs::qread(
    file = file.path("out", "tmp", "s09_frm_coef_plot.qs")
)


# Generate bootstrap sample ----------------------------------------------------


generate_bs_sample <- function(l) {
    n_obs <- nrow(l$Y)
    boot_i <- sample(seq_len(n_obs), size = n_obs, replace = TRUE)
    l$Y <- l$Y[boot_i, ]
    l$X <- l$X[boot_i, ]
    return(l)
}


# Compute bootstrap of coefficients --------------------------------------------


# Apply over all variables.
bs_res <- lapply(
    X = names(var_names),
    FUN = function(var) {

        # Apply over all bootstrap (n_bs).
        do.call(what = abind, args = lapply(
            X   = seq_len(n_bs),
            FUN = function(i) {

                # Fit functional model.
                obj <- .FDboost(
                    data         = generate_bs_sample(fd_curves_list[[var]]),
                    fdboost_opts = frm_results[[var]]$fdboost_opts,
                    mstop_best   = frm_results[[var]]$mstop_best
                )

                # Fitted coefficients.
                coef_fit <- coef(obj$fit)

                # Extract matrix.
                coef_mat <- coef_fit$smterms$`bsignal(X) %O% bbs(s)`$value

                # Add row and column names.
                rownames(coef_mat) <- coef_fit$smterms$`bsignal(X) %O% bbs(s)`$y
                colnames(coef_mat) <- coef_fit$smterms$`bsignal(X) %O% bbs(s)`$x

                # Return coef_mat.
                return(coef_mat)

            }
        ))
    }
)

# Add names to the results.
names(bs_res) <- names(var_names)


# Standard error of the coefficients -------------------------------------------


# Calculate standard error.
std <- lapply(bs_res, function(w) apply(w, FUN = sd, MARGIN = c(1L, 2L)))

# Melt into a data.table.
std_dt <- data.table::setDT(reshape2::melt(std))

# Update names.
names(std_dt) <- c("Y", "X", "Z", "MODEL")

# Replace values of <MODEL> with variables names and units.
std_dt[, MODEL := factor(
    x      = unlist(var_names_u[std_dt$MODEL], use.names = FALSE),
    levels = unlist(var_names_u)
)]


# Plot standard error of each regression coefficient ---------------------------


.plot_std <- function(data) {

    ggplot(
        data = data,
        mapping = aes(
            x = X,
            y = Y,
            z = Z,
            fill = Z
        )
    ) +
    geom_raster(
        alpha = 1L
    ) +
    geom_abline(
        slope     = 1L,
        intercept = 0L,
        lty       = 2L,
        lwd       = 0.1
    ) +
    scale_fill_gradient(
        low     = "#FFFFFF00",
        high    = "darkgrey",
        guide   = guide_colourbar(barwidth = 12L)
    ) +
    scale_x_continuous(
        expand = c(0.001, 0.001)
    ) +
    scale_y_continuous(
        expand = c(0.001, 0.001)
    ) +
    labs(
        y    = "",
        x    = "",
        fill = ""
    ) +
    facet_wrap(
        facets = ~ MODEL,
        nrow   = 1L,
        ncol   = 1L,
        scales = "free",
    ) +
    theme(
        panel.border     = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line        = element_line(colour = "white")
    ) +
    custom_theme()

}

# Generate all plots.
ps <- lapply(var_names_u, function(var) .plot_std(std_dt[MODEL == var, ]))

# Add x and y labs.
ps[[1L]] <- ps[[1L]] + ylab(paste0(hab_names[["SELECTED"]], " (s)"))
ps[[2L]] <- ps[[2L]] + xlab(paste0(hab_names[["AVAILABLE"]], " (r)"))

# Plot.
plot_bs <- ggpubr::ggarrange(
    plotlist = ps,
    ncol     = 3L
)


# Combine both plots -----------------------------------------------------------


# Add some spacing on top of the graphs.
for (i in 1:2) {

    plot_frm_coef <- ggpubr::annotate_figure(
        p  =   plot_frm_coef,
        top = "",
    )

    plot_bs <- ggpubr::annotate_figure(
        p  =   plot_bs,
        top = "",
    )

}

# Plot.
ggpubr::ggarrange(
    plotlist = list(plot_frm_coef, plot_bs),
    nrow     = 2L,
    labels   = c("a) B(s, r) coefficients", "b) Bootstrap standard error"),
    legend = "bottom",
    hjust = -0.1,
    heights = c(1.2, 1.2)
)

# Save plot.
ggsave(
    filename = file.path("out", "plots", "fig_8_frm_coef_bs.pdf"),
    width    = 10L,
    height   = 9L
)


# Exports ----------------------------------------------------------------------


qs::qsave(
    x    = bs_res,
    file = file.path("out", "tmp", "s10_frm_coef_bootstrap.qs")
)

