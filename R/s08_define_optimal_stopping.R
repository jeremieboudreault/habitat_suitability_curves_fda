# s08_define_optimal_stopping.R


# Step 08 : Define optimal stopping from the fitted FRMs.


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


source(file.path("R", "functions", "internals.R"))
source(file.path("R", "functions", "fdboost_helpers.R"))


# Imports ----------------------------------------------------------------------


# Fitted functional models.
frm_results <- qs::qread(
    file = file.path("out", "tmp", "s6_frm_results.qs")
)

# Functional observations.
fd_curves_list <- qs::qread(
    file = file.path("out", "tmp", "s5_fd_curves_list.qs")
)


# Generate results for plotting ------------------------------------------------


# Generate a data.table with the results of fitting.
early_stop_curves <- data.table::data.table(
    MSTOP = unlist(
        lapply(frm_results, function(w) w[["mstops"]])
    ),
    METRIC = unlist(
        lapply(frm_results, function(w) w[["metric"]])
    ),
    MODEL = factor(
        x      = c(
            rep(var_names[[1L]], length(frm_results[[1L]]$mstops)),
            rep(var_names[[2L]], length(frm_results[[2L]]$mstops)),
            rep(var_names[[3L]], length(frm_results[[3L]]$mstops))
        ),
        levels = unlist(var_names)
    )
)


# Genreate a data.table with the best mstop.
best_fit <- data.table::data.table(
    MSTOP_BEST = unlist(
        lapply(frm_results, function(w) w[["mstop_best"]])
    ),
    METRIC_BEST = unlist(
        lapply(frm_results, function(w) min(w[["metric"]]))
    ),
    LEARNING_RATE = unlist(
        lapply(frm_results, function(w) w[["fdboost_opts"]][["learning_rate"]])
        ),
    MODEL = factor(
        x      = var_names[names(unlist(lapply(frm_results, function(w) w[["mstop_best"]])))],
        levels = unlist(var_names)
    )
)

# Extract metric used to fit the models.
metric_name <- metric_names[[frm_results$DEPTH$fdboost_opts$metric]]


# Plot -------------------------------------------------------------------------


# Save as a pdf for future use.
pdf(
    file   = file.path("out", "plots", "fig_5_early_stopping.pdf"),
    width  = 10L,
    height = 4L
)

# Plot.
ggplot(
    data    = early_stop_curves,
    mapping = aes(
        x = MSTOP,
        y = METRIC
    )
) +
geom_line(
    lwd = 0.5
) +
geom_point(
    data    = best_fit,
    mapping = aes(
        x = MSTOP_BEST,
        y = METRIC_BEST,
    ),
    col = "darkred",
    bg  = "black",
    pch = 19L,
    cex = 2L
) +
geom_text(
    data = best_fit,
    mapping = aes(
        x     = Inf,
        y     = Inf,
        label = paste0("Learning  \nrate = ", format(LEARNING_RATE, nsmall = 1L))
    ),
    vjust = 1.2,
    hjust = 1.1,
    cex   = 2L
) +
labs(
    title = "Early stopping regularisation for the three models",
    x     = "Number of iterations",
    y     = metric_name
) +
facet_wrap(
    facets = ~ MODEL,
    nrow   = 1L,
    ncol   = 3L,
    scales = "free"
)

# Save plot.
dev.off()


# Fit full model ---------------------------------------------------------------


# Depth.
frm_results$DEPTH$full <- .FDboost(
    data         = fd_curves_list$DEPTH,
    fdboost_opts = frm_results$DEPTH$fdboost_opts,
    mstop        = frm_results$DEPTH$mstop_best
)

# D50.
frm_results$D50$full <- .FDboost(
    data         = fd_curves_list$D50,
    fdboost_opts = frm_results$D50$fdboost_opts,
    mstop        = frm_results$D50$mstop_best
)

# Velocity.
frm_results$VELOCITY$full <- .FDboost(
    data         = fd_curves_list$VELOCITY,
    fdboost_opts = frm_results$VELOCITY$fdboost_opts,
    mstop        = frm_results$VELOCITY$mstop_best
)


# Exports ----------------------------------------------------------------------


qs::qsave(
    x    = frm_results,
    file = file.path("out", "tmp", "s7_frm_results_full.qs")
)

