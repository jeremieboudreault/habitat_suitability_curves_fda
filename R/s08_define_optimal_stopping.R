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


source(file.path("R", "functions", "fdboost_helpers.R"))
source(file.path("R", "functions", "globals.R"))
source(file.path("R", "functions", "internals.R"))
source(file.path("R", "functions", "plot_helpers.R"))


# Imports ----------------------------------------------------------------------


# Fitted functional models.
frm_results <- qs::qread(
    file = file.path("out", "tmp", "s07_frm_results.qs")
)

# Functional observations.
fd_curves_list <- qs::qread(
    file = file.path("out", "tmp", "s06_fd_curves_list.qs")
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
    title = NULL,
    x     = "Number of iterations",
    y     = "Loss function"
) +
facet_wrap(
    facets = ~ MODEL,
    nrow   = 1L,
    ncol   = 3L,
    scales = "free"
) +
custom_theme()

# Save plot.
ggsave(
    file   = file.path("out", "plots", "fig_7_early_stopping.pdf"),
    width  = 9L,
    height = 3L
)


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


# Add <x> values ---------------------------------------------------------------


frm_results$DEPTH$full$x <- fd_curves_list$DEPTH$s
frm_results$D50$full$x <- fd_curves_list$D50$s
frm_results$VELOCITY$full$x <- fd_curves_list$VELOCITY$s


# Add <RIVER> values -----------------------------------------------------------


frm_results$DEPTH$full$river    <- fd_curves_list$DEPTH$RIVER
frm_results$D50$full$river      <- fd_curves_list$D50$RIVER
frm_results$VELOCITY$full$river <- fd_curves_list$VELOCITY$RIVER


# Exports ----------------------------------------------------------------------


qs::qsave(
    x    = frm_results,
    file = file.path("out", "tmp", "s08_frm_results_full.qs")
)

