# s6_fit_functional_model.R


# Fit functional regression model to curves.


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


# Functional observations.
fd_curves_list <- qs::qread(
    file = file.path("out", "tmp", "s5_fd_curves_list.qs")
)


# Depth model ------------------------------------------------------------------


# Options for the depth model.
depth_opts <- .fdboost_opts(mstop_step = 2L, mstop_max = 500L)

# Fit FDboost with k-fold cross validation using the options above.
res_depth <- FDboost_kfold(
    data         = fd_curves_list$DEPTH,
    fdboost_opts = depth_opts
)


# D50 model --------------------------------------------------------------------


# Options for the D50 model.
d50_opts <- .fdboost_opts(learning_rate = 1L, mstop_max = 2000L, mstop_step = 10L)

# Fit FDboost with k-fold cross validation using the options above.
res_d50 <- FDboost_kfold(
    data         = fd_curves_list$D50,
    fdboost_opts = d50_opts
)


# Velocity model ---------------------------------------------------------------


# Options for the velocity model.
velocity_opts <- .fdboost_opts(mstop_max = 500L, mstop_step = 2L)

# Fit FDboost with k-fold cross validation using the options above.
res_velocity <- FDboost_kfold(
    data         = fd_curves_list$VELOCITY,
    fdboost_opts = velocity_opts
)


# Generate results for plotting ------------------------------------------------


# Generate a data.table with the results of fitting.
early_stop_curves <- data.table::data.table(
    MSTOP = c(
        res_depth$mstops, res_d50$mstops, res_velocity$mstops
    ),
    METRIC = c(
        res_depth$metric, res_d50$metric, res_velocity$metric
    ),
    MODEL = factor(levels = c(unlist(var_names)), x = c(
        rep("Depth",    length(res_depth$mstops)),
        rep("D50",      length(res_d50$mstops)),
        rep("Velocity", length(res_velocity$mstops))
    ))
)

# Genreate a data.table with the best mstop.
best_fit <- data.table::data.table(
    MSTOP_BEST = c(
        res_depth$mstop_best,
        res_d50$mstop_best,
        res_velocity$mstop_best
    ),
    METRIC_BEST = c(
        min(res_depth$metric),
        min(res_d50$metric),
        min(res_velocity$metric)
    ),
    LEARNING_RATE = c(
        res_depth$fdboost_opts$learning_rate,
        res_d50$fdboost_opts$learning_rate,
        res_velocity$fdboost_opt$learning_rate
    ),
    MODEL = factor(
        x      = unlist(var_names),
        levels = c(unlist(var_names))
    )
)

# Extract metric used to fit the models.
metric_name <- metric_names[[res_depth$fdboost_opts$metric]]


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
res_depth$full <- .FDboost(
    data         = fd_curves_list$DEPTH,
    fdboost_opts = res_depth$fdboost_opts,
    mstop        = res_depth$mstop_best
)

# D50.
res_d50$full <- .FDboost(
    data         = fd_curves_list$D50,
    fdboost_opts = res_d50$fdboost_opts,
    mstop        = res_d50$mstop_best
)

# Velocity.
res_velocity$full <- .FDboost(
    data         = fd_curves_list$VELOCITY,
    fdboost_opts = res_velocity$fdboost_opts,
    mstop        = res_velocity$mstop_best
)


# Exports ----------------------------------------------------------------------


qs::qsave(
    x   = list(
        DEPTH    = res_depth,
        D50      = res_d50,
        VELOCITY = res_velocity
    ),
    file = file.path("out", "tmp", "s6_frm_results.qs")
)

