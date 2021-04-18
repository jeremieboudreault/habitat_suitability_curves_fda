# s7_extract_frm_coefficients.R


# Extract FRM coefficients and compute bootstrap 95% CI.


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


# Extract coefficients ---------------------------------------------------------


.extract_coef <- function(var_name) {

    # Extract results from the list of the 3 models.
    res <- frm_results[[var_name]]

    # Extract full fitted model from res.
    fit <- res$full$fit

    # Extract coefficient from fit.
    fit_coef <- coef(fit)

    # Extract x, y, z values.
    z <- fit_coef$smterms$`bsignal(X) %O% bbs(s)`$value
    rownames(z) <- fit_coef$smterms$`bsignal(X) %O% bbs(s)`$y
    colnames(z) <- fit_coef$smterms$`bsignal(X) %O% bbs(s)`$x

    # Add proper names to the matrix.
    names(dimnames(z)) <- c("Y", "X")

    # Generate a data.frame for plotting.
    z_df <- reshape2::melt(z, value.name = "Z")

    # Add the name of variable.
    z_df$MODEL <- var_names_u[[var_name]]

    # Return z_df as a data.table.
    return(data.table::setDT(z_df))

}


# Extract coefficients for all three models ------------------------------------


coef_list <- lapply(
    X   = names(var_names),
    FUN = .extract_coef
)


# Function to plot coefficients ------------------------------------------------


# Generate colors for the plot.
col_1 <- RColorBrewer::brewer.pal(9, "RdBu")
col_fun <- grDevices::colorRampPalette(col_1)

# Create a function to generate the plot.
.plot_coef <- function(data) {

    return(
        ggplot(
            data    = data,
            mapping = aes(
                x    = X,
                y    = Y,
                z    = Z,
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
        scale_fill_gradientn(
            colors   = col_fun(100L),
            limits   = c(-max(abs(data$Z)), max(abs(data$Z)))
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
        legend_bottom
    )
}


# Plot all coefficients --------------------------------------------------------


# Generate all plots.
ps <- lapply(coef_list, .plot_coef)

# Add x and y labs to some plots.
ps[[1L]] <- ps[[1L]] + ylab(hab_names[["SELECTED"]])
ps[[2L]] <- ps[[2L]] + xlab(hab_names[["AVAILABLE"]])

# Save as a pdf for future use.
pdf(
    file   = file.path("out", "plots", "fig_6_frm_coef.pdf"),
    width  = 10L,
    height = 4L
)

# Plot.
ggpubr::ggarrange(
    plotlist = ps,
    ncol     = 3L
)

# Save plot.
dev.off()

