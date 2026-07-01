#' Plot landings or catch compositions
#'
#' @description Landings or catch compositions by stock for selected years, countries, fleets, metiers etc
#'
#' @param data data.frame Contains information on fleet data to make catch (landings) compositions.
#' Required variables are: `year`, `area`,`country`, `fleet`, `metier`,`stock`,`landings`, `catch`,
#' and `fleet_type` which indicates if the `fleet` is a `main` or `residual` fleet.
#'
#' @param refTable data.frame A look-up reference table for stocks and associated attributes.
#' The \code{refTable} data.frame lists stock names and
#' corresponding colours for consistency across plots. To be used as a look-up
#' table in converting between variable stock names and printed ones.
#'
#' \itemize{
#'   \item 1) stock - ICES stock codes used in advice
#'   \item 2) order - stock order to be used in plots
#'   \item 3) col - stock colors for plots (e.g. pals::brewer.paired())
#'   \item 4) stock_short - short stock name used in mixed fishery model
#' }
#'
#' @param filters list of character strings listing the `year`, `area`,`country`, `fleet` and/or `metier`
#' to filter from \code{data}. Default value of \code{NULL} will produce catch compositions using all data in \code{data}.
#'
#' @param selectors character string of one of `year`, `area`,`country`, `fleet` or `metier`. The chosen
#'  selector will be plotted on the x-axis. Multiple variables can be listed as \code{selectors} and these will be
#'  concatenated into a "label" for plotting. The default value is \code{metier} and will produce catch
#'  compositions by `metier`.
#'
#' @param divider character string of one of `year`, `area`,`country`,
#'   `fleet` or `metier`. Only one variable can be listed as a `divider`.
#'   The chosen divider will be used to divide the catch compositions into
#'   subplots - e.g. one per`fleet`. The default value of \code{NULL} will
#'   plot just one catch composition (i.e. no subplots).
#'
#' @param yvar character string of variable to be plotted on the y-axis (Default: yvar = "landings")
#'
#' @param tryNumericSelector logical. Should the selector variable be converted
#'   to a numeric variable. Likely only makes sense when `selector = "year"`
#'   (Default: `tryNumericSelector = FALSE`)
#' @param flipAxes logical. Should `coord_flip` be applied to flip x- and
#'   y-axes. (Default: `flipAxes = FALSE`)
#' @param relative logical. Should composition be presented in relative terms
#'   (Default: `relative = TRUE`)
#'
#' @details Users will need to provide the data and refTable objects to produce the plot.
#'
#' @return plot output of class ggplot
#' @import ggplot2
#'
#' @export
#'
#' @examples
#'
#' # prepare example data
#' data(refTable)
#' data(stfMtStkSum)
#'
#' # subset data to a single scenario (e.g. min)
#' data <- subset(stfMtStkSum, scenario == "min")
#'
#' # add country and area identifiers (if desired)
#' tmp <- strsplit(data$metier, ".", fixed = TRUE)
#' data$area <- unlist(lapply(tmp, FUN = function(x){ifelse(length(x)==2, x[2], NA)}))
#' tmp <- strsplit(data$fleet, "_", fixed = TRUE)
#' data$country <- unlist(lapply(tmp, FUN = function(x){ifelse(length(x)==2, x[1], NA)}))
#'
#'
#' # replace stock with ICES stock code
#' data$stock <- refTable$stock[match(data$stock, refTable$stock_short)]
#'
#'
#' # Plot catch composition for each fleet over time
#' selectors <- c("year")
#' divider <- c("fleet")
#' p <- plot_catchComp(data,refTable, filters = NULL,
#'   selectors = selectors, divider = divider, yvar = "catch")
#' print(p)
#'
#'
#' # ggplot format adjustments
#' p2 <- p + theme(text = element_text(size = 8),
#'   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#'   facet_wrap(divider,  scales = "fixed") # remove free axes
#' print(p2)
#'
#' # Remove relative scaling, and treat year axis (selector) as numeric
#' plot_catchComp(data, refTable, filters = NULL, selectors, divider,
#'   yvar = "catch", relative = FALSE, tryNumericSelector = TRUE) +
#'   theme(text = element_text(size = 8),
#'     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#'   facet_wrap(divider, ncol = 6, scales = "free_y")
#'
#' # export plot
#' # png("catchComp1.png", width = 7, height = 7, units = "in", res = 400)
#' #  print(p2); dev.off()
#'
#'
#' # lot landings composition for each area by country-metier combinations
#' selectors <- c("country", "metier")
#' divider <- c("area")
#' p <- plot_catchComp(data, refTable, filters=NULL,
#'   selectors, divider, relative = TRUE, flipAxes = TRUE)
#' print(p)
#'
#'
#' # plot landings composition for each metier by country for 2022
#' filters <- list(year = 2022)
#' selectors <- c("metier")
#' divider <- c("country")
#' plot_catchComp(data, refTable, filters, selectors, divider, flipAxes = TRUE)
#'
#' # plot landings compositions for each fleet by metier for Scottish fleets.
#' filters <- list(year=2022, country="SC")
#' selectors <- c("metier")
#' divider <- c("fleet")
#' plot_catchComp(data,refTable,filters,selectors, divider, flipAxes = TRUE)
#'
#'
plot_catchComp <- function(
  data,
  refTable,
  filters = NULL,
  selectors = "metier",
  divider = NULL,
  yvar = "landings",
  tryNumericSelector = FALSE,
  flipAxes = FALSE,
  relative = TRUE
){

  position_type <- if (relative) "fill" else "stack"

  # ---------------------------
  # 1. Filters
  # ---------------------------
  if (!is.null(filters)) {
    for (var in names(filters)) {
      data <- data %>%
        dplyr::filter(.data[[var]] %in% filters[[var]])
    }
  }

  # ---------------------------
  # 2. Clean area
  # ---------------------------
  if ("area" %in% names(data)) {
    data$area[is.na(data$area)] <- "notSpecified"
  }

  # ---------------------------
  # 3. Build label
  # ---------------------------
  selector_cols <- intersect(selectors, names(data))

  if (length(selector_cols) == 0) {
    stop("None of the selectors exist in the data")
  }

  data <- data %>%
    dplyr::mutate(
      label = do.call(paste, c(dplyr::across(dplyr::all_of(selector_cols)), sep = "_"))
    )

  # ---------------------------
  # 4. Group & summarise
  # ---------------------------
  group_vars <- c("label", "stock", divider)
  group_vars <- group_vars[!is.na(group_vars)]

  data <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      VAR = sum(.data[[yvar]], na.rm = TRUE),
      .groups = "drop"
    )

  # ---------------------------
  # 5. Optional numeric label
  # ---------------------------
  if (tryNumericSelector) {
    data <- data %>%
      dplyr::mutate(label = as.numeric(label))
  }

  # ---------------------------
  # 6. Colours
  # ---------------------------
  data <- dplyr::left_join(data, refTable, by = "stock")

  tmp <- unique(data[, c("stock", "col", "order")])
  tmp <- tmp[order(tmp$order), ]

  stkColors <- tmp$col
  names(stkColors) <- tmp$stock

  stkColorScale <- ggplot2::scale_colour_manual(
    name = "stock",
    values = stkColors,
    aesthetics = c("colour", "fill")
  )

  data$stock <- factor(data$stock, levels = tmp$stock)

  # ---------------------------
  # 7. Plot
  # ---------------------------
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = label, y = VAR, colour = stock, fill = stock)
  ) +
    ggplot2::geom_col(position = position_type) +
    ggplot2::labs(x = "", y = "", fill = "", colour = "") +
    ggplot2::theme_bw() +
    stkColorScale +
    ggplot2::guides(
      fill = ggplot2::guide_legend(ncol = 1),
      colour = ggplot2::guide_legend(ncol = 1)
    )

  if (flipAxes) {
    p <- p + ggplot2::coord_flip()
  }

  # ---------------------------
  # 8. Facets (safe evaluation)
  # ---------------------------
  if (!is.null(divider)) {
    p <- p +
      ggplot2::facet_wrap(
        stats::as.formula(paste("~", divider)),
        scales = "free"
      )
  }

  return(p)
}
