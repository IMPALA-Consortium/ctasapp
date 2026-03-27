#' @keywords internal
"_PACKAGE"

#' @import ggplot2
#' @importFrom rlang .data .env
#' @importFrom dplyr left_join summarise mutate filter distinct pull n_distinct
#'   arrange desc select across all_of bind_rows case_when row_number first
#'   transmute rename between group_by ungroup n
#' @importFrom tidyr replace_na pivot_wider
#' @importFrom forcats fct_reorder fct_rev fct_relevel
#' @importFrom stringr str_detect str_trunc str_pad
#' @importFrom patchwork plot_annotation plot_layout
#' @importFrom purrr safely map2_dbl
#' @importFrom scales percent_format
#' @importFrom ggalluvial geom_flow geom_stratum
#' @importFrom shiny shinyApp moduleServer NS reactive reactiveVal
#'   observeEvent renderPlot renderText renderUI plotOutput textOutput uiOutput
#'   actionButton radioButtons sliderInput showNotification req icon tags
#' @importFrom bslib page_navbar nav_panel layout_sidebar sidebar card
#'   card_header card_body bs_theme
#' @importFrom DT dataTableOutput renderDataTable datatable formatStyle
#'   styleInterval
#' @importFrom htmltools tagList
#' @importFrom stats quantile IQR prop.test
#' @importFrom grDevices pdf dev.off
#' @importFrom yaml read_yaml
NULL
