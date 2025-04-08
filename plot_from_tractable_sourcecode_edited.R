# Use colorblind-friendly palette from http://jfly.iam.u-tokyo.ac.jp/color/
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Global Variables
PALETTE_NAMES    <- c("cb", "cbb", "colorblind", "colorblind_black", 
                      rownames(RColorBrewer::brewer.pal.info))
RIBBON_FUNCTIONS <- c("mean_cl_boot", "mean_cl_normal", "mean_sdl", 
                      "median_hilow")

# Set Dummy Global Variables to appease R CMD check
utils::globalVariables(c("x", "ymin", "ymax", "group"))

#' Retrieve tract name from abbreviation
#'
#' @param tract_abbr AFQ tract abbreviation
#'
#' @return Formatted tract name
#'
#' @examples
#' full_name <- tract_name("OR")
#' full_name <- tract_name("CST_L")
#' @export
tract_name <- function(tract_abbr) {
  name <- switch(
    tract_abbr,
    "OR"    = "Optic Radiation",
    "CST_R" = "Right Corticospinal",
    "CST_L" = "Left Corticospinal",
    "UNC_R" = "Right Uncinate",
    "UNC_L" = "Left Uncinate",
    "IFO_L" = "Left IFOF",
    "IFO_R" = "Right IFOF",
    "ARC_R" = "Right Arcuate",
    "ARC_L" = "Left Arcuate",
    "ATR_R" = "Right Thalamic Radiation",
    "ATR_L" = "Left Thalamic Radiation",
    "CGC_R" = "Right Cingulum Cingulate",
    "CGC_L" = "Left Cingulum Cingulate",
    "HCC_R" = "Right Cingulum Hippocampus",
    "HCC_L" = "Left Cingulum Hippocampus",
    "FP"    = "Callosum Forceps Major",
    "FA"    = "Callosum Forceps Minor",
    "ILF_R" = "RightILF",
    "ILF_L" = "LeftILF",
    "SLF_R" = "RightSLF",
    "SLF_L" = "LeftSLF",
    tract_abbr
  )
  return(name)
}

#' Plot tract profiles
#' 
#' @description
#' Create tract profile figures for each tract as a facet and for each `y`
#' value as a figure.
#'
#' @param df              The input dataframe. 
#' @param y               Column name(s) of the variables to plot on the y-axis.
#' @param tracts          Name(s) of the tract tracts to plot per facet. 
#'                        Default: NULL \cr \cr
#'                        If NULL, will be all tracts in the data frame.
#' @param tract_col       The column name that encodes the tract information.
#'                        Default: tractID
#' @param node_col        The column name that encodes tract node positions.
#'                        Default: "nodeID".
#' @param group_col       Column name that encodes group information. Will be 
#'                        drawn by color. Default: NULL. 
#'                        \itemize{
#'                          \item  If `group_col` is a factor, will use unique
#'                                 values as groups.
#'                          \item  If `group_col` is a numeric, will use
#'                                 [ggplot2::cut_interval] to create equal range
#'                                 `n_groups` as groups.
#'                        }
#' @param n_groups        Number of groups to split a numeric grouping variable.
#'                        Only used if `group_col` is numeric. 
#'                        Default: NULL.
#' @param group_pal       Grouping color palette name. Valid options include: 
#'                        "cb" (colorblind), "cbb" (colorblind_black), and all 
#'                        named [RColorBrewer] palettes.
#'                        Default: "cb" (colorblind)
#' @param participant_col The column name that encodes participant ID.
#'                        Default: "subjectID". 
#' @param ribbon_func     Ribbon summarizing function that provides the y, ymin,
#'                        and ymax for the ribbon. See `fun.data` argument from
#'                        [ggplot2::stat_summary] for more information.
#'                        Default: "mean_cl_boot"
#' @param ribbon_alpha    Ribbon alpha level. Default: 0.25
#' @param linewidth       Line thickness of the tract profile line. Default: 1.
#' @param save_figure     Boolean flag. If TRUE, save tract profiles. If FALSE, 
#'                        do not save tract profiles. Default: FALSE
#' @param output_dir      Output directory for the figure image(s).
#'                        Default: [getwd] (current working directory). 
#' @param ...             Keyword arguments to be passed to [ggplot2::ggsave].
#'                      
#' @return Named list of plot handles corresponding to the specified y values.
#' 
#' @details
#' If `save_figure` is TRUE, the naming convention is as follows:
#' 
#' - If `group_col`, "tract_by-(group_col)_param-(y)_profiles.png".
#' - If `group_col == NULL`, "tract_param-(y)_profiles.png"
#' 
#'
plot_tract_profiles_my_edit <- function (
    df,
    y, 
    metric,
    tracts          = NULL,
    tract_col       = "tractID",
    node_col        = "nodeID", 
    group_col       = NULL,
    n_groups        = NULL,
    group_pal       = "cb",
    participant_col = "subjectID", 
    ribbon_func     = "mean_cl_boot",
    ribbon_alpha    = 0.25,
    linewidth       = 1,
    save_figure     = TRUE, 
    output_dir      = getwd(),
    ... 
) {
  # argument input control
  stopifnot("`df` must be a class data.frame or tibble" = 
    any(class(df) %in% c("data.frame", "tbl_df")))
  stopifnot("`y` must be a character" = is.character(y))
  if (!is.null(tracts)) {
    stopifnot("`tracts` must be a character" = is.character(tracts))
  }
  stopifnot("`tract_col` must be a character" = is.character(tract_col))
  stopifnot("There can be only one `tract_col`" = length(tract_col) == 1)
  stopifnot("`node_col` must be a character" = is.character(node_col))
  stopifnot("There can be only one `node_col`" = length(node_col) == 1)
  if (!is.null(group_col)) { # group_col is NOT null 
    stopifnot("`group_col` must be a character" = is.character(group_col))
    stopifnot("There can be only one `group_col`" = length(group_col) == 1)
    stopifnot("`group_col` must be a column in the dataframe `df`" =
      group_col %in% colnames(df))
    if (is.numeric(df[[group_col]])) {
      stopifnot("`n_groups` must be a integer" = round(n_groups) == n_groups)
    }
    rlang::arg_match(group_pal, values = PALETTE_NAMES)
  }
  stopifnot("`linewidth` must be a numeric" = is.numeric(linewidth))
  rlang::arg_match(ribbon_func, values = RIBBON_FUNCTIONS) 
  stopifnot("`ribbon_alpha` must be a numeric" = is.numeric(ribbon_alpha))
  stopifnot("`save_figure` must be a logical." = is.logical(save_figure))
  if (save_figure) {
    stopifnot("`output_dir` must exist." = fs::is_dir(output_dir))
  }

  # prepare tracts, if NULL all unique tracts
  if (is.null(tracts)) {
    tracts <- unique(df[[tract_col]])
  } 
   
  # prepare grouping column all to factors
  if (!is.null(group_col)) { # if not NULL
    group_values  <- df[[group_col]] # grouping values
    color_palette <- `_get_palette`(group_pal) # get group color palette
    if (is.numeric(group_values)) {
      df[[group_col]] <- ggplot2::cut_interval(group_values, n = n_groups)
    } else if (is.character(group_values)) {
      df[[group_col]] <- forcats::fct(group_values)
    }  
  }

  # prepare data frame for plotting
  keep_cols <- c(participant_col, tract_col, node_col, group_col, y)
  df_plot <- df[df[[tract_col]] %in% tracts, ] %>%
    dplyr::select(tidyselect::all_of(keep_cols)) %>% 
    dplyr::rename(
      x = tidyselect::all_of(node_col), 
      tracts = tidyselect::all_of(tract_col)
    )

  # prepare summarizing function 
  ribbon_func <- `_get_ribbon_func`(ribbon_func) # get ribbon function
    
  plot_handles <- list() # initialize
  for (y_curr in y) { # for each y-axis variable
    if (is.null(group_col)) {
      # prepare current y-axis values to plot
      df_curr <- df_plot %>% 
        dplyr::rename(y = tidyselect::all_of(y_curr)) %>% 
        dplyr::group_by(x, tracts) %>% 
        dplyr::summarize(ribbon_func(y), .groups = "drop")

      # create current metric figure handle
      plot_handle <- df_curr %>% 
        ggplot2::ggplot(ggplot2::aes(x = x, y = y, ymin = ymin, ymax = ymax)) +
        ggplot2::geom_ribbon(color = NA, alpha = ribbon_alpha) +
        ggplot2::geom_line(linewidth = linewidth) + 
        ggplot2::scale_x_continuous(name = "Position") + 
        ggplot2::scale_y_continuous(name = stringr::str_to_upper(y_curr)) + 
        ggplot2::facet_wrap(~ tracts) + 
        ggplot2::theme_bw() +
        ###### My added code to change top of axis
        ggplot2::theme(
          strip..background = ggplot2::element_blank(),  #remove rectangle around plot title
          strip.text = ggplot2::element_text(size=14, face="bold") # increase fontsize of title
        ####################
        )

      # prepare the saved figure file name
      output_fname <- sprintf("tracts_param-%s_profile.png", y_curr)
    } else {
      # prepare current y-axis values to plot
      df_curr <- df_plot %>% 
        dplyr::rename(
          y = tidyselect::all_of(y_curr), 
          group = tidyselect::all_of(group_col)
        ) %>% 
        dplyr::group_by(x, group, tracts) %>% 
        dplyr::summarize(ribbon_func(y), .groups = "drop")
      
      # Prepare the data for plotting the normal (non-yellow) lines
      df_normal <- df_curr %>% 
        dplyr::mutate(
          line_color = dplyr::case_when(
            metric == "md" & y >= 0 ~ as.character(group),  # Normal color if 'md' and y >= 0
            metric == "fa" & y <= 0 ~ as.character(group),  # Normal color if 'fa' and y <= 0
            TRUE ~ as.character(group)                      # Default to original group color
          )
        ) %>%
        dplyr::filter(is.na(line_color) | line_color != "yellow")  # Remove yellow-colored rows
      
      # Prepare the data for plotting the yellow lines (for each group)
      df_yellow <- df_curr %>% 
        dplyr::mutate(
          line_color = dplyr::case_when(
            metric == "md" & y < 0 ~ "yellow",  # Yellow if 'md' and y < 0
            metric == "fa" & y > 0 ~ "yellow",  # Yellow if 'fa' and y > 0
            TRUE ~ NA_character_               # No color for other points
          )
        ) %>%
        dplyr::filter(!is.na(line_color))  # Only keep rows where the color is yellow

      #######
  
      # create current metric figure handle
      plot_handle <- df_curr %>% 
        ggplot2::ggplot(ggplot2::aes(x = x, y = y, ymin = ymin, ymax = ymax, 
          group = group, color = group, fill = group)) +
        ggplot2::geom_ribbon(color = NA, alpha = ribbon_alpha) +
        
        # Plot the normal lines (without the yellow parts)
        ggplot2::geom_line(data = df_normal, 
                           mapping = ggplot2::aes(x = x, y = y, group = group), 
                           linewidth = linewidth) + 
        
        # Plot the yellow lines (for each group)
        ggplot2::geom_line(data = df_yellow, 
                           mapping = ggplot2::aes(x = x, y = y, group = group), 
                           color = "yellow", linewidth = linewidth) + 
        
        ggplot2::geom_hline(yintercept = 0.0, linewidth = 1, linetype = "solid", color = "black") + # bold line at 0
        ggplot2::scale_x_continuous(name = "Position") + 
        ggplot2::scale_y_continuous(name = stringr::str_to_upper(y_curr)) + 
        ggplot2::scale_color_manual(name = group_col, values = color_palette) +
        ggplot2::scale_fill_manual(name = group_col, values = color_palette) +
        ggplot2::facet_wrap(~ tracts) + 
        ggplot2::theme_bw() +
        ###### My added code to change top of axis
        ggplot2::theme(
        strip.background = ggplot2::element_blank(),   # removes the rectangle around title
        strip.text = ggplot2::element_text(size = 16),  # makes the text larger
        axis.title.x = ggplot2::element_text(size = 18),   # larger x axis title
        axis.title.y = ggplot2::element_text(size = 18),   # larger y axis title
        axis.text.x  = ggplot2::element_text(size = 16),   # larger x tick labels
        axis.text.y  = ggplot2::element_text(size = 16),   # larger y tick labels
        legend.text  = ggplot2::element_text(size = 16),   # larger legend text
        legend.title = ggplot2::element_text(size = 18)    # larger legend title
      )
  
      # prepare the saved figure file name
      output_fname <- sprintf("tracts_by-%s_param-%s_profile.png", group_col, y_curr)
    }
      
    # save the figure 
    if (save_figure) {
      ggplot2::ggsave(
        filename = file.path(output_dir, output_fname),
        plot     = plot_handle,
        ...      = ...
      )  
    }

    # collect plot handles by metric
    plot_handles <- c(plot_handles, list(plot_handle))
  }

  # assign names to plot handles and return
  names(plot_handles) <- y

  return(plot_handles)
}


# Helper Functions that are NOT exported
`_get_ribbon_func` <- function(func_name) {
  rlang::arg_match(func_name, values = RIBBON_FUNCTIONS)
  ribbon_func <- switch(
    func_name,
    "mean_cl_boot"   = ggplot2::mean_cl_boot,
    "mean_cl_normal" = ggplot2::mean_cl_normal,
    "mean_sdl"       = ggplot2::mean_sdl,
    "median_hilow"   = ggplot2::median_hilow
  )
  return(ribbon_func)
}


`_get_palette` <- function(palette_name) {
  rlang::arg_match(palette_name, values = PALETTE_NAMES)
  color_palette <- switch(
    palette_name, 
    "cb"               = cbPalette, 
    "cbb"              = cbbPalette, 
    "colorblind"       = cbPalette, 
    "colorblind_black" = cbbPalette, 
    { # default RColorBrewer palettes
      n <- RColorBrewer::brewer.pal.info[palette_name, "maxcolors"]
      RColorBrewer::brewer.pal(n, palette_name)
    }
  )
  return(color_palette)
}