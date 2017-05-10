#' Creates table for PCR recipie
#'
#' Creates a markdown table for a PCR master mix recipe.
#' 
#' @param rxn_count The number of PCR reactions.
#' @param rxn_volume The total volume of each reaction
#' @param template_volume How much template to add
#' @param safety How much extra master mix to make to account for errors. For example 1.1 would be \%10 extra.
#' @param checklist If TRUE, print check list
#' @export
platinum_pcr_recipe <- function(rxn_count, rxn_volume, template_volume, safety = 1.1, checklist = TRUE) {
  # print description
  cat(paste0("Recipe for ", rxn_count, " ", rxn_volume, "uL reactions with a safety factor of ", safety, ".\n"))
  
  # Define components and concentrations
  pcr <- data.frame(stringsAsFactors = FALSE, 
                    component = c("Water", "10X PCR buffer -Mg", "50mM MgCl2", "10mM dNTP mix",
                                  "10uM ITS6", "10uM ITS4", "Template DNA", "Platinum Taq"),
                    conc = c(NA, 10, 50, 10, 10, 10, NA, 10),
                    conc_unit = c(NA, "X", "mM", "mM", "uM", "uM", NA, "U"),
                    rxn_conc = c(NA, 1, 1.5, 0.2, 0.2, 0.2, NA, 0.04))
  
  # Infer volume of components for one reaction
  pcr$per_tube <- pcr$rxn_conc / pcr$conc * rxn_volume
  pcr$per_tube[pcr$component ==  "Template DNA"] <- template_volume
  pcr$per_tube[pcr$component ==  "Water"] <- rxn_volume - sum(pcr$per_tube, na.rm = TRUE)
  
  # Infer master mix volumes 
  pcr$master_mix <- pcr$per_tube * rxn_count * safety
  pcr$master_mix[pcr$component == "Template DNA"] <- 0
  
  # Add component conc to display
  pcr$component_conc <- paste(pcr$rxn_conc, pcr$conc_unit)
  pcr$component_conc[pcr$component_conc == "NA NA"] <- NA 
  
  # Add "total" row
  pcr <- rbind(pcr, c("Total", NA, NA, NA, sum(pcr$per_tube), sum(pcr$master_mix), NA))
  
  # Display 
  cat(paste0("PCR recipe for ", rxn_count, " ", rxn_volume, "uL reactions with ",  template_volume, "uL of template each:"))
  pcr_to_display <- pcr[, c("component", "component_conc", "per_tube", "master_mix")]
  names(pcr_to_display) <- c("Component", "Concentration", "Volume per tube", "Volume in master mix")

  # Checklist
  if (checklist) {
    cat(
"
[ ] Clean workspace   
[ ] Thaw reagents + template    
[ ] Label tubes   
[ ] Vortex and centrifudge reagents + template
[ ] Make master mix
[ ] Add master mix   
[ ] Add template   
[ ] Vortex and centrifudge reactions   
[ ] Check for bubbles
"
)
  }
  
  return(knitr::kable(pcr_to_display))
}

#===================================================================================================
#' Creates a thermocycler profile
#' 
#' Creates a thermocycler profile in either graphical or verbal form. This is intended to be used
#' with arbitrary profiles. For generic PCR profiles, use \code{\link{pcr_profile}}.
#' 
#' @param profile (\code{list(list(vector(2)))}) A list of named 2-element numeric vectors, each
#'  element in the list representing a stage in a profile with the 2-element numeric vector 
#'  representing the temperature and time. Optionally, a named list of the data structure described
#'  above can be supplied to split the stages into named groups (see example).
#' @param repeats (\code{integer}) The number of times each \code{profile} is repeated.
#' @param width (\code{numeric}) The relative width of groupings in the graphical output. By
#'  default, the width of each group will be proportional to the number of stage it contains.
#'  
#' @seealso \code{\link{pcr_profile}}
#' 
#' @export
thermocycler_profile <- function(profile, repeats = NULL, width = NULL) {
  # Argument validation ----------------------------------------------------------------------------
  if (unique(rapply(profile, length)) != 2 || depth(profile) > 2 || depth(profile) < 1) {
    stop("incorrect input format.")
  }
  # Argument parsing -------------------------------------------------------------------------------
  if (depth(profile) == 1) profile = list(profile)
  if (is.null(width)) width <- sapply(profile, length)
  if (is.null(repeats)) repeats <- rep(1, length(profile))
  # Reformat profile into dataframe ----------------------------------------------------------------
  data <- reshape2::melt(profile)
  data <- cbind(data$value[seq(2, nrow(data), 2)], data[seq(1, nrow(data), 2), ])
  names(data) <- c("time", "temp", "stage", "group")
  data$group <- factor(data$group, levels = unique(data$group), ordered = TRUE)
  levels(data$group) <- paste(levels(data$group), " (x", repeats, ")", sep = "")
  data$label <- paste(data$time, "S at ", data$temp, "\u00B0C", sep = "")
  # Account for group repeats ----------------------------------------------------------------------
  duration_to_range <- function(data, start) {
    end_times <- cumsum(data$time) + start - 1
    start_times <- c(start, end_times[-length(end_times)] + 1)
    data <- plyr::adply(data, 1, function(x) x[c(1,1), ]) #duplicate each row 
    data$time[seq(1, nrow(data), 2)] <- start_times #replace odd rows with start time
    data$time[seq(2, nrow(data), 2)] <- end_times #replace even rows with end time
    return(data)
  }
  start_time <- plyr::daply(data, "group", function(x) sum(x$time)) * repeats
  start_time <- cumsum(c(1, start_time[-length(start_time)]))
  split_data <- split(data, data$group)
  data <- do.call(rbind,
                  lapply(seq_along(split_data),
                         function(i) duration_to_range(split_data[[i]], start = start_time[i])))
  data$time <- data$time/60
  # Make graph -------------------------------------------------------------------------------------
  data$stage[seq(2,nrow(data), 2)] <- ""
  data$label[seq(2,nrow(data), 2)] <- ""
  my_plot <- ggplot2::ggplot(data = data, ggplot2::aes_string(x = "time", y = "temp")) +
    ggplot2::geom_line() +
    ggplot2::geom_text(size = 4, hjust=-.03, vjust=-.4, ggplot2::aes_string(label = "stage")) +
    ggplot2::geom_text(size = 4, hjust=-.03, vjust=1.4, ggplot2::aes_string(label = "label")) +
    ggplot2::facet_grid (.~ group, scales = "free_x", space = "free_x") +
    ggplot2::labs(x="Run Time (Minutes)", y = "Temperature (C)") +
    ggplot2::scale_y_continuous(expand = c(.2, 0)) +
    ggplot2::theme(#panel.margin = grid::unit(0, "inches"),
      panel.background = ggplot2::element_blank(), 
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(size = 16))
  return(my_plot)
}

#===================================================================================================
#' Creates a thermocycler profile for PCR
#' 
#' Creates a thermocycler profile for PCR in either graphical or verbal form. This is intended to be
#' used for mostly generic profiles. For more advanced profiles, use
#' \code{\link{thermocycler_profile}}. All temperature and time parameters take \code{numeric}
#' inputs in celsius and seconds respectively.
#' 
#' @param cycles (\code{integer}) The number of cycles.
#' @param init_tm (\code{numeric}) The initial heating temperature used before cycling starts.
#' @param init_time (\code{numeric}) The duration of the initial heating used before cycling starts.
#' @param denat_tm (\code{numeric}) The temperature of the denaturation step.
#' @param denat_time (\code{numeric}) The duration of the denaturation step.
#' @param anneal_tm (\code{numeric}) The temperature of the annealing step.
#' @param anneal_time (\code{numeric}) The duration of the annealing step.
#' @param elong_tm (\code{numeric}) The temperature of the elongation step.
#' @param elong_time (\code{numeric}) The duration of the elongation step.
#' @param final_tm (\code{numeric}) The temperature of the final elongation step used after cycling completes.
#' @param final_time (\code{numeric}) The duration of the final elongation step used after cycling completes.
#' @param hold_tm (\code{numeric}) The temperature used after all other steps to hold the sample.
#' 
#' @keywords PCR
#' 
#' @seealso \code{\link{thermocycler_profile}}
#' 
#' @export
pcr_profile <- function(cycles = 30, init_tm = 95, init_time = 300, denat_tm = 96,
                        denat_time = 25, anneal_tm = 55, anneal_time = 30, elong_tm = 72,
                        elong_time = 100, final_tm = 72, final_time = 600, hold_tm = 5) {
  profile <- list("Initialization" = list("Initial denaturation" = c(init_tm, init_time)),
                  "Amplification"  = list("Denaturation" = c(denat_tm, denat_time),
                                          "Annealing" = c(anneal_tm, anneal_time),
                                          "Elongation" = c(elong_tm, elong_time)),
                  "Resolution"     = list("Final elongation" = c(final_tm, final_time),
                                          "Holding" = c(hold_tm, Inf)))
  thermocycler_profile(profile, repeats = c(1, cycles, 1))
}
