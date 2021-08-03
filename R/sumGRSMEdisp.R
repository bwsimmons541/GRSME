#' @title sumGRSMEdisp
#'
#' @description takes prepared FINS Data and generates summaries for weir report.
#'
#' @param data FINS data filtered for year, trap, and species of interest
#' @author Tyler Stright
#'
#' @examples 
#'
#' @import tidyverse, lubridate, ggplot2, viridis
#' @export
#' @return NULL


sumGRSMEdisp <- function(data, origin_) {
  
  # WITH RECAPS ----
  # Assign Dispositions based on moved_to
  disp_summary <- data %>%
    filter(trap_year == year(Sys.Date()),  
           species == "Chinook") %>%
    mutate(Class = case_when(
      age_designation == 'Jack/Jill' ~ 'J',
      sex == 'Male' ~ 'M',
      sex == 'Female' ~ 'F'),
      Disposition = case_when(
        living_status %in% c('DOA', 'TrapMort') ~ 'Mortality', 
        disposition == 'Released' & str_detect(moved_to, 'Wallowa River') & purpose == 'Recycled' ~ 'Recycled to Fishery',
        disposition == 'Released' & str_detect(moved_to, 'Wallowa River') & purpose == 'Outplant' ~ 'Wallowa River Outplant',
        disposition == 'Released' & moved_to %in% c('Lostine River: Above Weir', 'Lostine River: Acclimation Facility') ~ 'Upstream Release',
        disposition %in% c('Ponded', 'Transferred') ~ 'Brood Collection',
        disposition == 'Disposed' ~ 'Food Distribution'
      )) %>%
    group_by(Disposition, Class, origin) %>%
    summarize(Count = sum(count)) 
  
  disposition_list <- c('Upstream Release', 'Brood Collection', 'Food Distribution', 'Wallowa River Outplant', 'Recycled to Fishery', 'Mortality')
  
    # Dispositions Summary 
  disp_df <- disp_summary %>%
    filter(origin == origin_) %>%
    spread(key= Class, value = Count, fill = 0) 
  
  if(!"F" %in% colnames(disp_df)) {   # Add Females if not present
    disp_df$`F` <- 0
  }
  if(!"M" %in% colnames(disp_df)) {   # Add Males if not present
    disp_df$M <- 0
  }
  if(!"J" %in% colnames(disp_df)) {   # Add Jacks if not present
    disp_df$J <- 0
  }
  
  disp_df <- disp_df %>%
    mutate(`Total [>630]` = `F`+`M`,
           `Total [all]` = `J`+`F`+`M`) %>%
    select(`Disposition`, `J`, `F`, `M`, `Total [>630]`, `Total [all]`, `origin`) %>%
    ungroup() %>%
    select(-origin)
  
  disp_tot <- apply(disp_df[,c(2:6)], 2, sum) 
  
  disp_df <- disp_df %>%
    add_row(Disposition = 'Total', `J`= disp_tot[1], `F`= disp_tot[2], `M`= disp_tot[3], `Total [>630]`= disp_tot[4], `Total [all]`= disp_tot[5])
  
    # If there is no data for a disposition, add a row showing zeros.
  for (i in 1:length(disposition_list)) {
    if(!disposition_list[i] %in% unique(disp_df$Disposition)) {
      disp_df <- disp_df %>%
        add_row(Disposition = disposition_list[i], `J`= 0, `F`= 0, `M`= 0, `Total [>630]`= 0, `Total [all]`= 0)
    } else {
      next
    }
  }
  
  disp_df <- disp_df[order(match(disp_df$Disposition, c('Upstream Release', 'Brood Collection', 'Food Distribution', 
                                               'Wallowa River Outplant', 'Recycled to Fishery', 'Mortality', 'Total'))),]
  
  # EXCLUDING RECAPS ----
    # Assign Dispositions based on moved_to
  disp_summary2 <- data %>%
    filter(trap_year == year(Sys.Date()),  
           species == "Chinook",
           recap == FALSE) %>%
    mutate(Class = case_when(
      age_designation == 'Jack/Jill' ~ 'J',
      sex == 'Male' ~ 'M',
      sex == 'Female' ~ 'F'),
      Disposition = case_when(
        living_status %in% c('DOA', 'TrapMort') ~ 'Mortality',
        disposition == 'Released' & str_detect(moved_to, 'Wallowa River') & purpose == 'Recycled' ~ 'Recycled to Fishery',
        disposition == 'Released' & str_detect(moved_to, 'Wallowa River') ~ 'Wallowa River Outplant',
        disposition == 'Released' & moved_to %in% c('Lostine River: Above Weir', 'Lostine River: Acclimation Facility') ~ 'Upstream Release',
        disposition %in% c('Ponded', 'Transferred') ~ 'Brood Collection',
        disposition == 'Disposed' ~ 'Food Distribution'
      )) %>%
    group_by(Disposition, Class, origin) %>%
    summarize(Count = sum(count)) 
  
  disposition_list <- c('Upstream Release', 'Brood Collection', 'Food Distribution', 'Wallowa River Outplant', 'Recycled to Fishery', 'Mortality')
  
    # Dispositions Summary
  NoRecap_df <- disp_summary2 %>%
    filter(origin == origin_) %>%
    spread(key= Class, value = Count, fill = 0) 
  
  if(!"F" %in% colnames(NoRecap_df)) {   # Add Females if not present
    NoRecap_df$`F` <- 0
  }
  if(!"M" %in% colnames(NoRecap_df)) {   # Add Males if not present
    NoRecap_df$M <- 0
  }
  if(!"J" %in% colnames(NoRecap_df)) {   # Add Jacks if not present
    NoRecap_df$J <- 0
  }
  
  NoRecap_df <- NoRecap_df %>%
    mutate(`Total [>630]` = `F`+`M`,
           `Total [all]` = `J`+`F`+`M`) %>%
    select(`Disposition`, `J`, `F`, `M`, `Total [>630]`, `Total [all]`, `origin`) %>%
    ungroup() %>%
    select(-origin)
  
  NoRecap_tot <- apply(NoRecap_df[,c(2:6)], 2, sum) 
  
  NoRecap_df <- NoRecap_df %>%
    add_row(Disposition = 'Total', `J`= NoRecap_tot[1], `F`= NoRecap_tot[2], `M`= NoRecap_tot[3], `Total [>630]`= NoRecap_tot[4], `Total [all]`= NoRecap_tot[5])
  
  # If there is no data for a disposition, add a row showing zeros.
  for (i in 1:length(disposition_list)) {
    if(!disposition_list[i] %in% unique(NoRecap_df$Disposition)) {
      NoRecap_df <- NoRecap_df %>%
        add_row(Disposition = disposition_list[i], `J`= 0, `F`= 0, `M`= 0, `Total [>630]`= 0, `Total [all]`= 0)
    } else {
      next
    }
  }
  
  NoRecap_df <- NoRecap_df[order(match(NoRecap_df$Disposition, c('Upstream Release', 'Brood Collection', 'Food Distribution', 
                                                        'Wallowa River Outplant', 'Recycled to Fishery', 'Mortality', 'Total'))),]
  
  
  join_df <- left_join(disp_df, NoRecap_df, by = 'Disposition') %>%
    mutate(`J` = paste(`J.x`, ' (', `J.y`, ')', sep=''),
           `F` = paste(`F.x`, ' (', `F.y`, ')', sep=''),
           `M` = paste(`M.x`, ' (', `M.y`, ')', sep=''),
           `Total [>630]` = paste(`Total [>630].x`, ' (', `Total [>630].y`, ')', sep=''),
           `Total [all]` = paste(`Total [all].x`, ' (', `Total [all].y`, ')', sep='')) %>%
    select(Disposition, J, `F`, M, `Total [>630]`, `Total [all]`)
  
  
  return(join_df)
}
