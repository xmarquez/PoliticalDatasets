#' Merge a dataset with country and date info with the Gleditsch-Ward system of states
#'
#' @param data
#' @param country_col
#' @param date_col
#'
#' @return a dataset merged with the Gleditsch-Ward system of states \link{gw_system}
#' @export
#'
#' @import lubridate, dplyr
#'
#' @examples
#' library(dplyr)
#' data <- bmr %>% select(bmr_country, bmr_ccode, year, democracy) %>% filter(grepl("YUG|SERBIA",bmr_country))
#' to_gw_system(data, "bmr_country", "year")
to_gw_system <-
  function(data, country_col = "country", date_col = "year", code_col = NULL, match_on = "overlap", include_extras = TRUE, match_condition = "date_matches == max(date_matches) & !duplicated(date_matches)") {
    if (include_extras) {
      col_names <-
        c(
          "country_name","GWn","GWc","region","continent","startdate","GW_enddate", "microstate","lat","lon"
        )
    } else {
      col_names <- c("country_name","GWn")
    }
    
    match_on <-
      c("12-31", "1-1", "overlaps","within")[pmatch(match_on, c("31-dec", "1-jan", "overlaps", "within"))]
    
    if (is.na(match_on))
      stop("No matching method available")
    stopifnot(length(date_col) <= 2, length(date_col) >= 1)
    
    dict <- PoliticalDatasets::data
    dict$GW_enddate <-
      plyr::mapvalues(dict$GW_enddate,from = NA, to = as.character(round_date(now(),unit = "day")))
    dict <- arrange(dict,GWn,country_name,desc(startdate))
    
    destination_data <- data.frame()
    
    data <- as.data.frame(data)
    
    if (length(date_col) == 2) {
      if ((is.Date(data[, date_col][1]) |
           is.POSIXt(data[, date_col[1]])) &
          (is.Date(data[, date_col][2]) |
           is.POSIXt(data[, date_col[2]]))) {
        startdate <- data[,date_col[1]]
        enddate <- data[,date_col[2]]
      } else {
        startdate <- ymd(data[,date_col[1]])
        enddate <- ymd(data[,date_col[2]])
      }
      
    } else if (is.Date(data[, date_col]) |
               is.POSIXt(data[, date_col])) {
      startdate <- data[,date_col]
      enddate <- data[,date_col]
      
    } else {
      if (match_on == "12-31" | match_on == "1-1") {
        start_match <- paste0("-",match_on)
        end_match <- paste0("-",match_on)
      } else if (match_on == "overlaps") {
        start_match <- "-1-1"
        end_match <- "-12-31"
      } else {
        start_match <- "-6-15"
        end_match <- "-6-15"
      }
      startdate <- ymd(paste0(data[,date_col],start_match))
      enddate <- ymd(paste0(data[,date_col],end_match))
    }
    
    for (i in 1:nrow(dict)) {
      country_matches <-
        grepl(dict$regex[i], data[, country_col], perl = TRUE,  ignore.case = TRUE)
      
      if (!is.null(code_col)) {
        code_matches <- data[, code_col] %in% dict$GWn[i]
      }
      
      if (any(country_matches)) {
        if (match_on == "overlaps") {
          date_matches <-
            int_overlaps(
              new_interval(startdate,enddate),
              new_interval(dict$startdate[i],dict$GW_enddate[i])
            )
        } else if (match_on %in% c("within","12-31","1-1")) {
          date_matches <- new_interval(startdate,enddate) %within%
            new_interval(dict$startdate[i],dict$GW_enddate[i])
        } else {
          stop("No match method")
        }
        

        temp <-
          merge(data[country_matches, c(country_col, date_col)], dict[i, c(col_names, "num_periods", "problem_history")], by = NULL)
        temp$country_matches <- country_matches[country_matches]
        temp$date_matches <- date_matches[country_matches]

        if (!is.null(code_col)) {
          temp$code_matches <- code_matches[country_matches]
        }
        
        destination_data <- rbind(temp, destination_data)
        
      }
      
    }
    
    destination_data <-
      destination_data %>% group_by_(country_col, .dots = date_col) %>% filter_(.dots = match_condition)
    
    data <- left_join(data,destination_data)
    
    data <-
      data %>% distinct() %>% group_by_(country_col, .dots = date_col) %>% mutate(num_matches = n(), num_matches = as.numeric(ifelse(
        is.na(country_name),0,as.numeric(num_matches)
      )))
    
    
    if (any(is.na(data$country_name))) {
      print("The following countries were not matched:")
      print(data %>% filter(is.na(country_name)) %>% select_(country_col, date_col))
    }
    if (any(data$num_matches > 1)) {
      print("The following countries were matched more than once:")
      print(
        data %>% filter(num_matches > 1) %>% select_(.dots = c(country_col, date_col, "country_name", "num_matches"))
      )
    }
    
    return(data)
  }


#' Utility function for counting sequence breaks
#'
#' @param seq
#' @param seq_step
#'
#' @return a vector of periods
#' @export
#'
count_sequence_breaks <- function(seq, seq_step = 1) {
  first_diff <- c(seq_step, diff(seq)) - seq_step
  periods <- cumsum(abs(first_diff))
  periods
}


#' Merging datsets by date intervals
#'
#' @param data1
#' @param data2
#' @param key_col
#' @param startdate1
#' @param enddate1
#' @param startdate2
#' @param enddate2
#' @param group_vars
#'
#' @return A dataset merging \code{data1} and \code{data2} by the \code{key_col}
#'   with each row of \code{data1} split according to which part of it falls
#'   within the intervals in \code{data2}
#' @export
#'
#' @examples
#' library(dplyr)
#' data1 <- archigos2014 %>% select(country_name,obsid,leader,startdate,enddate) %>% filter(country_name == "Cuba")
#' data2 <- polity_cases %>% select(country_name, polity, polity_startdate, polity_enddate) %>% filter(country_name == "Cuba")
#' results <- merge_by_date_interval(data1, data2, "country_name", "startdate", "enddate", "polity_startdate", "polity_enddate", "obsid")
#' results
merge_by_date_interval <-
  function(data1, data2, key_col = "country_name", startdate1, enddate1, startdate2, enddate2, group_vars = NULL) {
    data1 <-
      rename_(data1, startdate1 = startdate1, enddate1 = enddate1, key_col = key_col)
    data2 <-
      rename_(data2, startdate2 = startdate2, enddate2 = enddate2, key_col = key_col)
    
    has_common_names <-
      any(common <- names(data1) %in% names(data2))
    
    print(names(data1)[common])
    
    key_set <- group_by(data1,key_col)
    
    if (!is.null(group_vars)) {
      key_set <- group_by_(data1,group_vars, add = TRUE)
    }
    
    key_set <-
      do(key_set, data.frame(data2[data2$key_col == .$key_col &
                                     int_overlaps(
                                       new_interval(data2$startdate2,data2$enddate2),
                                       new_interval(.$startdate1,.$enddate1)
                                     ),]))
    
    merged_data <- left_join(data1, key_set)
    
    return(merged_data)
  }



#' Generate deep history picture
#'
#' @param country
#' @param polity
#' @param coups
#' @param economic_data
#' @param uds_data
#' @param interruptions
#' @param conflict_data
#' @param leader_names
#' @param independence
#' @param debug
deep_history <- function(country, polity = TRUE, coups = TRUE, economic_data = FALSE, uds_data = TRUE, interruptions = TRUE, conflict_data = TRUE, leader_names = TRUE, independence = TRUE, debug = TRUE, gwf = FALSE, rescale_range = c(-10,10), bottom_label = "Polity score", fill_label = "Type of armed conflict (UCDP/PRIO)") {
  
  if(debug) {
    message(paste("Starting",country))
  }
  
  p <- ggplot()
  
  
  # Basic Polity score
  if(polity) {
    data <- polity_cases %>% filter(country_name %in% country) %>% mutate(polity = ifelse(polity > -11,scales::rescale(polity, to = rescale_range),NA)) 
    
    data2 <- data %>% ungroup() %>% mutate(polity_startdate = polity_enddate)
    data <- rbind(data,data2) %>% arrange(country_name,polity_startdate)
    rm(data2)
    
    p <- p +  
      geom_path(data=data,aes(y=polity_startdate,x=polity))  
  }
  
  # Coups
  if(coups) {
    if(nrow(PowellThyne %>% filter(country_name %in% country)) > 0) {
      data <- PowellThyne %>% filter(country_name %in% country)
      data$x <- rescale_range[1]
      data$xend <- rescale_range[2]
      p <- p + geom_segment(data = data,aes(y=date,yend=date,x=x,xend=xend,linetype=reorder(attempt_type,-coup)),alpha=0.5, color = "red")   # Coup lines
    }
  }

  # Economic data
  if(economic_data) {
    if(nrow(economic.data %>% filter(country_name %in% country,!is.na(per_capita))) > 0) {
      data <- economic.data %>% filter(country_name %in% country)
      data$value_rescaled_log <- scales::rescale(data$per_capita,to= rescale_range) 
      data$date <- ymd(paste0(data$year,"-12-31"))
      
      p <- p + geom_path(data = data,aes(y=date,x=value_rescaled_log, color = primary_source, group = variable),alpha=0.5)
      
      # Dollar values at start and end of periods of leaders
      if(nrow(data %>% filter(year %in% year(archigos2014$enddate[ archigos2014$country_name %in% country ]))) > 0) {
        p <- p + geom_text(data = data %>% filter(year %in% year(archigos2014$enddate[ archigos2014$country_name %in% country ])),aes(y=date,x=value_rescaled_log + 2,label=paste0(primary_source,": ", dollar(per_capita))),alpha=0.4, angle = -90, size = 2, position = "jitter")
      }
      
      p <- p + geom_text(data = data %>% group_by(country_name) %>% filter(year == max(year) | year == year(gw_system$startdate[ gw_system$country %in% country])),aes(y=date,x=value_rescaled_log + 2,label=paste0(primary_source,": ", dollar(per_capita))),alpha=0.4, angle = -90, size = 2, position = "jitter")
    }
  }
  
  # UDS score
  if(uds_data) {
    if(nrow(uds %>% filter(country_name %in% country)) > 1) {
      uds$mean_rescaled <- rescale(uds$mean, to= rescale_range, from=c(min(uds$pct025),max(uds$pct975)))
      uds$pct025_rescaled <- rescale(uds$pct025, to= rescale_range, from=c(min(uds$pct025),max(uds$pct975)))
      uds$pct975_rescaled <- rescale(uds$pct975, to= rescale_range, from=c(min(uds$pct025),max(uds$pct975)))
      data <- uds %>% filter(country_name %in% country)
      data$date <- ymd(paste0(data$year,"-12-31"))
      
      p <- p + geom_path(data = data,aes(y=date,x=mean_rescaled),alpha=0.3)
      
      positions <- data.frame(x = c(data$pct025_rescaled,data$pct975_rescaled[length(data$pct975_rescaled):1]), y = c(data$date,data$date[length(data$date):1]))
      p <- p + geom_polygon(data = positions,aes(x=x,y=y),alpha=0.2,fill="grey")
    }
  }
  
  # Polity interregnums and interruptions 
  if(interruptions) {
    interruptions <- polity_cases %>% filter(polity < -10,!is.na(country_name))
    interruptions$xmin <- rescale_range[1]
    interruptions$xmax <- rescale_range[2]
    interruptions <- interruptions %>% filter(country_name %in% country)
    
    if(nrow(interruptions) > 0) {
      p <- p + geom_rect(data=interruptions,aes(ymin=polity_startdate,ymax=polity_enddate,xmin=xmin,xmax=xmax),alpha=0.2) 
    }
  }
  
  # Conflicts
  if(conflict_data) {
    if(nrow(ucdpConflict %>% filter(country_name %in% country)) > 0) {
      ucdpConflict <- ucdpConflict %>% filter(country_name %in% country)
      ucdpConflict$xmin <- rescale_range[1]
      ucdpConflict$xmax <- rescale_range[2]
      
      p <- p + geom_rect(data=ucdpConflict,aes(ymin=startdate,ymax=enddate,xmin=xmin,xmax=xmax),fill = "lightgrey", alpha=0.2, color = "grey") + # Wars
        geom_text(data=ucdpConflict,aes(y=int_start(int_shift(interval(startdate,enddate),by=duration(int_length(interval(startdate,enddate))/2))),x=(xmin+xmax)/2,label=paste("Conflict: ", SideA,"vs",SideB, ", ",TypeOfConflict),size=IntensityLevel)) + scale_size_discrete(range=c(2,3))
    }
  }
  
  # GWF regime type
  if(gwf) {
    if(nrow(all_gwf_periods %>% filter(country_name %in% country)) > 0) {
      all_gwf_periods <- all_gwf_periods %>% filter(country_name %in% country)
      all_gwf_periods$xmin <- rescale_range[1]
      all_gwf_periods$xmax <- rescale_range[2]

      p <- p + geom_rect(data=all_gwf_periods,aes(ymin=gwf_startdate,ymax=gwf_enddate,xmin=xmin,xmax=xmax,fill=gwf_full_regimetype),alpha=0.2) + # Regime type
        geom_text(data=all_gwf_periods,aes(y=int_start(int_shift(interval(gwf_startdate,gwf_enddate),by=duration(int_length(interval(gwf_startdate,gwf_enddate))/2))),x=(xmin+xmax)/2,label=gwf_full_regimetype), size = 2.5) 
    }
  }

  # Leader names and exit types
  if(leader_names) {
    if(nrow(archigos2014 %>% filter(country_name %in% country)) > 0) {
      archigos2014 <- archigos2014 %>% filter(country_name %in% country)
      archigos2014$x <- rescale_range[1]
      archigos2014$xend <- rescale_range[2]
      
      p <- p + geom_segment(data = archigos2014,aes(y=enddate,yend=enddate,x=x,xend=xend),linetype = 3,alpha=0.5, color = "blue") #Exit lines
      p <- p + geom_text(data = archigos2014,aes(y=enddate,x=(x+xend)/2,label=paste0(leader," (",exit, ", ",exitcode,")")),position=position_dodge(width=1), size = 2.5) 
    }
  }
  
  # System of states data: entry and exit from state system
  if(independence) {
    country1 <- country
    data <- gw_system %>% filter(country %in% country1)
    data$x <- rescale_range[1]
    data$xend <- rescale_range[2]
    
    p <- p + geom_segment(data=data,aes(y=startdate,yend=startdate,x=x,xend=xend),color="green",linetype=4,size=2,alpha=0.2) +
      geom_text(data=data,aes(x=(x+xend)/2,y=startdate,label="Entry into state system/Independence"),alpha=0.2,color="lightblue") + 
      geom_segment(data=data,aes(y=enddate,yend=enddate,x=x,xend=xend),color="red",linetype=4,size=2,alpha=0.2) +
      geom_text(data=data,aes(x=(x+xend)/2,y=enddate,label="End of independence/exit from state system"),alpha=0.2,color="lightblue")
    }

  # final plotting
  country1 <- country
  data <- gw_system %>% filter(country %in% country1)
  p <- p + labs(y="Year",x=bottom_label,alpha="Mode of leader exit",size="Conflict intensity",fill=fill_label,color="Source for GDP per capita",linetype="Coup?") +
    facet_wrap(~country_name) +
    coord_cartesian(ylim=c(data$startdate + years(5),ymd(20150101))) +
    theme_bw() +
    guides(fill=guide_legend(title.position="top",ncol=2),color=guide_legend(title.position="top",ncol=2),alpha=guide_legend(title.position="top",ncol=2),size=guide_legend(title.position="top",ncol=2))+
    scale_y_datetime(breaks=c(round_date(archigos2014$enddate[ archigos2014$country_name %in% country ],"year"),round_date(polity_cases$enddate[ polity_cases$country_name %in% country ],"year")), labels=date_format("%Y")) +
    theme(legend.position="bottom")
  p
}
