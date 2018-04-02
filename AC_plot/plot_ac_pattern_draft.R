library(ggplot2)
library(ggvis)
library(plotly)
library(dplyr)

patinfo <- readRDS("patinfo.rds")
patinfo$id <- 1:nrow(patinfo)
patinfo <- patinfo %>%  mutate(fst_sup = as.Date(as.Date(fst_dt) + Days_Sup))

try.dat <- patinfo %>% filter(patid %in% c('802666500107193','802666500167640','802666500209463'))

  mutate(fst_dt = as.POSIXct(fst_dt), index_dt=as.POSIXct(index_dt), index_cancer_dt=as.POSIXct(index_cancer_dt), 
         category = factor(category), patid = factor(patid), fst_sup=as.POSIXct(fst_sup))
time_vars <- c("fst_dt", "index_dt", "index_cancer_dt", "fst_sup")
try.dat[,time_vars] <- lapply(try.dat[,time_vars],as.POSIXct)
try.dat$id <- 1:nrow(try.dat)

levels(as.factor(try.dat$category))

dotchart(as.numeric(try.dat$fst_dt), labels = try.dat$category) 

### GGPLOT
p <- try.dat %>% 
  ggplot(aes(fst_dt, category, group=patid)) +
  # geom_vline(aes(xintercept = index_dt, color=patid, group=patid),linetype=4) +
  # geom_line(aes(color=patid, group=patid)) +
  geom_point(aes(color=patid, size=Copay, group=patid)) +
  geom_segment(aes(x=fst_dt, xend=fst_sup, y=category, yend=category, colour=patid)) +
  scale_x_datetime(limits=c(min(try.dat$fst_dt,try.dat$index_dt),
                            max=max(try.dat$fst_dt,try.dat$index_dt)),
                   date_breaks = "1 month") +
  labs(x = "Fill Date", y = "Anti-coagulant Category", color = "Patient ID") 
ggplotly(p,tooltip = c("fst_dt", "Copay", "index_dt","index_cancer_dt"))






p <- try.dat %>%
  ggplot(aes(x=fst_dt, y=category_num, group=patid,
             text=paste(" Patient ID:", patid, "<br>",
                        "Fill Date: ", fst_dt, "<br>", 
                        "Copay: ", Copay_sum, "<br>", 
                        "Day_Sup: ", Days_Sup, "<br>",
                        "Index Cancer Date: ", index_cancer_dt, "<br>",
                        "Index VTE Date: ", index_dt))) +
  geom_vline(aes(xintercept = as.numeric(index_dt))) +
  geom_segment(aes(x=fst_dt, xend=fst_sup, y=id, yend=id, 
                   colour=factor(category_num))) + #, linetype = patid
  scale_y_continuous(breaks=c(10,24,38),labels = c("Warfarin", "LMWH", "DOACS")) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  # geom_line(data=selected(),aes(color=patid, group=patid)) +
  geom_point(aes(x=fst_dt, y=id, color=factor(category_num), shape=patid)) +
  scale_x_date(limits=c(min(try.dat$index_dt), max=max(try.dat$fst_sup))) +
  labs(x = "Fill Date", y = "Anti-coagulant Category", color = "Patient ID") +
  scale_linetype(name = "Vline ltype") +
  facet_wrap(~patid, ncol = 1) +
  theme(axis.ticks.y=element_blank())#,axis.text.y=element_blank(),aspect.ratio = 2
# theme(legend.position = 'none')
ggplotly(p,tooltip = c("fst_dt", "Copay_sum", "index_dt","index_cancer_dt"))




### GGVIS
try.dat %>% 
  ggvis(x=~fst_dt, y=~category, stroke=~patid) %>%
  layer_points(size=~Copay, fill=~patid) %>%
  layer_lines(stroke=~patid) %>%
  # add_tooltip(tooltip_info, 'hover') %>%
  add_axis("x", title = "Fill Date") %>%
  add_axis("y", title = "Anti-coagulant Category") %>%
  hide_legend('size') %>% hide_legend('fill') %>%
  add_legend('stroke', title='Patient ID', properties = legend_props(legend = list(y = 100)))
  


# CREATE THE TOOLTIP FUNCTION THAT DISPLAYS ADDITIONAL PATIENT INFORMATION ON DOTS IN THE PLOT
tooltip_info <- function(x){
  if (is.null(x)) return()
  if (is.null(x$id)) return()
  
  dat <- try.dat
  paste0('Fill Date: ', dat$fst_dt[dat$id==x$id], "<br>",
         'Copay: ', dat$Copay[dat$id==x$id], "<br>",
         'Index Cancer Date: ', dat$index_cancer_dt[dat$id==x$id], "<br>",
         'Index VTE Date: ', dat$index_dt[dat$id==x$id])
}


try.dat %>% 
  ggvis(~fst_dt, ~category, stroke = ~patid) %>%
  layer_points(fill = ~patid, size := ~Copay, key := ~id) %>%
  layer_lines(stroke=~patid) %>%
  add_tooltip(tooltip_info, 'hover') %>%
  add_axis("x", title = "Fill Date") %>%
  add_axis("y", title = "Anti-coagulant Category") %>%
  # hide_legend('size') %>% hide_legend('fill') %>%
  add_legend('stroke', title='Patient ID', properties = legend_props(legend = list(y = 100))) %>%
  set_options(width = 800, height = 600) %>%
  scale_datetime('x', nice = "month")












######################### CREATE WORKING DATA SET FOR THE SHINY APP ##############################
patinfo <- readRDS("patinfo_rm_lmwh.rds")
### CREATE NUMERIC SCALE ON THE Y AXIS FOR AC CATEGORIES
patinfo <- patinfo %>%
  group_by(patid, category) %>%
  mutate(id = (category=="DOACS")*seq(from=45, by=-0.3, length.out=n()) +
           (category=="LMWH")*seq(from=30, by=-0.3, length.out=n()) +
           (category=="Warfarin")*seq(from=15, by=-0.3, length.out=n()),
         category_num = 36*(category=="DOACS") + 24*(category=="LMWH") + 8*(category=="Warfarin")) %>%
  ungroup(category)
# SAVE THE DATA SET
saveRDS(patinfo, "ac_pattern3\\working_data.rds")

### CREATE RDS FILE FOR INR DATA
inr_info <- read.csv("lab_inr.csv")
inr_info <- inr_info %>% mutate(patid = as.character(patid), 
         index_dt = as.Date(strptime(index_dt, "%m/%d/%Y")),
         inr_dt = as.Date(strptime(inr_dt, "%m/%d/%Y")))
# SAVE THE DATA SET
saveRDS(inr_info, "inr_info.rds")








### CHECKING ARROW IN GEOM_SEGMENT
selected <- patinfo[patinfo$patid=="802666500171929",]
selected_inr <- inr_info[patinfo$patid=="802666500171929",]
ggplot(selected, aes(x=fst_dt, y=category_num, group=patid)) +
    geom_point(data=selected, aes(x=fst_dt, y=id, colour=factor(category), size=NULL)) +
    geom_segment(data=selected, aes(x=fst_dt, xend=fst_sup, y=id, yend=id, 
                     colour=factor(category)), size=0.5, show.legend=F) +
    geom_point(aes(x=selected_inr$inr_dt, y=selected_inr$inr_num), shape=3, inherit.aes = FALSE)
    # geom_segment(data = selected_inr,
    #              aes(x=inr_dt, xend=inr_dt, y=inr_num_start, yend=inr_num,
    #                  colour=factor(category)),
    #              arrow = grid::arrow(length = unit(0.25, "cm")),
    #              inherit.aes = FALSE)
  


#### test

    