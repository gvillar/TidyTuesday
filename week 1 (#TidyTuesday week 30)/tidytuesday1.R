#Tidy Tuesday week 30 (week 1 for me)
trace(grDevices::png, exit = quote({
  showtext::showtext_begin()
}), print = FALSE)

library("tidytuesdayR")
library("tidyverse")
library("ggtext")
library("showtext")

tuesdata <- tt_load(2020, week = 30)
#animal_outcomes, animal_complaints, brisbane_complaints
complaints <- tuesdata$animal_complaints

## refining data
df_complaints <- as.data.frame(complaints)
colnames(df_complaints) <- c("animal","complaint","date","suburb","division")
grouped_complaints <- df_complaints %>% separate(date, c("month","year"), " ") %>% filter(animal == "dog", month == c("February","March","April","May","June")) %>% group_by(month, year, complaint) %>% summarize(n())
colnames(grouped_complaints) <- c("month","year","complaint","number")
grouped_complaints$month <- factor(grouped_complaints$month, levels = c("February","March","April","May","June"))
levels(grouped_complaints$month)<-c("Feb","Mar","Apr","May","Jun")

## adding fonts
# https://fonts.google.com/specimen/Alfa+Slab+One
# https://fonts.google.com/specimen/Lato 
font_add_google("Alfa Slab One", "alfaslab")
font_add_google("Lato", "lato")

## desktop
desktop <- ggplot(grouped_complaints, aes(x = month, y = number, group = year, color = year)) +
  facet_wrap(~ complaint, scales="free", ncol = 3) +
  geom_line() +
  labs(title = "Complaints about dogs per month",
       subtitle =  "February-June. <span style='color-background:#751815;'>2020</span> vs <span style='color:#cc9f9f;'>previous years</span>",
       x = "", y = "", col = "",
       caption = "Data: RSPCA | Viz: Guillermo Villar *@_gvillar*    ***#TidyTuesday***") +
  theme(rect = element_rect(fill = "#FEFEFE",
                            color = "#6E6E6E"),
        text = element_text(family = "lato",
                            color = "#6E6E6E"),
        ## axis
        axis.text = element_text(color = "#6E6E6E"),
        axis.ticks = element_line(color = "#6E6E6E"),
        panel.background = element_rect(fill = "#FEFEFE"),
        panel.grid = element_line(size = 0.1, color = "#D3D3D3"),
        ## plot
        plot.title = element_text(size = 16.5, family = "alfaslab"),
        plot.subtitle = element_markdown(size = 14, margin=margin(0,0,30,0)),
        plot.caption = element_markdown(size = 7.5, margin=margin(20,0,0,0), hjust = 1.05),
        plot.margin=unit(c(20,30,20,20),"pt"),
        ## panel
        # to only keep the necessary lines of the grid
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(3, "lines"),
        ## (no) legend
        legend.position = "none",
        ## strip
        strip.background = element_rect(fill = NA),
        strip.text.x = element_text(color = "#6E6E6E",
                                    face = "bold",
                                    size = 10)) +
  #including lines for x and y axes in every facet
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
  #solving margins inside grid
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(limits = c(0,50)) +
  #personalizing colors
  scale_color_manual(values= c("#cc9f9f","#cc9f9f","#cc9f9f","#cc9f9f","#cc9f9f","#cc9f9f",'#751815'))

#saving plot for desktop
ggsave("desktop_week30_guillermo_villar.png", width = 6.7733, height = 5, dpi = 300)

## mobile
mobile <- desktop + facet_wrap(~ complaint, scales="free", ncol = 2) +
  labs(title = "Complaints about dogs\nper month",
       subtitle =  "February-June. <span style='color-background:#751815;'>2020</span> vs <span style='color:#cc9f9f;'>previous years</span>") + 
  theme(plot.margin = unit(c(20,20,5,5),"pt"),
        panel.spacing = unit(1.7, "lines"),
        plot.subtitle = element_markdown(size = 12.5),
        plot.caption = element_markdown(hjust = 1.2))
  
  

#saving plot for mobile
ggsave("mobile_week30_guillermo_villar.png", width = 3.81, height = 6.7733, dpi = 300)


