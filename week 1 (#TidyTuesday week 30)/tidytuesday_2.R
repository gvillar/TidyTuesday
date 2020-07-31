#Tidy Tuesday week 30 (week 1 for me)
trace(grDevices::png, exit = quote({
  showtext::showtext_begin()
}), print = FALSE)

library("tidytuesdayR")
library("tidyverse")
library("ggtext")
library("showtext")
library("ggimage")

tuesdata <- tt_load(2020, week = 31)
#animal_outcomes, animal_complaints, brisbane_complaints
penguins <- tuesdata$penguins

## refining data
normalize_penguins <- function(x) {
  normalized <- normalize(x)
  return(normalized)
}

registered_penguins <- penguins %>% filter(sex == c("female","male"))
registered_penguins[3:6] <- apply(registered_penguins[3:6], 2, normalize_penguins)
final_penguins <- registered_penguins[1:7]
final_penguins$image <- "https://github.com/gvillar/TidyTuesday/blob/master/week%202%20(%23TidyTuesday%20week%2031)/penguin_footprint.png?raw=true"
ggplot(final_penguins, aes(x = bill_length_mm, y = sex)) +
  geom_jitter(aes(x = bill_length_mm - 0.1, y = sex), size = 0.01, color = "#cacaca", alpha = 0.1) +
  geom_jitter(aes(x = bill_length_mm - 0.2, y = sex), size = 0.01, color = "#cacaca", alpha = 0.2) +
  geom_jitter(aes(x = bill_length_mm - 0.3, y = sex), size = 0.01, color = "#cacaca", alpha = 0.3) +
  geom_jitter(aes(x = bill_length_mm - 0.4, y = sex), size = 0.01, color = "#cacaca", alpha = 0.4) +
  geom_jitter(aes(x = bill_length_mm - 0.5, y = sex), size = 0.01, color = "#cacaca", alpha = 0.5) +
  geom_jitter(aes(x = bill_length_mm - 0.6, y = sex), size = 0.01, color = "#cacaca", alpha = 0.6) +
  geom_jitter(aes(x = bill_length_mm - 0.7, y = sex), size = 0.01, color = "#cacaca", alpha = 0.7) +
  geom_jitter(aes(x = bill_length_mm - 0.8, y = sex), size = 0.01, color = "#cacaca", alpha = 0.8) +
  geom_jitter(aes(x = bill_length_mm - 0.9, y = sex), size = 0.01, color = "#cacaca", alpha = 0.9) +
  geom_image(aes(image = image), position = "jitter", size = 0.03)



#df_complaints <- as.data.frame(complaints)
#colnames(df_complaints) <- c("animal","complaint","date","suburb","division")
#grouped_complaints <- df_complaints %>% separate(date, c("month","year"), " ") %>% filter(animal == "dog", month == c("February","March","April","May","June")) %>% group_by(month, year, complaint) %>% summarize(n())
#colnames(grouped_complaints) <- c("month","year","complaint","number")
#grouped_complaints$month <- factor(grouped_complaints$month, levels = c("February","March","April","May","June"))
#levels(grouped_complaints$month)<-c("Feb","Mar","Apr","May","Jun")

## adding fonts
# https://fonts.google.com/specimen/Alfa+Slab+One
# https://fonts.google.com/specimen/Lato 
font_add_google("Alfa Slab One", "alfaslab")
font_add_google("Lato", "lato")

## desktop
desktop <- ggplot(grouped_complaints, aes(x = month, y = number, group = year, color = year)) +
  facet_wrap(~ complaint, scales="free", ncol = 3) +
  geom_line() +

  
  

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


