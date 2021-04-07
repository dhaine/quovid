## Heat map taux de positivité classe d'âge par semaine
assign_week <- function(date, origin) {
    (as.numeric(ymd(date) - ymd(origin)) %/% 7) + 1
}
age_sum <- age_l %>%
    filter(date >= ymd("2020-07-19") & date < ymd("2021-04-04")) %>%
    mutate(week = assign_week(date, origin = "2020-07-19")) %>%
    group_by(week, age) %>%
    summarise(nbr_wk = sum(nbr),
              start = min(date))
age_sum <- left_join(age_sum, pop_agel, by = "age")
age_sum$incidence <- (100000*age_sum$nbr_wk)/age_sum$pop

cases_heat <- age_sum %>%
    ggplot(aes(as.factor(start), age, fill = incidence)) +
    geom_tile(colour = "white", show.legend = TRUE) +
    nord::scale_fill_nord(palette = "aurora", discrete = FALSE, reverse = TRUE) +
    coord_equal() +
    scale_y_discrete(name = "", expand = c(0, 0)) +
#    scale_x_date(labels = date_format("%d%h")) +
    theme(axis.line.y = element_blank(),
          plot.subtitle = element_text(size = rel(0.78)),
          plot.title.position = "plot",
          axis.text.y = element_text(colour = "Black"),
          plot.title = element_text(size = rel(2.3))) +
    labs(title = "Évolution des taux d'incidence au Québec",
         subtitle = "Par classe d'âge et semaine CDC",
         caption = "**Nombre de cas positifs/100&nbsp;000 personnes par semaine**<br>Denis Haine; source: INSPQ",
         x = "**Semaine débutant le:**") +
    theme_bw() +
    theme(plot.title = element_markdown(face = 'bold', size = 9),
          plot.title.position = 'plot',
          plot.subtitle = element_text(size = 8),
          plot.caption = element_markdown(margin = margin(t = 15),
                                          size = 5),
          plot.caption.position = 'plot',
          axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
          axis.title.x = element_markdown(size = 8),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 6),
          legend.title = element_blank(),
          legend.text = element_text(size = 6, face = "bold"))
ggsave(plot = cases_heat,
       filename = "cases_heat.png",
       path = "./static/homepage",
       dev = ragg::agg_png(),
       width = 8, height = 4, units = "in")
