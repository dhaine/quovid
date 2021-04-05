## Cases
## line plot
date_report <- format(max(inspq$Date), '%d%h%Y')
cases_line <- inspq %>%
    ggplot(aes(Date, Nb_Nvx_Cas)) +
    geom_col(
        colour = paletteer_d("nord::polarnight")[4]
    ) +
    geom_line(aes(Date, rolling_avg, group = 1, lty = "Moyenne mobile 7 jours"),
              colour = paletteer_d("nord::frost")[4], size = 1) +
    scale_x_date(date_breaks = "months",
                 labels = date_format("%b")) +
    labs(title = "Cas confirmés au Québec",
         subtitle = paste(date_report, ":", round(tail(inspq$rolling_avg, 1), 0),
                          "cas, moyenne mobile 7 jours"),
         caption = "Denis Haine; source: INSPQ",
         x = "**Date de déclaration du cas**") +
    theme_bw() +
    theme(plot.title = element_markdown(face = 'bold', size = 9),
          plot.title.position = 'plot',
          plot.subtitle = element_text(size = 8),
          plot.caption = element_markdown(margin = margin(t = 15),
                                          size = 5),
          plot.caption.position = 'plot',
          axis.text.x = element_text(size = 7),
          axis.title.x = element_markdown(size = 8),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.25, .75),
          legend.box.background = element_rect(fill = "transparent",
                                               colour = "transparent"))
ggsave(plot = cases_line,
       filename = "cases_line.png",
       path = "./static/homepage",
       dev = ragg::agg_png(),
       width = 6.472, height = 4, units = "in")

## 7-days line plot
cases7_line <- inspq %>%
    filter(Date >= (max(Date) - ddays(42))) %>%
    ggplot(aes(Date, Nb_Nvx_Cas)) +
    geom_col(colour = paletteer_d("nord::polarnight")[4]) +
    geom_line(aes(Date, rolling_avg, group = 1, lty = "Moyenne mobile 7 jours"),
              colour = paletteer_d("nord::frost")[4], size = 1) +
    scale_x_date(date_breaks = "weeks",
                 labels = date_format("%d%h")) +
    labs(title = "Cas confirmés au Québec, <i style='color:#5E81ACFF;'>6 dernières semaines</i>",
         subtitle = paste(date_report, ":", round(tail(inspq$rolling_avg, 1), 0),
                          "cas, moyenne mobile 7 jours"),
         caption = "Denis Haine, source: INSPQ",
         x = "**Date de déclaration du cas**") +
    theme_bw() +
    theme(plot.title = element_markdown(face = 'bold', size = 9),
          plot.title.position = 'plot',
          plot.subtitle = element_text(size = 8),
          plot.caption = element_markdown(margin = margin(t = 15),
                                          size = 5),
          plot.caption.position = 'plot',
          axis.text.x = element_text(size = 7),
          axis.title.x = element_markdown(size = 8),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.25, .8),
          legend.box.background = element_rect(fill = "transparent",
                                               colour = "transparent"))
ggsave(plot = cases7_line,
       filename = "cases7_line.png",
       width = 6.472, height = 4, units = "in",
       path = "./static/homepage",
       dev = ragg::agg_png())

##--------------------------------------------------------------------------
## Cumulatif
## line plot
timeline <- tibble(Date = c("2020-02-27",
#                            "2020-02-29",
                            "2020-03-13",
#                            "2020-03-18",
                            "2020-03-21",
                            "2020-03-22",
                            "2020-04-20",
#                            "2020-05-04",
#                            "2020-05-11",
#                            "2020-05-25",
#                            "2020-06-01",
#                            "2020-06-15",
                            "2020-06-08",
                            "2020-06-25",
                            "2020-07-18",
#                            "2020-07-19",
#                            "2020-08-01",
                            "2020-09-02",
                            "2020-09-08",
                            "2020-10-01",
                            "2020-12-14",
                            "2020-12-25",
                            "2020-12-29",
                            "2021-01-09",
                            "2021-01-11",
#                            "2021-02-02"
                            "2021-02-06",
                            "2021-02-09",
                            "2021-02-22",
#                            "2021-03-01",
                            "2021-03-06",
#                            "2021-03-22",
                            "2021-03-23"#,
#                            "2021-03-26"
                            ),
                   text = c("Premier cas déclaré.",
#                            "Début de la semaine de relâche.",
                            "Urgence sanitaire, fermeture écoles et garderies.",
#                            "Fermeture des frontières avec les États-Unis.",
                            "Premier décès.",
                            "Premier confinement.",
                            "Arrivée des militaires dans les CHSLD.",
 #                           "Ouverture des magasins hors Montréal.",
 #                           "Ouverture des garderies et écoles primaires hors Montréal.",
 #                           "Ouverture des magasins à Montréal.",
 #                           "Ouverture des garderies à Montréal, des centres d'achat et services de soins personnels hors Montréal.",
 #                           "Ouverture des restaurants hors Montréal.",
                            "5 000 décès.",
                            "Fin du premier confinement.",
                            "Port du masque obligatoire dans les lieux publics intérieurs.",
#                            "Début des vacances de la construction.",
#                            "Fin des vacances de la construction.",
                            "Début de la deuxième vague.",
                            "Mise en place du système régional de paliers d'alerte.",
                            "Début du défi 28 jours.",
                            "Début de la campagne de vaccination.",
                            "Début du deuxième confinement.",
                            "Détection du variant B.1.1.7.",
                            "Instauration d'un couvre-feu.",
                            "Fin du deuxième confinement.",
#                            "Couvre-feu étendu jusqu'au 22 février"
                            "10 000 décès.",
                            "Détection du variant B.1.351.",
                            "Quarantaine pour les voyageurs arrivant par voie aérienne.",
#                            "Début de la vaccination dans la population générale.",
                            "Détection du variant P.1.",
#                            "Détection du variant B.1.525.",
                            "Un million de personnes vaccinées."#,
#                            "Début de la troisième vague."
                            )#,
#                   speaker = c("Wikipedia",
#                               "Wikipedia",
#                               "Wikipedia",
#                               "Wikipedia",
#                               "Wikipedia",
#                               "Wikipedia")
                   ) %>%
    mutate(Date = lubridate::date(Date))

labels <- inner_join(inspq, timeline) %>%
    mutate(date_fr = format(as.Date(Date),'%d-%m-%Y'))

timeline <- inspq %>%
    ggplot(aes(Date, cumsum)) +
    geom_path(colour = paletteer_d("nord::frost")[4], size = 1) +
    geom_point(data = labels,
               fill = paletteer_d("nord::frost")[1],
               color = "white",
               size = 2,
               shape = 21) +
    geom_mark_circle(data = labels,
                     aes(description = glue('{text}'),
                         label = glue("{date_fr}:"),
                         group = Date),
                     expand = unit(2, "mm"),
                     label.buffer = unit(8, "mm"),
                     con.size = 0.2,
                     colour = paletteer_d("nord::polarnight")[1]) +
    geom_text(data = slice(labels, 1),
              aes(x = ymd("20200301"), y = 300000,
                  label = "Ligne du temps COVID-19 au Québec"),
              hjust = 0, size = 10, vjust = 0, fontface = "bold") +
    labs(x = NULL,
         y = "**Nombre de cas**",
         caption = "Denis Haine; source: INSPQ") +
    scale_x_date(#limits = parse_date(c("2020-01-31", "2021-03-01")),
        date_breaks = "1 month",
        labels = date_format("%B")) +
    scale_y_continuous(labels = label_number()) +
    theme_bw() +
    theme(plot.caption = element_markdown(margin = margin(t = 15),
                                          size = 5),
          plot.caption.position = 'plot',
          axis.text.x = element_text(size = 12),
          axis.title.y = element_markdown(size = 14),
          axis.text.y = element_text(size = 12))
ggsave(plot = timeline,
       filename = "timeline.png",
       path = "./static/homepage",
       dev = ragg::agg_png(),
       width = 26,
       height = 16)

##--------------------------------------------------------------------------
## Cases and death
ylim.prim <- c(0, 3000)
ylim.sec <- c(0, 320000)
b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]
cases_cum <- inspq %>%
    ggplot(aes(Date, Nb_Nvx_Cas)) +
    geom_col(colour = paletteer_d("nord::polarnight")[4]) +
    geom_line(aes(y = a + cumsum*b), colour = paletteer_d("nord::aurora")[1]) +
    scale_x_date(date_breaks = "months",
                 labels = date_format("%b")) +
    scale_y_continuous(name = "**Nombre quotidien de cas**",
                       sec.axis = sec_axis(~ (. - a)/b,
                                           name = "**Nombre cumulatif de cas**",
                                           labels = label_number()),
                       labels = label_number()
    ) +
    labs(x = NULL) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 7),
          axis.title.x = element_markdown(size = 8),
          axis.title.y = element_markdown(size = 8),
          axis.title.y.right = element_markdown(size = 8,
                                                colour = paletteer_d("nord::aurora")[1]),
          axis.text.y = element_text(size = 7),
          axis.line.y.right = element_line(colour = paletteer_d("nord::aurora")[1]),
          axis.ticks.y.right = element_line(colour = paletteer_d("nord::aurora")[1]),
          axis.text.y.right = element_text(colour = paletteer_d("nord::aurora")[1]))
ylim.prim <- c(0, 160)
ylim.sec <- c(0, 11000)
b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]
death_cum <- inspq %>%
    ggplot(aes(Date, Nb_Nvx_Deces_Total)) +
    geom_col(colour = paletteer_d("nord::polarnight")[4]) +
    geom_line(aes(y = a + death_cumsum*b), colour = paletteer_d("nord::aurora")[1]) +
    scale_x_date(date_breaks = "months",
                 labels = date_format("%b")) +
    scale_y_continuous(name = "**Nombre quotidien de décès**",
                       sec.axis = sec_axis(~ (. - a)/b,
                                           name = "**Nombre cumulatif de décès**",
                                           labels = label_number()),
                       labels = label_number()
    ) +
    labs(x = NULL,
         caption = "Denis Haine; source: INSPQ.") +
    theme_bw() +
    theme(plot.caption = element_markdown(margin = margin(t = 15),
                                          size = 5),
          plot.caption.position = 'plot',
          axis.text.x = element_text(size = 7),
          axis.title.x = element_markdown(size = 8),
          axis.title.y = element_markdown(size = 8),
          axis.title.y.right = element_markdown(size = 8,
                                                colour = paletteer_d("nord::aurora")[1]),
          axis.text.y = element_text(size = 7),
          axis.line.y.right = element_line(colour = paletteer_d("nord::aurora")[1]),
          axis.ticks.y.right = element_line(colour = paletteer_d("nord::aurora")[1]),
          axis.text.y.right = element_text(colour = paletteer_d("nord::aurora")[1]))
case_death_plot <- cases_cum / death_cum
ggsave(plot = case_death_plot,
       filename = "case_death.png",
       path = "./static/homepage",
       dev = ragg::agg_png(),
       width = 6.472,
       height = 8)

##---------------------------------------------------------------------------
## Incidence
inc <- opencovid %>%
    group_by(health_region) %>%
    summarize(roll_week = zoo::rollapplyr(cases, 7, sum),
              start = rollapply(date_report, 7, min),
              end = rollapply(date_report, 7, max))
inc <- left_join(inc,
                 pop %>% filter(Sexe == "Total") %>% select(Territoire, Tous.les.âges),
                 by = c("health_region" = "Territoire")) %>%
    rename(pop = Tous.les.âges) %>%
    mutate(incidence = (100000*roll_week)/pop,
           start = ymd(start),
           end = ymd(end))
date_report <- format(max(inc$end), '%d%h%Y')
incidence <- inc %>% filter(start > (max(end) - ddays(13))) %>%
    ggplot(aes(incidence, health_region, colour = end)) +
    geom_point(aes(alpha = end)) +
    scale_colour_gradientn(colours = c(paletteer_d("nord::snowstorm")[4],
                                       paletteer_d("nord::frost")[2:4],
                                       paletteer_d("nord::aurora")[1])) +
    scale_alpha(range = c(0.3, 1)) +
    scale_y_discrete(limits = rev) +
    labs(title = "Évolution du taux d'incidence au Québec",
         subtitle = date_report,
         caption = "**Le point rouge est l'incidence du jour (semaine glissante); les points bleus les incidences des derniers jours. Plus le point bleu est foncé plus il est proche de la date du jour (rouge).**<br>Denis Haine; source: opencovid.ca, ISQ, MSSS.",
         x = "**Taux d'incidence** (/100&nbsp;000 hab.)") +
    theme_bw() +
    theme(plot.title = element_markdown(face = 'bold', size = 9),
          plot.title.position = 'plot',
          plot.subtitle = element_text(size = 8),
          plot.caption = element_markdown(margin = margin(t = 15),
                                          size = 5),
          plot.caption.position = 'plot',
          axis.text.x = element_text(size = 7),
          axis.title.x = element_markdown(size = 8),
          axis.title.y = element_blank(),
          legend.position = "none")
ggsave(plot = incidence,
       filename = "incidence.png",
       path = "./static/homepage",
       dev = ragg::agg_png(),
       width = 6.472,
       height = 6.472, units = "in")

inc_anim <- inc %>% filter(start > (max(end) - ddays(13))) %>%
    ggplot(aes(incidence, health_region, colour = end)) +
    geom_point(aes(alpha = end)) +
    scale_colour_gradientn(colours = c(paletteer_d("nord::snowstorm")[4],
                                       paletteer_d("nord::frost")[2:4],
                                       paletteer_d("nord::aurora")[1])) +
    scale_alpha(range = c(0.3, 1)) +
    scale_y_discrete(limits = rev) +
    labs(title = "Évolution du taux d'incidence au Québec",
         subtitle = '{frame_time}',
         caption = "**Le point rouge est l'incidence du jour (semaine glissante); les points bleus les incidences des derniers jours. Plus le point bleu est foncé plus il est proche de la date du jour (rouge).**<br>Denis Haine; source: opencovid.ca, ISQ, MSSS.",
         x = "**Taux d'incidence** (/100&nbsp;000 hab.)") +
    theme_bw() +
    theme(plot.title = element_markdown(face = 'bold', size = 9),
          plot.title.position = 'plot',
          plot.subtitle = element_text(size = 8),
          plot.caption = element_markdown(margin = margin(t = 15),
                                          size = 5),
          plot.caption.position = 'plot',
          axis.text.x = element_text(size = 7),
          axis.title.x = element_markdown(size = 8),
          axis.title.y = element_blank(),
          legend.position = "none") +
#    geom_text(aes(x = 100, y = 1,
#                  label = as.factor(end)),
#              hjust = -2, vjust = -0.2, alpha = 0.2, col = "gray", size = 20) +
    transition_time(end)
#inc_anim + shadow_trail(0.02)
inc_trail <- inc_anim + shadow_trail(alpha = 0.3, max_frames = 3)
anim_save("incidence_trail.gif",
          inc_trail,
          path = "./static/homepage",
          width = 6.472, height = 6.472, units = "in", res = 150)
