f.plot.rotation <- function(df.rotation
)
{

    df.rotation <- rothc.calendar8 %>%
        dplyr::filter(rotation.cycle == 1) %>%
        mutate(Date = stringr::str_c(year, month,"01",sep="-")) %>%
        mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
        mutate(crop.id = as.factor(crop.id))

    label_year <- df.rotation %>%
        group_by(year) %>%
        summarise(
            mean_date = as.Date(mean(as.numeric(Date))),  # mean of dates
            .groups = "drop"
        )

    label_eom <- df.rotation %>%
        dplyr::filter(eom_app_q  != 0) %>%
        group_by(crop.id, year, eom_app_id,Catégorie,`Matière organique exogène`) %>%
        summarise(
            mean_date = as.Date(mean(as.numeric(Date))),  # mean of dates
            .groups = "drop"
        ) %>%
        dplyr::mutate(
                   label = stringr::str_c(
                                       Catégorie,`Matière organique exogène`,
                                       sep = " - "
                                   )
                   )

    label_irrigation <- df.rotation %>%
        dplyr::filter(irrigation != 0) %>%
        group_by(crop.id, year, irrigation) %>%
        summarise(
            mean_date = as.Date(mean(as.numeric(Date))),  # mean of dates
            .groups = "drop"
        )

    label_crop <- df.rotation %>%
        dplyr::filter(active == 1) %>%
        group_by(crop.id,year,Culture.x) %>%
        summarise(
            mean_date = as.Date(mean(as.numeric(Date))),  # mean of dates
            .groups = "drop"
        )
    
    label_covercrop <- df.rotation %>%
        dplyr::filter(cover.active == 1) %>%
        group_by(cover.id,year,Culture.y) %>%
        summarise(
            mean_date = as.Date(mean(as.numeric(Date))),  # mean of dates
            .groups = "drop"
        )

    g.rot <- (ggplot(
        data = df.rotation,
        aes(x = Date))
        ## Background information
        + geom_vline(
              data = df.rotation %>%
                  dplyr::filter(month == 1),
              aes(xintercept = Date),
              lwd = 3,
              colour = "darkgrey"
          )
        + geom_text(
              data = label_year,
              aes(x = mean_date, label = year),
              y = 0.9,
              size = 10,
              hjust = 0.5,
              colour = "darkgrey"
          )    
        ## Main crops
        + geom_rect(
              data = df.rotation %>%
                  dplyr::filter(active == 1),
              aes(xmin = (Date - 13),
                  xmax = (Date + 13),
                  ymin = 0.49,
                  ymax = 0.51,
                  color = Culture.x
                  ),
              inherit.aes = FALSE,
              lwd = 1,
              fill = NA
              )
        + geom_text(
              data = label_crop,
              aes(x = mean_date, y = 0.52,
                  label = crop.id,
                  color = Culture.x),
              fontface = "bold",
              size = 4
          )
        ## Sowing dates
        + geom_segment(
              data = df.rotation %>%
                  dplyr::filter(sowing),
              aes(
                  group = crop.id,
                  colour = Culture.x
              ),
              y = 0.64, yend = 0.54,
              arrow = arrow(type = "closed", length = unit(0.15, "cm")),
              linewidth = 0.6
          )
        + geom_text(
              data = df.rotation %>%
                  dplyr::filter(sowing),
              aes(
                  y = 0.65,
                  label = format(Date,"%b"),
                  colour = Culture.x),
              fontface = "bold",
              hjust = -0.3,
              size = 2
          )
        ## Harvest
        + geom_segment(
              data = df.rotation %>%
                  dplyr::filter(harvest),
              aes(
                  group = crop.id,
                  colour = Culture.x
              ),
              y = 0.58, yend = 0.68,
              arrow = arrow(type = "closed", length = unit(0.15, "cm")),
              linewidth = 0.6
          )
        + geom_text(
              data = df.rotation %>%
                  dplyr::filter(harvest),
              aes(y = 0.69,
                  label = format(Date,"%b"),
                  colour = Culture.x
                  ),
              fontface = "bold",
              hjust = 1.3,
              size = 2
          )
        ## Plant cover
        + geom_text(
              data = df.rotation %>%
                  dplyr::filter(active == 1 | cover.active == 1) %>%
                  dplyr::slice(1),
              aes(
                  y = 0.27
              ),
              label = "Plant cover",
              hjust = 0,
              fontface = "bold",
              size = 4,
              color="darkgreen"
          )
        + geom_rect(
            data = df.rotation %>%
                  dplyr::filter(active == 1 | cover.active == 1), 
            aes(xmin = (Date - 14),
                xmax = (Date + 14),
                ymin = 0.24,
                ymax = 0.26
                ),
            inherit.aes = FALSE,
            fill = "darkgreen"
        )
        ## Cover crops
        + geom_point(
              data = df.rotation %>%
                  dplyr::filter(cover.active == 1),
              aes(
                  y = 0.4,
                  color = Culture.y
              ),
              size = 4
          )
        + geom_text(
              data = label_covercrop,
              aes(x = mean_date, y = 0.42,
                  label = cover.id,
                  color = Culture.y),
              fontface = "bold",
              size = 4
          )
        + geom_text(
              data = label_covercrop,
              aes(x = mean_date, y = 0.45, label = "Cover crop"),
              fontface = "bold",
              size = 3,
              color="darkorange"
          )
        ## Sowing dates
        + geom_segment(
              data = df.rotation %>%
                  dplyr::filter(covercrop.sowing),
              aes(
                  group = crop.id
              ),
              y = 0.44, yend = 0.41,
              arrow = arrow(type = "closed", length = unit(0.15, "cm")),
              linewidth = 0.6,
              color="darkorange"
          )
        + geom_text(
              data = df.rotation %>%
                  dplyr::filter(covercrop.sowing),
              aes(
                  y = 0.45,
                  label = format(Date,"%b")),
              fontface = "bold",
              hjust = 1.2,
              size = 2,
              color="darkorange"
          )
        ## Harvest
        + geom_segment(
              data = df.rotation %>%
                  dplyr::filter(covercrop.harvest),
              aes(
                  group = crop.id
              ),
              y = 0.41, yend = 0.44,
              arrow = arrow(type = "closed", length = unit(0.15, "cm")),
              linewidth = 0.6,
              color="darkorange"
          )
        + geom_text(
              data = df.rotation %>%
                  dplyr::filter(covercrop.harvest),
              aes(y = 0.45, label = format(Date,"%b")),
              fontface = "bold",
              hjust = -0.2,
              size = 2,
              color="darkorange"
          )
        ## EOM
        + geom_point(
              data = df.rotation %>%
                  dplyr::filter(eom_app_q  != 0),
              aes(
                  y = 0.32
              ),
              size = 7,
              shape = 25,
              stroke = 3,
              colour = "gold4",
              fill = "sienna"
          )
        + geom_text(
              data = label_eom,
              aes(x = mean_date, y = 0.37, label = "External organic matter"),
              fontface = "bold",
              size = 3,
              color="sienna"
          )
        + geom_text(
              data = label_eom,
              aes(x = mean_date, y = 0.35, label = label),
              fontface = "bold",
              size = 4,
              color="sienna"
          )
        + geom_text(
              data = df.rotation %>%
                  dplyr::filter(eom_app_q  != 0),
              aes(y = 0.28, label = format(Date,"%b")),
              fontface = "bold",
              size = 2,
              color="sienna"
          )
        ## Irrigation
        + geom_path(
              data = df.rotation %>%
                  dplyr::filter(irrigation != 0),
              aes(
                  y = 0.75
              ),
              color="royalblue"
          )
        + geom_point(
              data = df.rotation %>%
                  dplyr::filter(irrigation != 0),
              aes(
                  y = 0.75
              ),
              color="royalblue"
          )
        + geom_text(
              data = label_irrigation,
              aes(x = mean_date, y = 0.79, label = "Irrigation"),
              fontface = "bold",
              size = 3,
              color="royalblue"
          )
        + geom_text(
              data = label_irrigation,
              aes(x = mean_date, y = 0.77, label = paste(irrigation,"mm")),
              fontface = "bold",
              size = 4,
              color="royalblue"
          )
        + geom_text(
              data = df.rotation %>%
                  dplyr::filter(irrigation != 0),
              aes(y = 0.74, label = format(Date,"%b")),
              fontface = "bold",
              size = 2,
              color="royalblue"
          )
        +  scale_x_date(
               date_breaks = "1 month",       # tick every month
               date_minor_breaks = "1 month",       # tick every month
               date_labels = "%b"          # e.g. "Jan 2024"
           )
        + scale_y_continuous(limits=c(0,1))
        + scale_color_viridis_d(option = "D", name = "Crop")
        + scale_fill_viridis_d(option = "D", name = "Crop")
        + theme_minimal()
        + theme(
              axis.title.x = element_blank(),
              axis.text.x = element_text(size = 6),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              legend.position = c(0, 0),
              legend.justification = c(0,0),
              legend.background = element_rect(colour = "light gray")
          )
    )
    return(g.rot)
}
