##################################################
# Install and Load Required Packages
##################################################

if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", 
                                         repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", 
                                     repos = "http://cran.us.r-project.org")
if(!require(ROCR)) install.packages("ROCR", 
                                     repos = "http://cran.us.r-project.org")
if(!require(interplot)) install.packages("interplot", 
                                    repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", 
                                         repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", 
                                     repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", 
                                     repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", 
                                            repos = "http://cran.us.r-project.org")
if(!require(gtable)) install.packages("gtable", 
                                         repos = "http://cran.us.r-project.org")
if(!require(lemon)) install.packages("lemon", 
                                      repos = "http://cran.us.r-project.org")
if(!require(kknn)) install.packages("kknn", 
                                     repos = "http://cran.us.r-project.org")
if(!require(NbClust)) install.packages("NbClust", 
                                    repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", 
                                       repos = "http://cran.us.r-project.org")
if(!require(cluster)) install.packages("cluster", 
                                        repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", 
                                       repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", 
                                     repos = "http://cran.us.r-project.org")
if(!require(markdown)) install.packages("markdown", 
                                        repos = "http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages("rmarkdown", 
                                         repos = "http://cran.us.r-project.org")
if(!require(formatR)) install.packages("formatR", 
                                         repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", 
                                       repos = "http://cran.us.r-project.org")

#################################################
# Wrangle the Data
#################################################

# Data is contained in two files: battedballs_2019_Raw.csv and hometofirst_2019_Raw.csv
# located at https://github.com/jfmusso/HarvardX/

# Import the data files into R data frames.

battedballs <- 
  read_csv("https://raw.githubusercontent.com/jfmusso/HarvardX/master/battedballs_2019_Raw.csv", 
           col_types = cols(
             game_date = col_date(format = "%m/%d/%Y"), 
             batter = col_character(), 
             player_name = col_character(), 
             stand = col_character(),  
             bb_type = col_factor(levels = c("ground_ball", "line_drive", "fly_ball", "popup")), 
             events = col_factor(), 
             des = col_character(), 
             launch_speed = col_double(), 
             launch_angle = col_double(), 
             hc_x = col_double(), 
             hc_y = col_double(), 
             hit_location = col_factor(), 
             if_fielding_alignment = col_factor(levels = c("Standard", "Strategic", "Infield shift")), 
             of_fielding_alignment = col_factor(), 
             hit_distance_sc = col_integer(), 
             launch_speed_angle = col_factor(), 
             game_pk = col_character(), 
             game_year = col_character(), 
             game_type = col_character(), 
             home_team = col_character(), 
             away_team = col_character(), 
             pitcher = col_factor(), 
             p_throws = col_character(), 
             description = col_character(), 
             type = col_character()
          )

  )

class(battedballs[])

hometofirst <- 
  read_csv("https://raw.githubusercontent.com/jfmusso/HarvardX/master/hometofirst_2019_Raw.csv", 
           col_types = cols(
             last_name = col_character(), 
             first_name = col_character(), 
             player_id = col_character(), 
             team_id = col_character(), 
             team = col_character(), 
             position = col_character(), 
             age = col_number(), 
             competitive_runs = col_integer(),
             hp_to_1b = col_double(), 
             sprint_speed = col_double()
           )
           
  )

class(hometofirst[])

# Remove players from hometofirst data frame if they have missing values for hp_to_1b.

hometofirst <- filter(hometofirst, hp_to_1b != "NA")

# Remove batted balls from battedballs data frame if they have missing values 
# for any of the following variables: hc_x, hc_y, if_fielding_alignment, or 
# of_fielding_alignment.

battedballs <- filter(battedballs, hc_x != "NA", hc_y != "NA", 
                      if_fielding_alignment != "NA", 
                      of_fielding_alignment != "NA")

# Combine battedballs and hometofirst data frames.

hometofirst <- rename(hometofirst, batter = player_id)
hometofirst <- hometofirst %>% dplyr::select(batter, team, position, age, hp_to_1b)
battedballs <- left_join(battedballs, hometofirst, by = "batter")

# Remove players from battedballs for whom there is no hp-to-1b data.

battedballs <- filter(battedballs, hp_to_1b != "NA")

# Remove batted balls hit into foul territory.

battedballs <- battedballs %>% mutate(foul = str_detect(des, "foul")) %>% 
  filter(foul == FALSE)

# Compute Spray Angle of each batted ball and add a new column containing results.

 # Transform x coordinate so that home plate is at 0.
battedballs <- battedballs %>% mutate(hc_x_Kolp = round(2.33 * (hc_x - 126), 2))

 # Transform y coordinate so that home plate is at 0.
battedballs <- battedballs %>% mutate(hc_y_Kolp = round(2.33 * (204.5 - hc_y), 2))

 # Compute spray_angle
battedballs <- battedballs %>% 
  mutate(spray_angle_Kolp = round((180 / pi) * atan(hc_x_Kolp / hc_y_Kolp), 1))

 # Adjust for side of plate batter stands on, so that batted balls hit to the side of the field 
 # the batter stands on (with respect to home plate) will always have a negative spray angle.

battedballs <- battedballs %>% 
  mutate(spray_angle_adj = ifelse(stand == "L", -spray_angle_Kolp, spray_angle_Kolp))

# Convert our categorical predictor variable, if_fielding_alignment, into a numeric variable.

battedballs <- battedballs %>% mutate(num_if_alignment = as.numeric(if_fielding_alignment))

# Add a more precise division of batted ball types (adv_bb_type) to more accurately categorize the values
# of our launch_angle predictor.

battedballs <- battedballs %>% 
  mutate(adv_bb_type = cut(battedballs$launch_angle, breaks = c(-90, -0.1, 10, 19, 26, 39, 90), 
                           labels = c("low_ground_ball", "high_ground_ball", "low_line_drive", 
                                      "high_line_drive", "fly_ball", "popup")))

# Add a categorical variable based on *launch_speed*.

battedballs <- battedballs %>% 
  mutate(launch_speed_cat = cut(battedballs$launch_speed, 
                                breaks = c(10, 69.9, 79.9, 89.9, 100, 125), 
                                labels = c("<70", "70-79.9", "80-89.9", "90-100", ">100")))

# Add categorical variables based on *spray_angle_Kolp* and *spray_angle_adj*.

battedballs <- battedballs %>% 
  mutate(spray_angle_Kolp_cat = cut(battedballs$spray_angle_Kolp, 
                                    breaks = c(-90, -45.1, -40, -31, -22, -9, 9, 25, 35, 43, 45, 90), 
                                    labels = c("-90:-45.1", "LF Line (-45:-40)", "3rd Baseman (-39.9:-31)", 
                                               "Hole 5-6 (-30.9:-22)", "Shortstop (-21.9:-9)", 
                                               "Hole Middle (-8.9:9)", "2nd Baseman (9.1:25)", 
                                               "Hole 3-4 (25.1:35)", "1st Baseman (35.1:43)", 
                                               "RF Line (43.1:45)", "45.1:90")))

battedballs <- battedballs %>% 
  mutate(spray_angle_adj_cat = cut(battedballs$spray_angle_adj, 
                                   breaks = c(-90, -45.1, -40, -31, -22, -9, 9, 25, 35, 43, 45, 90), 
                                   labels = c("-90:-45.1", "Pulled Down Line (-45:-40)", 
                                              "Pulled Corner Inf (-39.9:-31)", "Pulled Side Hole (-30.9:-22)", 
                                              "Pulled Mid Inf (-21.9:-9)", "Middle Hole (-8.9:9)", 
                                              "Oppo Mid Inf (9.1:25)", "Oppo Side Hole (25.1:35)", 
                                              "Oppo Corner Inf (35.1:43)", "Oppo Down Line (43.1:45)", 
                                              "45.1:90")))

# Add a categorical variable based on *hp_to_1b*.

battedballs <- battedballs %>% 
  mutate(hp_to_1b_cat = cut(battedballs$hp_to_1b, 
                                breaks = c(3.9, 4.2, 4.5, 4.8, 5.1), 
                                labels = c("3.9-4.2", "4.21-4.5", "4.51-4.8", "4.81-5.1")))

# Reorder the columns in battedballs.

battedballs <- battedballs %>% dplyr::select(game_date, batter, player_name, age, stand, position, team, 
       bb_type, adv_bb_type, events, des, hp_to_1b, hp_to_1b_cat, launch_speed, launch_speed_cat, 
       launch_angle, hc_x, hc_y, hc_x_Kolp, hc_y_Kolp, spray_angle_Kolp, spray_angle_Kolp_cat, 
       spray_angle_adj, spray_angle_adj_cat, hit_location, hit_distance_sc, 
       if_fielding_alignment, num_if_alignment, of_fielding_alignment, launch_speed_angle, 
       game_pk, game_year, game_type, home_team, away_team, pitcher, p_throws, description, type, foul)


#################################################
# Explore the Data
#################################################

# Create a tibble containing just the predictors and outcome we will use.

battedballs_var <- dplyr::select(battedballs, events, launch_angle, launch_speed, 
                                 spray_angle_Kolp, spray_angle_adj, hp_to_1b, 
                                 if_fielding_alignment)

# Generate a statistical distribution of battedballs_var.

summary(battedballs_var)

# Create a histogram showing the distribution of values for each 
# of the predictors.

battedballs_var %>% ggplot(aes(x = launch_angle)) + 
  geom_histogram(fill = "royalblue3", color = "black", 
                 binwidth = 5)
battedballs_var %>% ggplot(aes(x = launch_speed)) + 
  geom_histogram(fill = "royalblue3", color = "black", 
                 binwidth = 5)
battedballs_var %>% ggplot(aes(x = spray_angle_Kolp)) + 
  geom_histogram(fill = "royalblue3", color = "black", 
                 binwidth = 5)
battedballs_var %>% ggplot(aes(x = spray_angle_adj)) + 
  geom_histogram(fill = "royalblue3", color = "black", 
                 binwidth = 5)
battedballs_var %>% ggplot(aes(x = hp_to_1b)) + 
  geom_histogram(fill = "royalblue3", color = "black", 
                 binwidth = .02)
battedballs_var %>% ggplot(aes(x = if_fielding_alignment)) + 
  geom_bar(fill = "royalblue3", color = "black")
                           
# Create a correlation matrix of the predictors to understand the 
# relationship between them.

library(corrplot)

(cor_battedballs <- round(cor(battedballs_var[, 2:6], method = "spearman"), 2))
corrplot(cor_battedballs, type = "upper", method = "ellipse", tl.cex = 0.9, tl.col = "black")

# Visualize batted balls resulting in singles or outs.

set.seed(1)
battedballs_sample <- sample_n(battedballs, 4000, replace = FALSE)

ggplot(battedballs_sample, aes(hc_x_Kolp, hc_y_Kolp, color = events)) + 
  scale_color_manual(values = c("red", "royalblue3")) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 5, height = 5), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 5, height = 5), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 5, height = 5), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = 0.5) + theme_linedraw() + labs(title = "Fair Batted Balls Using Transformed Coordinates")

# Launch Angle

# Visualize singles and outs, by type of batted ball.

ggplot(battedballs_sample, aes(hc_x_Kolp, hc_y_Kolp, color = events)) + 
  scale_color_manual(values = c("red", "royalblue3")) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = 0.5) + theme_linedraw() + 
  labs(title = "Singles vs Outs, by Type of Batted Ball") + 
  facet_grid(bb_type ~ events)

# Visualize singles and outs, by adv_bb_type.

ggplot(battedballs_sample, aes(hc_x_Kolp, hc_y_Kolp, color = events)) + 
  scale_color_manual(values = c("red", "royalblue3")) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = 0.5) + theme_linedraw() + 
  labs(title = "Singles vs Outs, by adv_bb_type") + 
  facet_grid(adv_bb_type ~ events)

# Create boxplots to visualize the distribution of launch_angle by singles and outs.

bb_outcome_by_launch_angle <- ggplot(battedballs, 
                                  aes(x = events, y = launch_angle, color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(-75, 75, by = 15), 
                     labels = c("-75", "-60", "-45", "-30", "-15", "0",  "15", "30", "45", "60", "75"), 
                     limits = c(-90, 90)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by Launch Angle") + 
  theme(legend.position = "none")

bb_outcome_by_launch_angle_data <- layer_data(bb_outcome_by_launch_angle)

launch_angle_single_1quartile <- bb_outcome_by_launch_angle_data[1, 3]
launch_angle_single_median <- bb_outcome_by_launch_angle_data[1, 4]
launch_angle_single_3quartile <- bb_outcome_by_launch_angle_data[1, 5]
launch_angle_out_1quartile <- bb_outcome_by_launch_angle_data[2, 3]
launch_angle_out_median <- bb_outcome_by_launch_angle_data[2, 4]
launch_angle_out_3quartile <- bb_outcome_by_launch_angle_data[2, 5]

ggplot(battedballs, 
       aes(x = events, y = launch_angle, color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(-75, 75, by = 15), 
                     labels = c("-75", "-60", "-45", "-30", "-15", "0",  "15", "30", "45", "60", "75"), 
                     limits = c(-90, 90)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by Launch Angle") + 
  theme(legend.position = "none") + 
  annotate("text", x = c(.57, .57, .57), size = 2.5, fontface = 2, hjust = 1,  
           y = c(launch_angle_single_1quartile, launch_angle_single_median, launch_angle_single_3quartile), 
           label = c(launch_angle_single_1quartile, launch_angle_single_median, launch_angle_single_3quartile)) + 
  annotate("text", x = c(1.57,1.57, 1.57), size = 2.5, fontface = 2, hjust = 1, 
           y = c(launch_angle_out_1quartile, launch_angle_out_median, launch_angle_out_3quartile),
           label = c(launch_angle_out_1quartile, launch_angle_out_median, launch_angle_out_3quartile))

# Compute the proportion of singles for each adv_bb_type.

(la_prop_lgb <- battedballs %>% filter(adv_bb_type == "low_ground_ball" & battedballs$events == "single") %>% summarize(n = n())) / 
  (battedballs %>% filter(adv_bb_type == "low_ground_ball") %>% summarize(n = n()))
(la_prop_hgb <- battedballs %>% filter(adv_bb_type == "high_ground_ball" & battedballs$events == "single") %>% summarize(n = n())) / 
  (battedballs %>% filter(adv_bb_type == "high_ground_ball") %>% summarize(n = n()))
(la_prop_lld <- battedballs %>% filter(adv_bb_type == "low_line_drive" & battedballs$events == "single") %>% summarize(n = n())) / 
  (battedballs %>% filter(adv_bb_type == "low_line_drive") %>% summarize(n = n()))
(la_prop_hld <- battedballs %>% filter(adv_bb_type == "high_line_drive" & battedballs$events == "single") %>% summarize(n = n())) / 
  (battedballs %>% filter(adv_bb_type == "high_line_drive") %>% summarize(n = n()))
(la_prop_fb <- battedballs %>% filter(adv_bb_type == "fly_ball" & battedballs$events == "single") %>% summarize(n = n())) / 
  (battedballs %>% filter(adv_bb_type == "fly_ball") %>% summarize(n = n()))
(la_prop_pu <- battedballs %>% filter(adv_bb_type == "popup" & battedballs$events == "single") %>% summarize(n = n())) / 
  (battedballs %>% filter(adv_bb_type == "popup") %>% summarize(n = n()))

# Plot the density of our Launch Angle predictor by result (single or out).

x_scale <- scale_x_continuous(breaks = seq(-75, 75, by = 15), 
                              labels = c("-75", "-60", "-45", "-30", "-15", "0",  "15", "30", "45", "60", "75"),
                              limits = NULL)
y_scale <- scale_y_continuous(limits = c(0, 0.045))
(la_density <- ggplot(battedballs, aes(x = launch_angle, y = stat(density), color = events)) + 
  geom_freqpoly(binwidth = 0.4) + 
  x_scale + 
  y_scale + 
  scale_color_manual(values = c(single = "red4", field_out = "royalblue3")) + 
  labs(title = "Density Plot of Launch Angle by Result (Single or Out)", 
       x = "Launch Angle (degrees)") + 
    theme(legend.position = c(.85, .9))) 

la_density_data <- layer_data(la_density)
la_density_data <- filter(la_density_data, y != "NA")

# Exit Velocity (launch_speed)

# Visualize singles and outs, by Exit Velocity.          .

ggplot(battedballs_sample, aes(hc_x_Kolp, hc_y_Kolp, color = events)) + 
  scale_color_manual(values = c("red", "royalblue3")) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = 0.5) + theme_linedraw() + 
  labs(title = "Singles vs Outs, by launch_speed_cat") + 
  facet_grid(launch_speed_cat ~ events)

# Create boxplots to visualize the distribution of launch_speed by singles and outs.

bb_outcome_by_launch_speed <- ggplot(battedballs, 
                                     aes(x = events, y = launch_speed, color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(10, 130, by = 15), 
                     labels = c("10", "25", "40", "55", "70", "85", "100",  "115", "130"), 
                     limits = c(10, 130)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by Exit Velocity") + 
  theme(legend.position = "none")

bb_outcome_by_launch_speed_data <- layer_data(bb_outcome_by_launch_speed)

launch_speed_single_1quartile <- bb_outcome_by_launch_speed_data[1, 3]
launch_speed_single_median <- bb_outcome_by_launch_speed_data[1, 4]
launch_speed_single_3quartile <- bb_outcome_by_launch_speed_data[1, 5]
launch_speed_out_1quartile <- bb_outcome_by_launch_speed_data[2, 3]
launch_speed_out_median <- bb_outcome_by_launch_speed_data[2, 4]
launch_speed_out_3quartile <- bb_outcome_by_launch_speed_data[2, 5]

ggplot(battedballs, 
       aes(x = events, y = launch_speed, color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(10, 130, by = 15), 
                     labels = c("10", "25", "40", "55", "70", "85", "100",  "115", "130"), 
                     limits = c(10, 130)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by Exit Velocity") + 
  theme(legend.position = "none") + 
  annotate("text", size = 2.5, fontface = 2, hjust = 1, x = c(.58, .58, .58), 
           y = c(launch_speed_single_1quartile, launch_speed_single_median, launch_speed_single_3quartile), 
           label = c(launch_speed_single_1quartile, launch_speed_single_median, launch_speed_single_3quartile)) + 
  annotate("text", size = 2.5, fontface = 2, hjust = 1, x = c(1.6,1.6, 1.6), 
           y = c(launch_speed_out_1quartile, launch_speed_out_median, launch_speed_out_3quartile),
           label = c(launch_speed_out_1quartile, launch_speed_out_median, launch_speed_out_3quartile))

# Compute the proportion of singles for each launch_speed_cat.

(ls_prop_lt70 <- battedballs %>% filter(launch_speed_cat == "<70" & battedballs$events == "single") %>% summarize(n = n())) / 
  (battedballs %>% filter(launch_speed_cat == "<70") %>% summarize(n = n()))
(ls_prop_to80 <- battedballs %>% filter(launch_speed_cat == "70-79.9" & battedballs$events == "single") %>% summarize(n = n())) / 
  (battedballs %>% filter(launch_speed_cat == "70-79.9") %>% summarize(n = n()))
(ls_prop_to90 <- battedballs %>% filter(launch_speed_cat == "80-89.9" & battedballs$events == "single") %>% summarize(n = n())) / 
  (battedballs %>% filter(launch_speed_cat == "80-89.9") %>% summarize(n = n()))
(ls_prop_100 <- battedballs %>% filter(launch_speed_cat == "90-100" & battedballs$events == "single") %>% summarize(n = n())) / 
  (battedballs %>% filter(launch_speed_cat == "90-100") %>% summarize(n = n()))
(ls_prop_gt100 <- battedballs %>% filter(launch_speed_cat == ">100" & battedballs$events == "single") %>% summarize(n = n())) / 
  (battedballs %>% filter(launch_speed_cat == ">100") %>% summarize(n = n()))

# Plot the density of our Exit Velocity predictor (*launch_speed*) by result (single or out).

x_scale <- scale_x_continuous(breaks = seq(35, 125, by = 10), 
                              labels = c("35", "45", "55", "-65", "75", "85", 95,  "105", "115", "125"),
                              limits = c(35, 125))
y_scale <- scale_y_continuous(limits = c(0, 0.04))
(ls_density <- ggplot(battedballs, aes(x = launch_speed, y = stat(density), color = events)) + 
    geom_freqpoly(binwidth = 0.4) + 
    x_scale + 
    y_scale + 
    scale_color_manual(values = c(single = "red4", field_out = "royalblue3")) + 
    labs(title = "Density Plot of Exit Velocity by Result (Single or Out)", 
         x = "Exit Velocity (mph)") + 
    theme(legend.position = c(.15, .9)))  

ls_density_data <- layer_data(ls_density)
ls_density_data <- filter(ls_density_data, y != "NA", x != "NA")

# Spray Angle

# Create a random sample of 2,000 ground balls.

set.seed(1)
battedballs_gb_sample <- sample_n(filter(battedballs, launch_angle <= 10), 2000, replace = FALSE)

# Visualize singles and outs, by Spray Angle (Kolp's version).          .

spray_angle_Kolp_plot <- ggplot(battedballs_gb_sample, aes(hc_x_Kolp, hc_y_Kolp, color = if_fielding_alignment)) + 
  scale_color_manual(values = c(Standard = "black", Strategic = "cyan3", `Infield shift` = "magenta1")) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = 0.5) + theme_linedraw() + 
  labs(title = "Singles vs Outs, by spray_angle_Kolp") + 
  #facet_grid(events ~ spray_angle_Kolp_cat) 
  facet_wrap(~ spray_angle_Kolp_cat + events, dir = "v", nrow = 6) + 
  theme(strip.text = element_text(size = 8))

gtable_show_names(spray_angle_Kolp_plot)
reposition_legend(spray_angle_Kolp_plot, position = 'center', panel = 'panel-1-6')

# Visualize singles and outs, by *spray_angle_adj*

spray_angle_adj_plot <- ggplot(battedballs_gb_sample, aes(hc_x_Kolp, hc_y_Kolp, color = if_fielding_alignment)) + 
  scale_color_manual(values = c(Standard = "black", Strategic = "cyan3", `Infield shift` = "magenta1")) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = 0.5) + theme_linedraw() + 
  labs(title = "Singles vs Outs, by spray_angle_adj") + 
  facet_wrap(~ spray_angle_adj_cat + events, dir = "v", nrow = 6) + 
  theme(strip.text = element_text(size = 8))
  
gtable_show_names(spray_angle_adj_plot)
reposition_legend(spray_angle_adj_plot, position = 'center', panel = 'panel-1-6')

# Create boxplots to visualize the distribution of Spray Angle and Infield Alignment by singles and outs.

battedballs_gb <- battedballs %>% filter(launch_angle <= 10)
battedballs_gb_std <- battedballs %>% filter(launch_angle <= 10, if_fielding_alignment == "Standard")
battedballs_gb_strat <- battedballs %>% filter(launch_angle <= 10, if_fielding_alignment == "Strategic")
battedballs_gb_shift <- battedballs %>% filter(launch_angle <= 10, if_fielding_alignment == "Infield shift")

# Standard Infield Alignment

bb_outcome_by_spray_angle_Kolp_std <- ggplot(battedballs_gb_std, 
                                     aes(x = events, y = spray_angle_Kolp, color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(-50, 50, by = 10), 
                     labels = c("-50", "-40", "-30", "-20", "-10", "0", "10", "20", "30", "40",  "50"), 
                     limits = c(-50, 50)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by spray_angle_Kolp and Standard Infield Alignment") + 
  theme(legend.position = "none")

bb_outcome_by_spray_angle_Kolp_std_data <- layer_data(bb_outcome_by_spray_angle_Kolp_std)

spray_angle_Kolp_single_std_1quartile <- bb_outcome_by_spray_angle_Kolp_std_data[1, 3]
spray_angle_Kolp_single_std_median <- bb_outcome_by_spray_angle_Kolp_std_data[1, 4]
spray_angle_Kolp_single_std_3quartile <- bb_outcome_by_spray_angle_Kolp_std_data[1, 5]
spray_angle_Kolp_out_std_1quartile <- bb_outcome_by_spray_angle_Kolp_std_data[2, 3]
spray_angle_Kolp_out_std_median <- bb_outcome_by_spray_angle_Kolp_std_data[2, 4]
spray_angle_Kolp_out_std_3quartile <- bb_outcome_by_spray_angle_Kolp_std_data[2, 5]

ggplot(battedballs_gb_std, 
       aes(x = events, y = spray_angle_Kolp, color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(-50, 50, by = 10), 
                     labels = c("-50", "-40", "-30", "-20", "-10", "0", "10", "20", "30", "40",  "50"), 
                     limits = c(-50, 50)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by spray_angle_Kolp and Standard Infield Alignment") + 
  theme(legend.position = "none") + 
  annotate("text", size = 2.5, fontface = 2, hjust = 1, x = c(.6, .6, .6), 
           y = c(spray_angle_Kolp_single_std_1quartile, spray_angle_Kolp_single_std_median, spray_angle_Kolp_single_std_3quartile), 
           label = c(spray_angle_Kolp_single_std_1quartile, spray_angle_Kolp_single_std_median, spray_angle_Kolp_single_std_3quartile)) + 
  annotate("text", size = 2.5, fontface = 2, hjust = 1, x = c(1.6,1.6, 1.6), 
           y = c(spray_angle_Kolp_out_std_1quartile, spray_angle_Kolp_out_std_median, spray_angle_Kolp_out_std_3quartile),
           label = c(spray_angle_Kolp_out_std_1quartile, spray_angle_Kolp_out_std_median, spray_angle_Kolp_out_std_3quartile))

# Strategic Infield Alignment

bb_outcome_by_spray_angle_Kolp_strat <- ggplot(battedballs_gb_strat, 
                                               aes(x = events, y = spray_angle_Kolp, 
                                                   color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(-50, 50, by = 10), 
                     labels = c("-50", "-40", "-30", "-20", "-10", "0", "10", "20", "30", 
                                "40",  "50"), 
                     limits = c(-50, 50)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by spray_angle_Kolp and Strategic Infield Alignment") + 
  theme(legend.position = "none")

bb_outcome_by_spray_angle_Kolp_strat_data <- 
  layer_data(bb_outcome_by_spray_angle_Kolp_strat)

spray_angle_Kolp_single_strat_1quartile <- bb_outcome_by_spray_angle_Kolp_strat_data[1, 3]
spray_angle_Kolp_single_strat_median <- bb_outcome_by_spray_angle_Kolp_strat_data[1, 4]
spray_angle_Kolp_single_strat_3quartile <- bb_outcome_by_spray_angle_Kolp_strat_data[1, 5]
spray_angle_Kolp_out_strat_1quartile <- bb_outcome_by_spray_angle_Kolp_strat_data[2, 3]
spray_angle_Kolp_out_strat_median <- bb_outcome_by_spray_angle_Kolp_strat_data[2, 4]
spray_angle_Kolp_out_strat_3quartile <- bb_outcome_by_spray_angle_Kolp_strat_data[2, 5]

ggplot(battedballs_gb_strat, 
       aes(x = events, y = spray_angle_Kolp, color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(-50, 50, by = 10), 
                     labels = c("-50", "-40", "-30", "-20", "-10", "0", "10", "20", "30", 
                                "40",  "50"), 
                     limits = c(-50, 50)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by spray_angle_Kolp and Strategic Infield Alignment") + 
  theme(legend.position = "none") + 
  annotate("text", size = 2.5, fontface = 2, hjust = 1, x = c(.6, .6, .6), 
           y = c(spray_angle_Kolp_single_strat_1quartile, 
                 spray_angle_Kolp_single_strat_median, 
                 spray_angle_Kolp_single_strat_3quartile), 
           label = c(spray_angle_Kolp_single_strat_1quartile, 
                     spray_angle_Kolp_single_strat_median, 
                     spray_angle_Kolp_single_strat_3quartile)) + 
  annotate("text", size = 2.5, fontface = 2, hjust = 1, x = c(1.6,1.6, 1.6), 
           y = c(spray_angle_Kolp_out_strat_1quartile, 
                 spray_angle_Kolp_out_strat_median, 
                 spray_angle_Kolp_out_strat_3quartile),
           label = c(spray_angle_Kolp_out_strat_1quartile, 
                     spray_angle_Kolp_out_strat_median, 
                     spray_angle_Kolp_out_strat_3quartile))

# Infield Shift Alignment

bb_outcome_by_spray_angle_Kolp_shift <- ggplot(battedballs_gb_shift, 
                                               aes(x = events, y = spray_angle_Kolp, 
                                                   color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(-50, 50, by = 10), 
                     labels = c("-50", "-40", "-30", "-20", "-10", "0", "10", "20", "30", 
                                "40",  "50"), 
                     limits = c(-50, 50)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by spray_angle_Kolp and Infield Shift Alignment") + 
  theme(legend.position = "none")

bb_outcome_by_spray_angle_Kolp_shift_data <- 
  layer_data(bb_outcome_by_spray_angle_Kolp_shift)

spray_angle_Kolp_single_shift_1quartile <- bb_outcome_by_spray_angle_Kolp_shift_data[1, 3]
spray_angle_Kolp_single_shift_median <- bb_outcome_by_spray_angle_Kolp_shift_data[1, 4]
spray_angle_Kolp_single_shift_3quartile <- bb_outcome_by_spray_angle_Kolp_shift_data[1, 5]
spray_angle_Kolp_out_shift_1quartile <- bb_outcome_by_spray_angle_Kolp_shift_data[2, 3]
spray_angle_Kolp_out_shift_median <- bb_outcome_by_spray_angle_Kolp_shift_data[2, 4]
spray_angle_Kolp_out_shift_3quartile <- bb_outcome_by_spray_angle_Kolp_shift_data[2, 5]

ggplot(battedballs_gb_shift, 
       aes(x = events, y = spray_angle_Kolp, color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(-50, 50, by = 10), 
                     labels = c("-50", "-40", "-30", "-20", "-10", "0", "10", "20", "30", 
                                "40",  "50"), 
                     limits = c(-50, 50)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by spray_angle_Kolp and Infield Shift Alignment") + 
  theme(legend.position = "none") + 
  annotate("text", size = 2.5, fontface = 2, hjust = 1, x = c(.6, .6, .6), 
           y = c(spray_angle_Kolp_single_shift_1quartile, 
                 spray_angle_Kolp_single_shift_median, 
                 spray_angle_Kolp_single_shift_3quartile), 
           label = c(spray_angle_Kolp_single_shift_1quartile, 
                     spray_angle_Kolp_single_shift_median, 
                     spray_angle_Kolp_single_shift_3quartile)) + 
  annotate("text", size = 2.5, fontface = 2, hjust = 1, x = c(1.6,1.6, 1.6), 
           y = c(spray_angle_Kolp_out_shift_1quartile, 
                 spray_angle_Kolp_out_shift_median, 
                 spray_angle_Kolp_out_shift_3quartile),
           label = c(spray_angle_Kolp_out_shift_1quartile, 
                     spray_angle_Kolp_out_shift_median, 
                     spray_angle_Kolp_out_shift_3quartile))

# Compute the proportion of singles for each *spray_angle_Kolp_cat* with a Standard Infield Alignment.

(sa_std_prop_LFline <- (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "LF Line (-45:-40)" & events == "single") %>% summarize(n = n())) / 
  (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "LF Line (-45:-40)") %>% summarize(n = n())))

(sa_std_prop_3b <- (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "3rd Baseman (-39.9:-31)" & events == "single") %>% summarize(n = n())) / 
  (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "3rd Baseman (-39.9:-31)") %>% summarize(n = n())))

(sa_std_prop_hole56 <- (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "Hole 5-6 (-30.9:-22)" & events == "single") %>% summarize(n = n())) / 
  (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "Hole 5-6 (-30.9:-22)") %>% summarize(n = n())))

(sa_std_prop_ss <- (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "Shortstop (-21.9:-9)" & events == "single") %>% summarize(n = n())) / 
  (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "Shortstop (-21.9:-9)") %>% summarize(n = n())))

(sa_std_prop_holeMid <- (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "Hole Middle (-8.9:9)" & events == "single") %>% summarize(n = n())) / 
  (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "Hole Middle (-8.9:9)") %>% summarize(n = n())))

(sa_std_prop_2b <- (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "2nd Baseman (9.1:25)" & events == "single") %>% summarize(n = n())) / 
  (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "2nd Baseman (9.1:25)") %>% summarize(n = n())))

(sa_std_prop_hole34 <- (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "Hole 3-4 (25.1:35)" & events == "single") %>% summarize(n = n())) / 
  (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "Hole 3-4 (25.1:35)") %>% summarize(n = n())))

(sa_std_prop_1b <- (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "1st Baseman (35.1:43)" & events == "single") %>% summarize(n = n())) / 
  (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "1st Baseman (35.1:43)") %>% summarize(n = n())))

(sa_std_prop_RFline <- (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "RF Line (43.1:45)" & events == "single") %>% summarize(n = n())) / 
  (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "RF Line (43.1:45)") %>% summarize(n = n())))

# Compute the proportion of singles for each *spray_angle_adj_cat* with an Infield Shift Alignment.

(sa_shift_prop_LFline <- (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Pulled Down Line (-45:-40)" & events == "single") %>% summarize(n = n())) / 
    (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Pulled Down Line (-45:-40)") %>% summarize(n = n())))

(sa_shift_prop_3b <- (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Pulled Corner Inf (-39.9:-31)" & events == "single") %>% summarize(n = n())) / 
    (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Pulled Corner Inf (-39.9:-31)") %>% summarize(n = n())))

(sa_shift_prop_hole56 <- (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Pulled Side Hole (-30.9:-22)" & events == "single") %>% summarize(n = n())) / 
    (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Pulled Side Hole (-30.9:-22)") %>% summarize(n = n())))

(sa_shift_prop_ss <- (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Pulled Mid Inf (-21.9:-9)" & events == "single") %>% summarize(n = n())) / 
    (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Pulled Mid Inf (-21.9:-9)") %>% summarize(n = n())))

(sa_shift_prop_holeMid <- (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Middle Hole (-8.9:9)" & events == "single") %>% summarize(n = n())) / 
    (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Middle Hole (-8.9:9)") %>% summarize(n = n())))

(sa_shift_prop_2b <- (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Oppo Mid Inf (9.1:25)" & events == "single") %>% summarize(n = n())) / 
    (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Oppo Mid Inf (9.1:25)") %>% summarize(n = n())))

(sa_shift_prop_hole34 <- (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Oppo Side Hole (25.1:35)" & events == "single") %>% summarize(n = n())) / 
    (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Oppo Side Hole (25.1:35)") %>% summarize(n = n())))

(sa_shift_prop_1b <- (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Oppo Corner Inf (35.1:43)" & events == "single") %>% summarize(n = n())) / 
    (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Oppo Corner Inf (35.1:43)") %>% summarize(n = n())))

(sa_shift_prop_RFline <- (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Oppo Down Line (43.1:45)" & events == "single") %>% summarize(n = n())) / 
    (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Oppo Down Line (43.1:45)") %>% summarize(n = n())))

# Plot the density of *spray_angle_Kolp* by result (single or out) and a Standard infield alignment.

x_scale <- scale_x_continuous(breaks = seq(-45, 45, by = 10), 
                              labels = c("-45", "-35", "-25", "-15", "-5", "5", "15", "25", "35", "45"), 
                              limits = c(-50, 50)) 
y_scale <- scale_y_continuous(limits = c(0, 0.035))
(sa_Kolp_density <- ggplot(battedballs_gb_std, aes(x = spray_angle_Kolp, y = stat(density), color = events)) + 
    geom_freqpoly(binwidth = 0.4) + 
    x_scale + 
    y_scale + 
    scale_color_manual(values = c(single = "red4", field_out = "royalblue3")) + 
    labs(title = "Density Plot of Spray Angle (Kolp) by Result (Single or Out) 
         and a Standard Infield Alignment", x = "Spray Angle Kolp (degrees)") + 
    theme(legend.position = c(.85, .85)))

sa_Kolp_density_data <- layer_data(sa_Kolp_density)
sa_Kolp_density_data <- filter(sa_Kolp_density_data, y != "NA", x != "NA")

# Plot the density of *spray_angle_adj* by result (single or out) and a shifted infield alignment.

x_scale <- scale_x_continuous(breaks = seq(-45, 45, by = 10), 
                              labels = c("-45", "-35", "-25", "-15", "-5", "5", "15", "25", "35", "45"), 
                              limits = c(-50, 50)) 
y_scale <- scale_y_continuous(limits = c(0, 0.035))
(sa_adj_density <- ggplot(battedballs_gb_shift, aes(x = spray_angle_adj, y = stat(density), color = events)) + 
    geom_freqpoly(binwidth = 1) + 
    x_scale + 
    y_scale + 
    scale_color_manual(values = c(single = "red4", field_out = "royalblue3")) + 
    labs(title = "Density Plot of spray_angle_adj by Result (Single or Out) 
         and a Shifted Infield Alignment", x = "spray_angle_adj (degrees)") + 
    theme(legend.position = c(.85, .85)))

sa_adj_density_data <- layer_data(sa_adj_density)
sa_adj_density_data <- filter(sa_adj_density_data, y != "NA", x != "NA")

# Home to First

# Create a subset of *battedballs* consisting only of Topped and Weakly Hit Ground Balls.

battedballs_lowgb_topweak <- battedballs %>% filter(bb_type == "ground_ball" & adv_bb_type == "low_ground_ball")
battedballs_lowgb_topweak <- battedballs_lowgb_topweak %>% filter(launch_speed_angle == "1" | 
                                                                    launch_speed_angle == "2")
                                              
set.seed(1)
battedballs_lowgb_topweak_sample <- sample_n(battedballs_lowgb_topweak, 3000, replace = FALSE)

# Visualize singles and outs, by Home to First predictor (topped and weakly hit ground balls only).          .

ggplot(battedballs_lowgb_topweak_sample, aes(hc_x_Kolp, hc_y_Kolp, color = events)) + 
  scale_color_manual(values = c("red", "royalblue3")) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = 0.5) + theme_linedraw() + 
  labs(title = "Singles vs Outs, by hp_to_1b") + 
  facet_grid(hp_to_1b_cat ~ events)

# Create boxplots to visualize the distribution of hp_to_1b by singles and outs.

bb_outcome_by_hp_to_1b <- ggplot(battedballs_lowgb_topweak, 
                                 aes(x = events, y = hp_to_1b, color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(3.9, 5.1, by = 0.1), 
                     labels = c("3.9", "4.0", "4.1", "4.2", "4.3", "4.4", "4.5",  "4.6", 
                                "4.7", "4.8", "4.9", "5.0", "5.1"), 
                     limits = c(3.9, 5.1)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by Home to First 
          (topped and weakly hit ground balls only)") + 
  theme(legend.position = "none")

bb_outcome_by_hp_to_1b_data <- layer_data(bb_outcome_by_hp_to_1b)

hp_to_1b_single_1quartile <- bb_outcome_by_hp_to_1b_data[1, 3]
hp_to_1b_single_median <- bb_outcome_by_hp_to_1b_data[1, 4]
hp_to_1b_single_3quartile <- bb_outcome_by_hp_to_1b_data[1, 5]
hp_to_1b_out_1quartile <- bb_outcome_by_hp_to_1b_data[2, 3]
hp_to_1b_out_median <- bb_outcome_by_hp_to_1b_data[2, 4]
hp_to_1b_out_3quartile <- bb_outcome_by_hp_to_1b_data[2, 5]

ggplot(battedballs_lowgb_topweak, 
       aes(x = events, y = hp_to_1b, color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(3.9, 5.1, by = 0.1), 
                     labels = c("3.9", "4.0", "4.1", "4.2", "4.3", "4.4", "4.5",  "4.6", 
                                "4.7", "4.8", "4.9", "5.0", "5.1"), 
                     limits = c(3.9, 5.1)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by Home to First 
          (topped and weakly hit ground balls only)") + 
  theme(legend.position = "none") + 
  annotate("text", size = 2.5, fontface = 2, hjust = 1, x = c(.6, .6, .6), 
           y = c(hp_to_1b_single_1quartile, hp_to_1b_single_median, 
                 hp_to_1b_single_3quartile), 
           label = c(hp_to_1b_single_1quartile, hp_to_1b_single_median, 
                     hp_to_1b_single_3quartile)) + 
  annotate("text", size = 2.5, fontface = 2, hjust = 1, x = c(1.6,1.6, 1.6), 
           y = c(hp_to_1b_out_1quartile, hp_to_1b_out_median, hp_to_1b_out_3quartile),
           label = c(hp_to_1b_out_1quartile, hp_to_1b_out_median, hp_to_1b_out_3quartile))

# Create violin plots to visualize the distribution of hp_to_1b by singles and outs.

ggplot(battedballs_lowgb_topweak, 
       aes(x = events, y = hp_to_1b)) + 
  geom_violin(fill = "slategray1") + 
  scale_y_continuous(breaks = seq(3.9, 5.1, by = 0.1), 
                     labels = c("3.9", "4.0", "4.1", "4.2", "4.3", "4.4", "4.5",  "4.6", 
                                "4.7", "4.8", "4.9", "5.0", "5.1"), 
                     limits = c(3.9, 5.1)) + 
  ggtitle("Singles vs Outs, by Home to First 
          (topped and weakly hit ground balls only)") + 
  theme(legend.position = "none")

# Plot the density of our Home to First predictor (*hp_to_1b*) by result (single or out).

x_scale <- scale_x_continuous(breaks = seq(3.9, 5.1, by = 0.1), 
                              labels = c("3.9", "4.0", "4.1", "4.2", "4.3", "4.4", "4.5",  "4.6", 
                                         "4.7", "4.8", "4.9", "5.0", "5.1"), 
                              limits = c(3.9, 5.1))
y_scale <- scale_y_continuous() 
(h1b_density <- ggplot(battedballs_lowgb_topweak, aes(x = hp_to_1b, y = stat(density), color = events)) + 
    geom_freqpoly(binwidth = .08) + 
    x_scale + 
    y_scale + 
    scale_color_manual(values = c(single = "red4", field_out = "royalblue3")) + 
    labs(title = "Density Plot of Home to First by Result (Single or Out)", 
         x = "Home to First (seconds)") + 
    theme(legend.position = c(.85, .85))) 

h1b_density_data <- layer_data(h1b_density)
h1b_density_data <- filter(h1b_density_data, y != "NA", x != "NA")

######################################################
# Prepare Data for Use in Machine Learning Algorithms 
######################################################

# Make sure battedballs$events is coded as a factor with levels 
# single, followed by field_out.

battedballs$events <- factor(battedballs$events, levels = c("single", "field_out"))

# Confirm which level of battedballs$events has been assigned to 0.

contrasts(as.factor(battedballs$events)) 

# Center the predictor variables to reduce non-essential multicollinearity among the predictors and 
# improve the interpretation of their coefficients.

battedballs <- battedballs %>% mutate(c_launch_angle = launch_angle - mean(launch_angle))
battedballs <- battedballs %>% mutate(c_launch_speed = launch_speed - mean(launch_speed))
battedballs <- battedballs %>% mutate(c_spray_angle_Kolp = spray_angle_Kolp - mean(spray_angle_Kolp))
battedballs <- battedballs %>% mutate(c_spray_angle_adj = spray_angle_adj - mean(spray_angle_adj))
battedballs <- battedballs %>% mutate(c_hp_to_1b = hp_to_1b - mean(hp_to_1b))
battedballs <- battedballs %>% mutate(c_if_alignment = as.numeric(num_if_alignment) - mean(as.numeric(num_if_alignment)))

# Standardize the predictor variables.

battedballs <- battedballs %>% mutate(s_launch_angle = c_launch_angle / sd(launch_angle))
battedballs <- battedballs %>% mutate(s_launch_speed = c_launch_speed / sd(launch_speed))
battedballs <- battedballs %>% mutate(s_spray_angle_Kolp = c_spray_angle_Kolp / sd(spray_angle_Kolp))
battedballs <- battedballs %>% mutate(s_spray_angle_adj = c_spray_angle_adj / sd(spray_angle_adj))
battedballs <- battedballs %>% mutate(s_hp_to_1b = c_hp_to_1b / sd(hp_to_1b))
battedballs <- battedballs %>% mutate(s_if_alignment = c_if_alignment / sd(as.numeric(num_if_alignment)))

# Normalize the predictor variables.

battedballs <- battedballs %>% 
  mutate(n_launch_angle = (launch_angle - min(launch_angle)) / (max(launch_angle) - min(launch_angle)))
battedballs <- battedballs %>% 
  mutate(n_launch_speed = (launch_speed - min(launch_speed)) / (max(launch_speed) - min(launch_speed)))
battedballs <- battedballs %>% 
  mutate(n_spray_angle_Kolp = (spray_angle_Kolp - min(spray_angle_Kolp)) / (max(spray_angle_Kolp) - min(spray_angle_Kolp))) 
battedballs <- battedballs %>% 
  mutate(n_spray_angle_adj = (spray_angle_adj - min(spray_angle_adj)) / (max(spray_angle_adj) - min(spray_angle_adj))) 
battedballs <- battedballs %>% 
  mutate(n_hp_to_1b = (hp_to_1b - min(hp_to_1b)) / (max(hp_to_1b) - min(hp_to_1b))) 
battedballs <- battedballs %>% 
  mutate(n_if_alignment = (num_if_alignment - min(num_if_alignment)) / (max(num_if_alignment) - min(num_if_alignment))) 

# Partion battedballs.

set.seed(1)
test_index <- createDataPartition(y = battedballs$events, times = 1, p = 0.20, 
                                  list = FALSE)
train_set <- battedballs[-test_index,]
test_set <- battedballs[test_index,]

#################################################
# Build and Assess a Baseline Model
#################################################

# Develop a baseline model that guesses the outcomes in the test_set.

set.seed(1)
bl_preds <- sample(c("single", "field_out"), size = length(test_index), 
                   replace = TRUE, prob = c(0.25, 0.75)) %>% 
    factor(levels = levels(test_set$events))
(bl_confusionM <- confusionMatrix(data = bl_preds, reference = test_set$events))
table(predicted = bl_preds, actual = test_set$events)
(bl_tpr <- round(sensitivity(bl_preds, reference = factor(test_set$events)), 6))
(bl_tnr <- round(specificity(bl_preds, reference = factor(test_set$events)), 6))
(bl_fpr <- round(1 - bl_tnr, 6))
(bl_truescore <- round((2 * bl_tpr * bl_tnr) / (bl_tpr + bl_tnr) ,6))

#################################################
# Build and Assess a Logistic Regression Model
#################################################

# Confirm which level of train_set$events has been assigned to 0.

contrasts(as.factor(train_set$events))

# Fit a model to train_set$events using logistic regression.

fit_logit <- train_set %>% mutate(y = as.numeric(events == "field_out")) %>% 
  glm(y ~ s_launch_angle + s_launch_speed + s_spray_angle_Kolp + s_spray_angle_adj + 
        s_hp_to_1b + s_if_alignment, data = ., family = "binomial")

# Make sure each regression coefficient is statistically significant (p < .05).

summary(fit_logit)
exp(coef(fit_logit))
exp(confint(fit_logit))

# Using our logistic regression model (fit_logit), estimate the probability
# of a single for each observation (batted ball) in test_set.

p_hat_logit <- predict(fit_logit, newdata = test_set, type = "response") 
test_set <- add_column(test_set, p_hat_logit)

# Apply a decision rule allowing the model to predict whether a particular
# batted ball in test_set will result in a single.

  # Determine the cutoff in the decision rule by identifying the cutoff (probability) 
  # that maximizes the truescore.

cutoffs <- test_set$p_hat_logit
truescores <- map_dbl(cutoffs, function(x){
  truescore_preds <- ifelse(test_set$p_hat_logit >= x, "single", "field_out") %>% 
    factor(levels = levels(test_set$events))
  truescore_tprs <- round(sensitivity(truescore_preds, reference = factor(test_set$events)), 6)
  truescore_tnrs <- round(specificity(truescore_preds, reference = factor(test_set$events)), 6)
  truescores <- round((2 * truescore_tprs * truescore_tnrs) / (truescore_tprs + truescore_tnrs) ,6) 
})
test_set <- mutate(test_set, truescores = truescores)
(max_truescore <- max(truescores))
(max_truescore_cutoff <- round(cutoffs[which.max(truescores)], 6))
qplot(x = cutoffs, y = truescores) + annotate("text", x = 0.593, y = 0.39, 
                                              label = "Max. Truescore (0.4081) at Cutoff (0.7500)")

  # Apply a decision rule based on the cutoff that maximizes the truescore.

max_truescore_preds <- ifelse(p_hat_logit >= max_truescore_cutoff, "single", "field_out") %>% 
  factor(levels = levels(test_set$events))
test_set <- add_column(test_set, max_truescore_preds)
(max_truescore_tpr <- round(sensitivity(max_truescore_preds, reference = factor(test_set$events)), 6))
(max_truescore_tnr <- round(specificity(max_truescore_preds, reference = factor(test_set$events)), 6))
(max_truescore_fpr <- round(1 - max_truescore_tnr, 6))


  # Determine the cutoff in the decision rule by identifying the probability that 
  # minimizes the distance from the model's ROC curve to the coordinate (0, 1).

pred <- prediction(test_set$p_hat_logit, test_set$events, label.ordering = c("field_out", "single"))
perf <- performance(pred, measure="tpr", x.measure="fpr")
rocr_perf_elements <- tibble(cutoffs = perf@alpha.values[[1]], 
                             FPR = round(perf@x.values[[1]], 6), 
                             TPR = round(perf@y.values[[1]], 6))
rocr_perf_elements <- rocr_perf_elements %>% mutate(distance = sqrt((1 - TPR)^2 + (FPR)^2))

(min_distance <- round(min(rocr_perf_elements$distance), 6))
(min_distance_cutoff <- round(rocr_perf_elements$cutoffs[which.min(rocr_perf_elements$distance)], 6))
(min_distance_tpr <- round(rocr_perf_elements$TPR[which.min(rocr_perf_elements$distance)], 6))
(min_distance_fpr <- round(rocr_perf_elements$FPR[which.min(rocr_perf_elements$distance)], 6))
(min_distance_tnr <- round(1 - min_distance_fpr, 6))

# Create a ROC curve visualizing the optimal cutoff points identified 
# by our two methods: Max Truescore and Min Distance.

plot(perf, main = "ROC Curve Showing Single-Out TPR-FPR Trade-Offs 
     at Max Truescore Cutoff and Min Distance Cutoff", 
     print.cutoffs.at = c(0.749984, 0.764628, 0.8213, 0.7022, 0.6646), 
     cutoff.label.function = function(x) { round(x, 4) }, 
     points.pch = c(15, 17, 21, 21, 21), 
     points.col = c("dark red", "blue", "black", "black", "black"), 
     points.cex = c(1.8, 1.8, 1.8, 1.8, 1.8), text.font = c(2, 2, 2, 2, 2), 
     text.pos = c(4, 2, 4, 4, 4), 
     cutoff.label.text = c("Max Truescore", "Min Distance", "ref", "ref", "ref"), 
     xaxis.tck = 1, yaxis.tck = 1, xaxis.col.tick = "light gray", 
     yaxis.col.tick = "light gray") 
legend("topleft", inset = 0.1, title = "Cutoff Method", 
       legend = c("Max Truescore", "ROC Min Distance to (0, 1)"), 
       pch = c(15, 17), col = c("dark red", "blue"))

# Compute the truescore of the optimal cutoff point identified by minimizing the distance.

(min_distance_truescore <- round((2 * min_distance_tpr * min_distance_tnr) / (min_distance_tpr + min_distance_tnr), 6))

# Compute the distance of the optimal cutoff point identified by maximizing the truescore.

(max_truescore_distance <- round(sqrt((1 - max_truescore_tpr)^2 + (max_truescore_fpr)^2), 6))

# Create a table displaying the assessment results obtained thus far.

assessment_results <- tibble(Model = c("Baseline", "Log. Regression", "Log. Regression"), 
                               `Cutoff Method` = c(NA, "Max Truescore", "ROC Min Distance"),
                               `Truescore` = c(bl_truescore, max_truescore, min_distance_truescore), 
                               TPR = c(bl_tpr, max_truescore_tpr, min_distance_tpr),
                               TNR = c(bl_tnr, max_truescore_tnr, min_distance_tnr), 
                               ROC_Distance = c(NA, max_truescore_distance, min_distance), 
                               FPR = c((1 - bl_tnr), max_truescore_fpr, min_distance_fpr), 
                               `Best Cutoff` = c(NA, max_truescore_cutoff, min_distance_cutoff))
knitr::kable(assessment_results[1:3, ], caption = "Assessment Results - First Logistic Regression Model")

# Fit a refined logistic regression model (r_fit_logit) with interaction terms to 
# train_set$events.

r_fit_logit <- train_set %>% mutate(y = as.numeric(events == "field_out")) %>% 
  glm(y ~ s_launch_angle + s_launch_speed + s_spray_angle_Kolp + 
        s_spray_angle_adj + s_hp_to_1b + s_if_alignment + 
        s_spray_angle_Kolp:s_if_alignment + s_spray_angle_adj:s_if_alignment,  
      data = ., family = "binomial")

# Make sure each regression coefficient is statistically significant (p < .05).

summary(r_fit_logit)
exp(coef(r_fit_logit))
exp(confint(r_fit_logit))

# Create a conditional effect plot to visualize how infield alignment affects 
# the coefficient for s_spray_angle_adj.

interplot(m = r_fit_logit, var1 = "s_spray_angle_adj", var2 = "s_if_alignment") + 
  geom_point(color = "dark red") + 
  ggtitle("Estimated Coefficient for Spray Angle Effect on Single,  
          by Alignment of Infielders") + 
  theme(plot.title = element_text(face = "bold")) + 
  xlab("Infield Alignment Effect (Standard, Strategic, Shift)") + 
  ylab("Estimated Coefficient for s_spray_angle_adj") + 
  theme_bw()

# Create a conditional effect plot to visualize how infield alignment affects 
# the coefficient for s_spray_angle_Kolp.

interplot(m = r_fit_logit, var1 = "s_spray_angle_Kolp", var2 = "s_if_alignment") + 
  geom_point(color = "dark red") + 
  ggtitle("Estimated Coefficient for Spray Angle Effect on Single,  
          by Alignment of Infielders") + 
  theme(plot.title = element_text(face = "bold")) + 
  xlab("Infield Alignment Effect (Standard, Strategic, Shift)") + 
  ylab("Estimated Coefficient for s_spray_angle_Kolp") + 
  theme_bw()

# Using our refined logistic regression model (r_fit_logit), estimate the probability
# of a single for each observation (batted ball) in test_set.

r_p_hat_logit <- predict(r_fit_logit, newdata = test_set, type = "response") 
test_set <- add_column(test_set, r_p_hat_logit)

# Apply a decision rule allowing the refined model to predict whether a particular
# batted ball in test_set will result in a single.

  # Determine the cutoff in the decision rule by identifying the cutoff (probability) 
  # that maximizes the truescore.

r_cutoffs <- test_set$r_p_hat_logit
r_truescores <- map_dbl(r_cutoffs, function(x){
  r_truescore_preds <- ifelse(test_set$r_p_hat_logit >= x, "single", "field_out") %>% 
    factor(levels = levels(test_set$events))
  r_truescore_tprs <- round(sensitivity(r_truescore_preds, reference = factor(test_set$events)), 6)
  r_truescore_tnrs <- round(specificity(r_truescore_preds, reference = factor(test_set$events)), 6)
  r_truescores <- round((2 * r_truescore_tprs * r_truescore_tnrs) / (r_truescore_tprs + r_truescore_tnrs) ,6) 
})
test_set <- mutate(test_set, r_truescores = r_truescores)
(r_max_truescore <- max(r_truescores))
(r_max_truescore_cutoff <- round(r_cutoffs[which.max(r_truescores)], 6))
qplot(x = r_cutoffs, y = r_truescores) + annotate("text", x = 0.575, y = 0.39, 
                                                  label = "Max. Truescore (0.4077) at Cutoff (0.7509)")

  # Apply a decision rule based on the cutoff that maximizes the truescore.

r_max_truescore_preds <- ifelse(r_p_hat_logit >= r_max_truescore_cutoff, "single", "field_out") %>% 
  factor(levels = levels(test_set$events))
test_set <- add_column(test_set, r_max_truescore_preds)
(r_max_truescore_tpr <- round(sensitivity(r_max_truescore_preds, reference = factor(test_set$events)), 6))
(r_max_truescore_tnr <- round(specificity(r_max_truescore_preds, reference = factor(test_set$events)), 6))
(r_max_truescore_fpr <- round(1 - r_max_truescore_tnr, 6))

  # Determine the cutoff in the decision rule by identifying the probability that 
  # minimizes the distance from the model's ROC curve to the coordinate (0, 1). Then apply
  # a decision rule using that probability (cutoff).

r_pred <- prediction(test_set$r_p_hat_logit, test_set$events, label.ordering = c("field_out", "single"))
r_perf <- performance(r_pred, measure="tpr", x.measure="fpr")
r_rocr_perf_elements <- tibble(r_cutoffs = r_perf@alpha.values[[1]], 
                               r_FPR = round(r_perf@x.values[[1]], 6), 
                               r_TPR = round(r_perf@y.values[[1]], 6))
r_rocr_perf_elements <- r_rocr_perf_elements %>% mutate(r_distance = sqrt((1 - r_TPR)^2 + (r_FPR)^2))

(r_min_distance <- round(min(r_rocr_perf_elements$r_distance), 6))
(r_min_distance_cutoff <- round(r_rocr_perf_elements$r_cutoffs[which.min(r_rocr_perf_elements$r_distance)], 6))
(r_min_distance_tpr <- round(r_rocr_perf_elements$r_TPR[which.min(r_rocr_perf_elements$r_distance)], 6))
(r_min_distance_fpr <- round(r_rocr_perf_elements$r_FPR[which.min(r_rocr_perf_elements$r_distance)], 6))
(r_min_distance_tnr <- round(1 - r_min_distance_fpr, 6))

# Create a ROC curve allowing us to visualize the optimal cutoff points identified 
# by our two methods: Max Truescore and Min Distance.

plot(r_perf, main = "ROC Curve (Refined Model):
Single-Out TPR-FPR Trade-Offs at Max Truescore Cutoff 
     and Min Distance Cutoff", 
     print.cutoffs.at = c(0.750887, 0.766355, 0.6610, 0.6994, 0.8240), 
     cutoff.label.function = function(x) { round(x, 4) }, 
     points.pch = c(15, 17, 21, 21, 21), 
     points.col = c("dark red", "blue", "black", "black", "black"), 
     points.cex = c(1.8, 1.8, 1.8, 1.8, 1.8), text.font = c(2, 2, 2, 2, 2), 
     text.pos = c(4, 2, 4, 4, 4), 
     cutoff.label.text = c("Max Truescore", "Min Distance", "ref", "ref", "ref"), 
     xaxis.tck = 1, yaxis.tck = 1, xaxis.col.tick = "light gray", 
     yaxis.col.tick = "light gray") 
legend("topleft", inset = 0.1, title = "Cutoff Method", 
       legend = c("Max Truescore", "ROC Min Distance to (0, 1)"), 
       pch = c(15, 17), col = c("dark red", "blue"))

# Compute the truescore of the optimal cutoff point identified by minimizing the distance.

(r_min_distance_truescore <- round((2 * r_min_distance_tpr * r_min_distance_tnr) / (r_min_distance_tpr + r_min_distance_tnr), 6))

# Compute the distance of the optimal point identified by maximizing the weighted truescore.

(r_max_truescore_distance <- round(sqrt((1 - r_max_truescore_tpr)^2 + (r_max_truescore_fpr)^2), 6))

# Create a table displaying all of the assessment results.

r_assessment_results <- tibble(Model = c("Baseline", "Log Regression", "Log Regression", 
                                         "Log Regression", "Log Regression"), 
                               `Cutoff Method` = c(NA, "Max Truescore", "ROC Min Distance", 
                                                   "Max Truescore (refined)", "ROC Distance (ref)"),
                               `Truescore` = c(bl_truescore, max_truescore, min_distance_truescore, 
                                               r_max_truescore, r_min_distance_truescore), 
                               TPR = c(bl_tpr, max_truescore_tpr, min_distance_tpr, r_max_truescore_tpr, 
                                       r_min_distance_tpr),
                               TNR = c(bl_tnr, max_truescore_tnr, min_distance_tnr, r_max_truescore_tnr, 
                                       r_min_distance_tnr), 
                               `ROC Distance` = c("na", max_truescore_distance, min_distance, 
                                                  r_max_truescore_distance, r_min_distance), 
                               FPR = c((1 - bl_tnr), max_truescore_fpr, min_distance_fpr, 
                                       r_max_truescore_fpr, r_min_distance_fpr), 
                               `Best Cutoff` = c("na", max_truescore_cutoff, min_distance_cutoff, 
                                                 r_max_truescore_cutoff, r_min_distance_cutoff))
knitr::kable(r_assessment_results[1:5, ], caption = "Assessment Results - Refined Logistic Regression Model")

#####################################################
# Build and Assess a K-Nearest Neighbors (knn) Model
#####################################################

# Build a knn model with the default k=5. Prepare the train and test sets, 
# define the response vector and matrix of predictors, and fit the model 
# using the knn3 function.

train_set2 <- dplyr::select(train_set, -(hit_distance_sc))
test_set2 <- dplyr::select(test_set, -(hit_distance_sc), -(p_hat_logit), -(truescores), 
                           -(max_truescore_preds), -(r_p_hat_logit), -(r_truescores), -(r_max_truescore_preds))
x <- as.matrix(train_set2[, c("n_launch_angle", "n_launch_speed", "n_spray_angle_Kolp", "n_spray_angle_adj", 
                              "n_hp_to_1b", "n_if_alignment")])
y <- factor(train_set2$events)
knn_fit_k5 <- knn3(x, y, k = 5)

# Test the knn_fit_k5 model. Define the matrix of new data (test_set) and use it to predict the outcomes.

z <- as.matrix(test_set2[, c("n_launch_angle", "n_launch_speed", "n_spray_angle_Kolp", "n_spray_angle_adj", 
                             "n_hp_to_1b", "n_if_alignment")])
knn_y_hat <- predict(knn_fit_k5, z, type = "class")
knn_y_hat_prob <- predict(knn_fit_k5, z, type = "prob")

# Assess the predictions using both the Truescore method and the Minimum Distance method. 
# Compare the performance of knn_fit_k5 to that of the logistic regression models.

confusionMatrix(data = knn_y_hat, reference = test_set2$events)
(knn_k5_tpr <- round(sensitivity(knn_y_hat, reference = factor(test_set2$events)), 6))
(knn_k5_tnr <- round(specificity(knn_y_hat, reference = factor(test_set2$events)), 6))
(knn_k5_fpr <- round(1 - knn_k5_tnr, 6))
(knn_k5_truescore <- round((2 * knn_k5_tpr * knn_k5_tnr) / (knn_k5_tpr + knn_k5_tnr) , 6))
(knn_k5_distance <- round(sqrt((1 - knn_k5_tpr)^2 + (knn_k5_fpr)^2), 6))

assessment_results <- tibble(Model = c("Baseline", "Log. Regression", "Log. Regression", "KNN_k5"), 
                             `Cutoff Method` = c(NA, "Truescore", "ROC Distance", NA),
                             `Truescore` = c(bl_truescore, max_truescore, min_distance_truescore, knn_k5_truescore), 
                             TPR = c(bl_tpr, max_truescore_tpr, min_distance_tpr, knn_k5_tpr),
                             TNR = c(bl_tnr, max_truescore_tnr, min_distance_tnr, knn_k5_tnr), 
                             ROC_Distance = c(NA, max_truescore_distance, min_distance, knn_k5_distance), 
                             FPR = c((1 - bl_tnr), max_truescore_fpr, min_distance_fpr, knn_k5_fpr), 
                             `Best Cutoff` = c(NA, max_truescore_cutoff, min_distance_cutoff, NA))
knitr::kable(assessment_results[1:4, ], caption = "Assessment Results")

################################

# Use caret::train() to develop a knn algorithm with an optimal value 
# for k and the default decision threshold of 0.50. The optimal value 
# of k should maximize the truescore of the model.

train_set2 <- dplyr::select(train_set, -(hit_distance_sc))
test_set2 <- dplyr::select(test_set, -(hit_distance_sc), -(p_hat_logit), -(truescores), 
                           -(max_truescore_preds), -(r_p_hat_logit), -(r_truescores), -(r_max_truescore_preds))

x <- as.matrix(train_set2[, c("n_launch_angle", "n_launch_speed", "n_spray_angle_Kolp", "n_spray_angle_adj", 
                              "n_hp_to_1b", "n_if_alignment")])
y <- train_set2$events

# Use the train function and cross-validation to predict "single" 
# or "field_out" for each value of k from 3 to 39, by twos.

set.seed(1)
fitControl <- trainControl(method = "cv", number = 10, p = 0.8, returnData = TRUE,
                           returnResamp = "all", savePredictions = "all",
                           summaryFunction = twoClassSummary, classProbs = TRUE)

knn_train <- train(x, y, method = "knn",
                   tuneGrid = data.frame(k = seq(3, 39, 2)),
                   trControl = fitControl)

# Extract the average sensitivity and specificity for each value 
# of k, compute the corresponding Truescore and Distance, and 
# identify the value of k that maximizes the Truescore and 
# minimizes the Distance to (0, 1).

knn_train_cms <- as_tibble(knn_train$results)
knn_train_cms <- knn_train_cms %>% dplyr::select(k, Sens, Spec) %>% 
  mutate(Truescore = round((2 * Sens * Spec) / (Sens + Spec) , 6)) %>% 
  mutate(Distance = round(sqrt((1 - Sens)^2 + (1 - Spec)^2), 6))

max(knn_train_cms$Truescore)
knn_train_cms$k[which.max(knn_train_cms$Truescore)]

min(knn_train_cms$Distance)
knn_train_cms$k[which.min(knn_train_cms$Distance)]

# Fit an optimized knn model (k = 7) to the entire predictor matrix and outcome vector.

knn_fit_k7 <- knn3(x, y, k = 7)

# Apply our latest knn model (knn_fit_k7) to predict 
# outcomes based on the predictors in test_set2.

knn_y_hat_class <- predict(knn_fit_k7, z, type = "class")
knn_y_hat_class_tbl <- as_tibble(knn_y_hat_class)
knn_y_hat_class_tbl <- knn_y_hat_class_tbl %>% mutate(preds = value) %>% dplyr::select(preds)

# Assess the predictive ability of our latest knn model (knn_fit_k7).

confusionMatrix(data = knn_y_hat_class_tbl$preds, reference = test_set2$events)
(knn_k7_tpr <- round(sensitivity(knn_y_hat_class_tbl$preds, reference = factor(test_set2$events)), 6))
(knn_k7_tnr <- round(specificity(knn_y_hat_class_tbl$preds, reference = factor(test_set2$events)), 6))
(knn_k7_fpr <- round(1 - knn_k7_tnr, 6))
(knn_k7_truescore <- round((2 * knn_k7_tpr * knn_k7_tnr) / (knn_k7_tpr + knn_k7_tnr) , 6))
(knn_k7_distance <- round(sqrt((1 - knn_k7_tpr)^2 + (knn_k7_fpr)^2), 6))

assessment_results <- tibble(Model = c("Baseline", "Logistic Regression", "Logistic Regression", 
                                       "KNN_k5", "KNN_k7"), 
                             `Cutoff Method` = c(NA, "Truescore", "ROC Distance to (0,1)", 
                                                 "default (0.50)", "default (0.50)"),
                             `Truescore` = c(bl_truescore, max_truescore, min_distance_truescore, 
                                             knn_k5_truescore, knn_k7_truescore), 
                             TPR = c(bl_tpr, max_truescore_tpr, min_distance_tpr, 
                                     knn_k5_tpr, knn_k7_tpr),
                             TNR = c(bl_tnr, max_truescore_tnr, min_distance_tnr, 
                                     knn_k5_tnr, knn_k7_tnr), 
                             ROC_Distance = c(NA, max_truescore_distance, min_distance, 
                                              knn_k5_distance, knn_k7_distance), 
                             FPR = c((1 - bl_tnr), max_truescore_fpr, min_distance_fpr, 
                                     knn_k5_fpr, knn_k7_fpr), 
                             `Best Cutoff` = c(NA, max_truescore_cutoff, min_distance_cutoff, 
                                               "default", "default"))
knitr::kable(assessment_results[1:5, ], caption = "Assessment Results")

# Determine the optimal cutoff in the decision rule by identifying the decision 
# threshold that maximizes the Truescore, given a value of k equal to 7.

knn_y_hat_prob <- predict(knn_fit_k7, z, type = "prob")
knn_y_hat_prob_tbl <- as_tibble(knn_y_hat_prob)
knn_cutoffs <- c(0.25, 0.40, 0.55)
knn_truescores <- map_dbl(knn_cutoffs, function(x){
  truescore_preds <- ifelse(knn_y_hat_prob_tbl$single > x, "single", 
                            "field_out") %>% 
    factor(levels = levels(test_set2$events))
  truescore_tprs <- round(sensitivity(truescore_preds, 
                                      reference = factor(test_set2$events)), 6)
  truescore_tnrs <- round(specificity(truescore_preds, 
                                      reference = factor(test_set2$events)), 6)
  truescores <- round((2 * truescore_tprs * truescore_tnrs) / 
                        (truescore_tprs + truescore_tnrs), 6) 
})

(knn_max_truescore <- max(knn_truescores))
(knn_max_truescore_cutoff <- round(knn_cutoffs[which.max(knn_truescores)], 6))
knn_decision_thresholds <- tibble(Cutoffs = knn_cutoffs, 
                                  Truescore = knn_truescores)
knitr::kable(knn_decision_thresholds[1:3, ], caption = "Decision Thresholds")

# Determine the cutoff in the decision rule by identifying the decision 
# threshold that minimizes the distance to the coordinate (0, 1).

knn_distances <- map_dbl(knn_cutoffs, function(x){
  distance_preds <- ifelse(knn_y_hat_prob_tbl$single > x, "single", 
                           "field_out") %>% 
    factor(levels = levels(test_set2$events))
  distance_tprs <- round(sensitivity(distance_preds, 
                                     reference = factor(test_set2$events)), 6)
  distance_fprs <- round(1 - specificity(distance_preds, 
                                         reference = factor(test_set2$events)), 6)
  distances <- round(sqrt((1 - distance_tprs)^2 + (distance_fprs)^2), 6) 
})

(knn_min_distance <- round(min(knn_distances), 6))
(knn_min_distance_cutoff <- round(knn_cutoffs[which.min(knn_distances)], 6))
knn_decision_thresholds <- tibble(Cutoffs = knn_cutoffs, 
                                  Truescore = knn_truescores, 
                                  Distance = knn_distances)
knitr::kable(knn_decision_thresholds[1:3, ], caption = "Decision Thresholds")

# Assess the predictive ability of our latest knn model (knn_fit_k7 with a 
# neighborhood of 7 and a decision threshold of 3 votes to predict a single).

knn_k7c40_cutoff <- knn_min_distance_cutoff
preds_k7c40 <- ifelse(knn_y_hat_prob_tbl$single > knn_k7c40_cutoff, "single", "field_out")
preds_k7c40 <- factor(preds_k7c40, levels = c("single", "field_out"))

confusionMatrix(data = preds_k7c40, reference = factor(test_set2$events))
(knn_k7c40_tpr <- round(sensitivity(preds_k7c40, reference = factor(test_set2$events)), 6))
(knn_k7c40_tnr <- round(specificity(preds_k7c40, reference = factor(test_set2$events)), 6))
(knn_k7c40_fpr <- round(1 - knn_k7c40_tnr, 6))
(knn_k7c40_truescore <- round((2 * knn_k7c40_tpr * knn_k7c40_tnr) / (knn_k7c40_tpr + knn_k7c40_tnr) , 6))
(knn_k7c40_distance <- round(sqrt((1 - knn_k7c40_tpr)^2 + (knn_k7c40_fpr)^2), 6))

assessment_results <- tibble(Model = c("Baseline", "Logistic Regression", "Logistic Regression", 
                                       "KNN_k5", "KNN_k7", "KNN_k7c40"), 
                             `Cutoff Method` = c(NA, "Truescore", "ROC Distance to (0,1)", 
                                                 "default (0.50)", "default (0.50)", "both"),
                             `Truescore` = c(bl_truescore, max_truescore, min_distance_truescore, 
                                             knn_k5_truescore, knn_k7_truescore, knn_k7c40_truescore), 
                             TPR = c(bl_tpr, max_truescore_tpr, min_distance_tpr, 
                                     knn_k5_tpr, knn_k7_tpr, knn_k7c40_tpr),
                             TNR = c(bl_tnr, max_truescore_tnr, min_distance_tnr, 
                                     knn_k5_tnr, knn_k7_tnr, knn_k7c40_tnr), 
                             ROC_Distance = c(NA, max_truescore_distance, min_distance, 
                                              knn_k5_distance, knn_k7_distance, knn_k7c40_distance), 
                             FPR = c((1 - bl_tnr), max_truescore_fpr, min_distance_fpr, 
                                     knn_k5_fpr, knn_k7_fpr, knn_k7c40_fpr), 
                             `Best Cutoff` = c(NA, max_truescore_cutoff, min_distance_cutoff, 
                                               "default", "default", knn_k7c40_cutoff))
knitr::kable(assessment_results[1:6, ], caption = "Assessment Results")

################################

# Use caret::train() to develop a knn algorithm with an optimal combination of values for k 
# and the decision threshold. The optimal values of k and the decision threshold should 
# maximize the truescore of the model.

train_set2 <- dplyr::select(train_set, -(hit_distance_sc))
test_set2 <- dplyr::select(test_set, -(hit_distance_sc), -(p_hat_logit), -(truescores), 
                           -(max_truescore_preds), -(r_p_hat_logit), -(r_truescores), -(r_max_truescore_preds))

x <- as.matrix(train_set2[, c("n_launch_angle", "n_launch_speed", "n_spray_angle_Kolp", "n_spray_angle_adj", 
                                                      "n_hp_to_1b", "n_if_alignment")])
y <- train_set2$events


  # Use the train function and cross-validation to compute the probabilities of "single" 
  # for a range of k's.

set.seed(1)
fitControl <- trainControl(method = "cv", number = 10, p = 0.8, returnData = TRUE,
                           returnResamp = "all", savePredictions = "all",
                           summaryFunction = twoClassSummary, classProbs = TRUE, 
                           verboseIter = TRUE)

knn_train <- train(x, y, method = "knn",
                   tuneGrid = data.frame(k = seq(3, 39, 2)),
                   trControl = fitControl)
  
  # Generate predictions by applying a range of thresholds (cutoffs) to the 
  # probabilities for each value of k.

knn_train_pred_tib <- as_tibble(knn_train$pred)
knn_train_pred_tib <- knn_train_pred_tib %>% 
  mutate(pred25 = ifelse(knn_train_pred_tib$single > 0.25, "single", "field_out"))
knn_train_pred_tib <- knn_train_pred_tib %>% 
  mutate(pred26 = ifelse(knn_train_pred_tib$single > 0.26, "single", "field_out"))
knn_train_pred_tib <- knn_train_pred_tib %>% 
  mutate(pred27 = ifelse(knn_train_pred_tib$single > 0.27, "single", "field_out"))
knn_train_pred_tib <- knn_train_pred_tib %>% 
  mutate(pred28 = ifelse(knn_train_pred_tib$single > 0.28, "single", "field_out"))
knn_train_pred_tib <- knn_train_pred_tib %>% 
  mutate(pred29 = ifelse(knn_train_pred_tib$single > 0.29, "single", "field_out"))
knn_train_pred_tib <- knn_train_pred_tib %>% 
  mutate(pred30 = ifelse(knn_train_pred_tib$single > 0.30, "single", "field_out"))
knn_train_pred_tib <- knn_train_pred_tib %>% 
  mutate(pred31 = ifelse(knn_train_pred_tib$single > 0.31, "single", "field_out"))
knn_train_pred_tib <- knn_train_pred_tib %>% 
  mutate(pred32 = ifelse(knn_train_pred_tib$single > 0.32, "single", "field_out"))
knn_train_pred_tib <- knn_train_pred_tib %>% 
  mutate(pred33 = ifelse(knn_train_pred_tib$single > 0.33, "single", "field_out"))
knn_train_pred_tib <- knn_train_pred_tib %>% 
  mutate(pred34 = ifelse(knn_train_pred_tib$single > 0.34, "single", "field_out"))
knn_train_pred_tib <- knn_train_pred_tib %>% 
  mutate(pred35 = ifelse(knn_train_pred_tib$single > 0.35, "single", "field_out"))
knn_train_pred_tib <- knn_train_pred_tib %>% 
  mutate(pred36 = ifelse(knn_train_pred_tib$single > 0.36, "single", "field_out"))
knn_train_pred_tib <- knn_train_pred_tib %>% 
  mutate(pred37 = ifelse(knn_train_pred_tib$single > 0.37, "single", "field_out"))
knn_train_pred_tib <- knn_train_pred_tib %>% 
  mutate(pred38 = ifelse(knn_train_pred_tib$single > 0.38, "single", "field_out"))
knn_train_pred_tib <- knn_train_pred_tib %>% 
  mutate(pred39 = ifelse(knn_train_pred_tib$single > 0.39, "single", "field_out"))

  # Convert all character columns to factors using dplyr package 
  # (https://gist.github.com/ramhiser/character2factor.r).

knn_train_pred_tib_f <- knn_train_pred_tib %>% 
  mutate_if(sapply(knn_train_pred_tib, is.character), as.factor)
rm(knn_train_pred_tib)
gc(reset = TRUE)
sapply(knn_train_pred_tib_f, class)

  # Make sure all of the factor outcomes have the same levels in the same order.

knn_train_pred_tib_f$pred25 <- factor(knn_train_pred_tib_f$pred25, levels = c("single", "field_out"))
knn_train_pred_tib_f$pred26 <- factor(knn_train_pred_tib_f$pred26, levels = c("single", "field_out"))
knn_train_pred_tib_f$pred27 <- factor(knn_train_pred_tib_f$pred27, levels = c("single", "field_out"))
knn_train_pred_tib_f$pred28 <- factor(knn_train_pred_tib_f$pred28, levels = c("single", "field_out"))                                      
knn_train_pred_tib_f$pred29 <- factor(knn_train_pred_tib_f$pred29, levels = c("single", "field_out"))
knn_train_pred_tib_f$pred30 <- factor(knn_train_pred_tib_f$pred30, levels = c("single", "field_out"))   
knn_train_pred_tib_f$pred31 <- factor(knn_train_pred_tib_f$pred31, levels = c("single", "field_out"))                                      
knn_train_pred_tib_f$pred32 <- factor(knn_train_pred_tib_f$pred32, levels = c("single", "field_out"))
knn_train_pred_tib_f$pred33 <- factor(knn_train_pred_tib_f$pred33, levels = c("single", "field_out"))                                      
knn_train_pred_tib_f$pred34 <- factor(knn_train_pred_tib_f$pred34, levels = c("single", "field_out"))                                      
knn_train_pred_tib_f$pred35 <- factor(knn_train_pred_tib_f$pred35, levels = c("single", "field_out"))                                      
knn_train_pred_tib_f$pred36 <- factor(knn_train_pred_tib_f$pred36, levels = c("single", "field_out"))                                      
knn_train_pred_tib_f$pred37 <- factor(knn_train_pred_tib_f$pred37, levels = c("single", "field_out"))                                      
knn_train_pred_tib_f$pred38 <- factor(knn_train_pred_tib_f$pred38, levels = c("single", "field_out"))                                      
knn_train_pred_tib_f$pred39 <- factor(knn_train_pred_tib_f$pred39, levels = c("single", "field_out"))                                      

sapply(knn_train_pred_tib_f, levels)

  # Convert knn_train_pred_tib_f$Resample into a numeric type.

knn_train_pred_tib_f$Resample <- as.numeric(knn_train_pred_tib_f$Resample)

  # Form separate tibbles for each combination of k and fold (Resample).

k3f1  <- knn_train_pred_tib_f %>% filter(Resample == 1, k == 3)
k3f2  <- knn_train_pred_tib_f %>% filter(Resample == 2, k == 3)
k3f3  <- knn_train_pred_tib_f %>% filter(Resample == 3, k == 3)
k3f4  <- knn_train_pred_tib_f %>% filter(Resample == 4, k == 3)
k3f5  <- knn_train_pred_tib_f %>% filter(Resample == 5, k == 3)
k3f6  <- knn_train_pred_tib_f %>% filter(Resample == 6, k == 3)
k3f7  <- knn_train_pred_tib_f %>% filter(Resample == 7, k == 3)
k3f8  <- knn_train_pred_tib_f %>% filter(Resample == 8, k == 3)
k3f9  <- knn_train_pred_tib_f %>% filter(Resample == 9, k == 3)
k3f10  <- knn_train_pred_tib_f %>% filter(Resample == 10, k == 3)

k5f1  <- knn_train_pred_tib_f %>% filter(Resample == 1, k == 5)
k5f2  <- knn_train_pred_tib_f %>% filter(Resample == 2, k == 5)
k5f3  <- knn_train_pred_tib_f %>% filter(Resample == 3, k == 5)
k5f4  <- knn_train_pred_tib_f %>% filter(Resample == 4, k == 5)
k5f5  <- knn_train_pred_tib_f %>% filter(Resample == 5, k == 5)
k5f6  <- knn_train_pred_tib_f %>% filter(Resample == 6, k == 5)
k5f7  <- knn_train_pred_tib_f %>% filter(Resample == 7, k == 5)
k5f8  <- knn_train_pred_tib_f %>% filter(Resample == 8, k == 5)
k5f9  <- knn_train_pred_tib_f %>% filter(Resample == 9, k == 5)
k5f10  <- knn_train_pred_tib_f %>% filter(Resample == 10, k == 5)

k7f1  <- knn_train_pred_tib_f %>% filter(Resample == 1, k == 7)
k7f2  <- knn_train_pred_tib_f %>% filter(Resample == 2, k == 7)
k7f3  <- knn_train_pred_tib_f %>% filter(Resample == 3, k == 7)
k7f4  <- knn_train_pred_tib_f %>% filter(Resample == 4, k == 7)
k7f5  <- knn_train_pred_tib_f %>% filter(Resample == 5, k == 7)
k7f6  <- knn_train_pred_tib_f %>% filter(Resample == 6, k == 7)
k7f7  <- knn_train_pred_tib_f %>% filter(Resample == 7, k == 7)
k7f8  <- knn_train_pred_tib_f %>% filter(Resample == 8, k == 7)
k7f9  <- knn_train_pred_tib_f %>% filter(Resample == 9, k == 7)
k7f10  <- knn_train_pred_tib_f %>% filter(Resample == 10, k == 7)

k9f1  <- knn_train_pred_tib_f %>% filter(Resample == 1, k == 9)
k9f2  <- knn_train_pred_tib_f %>% filter(Resample == 2, k == 9)
k9f3  <- knn_train_pred_tib_f %>% filter(Resample == 3, k == 9)
k9f4  <- knn_train_pred_tib_f %>% filter(Resample == 4, k == 9)
k9f5  <- knn_train_pred_tib_f %>% filter(Resample == 5, k == 9)
k9f6  <- knn_train_pred_tib_f %>% filter(Resample == 6, k == 9)
k9f7  <- knn_train_pred_tib_f %>% filter(Resample == 7, k == 9)
k9f8  <- knn_train_pred_tib_f %>% filter(Resample == 8, k == 9)
k9f9  <- knn_train_pred_tib_f %>% filter(Resample == 9, k == 9)
k9f10  <- knn_train_pred_tib_f %>% filter(Resample == 10, k == 9)

k11f1  <- knn_train_pred_tib_f %>% filter(Resample == 1, k == 11)
k11f2  <- knn_train_pred_tib_f %>% filter(Resample == 2, k == 11)
k11f3  <- knn_train_pred_tib_f %>% filter(Resample == 3, k == 11)
k11f4  <- knn_train_pred_tib_f %>% filter(Resample == 4, k == 11)
k11f5  <- knn_train_pred_tib_f %>% filter(Resample == 5, k == 11)
k11f6  <- knn_train_pred_tib_f %>% filter(Resample == 6, k == 11)
k11f7  <- knn_train_pred_tib_f %>% filter(Resample == 7, k == 11)
k11f8  <- knn_train_pred_tib_f %>% filter(Resample == 8, k == 11)
k11f9  <- knn_train_pred_tib_f %>% filter(Resample == 9, k == 11)
k11f10  <- knn_train_pred_tib_f %>% filter(Resample == 10, k == 11)

k13f1  <- knn_train_pred_tib_f %>% filter(Resample == 1, k == 13)
k13f2  <- knn_train_pred_tib_f %>% filter(Resample == 2, k == 13)
k13f3  <- knn_train_pred_tib_f %>% filter(Resample == 3, k == 13)
k13f4  <- knn_train_pred_tib_f %>% filter(Resample == 4, k == 13)
k13f5  <- knn_train_pred_tib_f %>% filter(Resample == 5, k == 13)
k13f6  <- knn_train_pred_tib_f %>% filter(Resample == 6, k == 13)
k13f7  <- knn_train_pred_tib_f %>% filter(Resample == 7, k == 13)
k13f8  <- knn_train_pred_tib_f %>% filter(Resample == 8, k == 13)
k13f9  <- knn_train_pred_tib_f %>% filter(Resample == 9, k == 13)
k13f10  <- knn_train_pred_tib_f %>% filter(Resample == 10, k == 13)

k15f1  <- knn_train_pred_tib_f %>% filter(Resample == 1, k == 15)
k15f2  <- knn_train_pred_tib_f %>% filter(Resample == 2, k == 15)
k15f3  <- knn_train_pred_tib_f %>% filter(Resample == 3, k == 15)
k15f4  <- knn_train_pred_tib_f %>% filter(Resample == 4, k == 15)
k15f5  <- knn_train_pred_tib_f %>% filter(Resample == 5, k == 15)
k15f6  <- knn_train_pred_tib_f %>% filter(Resample == 6, k == 15)
k15f7  <- knn_train_pred_tib_f %>% filter(Resample == 7, k == 15)
k15f8  <- knn_train_pred_tib_f %>% filter(Resample == 8, k == 15)
k15f9  <- knn_train_pred_tib_f %>% filter(Resample == 9, k == 15)
k15f10  <- knn_train_pred_tib_f %>% filter(Resample == 10, k == 15)

k17f1  <- knn_train_pred_tib_f %>% filter(Resample == 1, k == 17)
k17f2  <- knn_train_pred_tib_f %>% filter(Resample == 2, k == 17)
k17f3  <- knn_train_pred_tib_f %>% filter(Resample == 3, k == 17)
k17f4  <- knn_train_pred_tib_f %>% filter(Resample == 4, k == 17)
k17f5  <- knn_train_pred_tib_f %>% filter(Resample == 5, k == 17)
k17f6  <- knn_train_pred_tib_f %>% filter(Resample == 6, k == 17)
k17f7  <- knn_train_pred_tib_f %>% filter(Resample == 7, k == 17)
k17f8  <- knn_train_pred_tib_f %>% filter(Resample == 8, k == 17)
k17f9  <- knn_train_pred_tib_f %>% filter(Resample == 9, k == 17)
k17f10  <- knn_train_pred_tib_f %>% filter(Resample == 10, k == 17)

k19f1  <- knn_train_pred_tib_f %>% filter(Resample == 1, k == 19)
k19f2  <- knn_train_pred_tib_f %>% filter(Resample == 2, k == 19)
k19f3  <- knn_train_pred_tib_f %>% filter(Resample == 3, k == 19)
k19f4  <- knn_train_pred_tib_f %>% filter(Resample == 4, k == 19)
k19f5  <- knn_train_pred_tib_f %>% filter(Resample == 5, k == 19)
k19f6  <- knn_train_pred_tib_f %>% filter(Resample == 6, k == 19)
k19f7  <- knn_train_pred_tib_f %>% filter(Resample == 7, k == 19)
k19f8  <- knn_train_pred_tib_f %>% filter(Resample == 8, k == 19)
k19f9  <- knn_train_pred_tib_f %>% filter(Resample == 9, k == 19)
k19f10  <- knn_train_pred_tib_f %>% filter(Resample == 10, k == 19)

k21f1  <- knn_train_pred_tib_f %>% filter(Resample == 1, k == 21)
k21f2  <- knn_train_pred_tib_f %>% filter(Resample == 2, k == 21)
k21f3  <- knn_train_pred_tib_f %>% filter(Resample == 3, k == 21)
k21f4  <- knn_train_pred_tib_f %>% filter(Resample == 4, k == 21)
k21f5  <- knn_train_pred_tib_f %>% filter(Resample == 5, k == 21)
k21f6  <- knn_train_pred_tib_f %>% filter(Resample == 6, k == 21)
k21f7  <- knn_train_pred_tib_f %>% filter(Resample == 7, k == 21)
k21f8  <- knn_train_pred_tib_f %>% filter(Resample == 8, k == 21)
k21f9  <- knn_train_pred_tib_f %>% filter(Resample == 9, k == 21)
k21f10  <- knn_train_pred_tib_f %>% filter(Resample == 10, k == 21)

k23f1  <- knn_train_pred_tib_f %>% filter(Resample == 1, k == 23)
k23f2  <- knn_train_pred_tib_f %>% filter(Resample == 2, k == 23)
k23f3  <- knn_train_pred_tib_f %>% filter(Resample == 3, k == 23)
k23f4  <- knn_train_pred_tib_f %>% filter(Resample == 4, k == 23)
k23f5  <- knn_train_pred_tib_f %>% filter(Resample == 5, k == 23)
k23f6  <- knn_train_pred_tib_f %>% filter(Resample == 6, k == 23)
k23f7  <- knn_train_pred_tib_f %>% filter(Resample == 7, k == 23)
k23f8  <- knn_train_pred_tib_f %>% filter(Resample == 8, k == 23)
k23f9  <- knn_train_pred_tib_f %>% filter(Resample == 9, k == 23)
k23f10  <- knn_train_pred_tib_f %>% filter(Resample == 10, k == 23)

k25f1  <- knn_train_pred_tib_f %>% filter(Resample == 1, k == 25)
k25f2  <- knn_train_pred_tib_f %>% filter(Resample == 2, k == 25)
k25f3  <- knn_train_pred_tib_f %>% filter(Resample == 3, k == 25)
k25f4  <- knn_train_pred_tib_f %>% filter(Resample == 4, k == 25)
k25f5  <- knn_train_pred_tib_f %>% filter(Resample == 5, k == 25)
k25f6  <- knn_train_pred_tib_f %>% filter(Resample == 6, k == 25)
k25f7  <- knn_train_pred_tib_f %>% filter(Resample == 7, k == 25)
k25f8  <- knn_train_pred_tib_f %>% filter(Resample == 8, k == 25)
k25f9  <- knn_train_pred_tib_f %>% filter(Resample == 9, k == 25)
k25f10  <- knn_train_pred_tib_f %>% filter(Resample == 10, k == 25)

k27f1  <- knn_train_pred_tib_f %>% filter(Resample == 1, k == 27)
k27f2  <- knn_train_pred_tib_f %>% filter(Resample == 2, k == 27)
k27f3  <- knn_train_pred_tib_f %>% filter(Resample == 3, k == 27)
k27f4  <- knn_train_pred_tib_f %>% filter(Resample == 4, k == 27)
k27f5  <- knn_train_pred_tib_f %>% filter(Resample == 5, k == 27)
k27f6  <- knn_train_pred_tib_f %>% filter(Resample == 6, k == 27)
k27f7  <- knn_train_pred_tib_f %>% filter(Resample == 7, k == 27)
k27f8  <- knn_train_pred_tib_f %>% filter(Resample == 8, k == 27)
k27f9  <- knn_train_pred_tib_f %>% filter(Resample == 9, k == 27)
k27f10  <- knn_train_pred_tib_f %>% filter(Resample == 10, k == 27)

k29f1  <- knn_train_pred_tib_f %>% filter(Resample == 1, k == 29)
k29f2  <- knn_train_pred_tib_f %>% filter(Resample == 2, k == 29)
k29f3  <- knn_train_pred_tib_f %>% filter(Resample == 3, k == 29)
k29f4  <- knn_train_pred_tib_f %>% filter(Resample == 4, k == 29)
k29f5  <- knn_train_pred_tib_f %>% filter(Resample == 5, k == 29)
k29f6  <- knn_train_pred_tib_f %>% filter(Resample == 6, k == 29)
k29f7  <- knn_train_pred_tib_f %>% filter(Resample == 7, k == 29)
k29f8  <- knn_train_pred_tib_f %>% filter(Resample == 8, k == 29)
k29f9  <- knn_train_pred_tib_f %>% filter(Resample == 9, k == 29)
k29f10  <- knn_train_pred_tib_f %>% filter(Resample == 10, k == 29)

k31f1  <- knn_train_pred_tib_f %>% filter(Resample == 1, k == 31)
k31f2  <- knn_train_pred_tib_f %>% filter(Resample == 2, k == 31)
k31f3  <- knn_train_pred_tib_f %>% filter(Resample == 3, k == 31)
k31f4  <- knn_train_pred_tib_f %>% filter(Resample == 4, k == 31)
k31f5  <- knn_train_pred_tib_f %>% filter(Resample == 5, k == 31)
k31f6  <- knn_train_pred_tib_f %>% filter(Resample == 6, k == 31)
k31f7  <- knn_train_pred_tib_f %>% filter(Resample == 7, k == 31)
k31f8  <- knn_train_pred_tib_f %>% filter(Resample == 8, k == 31)
k31f9  <- knn_train_pred_tib_f %>% filter(Resample == 9, k == 31)
k31f10  <- knn_train_pred_tib_f %>% filter(Resample == 10, k == 31)

k33f1  <- knn_train_pred_tib_f %>% filter(Resample == 1, k == 33)
k33f2  <- knn_train_pred_tib_f %>% filter(Resample == 2, k == 33)
k33f3  <- knn_train_pred_tib_f %>% filter(Resample == 3, k == 33)
k33f4  <- knn_train_pred_tib_f %>% filter(Resample == 4, k == 33)
k33f5  <- knn_train_pred_tib_f %>% filter(Resample == 5, k == 33)
k33f6  <- knn_train_pred_tib_f %>% filter(Resample == 6, k == 33)
k33f7  <- knn_train_pred_tib_f %>% filter(Resample == 7, k == 33)
k33f8  <- knn_train_pred_tib_f %>% filter(Resample == 8, k == 33)
k33f9  <- knn_train_pred_tib_f %>% filter(Resample == 9, k == 33)
k33f10  <- knn_train_pred_tib_f %>% filter(Resample == 10, k == 33)

k35f1  <- knn_train_pred_tib_f %>% filter(Resample == 1, k == 35)
k35f2  <- knn_train_pred_tib_f %>% filter(Resample == 2, k == 35)
k35f3  <- knn_train_pred_tib_f %>% filter(Resample == 3, k == 35)
k35f4  <- knn_train_pred_tib_f %>% filter(Resample == 4, k == 35)
k35f5  <- knn_train_pred_tib_f %>% filter(Resample == 5, k == 35)
k35f6  <- knn_train_pred_tib_f %>% filter(Resample == 6, k == 35)
k35f7  <- knn_train_pred_tib_f %>% filter(Resample == 7, k == 35)
k35f8  <- knn_train_pred_tib_f %>% filter(Resample == 8, k == 35)
k35f9  <- knn_train_pred_tib_f %>% filter(Resample == 9, k == 35)
k35f10  <- knn_train_pred_tib_f %>% filter(Resample == 10, k == 35)

k37f1  <- knn_train_pred_tib_f %>% filter(Resample == 1, k == 37)
k37f2  <- knn_train_pred_tib_f %>% filter(Resample == 2, k == 37)
k37f3  <- knn_train_pred_tib_f %>% filter(Resample == 3, k == 37)
k37f4  <- knn_train_pred_tib_f %>% filter(Resample == 4, k == 37)
k37f5  <- knn_train_pred_tib_f %>% filter(Resample == 5, k == 37)
k37f6  <- knn_train_pred_tib_f %>% filter(Resample == 6, k == 37)
k37f7  <- knn_train_pred_tib_f %>% filter(Resample == 7, k == 37)
k37f8  <- knn_train_pred_tib_f %>% filter(Resample == 8, k == 37)
k37f9  <- knn_train_pred_tib_f %>% filter(Resample == 9, k == 37)
k37f10  <- knn_train_pred_tib_f %>% filter(Resample == 10, k == 37)

k39f1  <- knn_train_pred_tib_f %>% filter(Resample == 1, k == 39)
k39f2  <- knn_train_pred_tib_f %>% filter(Resample == 2, k == 39)
k39f3  <- knn_train_pred_tib_f %>% filter(Resample == 3, k == 39)
k39f4  <- knn_train_pred_tib_f %>% filter(Resample == 4, k == 39)
k39f5  <- knn_train_pred_tib_f %>% filter(Resample == 5, k == 39)
k39f6  <- knn_train_pred_tib_f %>% filter(Resample == 6, k == 39)
k39f7  <- knn_train_pred_tib_f %>% filter(Resample == 7, k == 39)
k39f8  <- knn_train_pred_tib_f %>% filter(Resample == 8, k == 39)
k39f9  <- knn_train_pred_tib_f %>% filter(Resample == 9, k == 39)
k39f10  <- knn_train_pred_tib_f %>% filter(Resample == 10, k == 39)

rm(knn_train_pred_tib_f)
gc(reset = TRUE)

  # Form a list and vector of these tibbles.

fk_dfs_l <- list(k3f1, k3f2,  k3f3, k3f4, k3f5, k3f6, k3f7, k3f8, k3f9, k3f10,  
                 k5f1, k5f2,  k5f3, k5f4, k5f5, k5f6, k5f7, k5f8, k5f9, k5f10,  
                 k7f1, k7f2,  k7f3, k7f4, k7f5, k7f6, k7f7, k7f8, k7f9, k7f10,  
                 k9f1, k9f2,  k9f3, k9f4, k9f5, k9f6, k9f7, k9f8, k9f9, k9f10,  
                 k11f1, k11f2,  k11f3, k11f4, k11f5, k11f6, k11f7, k11f8, k11f9, k11f10,  
                 k13f1, k13f2,  k13f3, k13f4, k13f5, k13f6, k13f7, k13f8, k13f9, k13f10,  
                 k15f1, k15f2,  k15f3, k15f4, k15f5, k15f6, k15f7, k15f8, k15f9, k15f10,  
                 k17f1, k17f2,  k17f3, k17f4, k17f5, k17f6, k17f7, k17f8, k17f9, k17f10,  
                 k19f1, k19f2,  k19f3, k19f4, k19f5, k19f6, k19f7, k19f8, k19f9, k19f10,  
                 k21f1, k21f2,  k21f3, k21f4, k21f5, k21f6, k21f7, k21f8, k21f9, k21f10,  
                 k23f1, k23f2,  k23f3, k23f4, k23f5, k23f6, k23f7, k23f8, k23f9, k23f10,  
                 k25f1, k25f2,  k25f3, k25f4, k25f5, k25f6, k25f7, k25f8, k25f9, k25f10,  
                 k27f1, k27f2,  k27f3, k27f4, k27f5, k27f6, k27f7, k27f8, k27f9, k27f10,  
                 k29f1, k29f2,  k29f3, k29f4, k29f5, k29f6, k29f7, k29f8, k29f9, k29f10,  
                 k31f1, k31f2,  k31f3, k31f4, k31f5, k31f6, k31f7, k31f8, k31f9, k31f10,  
                 k33f1, k33f2,  k33f3, k33f4, k33f5, k33f6, k33f7, k33f8, k33f9, k33f10,  
                 k35f1, k35f2,  k35f3, k35f4, k35f5, k35f6, k35f7, k35f8, k35f9, k35f10,  
                 k37f1, k37f2,  k37f3, k37f4, k37f5, k37f6, k37f7, k37f8, k37f9, k37f10,  
                 k39f1, k39f2,  k39f3, k39f4, k39f5, k39f6, k39f7, k39f8, k39f9, k39f10)

fk_dfs_v <- c("k3f1", "k3f2", " k3f3", "k3f4", "k3f5", "k3f6", "k3f7", "k3f8", "k3f9", "k3f10", 
              "k5f1", "k5f2", " k5f3", "k5f4", "k5f5", "k5f6", "k5f7", "k5f8", "k5f9", "k5f10", 
              "k7f1", "k7f2", " k7f3", "k7f4", "k7f5", "k7f6", "k7f7", "k7f8", "k7f9", "k7f10", 
              "k9f1", "k9f2", " k9f3", "k9f4", "k9f5", "k9f6", "k9f7", "k9f8", "k9f9", "k9f10", 
              "k11f1", "k11f2", " k11f3", "k11f4", "k11f5", "k11f6", "k11f7", "k11f8", "k11f9", "k11f10", 
              "k13f1", "k13f2", " k13f3", "k13f4", "k13f5", "k13f6", "k13f7", "k13f8", "k13f9", "k13f10", 
              "k15f1", "k15f2", " k15f3", "k15f4", "k15f5", "k15f6", "k15f7", "k15f8", "k15f9", "k15f10", 
              "k17f1", "k17f2", " k17f3", "k17f4", "k17f5", "k17f6", "k17f7", "k17f8", "k17f9", "k17f10", 
              "k19f1", "k19f2", " k19f3", "k19f4", "k19f5", "k19f6", "k19f7", "k19f8", "k19f9", "k19f10", 
              "k21f1", "k21f2", " k21f3", "k21f4", "k21f5", "k21f6", "k21f7", "k21f8", "k21f9", "k21f10", 
              "k23f1", "k23f2", " k23f3", "k23f4", "k23f5", "k23f6", "k23f7", "k23f8", "k23f9", "k23f10", 
              "k25f1", "k25f2", " k25f3", "k25f4", "k25f5", "k25f6", "k25f7", "k25f8", "k25f9", "k25f10", 
              "k27f1", "k27f2", " k27f3", "k27f4", "k27f5", "k27f6", "k27f7", "k27f8", "k27f9", "k27f10", 
              "k29f1", "k29f2", " k29f3", "k29f4", "k29f5", "k29f6", "k29f7", "k29f8", "k29f9", "k29f10", 
              "k31f1", "k31f2", " k31f3", "k31f4", "k31f5", "k31f6", "k31f7", "k31f8", "k31f9", "k31f10", 
              "k33f1", "k33f2", " k33f3", "k33f4", "k33f5", "k33f6", "k33f7", "k33f8", "k33f9", "k33f10", 
              "k35f1", "k35f2", " k35f3", "k35f4", "k35f5", "k35f6", "k35f7", "k35f8", "k35f9", "k35f10", 
              "k37f1", "k37f2", " k37f3", "k37f4", "k37f5", "k37f6", "k37f7", "k37f8", "k37f9", "k37f10", 
              "k39f1", "k39f2", " k39f3", "k39f4", "k39f5", "k39f6", "k39f7", "k39f8", "k39f9", "k39f10")

############################
# 0.25 Cutoff
############################

  # For the decision cutoff of 0.25, generate a confusion matrix for
  # every combination of k (3:39 by two) and fold (1 to 10).

cm_c25 <- sapply(fk_dfs_l, function(x) {
  ss <- subset(x, select = c(pred25, obs))
  confusionMatrix(ss$pred25, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c25) <- fk_dfs_v

cm_c25_tables <- sapply(cm_c25, "[[", 2)
cm_c25_tables <- as_tibble(cm_c25_tables)

  # For each value of k, sum the True Positives, False Negatives, 
  # True Negatives, and False Positives respectively across all 10 
  # folds. Then use these totals to compute the Truescore for 
  # each value of k when the decision cutoff is 0.25.

(k3c25_tpr <- round(sum(cm_c25_tables[1, 1:10]) / (sum(cm_c25_tables[1, 1:10]) + sum(cm_c25_tables[2, 1:10])), 6))
(k3c25_tnr <- round(sum(cm_c25_tables[4, 1:10]) / (sum(cm_c25_tables[4, 1:10]) + sum(cm_c25_tables[3, 1:10])), 6))
(k3c25_truescore <- round((2 * k3c25_tpr * k3c25_tnr) / (k3c25_tpr + k3c25_tnr), 6))

(k5c25_tpr <- round(sum(cm_c25_tables[1, 11:20]) / (sum(cm_c25_tables[1, 11:20]) + sum(cm_c25_tables[2, 11:20])), 6))
(k5c25_tnr <- round(sum(cm_c25_tables[4, 11:20]) / (sum(cm_c25_tables[4, 11:20]) + sum(cm_c25_tables[3, 11:20])), 6))
(k5c25_truescore <- round((2 * k5c25_tpr * k5c25_tnr) / (k5c25_tpr + k5c25_tnr), 6))

(k7c25_tpr <- round(sum(cm_c25_tables[1, 21:30]) / (sum(cm_c25_tables[1, 21:30]) + sum(cm_c25_tables[2, 21:30])), 6))
(k7c25_tnr <- round(sum(cm_c25_tables[4, 21:30]) / (sum(cm_c25_tables[4, 21:30]) + sum(cm_c25_tables[3, 21:30])), 6))
(k7c25_truescore <- round((2 * k7c25_tpr * k7c25_tnr) / (k7c25_tpr + k7c25_tnr), 6))

(k9c25_tpr <- round(sum(cm_c25_tables[1, 31:40]) / (sum(cm_c25_tables[1, 31:40]) + sum(cm_c25_tables[2, 31:40])), 6))
(k9c25_tnr <- round(sum(cm_c25_tables[4, 31:40]) / (sum(cm_c25_tables[4, 31:40]) + sum(cm_c25_tables[3, 31:40])), 6))
(k9c25_truescore <- round((2 * k9c25_tpr * k9c25_tnr) / (k9c25_tpr + k9c25_tnr), 6))

(k11c25_tpr <- round(sum(cm_c25_tables[1, 41:50]) / (sum(cm_c25_tables[1, 41:50]) + sum(cm_c25_tables[2, 41:50])), 6))
(k11c25_tnr <- round(sum(cm_c25_tables[4, 41:50]) / (sum(cm_c25_tables[4, 41:50]) + sum(cm_c25_tables[3, 41:50])), 6))
(k11c25_truescore <- round((2 * k11c25_tpr * k11c25_tnr) / (k11c25_tpr + k11c25_tnr), 6))

(k13c25_tpr <- round(sum(cm_c25_tables[1, 51:60]) / (sum(cm_c25_tables[1, 51:60]) + sum(cm_c25_tables[2, 51:60])), 6))
(k13c25_tnr <- round(sum(cm_c25_tables[4, 51:60]) / (sum(cm_c25_tables[4, 51:60]) + sum(cm_c25_tables[3, 51:60])), 6))
(k13c25_truescore <- round((2 * k13c25_tpr * k13c25_tnr) / (k13c25_tpr + k13c25_tnr), 6))

(k15c25_tpr <- round(sum(cm_c25_tables[1, 61:70]) / (sum(cm_c25_tables[1, 61:70]) + sum(cm_c25_tables[2, 61:70])), 6))
(k15c25_tnr <- round(sum(cm_c25_tables[4, 61:70]) / (sum(cm_c25_tables[4, 61:70]) + sum(cm_c25_tables[3, 61:70])), 6))
(k15c25_truescore <- round((2 * k15c25_tpr * k15c25_tnr) / (k15c25_tpr + k15c25_tnr), 6))

(k17c25_tpr <- round(sum(cm_c25_tables[1, 71:80]) / (sum(cm_c25_tables[1, 71:80]) + sum(cm_c25_tables[2, 71:80])), 6))
(k17c25_tnr <- round(sum(cm_c25_tables[4, 71:80]) / (sum(cm_c25_tables[4, 71:80]) + sum(cm_c25_tables[3, 71:80])), 6))
(k17c25_truescore <- round((2 * k17c25_tpr * k17c25_tnr) / (k17c25_tpr + k17c25_tnr), 6))

(k19c25_tpr <- round(sum(cm_c25_tables[1, 81:90]) / (sum(cm_c25_tables[1, 81:90]) + sum(cm_c25_tables[2, 81:90])), 6))
(k19c25_tnr <- round(sum(cm_c25_tables[4, 81:90]) / (sum(cm_c25_tables[4, 81:90]) + sum(cm_c25_tables[3, 81:90])), 6))
(k19c25_truescore <- round((2 * k19c25_tpr * k19c25_tnr) / (k19c25_tpr + k19c25_tnr), 6))

(k21c25_tpr <- round(sum(cm_c25_tables[1, 91:100]) / (sum(cm_c25_tables[1, 91:100]) + sum(cm_c25_tables[2, 91:100])), 6))
(k21c25_tnr <- round(sum(cm_c25_tables[4, 91:100]) / (sum(cm_c25_tables[4, 91:100]) + sum(cm_c25_tables[3, 91:100])), 6))
(k21c25_truescore <- round((2 * k21c25_tpr * k21c25_tnr) / (k21c25_tpr + k21c25_tnr), 6))

(k23c25_tpr <- round(sum(cm_c25_tables[1, 101:110]) / (sum(cm_c25_tables[1, 101:110]) + sum(cm_c25_tables[2, 101:110])), 6))
(k23c25_tnr <- round(sum(cm_c25_tables[4, 101:110]) / (sum(cm_c25_tables[4, 101:110]) + sum(cm_c25_tables[3, 101:110])), 6))
(k23c25_truescore <- round((2 * k23c25_tpr * k23c25_tnr) / (k23c25_tpr + k23c25_tnr), 6))

(k25c25_tpr <- round(sum(cm_c25_tables[1, 111:120]) / (sum(cm_c25_tables[1, 111:120]) + sum(cm_c25_tables[2, 111:120])), 6))
(k25c25_tnr <- round(sum(cm_c25_tables[4, 111:120]) / (sum(cm_c25_tables[4, 111:120]) + sum(cm_c25_tables[3, 111:120])), 6))
(k25c25_truescore <- round((2 * k25c25_tpr * k25c25_tnr) / (k25c25_tpr + k25c25_tnr), 6))

(k27c25_tpr <- round(sum(cm_c25_tables[1, 121:130]) / (sum(cm_c25_tables[1, 121:130]) + sum(cm_c25_tables[2, 121:130])), 6))
(k27c25_tnr <- round(sum(cm_c25_tables[4, 121:130]) / (sum(cm_c25_tables[4, 121:130]) + sum(cm_c25_tables[3, 121:130])), 6))
(k27c25_truescore <- round((2 * k27c25_tpr * k27c25_tnr) / (k27c25_tpr + k27c25_tnr), 6))

(k29c25_tpr <- round(sum(cm_c25_tables[1, 131:140]) / (sum(cm_c25_tables[1, 131:140]) + sum(cm_c25_tables[2, 131:140])), 6))
(k29c25_tnr <- round(sum(cm_c25_tables[4, 131:140]) / (sum(cm_c25_tables[4, 131:140]) + sum(cm_c25_tables[3, 131:140])), 6))
(k29c25_truescore <- round((2 * k29c25_tpr * k29c25_tnr) / (k29c25_tpr + k29c25_tnr), 6))

(k31c25_tpr <- round(sum(cm_c25_tables[1, 141:150]) / (sum(cm_c25_tables[1, 141:150]) + sum(cm_c25_tables[2, 141:150])), 6))
(k31c25_tnr <- round(sum(cm_c25_tables[4, 141:150]) / (sum(cm_c25_tables[4, 141:150]) + sum(cm_c25_tables[3, 141:150])), 6))
(k31c25_truescore <- round((2 * k31c25_tpr * k31c25_tnr) / (k31c25_tpr + k31c25_tnr), 6))

(k33c25_tpr <- round(sum(cm_c25_tables[1, 151:160]) / (sum(cm_c25_tables[1, 151:160]) + sum(cm_c25_tables[2, 151:160])), 6))
(k33c25_tnr <- round(sum(cm_c25_tables[4, 151:160]) / (sum(cm_c25_tables[4, 151:160]) + sum(cm_c25_tables[3, 151:160])), 6))
(k33c25_truescore <- round((2 * k33c25_tpr * k33c25_tnr) / (k33c25_tpr + k33c25_tnr), 6))

(k35c25_tpr <- round(sum(cm_c25_tables[1, 161:170]) / (sum(cm_c25_tables[1, 161:170]) + sum(cm_c25_tables[2, 161:170])), 6))
(k35c25_tnr <- round(sum(cm_c25_tables[4, 161:170]) / (sum(cm_c25_tables[4, 161:170]) + sum(cm_c25_tables[3, 161:170])), 6))
(k35c25_truescore <- round((2 * k35c25_tpr * k35c25_tnr) / (k35c25_tpr + k35c25_tnr), 6))

(k37c25_tpr <- round(sum(cm_c25_tables[1, 171:180]) / (sum(cm_c25_tables[1, 171:180]) + sum(cm_c25_tables[2, 171:180])), 6))
(k37c25_tnr <- round(sum(cm_c25_tables[4, 171:180]) / (sum(cm_c25_tables[4, 171:180]) + sum(cm_c25_tables[3, 171:180])), 6))
(k37c25_truescore <- round((2 * k37c25_tpr * k37c25_tnr) / (k37c25_tpr + k37c25_tnr), 6))

(k39c25_tpr <- round(sum(cm_c25_tables[1, 181:190]) / (sum(cm_c25_tables[1, 181:190]) + sum(cm_c25_tables[2, 181:190])), 6))
(k39c25_tnr <- round(sum(cm_c25_tables[4, 181:190]) / (sum(cm_c25_tables[4, 181:190]) + sum(cm_c25_tables[3, 181:190])), 6))
(k39c25_truescore <- round((2 * k39c25_tpr * k39c25_tnr) / (k39c25_tpr + k39c25_tnr), 6))

  # Compile the 0.25 cutoff results in a table, and identify the value
  # of k that maximizes the truescore.

c25_results <- tibble(k = seq(3, 39, 2), 
                      Cut = 0.25, 
                      TPR = c(k3c25_tpr, k5c25_tpr, k7c25_tpr, k9c25_tpr, k11c25_tpr, 
                              k13c25_tpr, k15c25_tpr, k17c25_tpr, k19c25_tpr, k21c25_tpr, 
                              k23c25_tpr, k25c25_tpr, k27c25_tpr, k29c25_tpr, k31c25_tpr, 
                              k33c25_tpr, k35c25_tpr, k37c25_tpr, k39c25_tpr), 
                      TNR = c(k3c25_tnr, k5c25_tnr, k7c25_tnr, k9c25_tnr, k11c25_tnr, 
                              k13c25_tnr, k15c25_tnr, k17c25_tnr, k19c25_tnr, k21c25_tnr, 
                              k23c25_tnr, k25c25_tnr, k27c25_tnr, k29c25_tnr, k31c25_tnr, 
                              k33c25_tnr, k35c25_tnr, k37c25_tnr, k39c25_tnr), 
                      Truescore = c(k3c25_truescore, k5c25_truescore, k7c25_truescore, 
                                    k9c25_truescore, k11c25_truescore, k13c25_truescore, 
                                    k15c25_truescore, k17c25_truescore, k19c25_truescore, 
                                    k21c25_truescore, k23c25_truescore, k25c25_truescore, 
                                    k27c25_truescore, k29c25_truescore, k31c25_truescore, 
                                    k33c25_truescore, k35c25_truescore, k37c25_truescore, 
                                    k39c25_truescore))

knitr::kable(c25_results[1:19, ], caption = "c25_results")

ggplot(c25_results, aes(k, Truescore)) + geom_point() + geom_line() + 
  labs(title = "Optimal k for Decision Cutoff 0.25")

############################
# 0.26 Cutoff
############################

# For the decision cutoff of 0.26, generate a confusion matrix for
# every combination of k (3:39 by two) and fold (1 to 10).

cm_c26 <- sapply(fk_dfs_l, function(x) {
  ss <- subset(x, select = c(pred26, obs))
  confusionMatrix(ss$pred26, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c26) <- fk_dfs_v

cm_c26_tables <- sapply(cm_c26, "[[", 2)
cm_c26_tables <- as_tibble(cm_c26_tables)

# For each value of k, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each value of k when the decision cutoff is 0.26.

(k3c26_tpr <- round(sum(cm_c26_tables[1, 1:10]) / (sum(cm_c26_tables[1, 1:10]) + sum(cm_c26_tables[2, 1:10])), 6))
(k3c26_tnr <- round(sum(cm_c26_tables[4, 1:10]) / (sum(cm_c26_tables[4, 1:10]) + sum(cm_c26_tables[3, 1:10])), 6))
(k3c26_truescore <- round((2 * k3c26_tpr * k3c26_tnr) / (k3c26_tpr + k3c26_tnr), 6))

(k5c26_tpr <- round(sum(cm_c26_tables[1, 11:20]) / (sum(cm_c26_tables[1, 11:20]) + sum(cm_c26_tables[2, 11:20])), 6))
(k5c26_tnr <- round(sum(cm_c26_tables[4, 11:20]) / (sum(cm_c26_tables[4, 11:20]) + sum(cm_c26_tables[3, 11:20])), 6))
(k5c26_truescore <- round((2 * k5c26_tpr * k5c26_tnr) / (k5c26_tpr + k5c26_tnr), 6))

(k7c26_tpr <- round(sum(cm_c26_tables[1, 21:30]) / (sum(cm_c26_tables[1, 21:30]) + sum(cm_c26_tables[2, 21:30])), 6))
(k7c26_tnr <- round(sum(cm_c26_tables[4, 21:30]) / (sum(cm_c26_tables[4, 21:30]) + sum(cm_c26_tables[3, 21:30])), 6))
(k7c26_truescore <- round((2 * k7c26_tpr * k7c26_tnr) / (k7c26_tpr + k7c26_tnr), 6))

(k9c26_tpr <- round(sum(cm_c26_tables[1, 31:40]) / (sum(cm_c26_tables[1, 31:40]) + sum(cm_c26_tables[2, 31:40])), 6))
(k9c26_tnr <- round(sum(cm_c26_tables[4, 31:40]) / (sum(cm_c26_tables[4, 31:40]) + sum(cm_c26_tables[3, 31:40])), 6))
(k9c26_truescore <- round((2 * k9c26_tpr * k9c26_tnr) / (k9c26_tpr + k9c26_tnr), 6))

(k11c26_tpr <- round(sum(cm_c26_tables[1, 41:50]) / (sum(cm_c26_tables[1, 41:50]) + sum(cm_c26_tables[2, 41:50])), 6))
(k11c26_tnr <- round(sum(cm_c26_tables[4, 41:50]) / (sum(cm_c26_tables[4, 41:50]) + sum(cm_c26_tables[3, 41:50])), 6))
(k11c26_truescore <- round((2 * k11c26_tpr * k11c26_tnr) / (k11c26_tpr + k11c26_tnr), 6))

(k13c26_tpr <- round(sum(cm_c26_tables[1, 51:60]) / (sum(cm_c26_tables[1, 51:60]) + sum(cm_c26_tables[2, 51:60])), 6))
(k13c26_tnr <- round(sum(cm_c26_tables[4, 51:60]) / (sum(cm_c26_tables[4, 51:60]) + sum(cm_c26_tables[3, 51:60])), 6))
(k13c26_truescore <- round((2 * k13c26_tpr * k13c26_tnr) / (k13c26_tpr + k13c26_tnr), 6))

(k15c26_tpr <- round(sum(cm_c26_tables[1, 61:70]) / (sum(cm_c26_tables[1, 61:70]) + sum(cm_c26_tables[2, 61:70])), 6))
(k15c26_tnr <- round(sum(cm_c26_tables[4, 61:70]) / (sum(cm_c26_tables[4, 61:70]) + sum(cm_c26_tables[3, 61:70])), 6))
(k15c26_truescore <- round((2 * k15c26_tpr * k15c26_tnr) / (k15c26_tpr + k15c26_tnr), 6))

(k17c26_tpr <- round(sum(cm_c26_tables[1, 71:80]) / (sum(cm_c26_tables[1, 71:80]) + sum(cm_c26_tables[2, 71:80])), 6))
(k17c26_tnr <- round(sum(cm_c26_tables[4, 71:80]) / (sum(cm_c26_tables[4, 71:80]) + sum(cm_c26_tables[3, 71:80])), 6))
(k17c26_truescore <- round((2 * k17c26_tpr * k17c26_tnr) / (k17c26_tpr + k17c26_tnr), 6))

(k19c26_tpr <- round(sum(cm_c26_tables[1, 81:90]) / (sum(cm_c26_tables[1, 81:90]) + sum(cm_c26_tables[2, 81:90])), 6))
(k19c26_tnr <- round(sum(cm_c26_tables[4, 81:90]) / (sum(cm_c26_tables[4, 81:90]) + sum(cm_c26_tables[3, 81:90])), 6))
(k19c26_truescore <- round((2 * k19c26_tpr * k19c26_tnr) / (k19c26_tpr + k19c26_tnr), 6))

(k21c26_tpr <- round(sum(cm_c26_tables[1, 91:100]) / (sum(cm_c26_tables[1, 91:100]) + sum(cm_c26_tables[2, 91:100])), 6))
(k21c26_tnr <- round(sum(cm_c26_tables[4, 91:100]) / (sum(cm_c26_tables[4, 91:100]) + sum(cm_c26_tables[3, 91:100])), 6))
(k21c26_truescore <- round((2 * k21c26_tpr * k21c26_tnr) / (k21c26_tpr + k21c26_tnr), 6))

(k23c26_tpr <- round(sum(cm_c26_tables[1, 101:110]) / (sum(cm_c26_tables[1, 101:110]) + sum(cm_c26_tables[2, 101:110])), 6))
(k23c26_tnr <- round(sum(cm_c26_tables[4, 101:110]) / (sum(cm_c26_tables[4, 101:110]) + sum(cm_c26_tables[3, 101:110])), 6))
(k23c26_truescore <- round((2 * k23c26_tpr * k23c26_tnr) / (k23c26_tpr + k23c26_tnr), 6))

(k25c26_tpr <- round(sum(cm_c26_tables[1, 111:120]) / (sum(cm_c26_tables[1, 111:120]) + sum(cm_c26_tables[2, 111:120])), 6))
(k25c26_tnr <- round(sum(cm_c26_tables[4, 111:120]) / (sum(cm_c26_tables[4, 111:120]) + sum(cm_c26_tables[3, 111:120])), 6))
(k25c26_truescore <- round((2 * k25c26_tpr * k25c26_tnr) / (k25c26_tpr + k25c26_tnr), 6))

(k27c26_tpr <- round(sum(cm_c26_tables[1, 121:130]) / (sum(cm_c26_tables[1, 121:130]) + sum(cm_c26_tables[2, 121:130])), 6))
(k27c26_tnr <- round(sum(cm_c26_tables[4, 121:130]) / (sum(cm_c26_tables[4, 121:130]) + sum(cm_c26_tables[3, 121:130])), 6))
(k27c26_truescore <- round((2 * k27c26_tpr * k27c26_tnr) / (k27c26_tpr + k27c26_tnr), 6))

(k29c26_tpr <- round(sum(cm_c26_tables[1, 131:140]) / (sum(cm_c26_tables[1, 131:140]) + sum(cm_c26_tables[2, 131:140])), 6))
(k29c26_tnr <- round(sum(cm_c26_tables[4, 131:140]) / (sum(cm_c26_tables[4, 131:140]) + sum(cm_c26_tables[3, 131:140])), 6))
(k29c26_truescore <- round((2 * k29c26_tpr * k29c26_tnr) / (k29c26_tpr + k29c26_tnr), 6))

(k31c26_tpr <- round(sum(cm_c26_tables[1, 141:150]) / (sum(cm_c26_tables[1, 141:150]) + sum(cm_c26_tables[2, 141:150])), 6))
(k31c26_tnr <- round(sum(cm_c26_tables[4, 141:150]) / (sum(cm_c26_tables[4, 141:150]) + sum(cm_c26_tables[3, 141:150])), 6))
(k31c26_truescore <- round((2 * k31c26_tpr * k31c26_tnr) / (k31c26_tpr + k31c26_tnr), 6))

(k33c26_tpr <- round(sum(cm_c26_tables[1, 151:160]) / (sum(cm_c26_tables[1, 151:160]) + sum(cm_c26_tables[2, 151:160])), 6))
(k33c26_tnr <- round(sum(cm_c26_tables[4, 151:160]) / (sum(cm_c26_tables[4, 151:160]) + sum(cm_c26_tables[3, 151:160])), 6))
(k33c26_truescore <- round((2 * k33c26_tpr * k33c26_tnr) / (k33c26_tpr + k33c26_tnr), 6))

(k35c26_tpr <- round(sum(cm_c26_tables[1, 161:170]) / (sum(cm_c26_tables[1, 161:170]) + sum(cm_c26_tables[2, 161:170])), 6))
(k35c26_tnr <- round(sum(cm_c26_tables[4, 161:170]) / (sum(cm_c26_tables[4, 161:170]) + sum(cm_c26_tables[3, 161:170])), 6))
(k35c26_truescore <- round((2 * k35c26_tpr * k35c26_tnr) / (k35c26_tpr + k35c26_tnr), 6))

(k37c26_tpr <- round(sum(cm_c26_tables[1, 171:180]) / (sum(cm_c26_tables[1, 171:180]) + sum(cm_c26_tables[2, 171:180])), 6))
(k37c26_tnr <- round(sum(cm_c26_tables[4, 171:180]) / (sum(cm_c26_tables[4, 171:180]) + sum(cm_c26_tables[3, 171:180])), 6))
(k37c26_truescore <- round((2 * k37c26_tpr * k37c26_tnr) / (k37c26_tpr + k37c26_tnr), 6))

(k39c26_tpr <- round(sum(cm_c26_tables[1, 181:190]) / (sum(cm_c26_tables[1, 181:190]) + sum(cm_c26_tables[2, 181:190])), 6))
(k39c26_tnr <- round(sum(cm_c26_tables[4, 181:190]) / (sum(cm_c26_tables[4, 181:190]) + sum(cm_c26_tables[3, 181:190])), 6))
(k39c26_truescore <- round((2 * k39c26_tpr * k39c26_tnr) / (k39c26_tpr + k39c26_tnr), 6))

# Compile the 0.26 cutoff results in a table, and identify the value
# of k that maximizes the truescore.

c26_results <- tibble(k = seq(3, 39, 2), 
                      Cut = 0.26, 
                      TPR = c(k3c26_tpr, k5c26_tpr, k7c26_tpr, k9c26_tpr, k11c26_tpr, 
                              k13c26_tpr, k15c26_tpr, k17c26_tpr, k19c26_tpr, k21c26_tpr, 
                              k23c26_tpr, k25c26_tpr, k27c26_tpr, k29c26_tpr, k31c26_tpr, 
                              k33c26_tpr, k35c26_tpr, k37c26_tpr, k39c26_tpr), 
                      TNR = c(k3c26_tnr, k5c26_tnr, k7c26_tnr, k9c26_tnr, k11c26_tnr, 
                              k13c26_tnr, k15c26_tnr, k17c26_tnr, k19c26_tnr, k21c26_tnr, 
                              k23c26_tnr, k25c26_tnr, k27c26_tnr, k29c26_tnr, k31c26_tnr, 
                              k33c26_tnr, k35c26_tnr, k37c26_tnr, k39c26_tnr), 
                      Truescore = c(k3c26_truescore, k5c26_truescore, k7c26_truescore, 
                                    k9c26_truescore, k11c26_truescore, k13c26_truescore, 
                                    k15c26_truescore, k17c26_truescore, k19c26_truescore, 
                                    k21c26_truescore, k23c26_truescore, k25c26_truescore, 
                                    k27c26_truescore, k29c26_truescore, k31c26_truescore, 
                                    k33c26_truescore, k35c26_truescore, k37c26_truescore, 
                                    k39c26_truescore))

knitr::kable(c26_results[1:19, ], caption = "c26_results")

ggplot(c26_results, aes(k, Truescore)) + geom_point() + geom_line() + 
  labs(title = "Optimal k for Decision Cutoff 0.26")

############################
# 0.27 Cutoff
############################

# For the decision cutoff of 0.27, generate a confusion matrix for
# every combination of k (3:39 by two) and fold (1 to 10).

cm_c27 <- sapply(fk_dfs_l, function(x) {
  ss <- subset(x, select = c(pred27, obs))
  confusionMatrix(ss$pred27, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c27) <- fk_dfs_v

cm_c27_tables <- sapply(cm_c27, "[[", 2)
cm_c27_tables <- as_tibble(cm_c27_tables)

# For each value of k, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each value of k when the decision cutoff is 0.27.

(k3c27_tpr <- round(sum(cm_c27_tables[1, 1:10]) / (sum(cm_c27_tables[1, 1:10]) + sum(cm_c27_tables[2, 1:10])), 6))
(k3c27_tnr <- round(sum(cm_c27_tables[4, 1:10]) / (sum(cm_c27_tables[4, 1:10]) + sum(cm_c27_tables[3, 1:10])), 6))
(k3c27_truescore <- round((2 * k3c27_tpr * k3c27_tnr) / (k3c27_tpr + k3c27_tnr), 6))

(k5c27_tpr <- round(sum(cm_c27_tables[1, 11:20]) / (sum(cm_c27_tables[1, 11:20]) + sum(cm_c27_tables[2, 11:20])), 6))
(k5c27_tnr <- round(sum(cm_c27_tables[4, 11:20]) / (sum(cm_c27_tables[4, 11:20]) + sum(cm_c27_tables[3, 11:20])), 6))
(k5c27_truescore <- round((2 * k5c27_tpr * k5c27_tnr) / (k5c27_tpr + k5c27_tnr), 6))

(k7c27_tpr <- round(sum(cm_c27_tables[1, 21:30]) / (sum(cm_c27_tables[1, 21:30]) + sum(cm_c27_tables[2, 21:30])), 6))
(k7c27_tnr <- round(sum(cm_c27_tables[4, 21:30]) / (sum(cm_c27_tables[4, 21:30]) + sum(cm_c27_tables[3, 21:30])), 6))
(k7c27_truescore <- round((2 * k7c27_tpr * k7c27_tnr) / (k7c27_tpr + k7c27_tnr), 6))

(k9c27_tpr <- round(sum(cm_c27_tables[1, 31:40]) / (sum(cm_c27_tables[1, 31:40]) + sum(cm_c27_tables[2, 31:40])), 6))
(k9c27_tnr <- round(sum(cm_c27_tables[4, 31:40]) / (sum(cm_c27_tables[4, 31:40]) + sum(cm_c27_tables[3, 31:40])), 6))
(k9c27_truescore <- round((2 * k9c27_tpr * k9c27_tnr) / (k9c27_tpr + k9c27_tnr), 6))

(k11c27_tpr <- round(sum(cm_c27_tables[1, 41:50]) / (sum(cm_c27_tables[1, 41:50]) + sum(cm_c27_tables[2, 41:50])), 6))
(k11c27_tnr <- round(sum(cm_c27_tables[4, 41:50]) / (sum(cm_c27_tables[4, 41:50]) + sum(cm_c27_tables[3, 41:50])), 6))
(k11c27_truescore <- round((2 * k11c27_tpr * k11c27_tnr) / (k11c27_tpr + k11c27_tnr), 6))

(k13c27_tpr <- round(sum(cm_c27_tables[1, 51:60]) / (sum(cm_c27_tables[1, 51:60]) + sum(cm_c27_tables[2, 51:60])), 6))
(k13c27_tnr <- round(sum(cm_c27_tables[4, 51:60]) / (sum(cm_c27_tables[4, 51:60]) + sum(cm_c27_tables[3, 51:60])), 6))
(k13c27_truescore <- round((2 * k13c27_tpr * k13c27_tnr) / (k13c27_tpr + k13c27_tnr), 6))

(k15c27_tpr <- round(sum(cm_c27_tables[1, 61:70]) / (sum(cm_c27_tables[1, 61:70]) + sum(cm_c27_tables[2, 61:70])), 6))
(k15c27_tnr <- round(sum(cm_c27_tables[4, 61:70]) / (sum(cm_c27_tables[4, 61:70]) + sum(cm_c27_tables[3, 61:70])), 6))
(k15c27_truescore <- round((2 * k15c27_tpr * k15c27_tnr) / (k15c27_tpr + k15c27_tnr), 6))

(k17c27_tpr <- round(sum(cm_c27_tables[1, 71:80]) / (sum(cm_c27_tables[1, 71:80]) + sum(cm_c27_tables[2, 71:80])), 6))
(k17c27_tnr <- round(sum(cm_c27_tables[4, 71:80]) / (sum(cm_c27_tables[4, 71:80]) + sum(cm_c27_tables[3, 71:80])), 6))
(k17c27_truescore <- round((2 * k17c27_tpr * k17c27_tnr) / (k17c27_tpr + k17c27_tnr), 6))

(k19c27_tpr <- round(sum(cm_c27_tables[1, 81:90]) / (sum(cm_c27_tables[1, 81:90]) + sum(cm_c27_tables[2, 81:90])), 6))
(k19c27_tnr <- round(sum(cm_c27_tables[4, 81:90]) / (sum(cm_c27_tables[4, 81:90]) + sum(cm_c27_tables[3, 81:90])), 6))
(k19c27_truescore <- round((2 * k19c27_tpr * k19c27_tnr) / (k19c27_tpr + k19c27_tnr), 6))

(k21c27_tpr <- round(sum(cm_c27_tables[1, 91:100]) / (sum(cm_c27_tables[1, 91:100]) + sum(cm_c27_tables[2, 91:100])), 6))
(k21c27_tnr <- round(sum(cm_c27_tables[4, 91:100]) / (sum(cm_c27_tables[4, 91:100]) + sum(cm_c27_tables[3, 91:100])), 6))
(k21c27_truescore <- round((2 * k21c27_tpr * k21c27_tnr) / (k21c27_tpr + k21c27_tnr), 6))

(k23c27_tpr <- round(sum(cm_c27_tables[1, 101:110]) / (sum(cm_c27_tables[1, 101:110]) + sum(cm_c27_tables[2, 101:110])), 6))
(k23c27_tnr <- round(sum(cm_c27_tables[4, 101:110]) / (sum(cm_c27_tables[4, 101:110]) + sum(cm_c27_tables[3, 101:110])), 6))
(k23c27_truescore <- round((2 * k23c27_tpr * k23c27_tnr) / (k23c27_tpr + k23c27_tnr), 6))

(k25c27_tpr <- round(sum(cm_c27_tables[1, 111:120]) / (sum(cm_c27_tables[1, 111:120]) + sum(cm_c27_tables[2, 111:120])), 6))
(k25c27_tnr <- round(sum(cm_c27_tables[4, 111:120]) / (sum(cm_c27_tables[4, 111:120]) + sum(cm_c27_tables[3, 111:120])), 6))
(k25c27_truescore <- round((2 * k25c27_tpr * k25c27_tnr) / (k25c27_tpr + k25c27_tnr), 6))

(k27c27_tpr <- round(sum(cm_c27_tables[1, 121:130]) / (sum(cm_c27_tables[1, 121:130]) + sum(cm_c27_tables[2, 121:130])), 6))
(k27c27_tnr <- round(sum(cm_c27_tables[4, 121:130]) / (sum(cm_c27_tables[4, 121:130]) + sum(cm_c27_tables[3, 121:130])), 6))
(k27c27_truescore <- round((2 * k27c27_tpr * k27c27_tnr) / (k27c27_tpr + k27c27_tnr), 6))

(k29c27_tpr <- round(sum(cm_c27_tables[1, 131:140]) / (sum(cm_c27_tables[1, 131:140]) + sum(cm_c27_tables[2, 131:140])), 6))
(k29c27_tnr <- round(sum(cm_c27_tables[4, 131:140]) / (sum(cm_c27_tables[4, 131:140]) + sum(cm_c27_tables[3, 131:140])), 6))
(k29c27_truescore <- round((2 * k29c27_tpr * k29c27_tnr) / (k29c27_tpr + k29c27_tnr), 6))

(k31c27_tpr <- round(sum(cm_c27_tables[1, 141:150]) / (sum(cm_c27_tables[1, 141:150]) + sum(cm_c27_tables[2, 141:150])), 6))
(k31c27_tnr <- round(sum(cm_c27_tables[4, 141:150]) / (sum(cm_c27_tables[4, 141:150]) + sum(cm_c27_tables[3, 141:150])), 6))
(k31c27_truescore <- round((2 * k31c27_tpr * k31c27_tnr) / (k31c27_tpr + k31c27_tnr), 6))

(k33c27_tpr <- round(sum(cm_c27_tables[1, 151:160]) / (sum(cm_c27_tables[1, 151:160]) + sum(cm_c27_tables[2, 151:160])), 6))
(k33c27_tnr <- round(sum(cm_c27_tables[4, 151:160]) / (sum(cm_c27_tables[4, 151:160]) + sum(cm_c27_tables[3, 151:160])), 6))
(k33c27_truescore <- round((2 * k33c27_tpr * k33c27_tnr) / (k33c27_tpr + k33c27_tnr), 6))

(k35c27_tpr <- round(sum(cm_c27_tables[1, 161:170]) / (sum(cm_c27_tables[1, 161:170]) + sum(cm_c27_tables[2, 161:170])), 6))
(k35c27_tnr <- round(sum(cm_c27_tables[4, 161:170]) / (sum(cm_c27_tables[4, 161:170]) + sum(cm_c27_tables[3, 161:170])), 6))
(k35c27_truescore <- round((2 * k35c27_tpr * k35c27_tnr) / (k35c27_tpr + k35c27_tnr), 6))

(k37c27_tpr <- round(sum(cm_c27_tables[1, 171:180]) / (sum(cm_c27_tables[1, 171:180]) + sum(cm_c27_tables[2, 171:180])), 6))
(k37c27_tnr <- round(sum(cm_c27_tables[4, 171:180]) / (sum(cm_c27_tables[4, 171:180]) + sum(cm_c27_tables[3, 171:180])), 6))
(k37c27_truescore <- round((2 * k37c27_tpr * k37c27_tnr) / (k37c27_tpr + k37c27_tnr), 6))

(k39c27_tpr <- round(sum(cm_c27_tables[1, 181:190]) / (sum(cm_c27_tables[1, 181:190]) + sum(cm_c27_tables[2, 181:190])), 6))
(k39c27_tnr <- round(sum(cm_c27_tables[4, 181:190]) / (sum(cm_c27_tables[4, 181:190]) + sum(cm_c27_tables[3, 181:190])), 6))
(k39c27_truescore <- round((2 * k39c27_tpr * k39c27_tnr) / (k39c27_tpr + k39c27_tnr), 6))

# Compile the 0.27 cutoff results in a table, and identify the value
# of k that maximizes the truescore.

c27_results <- tibble(k = seq(3, 39, 2), 
                      Cut = 0.27, 
                      TPR = c(k3c27_tpr, k5c27_tpr, k7c27_tpr, k9c27_tpr, k11c27_tpr, 
                              k13c27_tpr, k15c27_tpr, k17c27_tpr, k19c27_tpr, k21c27_tpr, 
                              k23c27_tpr, k25c27_tpr, k27c27_tpr, k29c27_tpr, k31c27_tpr, 
                              k33c27_tpr, k35c27_tpr, k37c27_tpr, k39c27_tpr), 
                      TNR = c(k3c27_tnr, k5c27_tnr, k7c27_tnr, k9c27_tnr, k11c27_tnr, 
                              k13c27_tnr, k15c27_tnr, k17c27_tnr, k19c27_tnr, k21c27_tnr, 
                              k23c27_tnr, k25c27_tnr, k27c27_tnr, k29c27_tnr, k31c27_tnr, 
                              k33c27_tnr, k35c27_tnr, k37c27_tnr, k39c27_tnr), 
                      Truescore = c(k3c27_truescore, k5c27_truescore, k7c27_truescore, 
                                    k9c27_truescore, k11c27_truescore, k13c27_truescore, 
                                    k15c27_truescore, k17c27_truescore, k19c27_truescore, 
                                    k21c27_truescore, k23c27_truescore, k25c27_truescore, 
                                    k27c27_truescore, k29c27_truescore, k31c27_truescore, 
                                    k33c27_truescore, k35c27_truescore, k37c27_truescore, 
                                    k39c27_truescore))

knitr::kable(c27_results[1:19, ], caption = "c27_results")

ggplot(c27_results, aes(k, Truescore)) + geom_point() + geom_line() + 
  labs(title = "Optimal k for Decision Cutoff 0.27")

############################
# 0.28 Cutoff
############################

# For the decision cutoff of 0.28, generate a confusion matrix for
# every combination of k (3:39 by two) and fold (1 to 10).

cm_c28 <- sapply(fk_dfs_l, function(x) {
  ss <- subset(x, select = c(pred28, obs))
  confusionMatrix(ss$pred28, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c28) <- fk_dfs_v

cm_c28_tables <- sapply(cm_c28, "[[", 2)
cm_c28_tables <- as_tibble(cm_c28_tables)

# For each value of k, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each value of k when the decision cutoff is 0.28.

(k3c28_tpr <- round(sum(cm_c28_tables[1, 1:10]) / (sum(cm_c28_tables[1, 1:10]) + sum(cm_c28_tables[2, 1:10])), 6))
(k3c28_tnr <- round(sum(cm_c28_tables[4, 1:10]) / (sum(cm_c28_tables[4, 1:10]) + sum(cm_c28_tables[3, 1:10])), 6))
(k3c28_truescore <- round((2 * k3c28_tpr * k3c28_tnr) / (k3c28_tpr + k3c28_tnr), 6))

(k5c28_tpr <- round(sum(cm_c28_tables[1, 11:20]) / (sum(cm_c28_tables[1, 11:20]) + sum(cm_c28_tables[2, 11:20])), 6))
(k5c28_tnr <- round(sum(cm_c28_tables[4, 11:20]) / (sum(cm_c28_tables[4, 11:20]) + sum(cm_c28_tables[3, 11:20])), 6))
(k5c28_truescore <- round((2 * k5c28_tpr * k5c28_tnr) / (k5c28_tpr + k5c28_tnr), 6))

(k7c28_tpr <- round(sum(cm_c28_tables[1, 21:30]) / (sum(cm_c28_tables[1, 21:30]) + sum(cm_c28_tables[2, 21:30])), 6))
(k7c28_tnr <- round(sum(cm_c28_tables[4, 21:30]) / (sum(cm_c28_tables[4, 21:30]) + sum(cm_c28_tables[3, 21:30])), 6))
(k7c28_truescore <- round((2 * k7c28_tpr * k7c28_tnr) / (k7c28_tpr + k7c28_tnr), 6))

(k9c28_tpr <- round(sum(cm_c28_tables[1, 31:40]) / (sum(cm_c28_tables[1, 31:40]) + sum(cm_c28_tables[2, 31:40])), 6))
(k9c28_tnr <- round(sum(cm_c28_tables[4, 31:40]) / (sum(cm_c28_tables[4, 31:40]) + sum(cm_c28_tables[3, 31:40])), 6))
(k9c28_truescore <- round((2 * k9c28_tpr * k9c28_tnr) / (k9c28_tpr + k9c28_tnr), 6))

(k11c28_tpr <- round(sum(cm_c28_tables[1, 41:50]) / (sum(cm_c28_tables[1, 41:50]) + sum(cm_c28_tables[2, 41:50])), 6))
(k11c28_tnr <- round(sum(cm_c28_tables[4, 41:50]) / (sum(cm_c28_tables[4, 41:50]) + sum(cm_c28_tables[3, 41:50])), 6))
(k11c28_truescore <- round((2 * k11c28_tpr * k11c28_tnr) / (k11c28_tpr + k11c28_tnr), 6))

(k13c28_tpr <- round(sum(cm_c28_tables[1, 51:60]) / (sum(cm_c28_tables[1, 51:60]) + sum(cm_c28_tables[2, 51:60])), 6))
(k13c28_tnr <- round(sum(cm_c28_tables[4, 51:60]) / (sum(cm_c28_tables[4, 51:60]) + sum(cm_c28_tables[3, 51:60])), 6))
(k13c28_truescore <- round((2 * k13c28_tpr * k13c28_tnr) / (k13c28_tpr + k13c28_tnr), 6))

(k15c28_tpr <- round(sum(cm_c28_tables[1, 61:70]) / (sum(cm_c28_tables[1, 61:70]) + sum(cm_c28_tables[2, 61:70])), 6))
(k15c28_tnr <- round(sum(cm_c28_tables[4, 61:70]) / (sum(cm_c28_tables[4, 61:70]) + sum(cm_c28_tables[3, 61:70])), 6))
(k15c28_truescore <- round((2 * k15c28_tpr * k15c28_tnr) / (k15c28_tpr + k15c28_tnr), 6))

(k17c28_tpr <- round(sum(cm_c28_tables[1, 71:80]) / (sum(cm_c28_tables[1, 71:80]) + sum(cm_c28_tables[2, 71:80])), 6))
(k17c28_tnr <- round(sum(cm_c28_tables[4, 71:80]) / (sum(cm_c28_tables[4, 71:80]) + sum(cm_c28_tables[3, 71:80])), 6))
(k17c28_truescore <- round((2 * k17c28_tpr * k17c28_tnr) / (k17c28_tpr + k17c28_tnr), 6))

(k19c28_tpr <- round(sum(cm_c28_tables[1, 81:90]) / (sum(cm_c28_tables[1, 81:90]) + sum(cm_c28_tables[2, 81:90])), 6))
(k19c28_tnr <- round(sum(cm_c28_tables[4, 81:90]) / (sum(cm_c28_tables[4, 81:90]) + sum(cm_c28_tables[3, 81:90])), 6))
(k19c28_truescore <- round((2 * k19c28_tpr * k19c28_tnr) / (k19c28_tpr + k19c28_tnr), 6))

(k21c28_tpr <- round(sum(cm_c28_tables[1, 91:100]) / (sum(cm_c28_tables[1, 91:100]) + sum(cm_c28_tables[2, 91:100])), 6))
(k21c28_tnr <- round(sum(cm_c28_tables[4, 91:100]) / (sum(cm_c28_tables[4, 91:100]) + sum(cm_c28_tables[3, 91:100])), 6))
(k21c28_truescore <- round((2 * k21c28_tpr * k21c28_tnr) / (k21c28_tpr + k21c28_tnr), 6))

(k23c28_tpr <- round(sum(cm_c28_tables[1, 101:110]) / (sum(cm_c28_tables[1, 101:110]) + sum(cm_c28_tables[2, 101:110])), 6))
(k23c28_tnr <- round(sum(cm_c28_tables[4, 101:110]) / (sum(cm_c28_tables[4, 101:110]) + sum(cm_c28_tables[3, 101:110])), 6))
(k23c28_truescore <- round((2 * k23c28_tpr * k23c28_tnr) / (k23c28_tpr + k23c28_tnr), 6))

(k25c28_tpr <- round(sum(cm_c28_tables[1, 111:120]) / (sum(cm_c28_tables[1, 111:120]) + sum(cm_c28_tables[2, 111:120])), 6))
(k25c28_tnr <- round(sum(cm_c28_tables[4, 111:120]) / (sum(cm_c28_tables[4, 111:120]) + sum(cm_c28_tables[3, 111:120])), 6))
(k25c28_truescore <- round((2 * k25c28_tpr * k25c28_tnr) / (k25c28_tpr + k25c28_tnr), 6))

(k27c28_tpr <- round(sum(cm_c28_tables[1, 121:130]) / (sum(cm_c28_tables[1, 121:130]) + sum(cm_c28_tables[2, 121:130])), 6))
(k27c28_tnr <- round(sum(cm_c28_tables[4, 121:130]) / (sum(cm_c28_tables[4, 121:130]) + sum(cm_c28_tables[3, 121:130])), 6))
(k27c28_truescore <- round((2 * k27c28_tpr * k27c28_tnr) / (k27c28_tpr + k27c28_tnr), 6))

(k29c28_tpr <- round(sum(cm_c28_tables[1, 131:140]) / (sum(cm_c28_tables[1, 131:140]) + sum(cm_c28_tables[2, 131:140])), 6))
(k29c28_tnr <- round(sum(cm_c28_tables[4, 131:140]) / (sum(cm_c28_tables[4, 131:140]) + sum(cm_c28_tables[3, 131:140])), 6))
(k29c28_truescore <- round((2 * k29c28_tpr * k29c28_tnr) / (k29c28_tpr + k29c28_tnr), 6))

(k31c28_tpr <- round(sum(cm_c28_tables[1, 141:150]) / (sum(cm_c28_tables[1, 141:150]) + sum(cm_c28_tables[2, 141:150])), 6))
(k31c28_tnr <- round(sum(cm_c28_tables[4, 141:150]) / (sum(cm_c28_tables[4, 141:150]) + sum(cm_c28_tables[3, 141:150])), 6))
(k31c28_truescore <- round((2 * k31c28_tpr * k31c28_tnr) / (k31c28_tpr + k31c28_tnr), 6))

(k33c28_tpr <- round(sum(cm_c28_tables[1, 151:160]) / (sum(cm_c28_tables[1, 151:160]) + sum(cm_c28_tables[2, 151:160])), 6))
(k33c28_tnr <- round(sum(cm_c28_tables[4, 151:160]) / (sum(cm_c28_tables[4, 151:160]) + sum(cm_c28_tables[3, 151:160])), 6))
(k33c28_truescore <- round((2 * k33c28_tpr * k33c28_tnr) / (k33c28_tpr + k33c28_tnr), 6))

(k35c28_tpr <- round(sum(cm_c28_tables[1, 161:170]) / (sum(cm_c28_tables[1, 161:170]) + sum(cm_c28_tables[2, 161:170])), 6))
(k35c28_tnr <- round(sum(cm_c28_tables[4, 161:170]) / (sum(cm_c28_tables[4, 161:170]) + sum(cm_c28_tables[3, 161:170])), 6))
(k35c28_truescore <- round((2 * k35c28_tpr * k35c28_tnr) / (k35c28_tpr + k35c28_tnr), 6))

(k37c28_tpr <- round(sum(cm_c28_tables[1, 171:180]) / (sum(cm_c28_tables[1, 171:180]) + sum(cm_c28_tables[2, 171:180])), 6))
(k37c28_tnr <- round(sum(cm_c28_tables[4, 171:180]) / (sum(cm_c28_tables[4, 171:180]) + sum(cm_c28_tables[3, 171:180])), 6))
(k37c28_truescore <- round((2 * k37c28_tpr * k37c28_tnr) / (k37c28_tpr + k37c28_tnr), 6))

(k39c28_tpr <- round(sum(cm_c28_tables[1, 181:190]) / (sum(cm_c28_tables[1, 181:190]) + sum(cm_c28_tables[2, 181:190])), 6))
(k39c28_tnr <- round(sum(cm_c28_tables[4, 181:190]) / (sum(cm_c28_tables[4, 181:190]) + sum(cm_c28_tables[3, 181:190])), 6))
(k39c28_truescore <- round((2 * k39c28_tpr * k39c28_tnr) / (k39c28_tpr + k39c28_tnr), 6))

# Compile the 0.28 cutoff results in a table, and identify the value
# of k that maximizes the truescore.

c28_results <- tibble(k = seq(3, 39, 2), 
                      Cut = 0.28, 
                      TPR = c(k3c28_tpr, k5c28_tpr, k7c28_tpr, k9c28_tpr, k11c28_tpr, 
                              k13c28_tpr, k15c28_tpr, k17c28_tpr, k19c28_tpr, k21c28_tpr, 
                              k23c28_tpr, k25c28_tpr, k27c28_tpr, k29c28_tpr, k31c28_tpr, 
                              k33c28_tpr, k35c28_tpr, k37c28_tpr, k39c28_tpr), 
                      TNR = c(k3c28_tnr, k5c28_tnr, k7c28_tnr, k9c28_tnr, k11c28_tnr, 
                              k13c28_tnr, k15c28_tnr, k17c28_tnr, k19c28_tnr, k21c28_tnr, 
                              k23c28_tnr, k25c28_tnr, k27c28_tnr, k29c28_tnr, k31c28_tnr, 
                              k33c28_tnr, k35c28_tnr, k37c28_tnr, k39c28_tnr), 
                      Truescore = c(k3c28_truescore, k5c28_truescore, k7c28_truescore, 
                                    k9c28_truescore, k11c28_truescore, k13c28_truescore, 
                                    k15c28_truescore, k17c28_truescore, k19c28_truescore, 
                                    k21c28_truescore, k23c28_truescore, k25c28_truescore, 
                                    k27c28_truescore, k29c28_truescore, k31c28_truescore, 
                                    k33c28_truescore, k35c28_truescore, k37c28_truescore, 
                                    k39c28_truescore))

knitr::kable(c28_results[1:19, ], caption = "c28_results")

ggplot(c28_results, aes(k, Truescore)) + geom_point() + geom_line() + 
  labs(title = "Optimal k for Decision Cutoff 0.28")

############################
# 0.29 Cutoff
############################

# For the decision cutoff of 0.29, generate a confusion matrix for
# every combination of k (3:39 by two) and fold (1 to 10).

cm_c29 <- sapply(fk_dfs_l, function(x) {
  ss <- subset(x, select = c(pred29, obs))
  confusionMatrix(ss$pred29, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c29) <- fk_dfs_v

cm_c29_tables <- sapply(cm_c29, "[[", 2)
cm_c29_tables <- as_tibble(cm_c29_tables)

# For each value of k, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each value of k when the decision cutoff is 0.29.

(k3c29_tpr <- round(sum(cm_c29_tables[1, 1:10]) / (sum(cm_c29_tables[1, 1:10]) + sum(cm_c29_tables[2, 1:10])), 6))
(k3c29_tnr <- round(sum(cm_c29_tables[4, 1:10]) / (sum(cm_c29_tables[4, 1:10]) + sum(cm_c29_tables[3, 1:10])), 6))
(k3c29_truescore <- round((2 * k3c29_tpr * k3c29_tnr) / (k3c29_tpr + k3c29_tnr), 6))

(k5c29_tpr <- round(sum(cm_c29_tables[1, 11:20]) / (sum(cm_c29_tables[1, 11:20]) + sum(cm_c29_tables[2, 11:20])), 6))
(k5c29_tnr <- round(sum(cm_c29_tables[4, 11:20]) / (sum(cm_c29_tables[4, 11:20]) + sum(cm_c29_tables[3, 11:20])), 6))
(k5c29_truescore <- round((2 * k5c29_tpr * k5c29_tnr) / (k5c29_tpr + k5c29_tnr), 6))

(k7c29_tpr <- round(sum(cm_c29_tables[1, 21:30]) / (sum(cm_c29_tables[1, 21:30]) + sum(cm_c29_tables[2, 21:30])), 6))
(k7c29_tnr <- round(sum(cm_c29_tables[4, 21:30]) / (sum(cm_c29_tables[4, 21:30]) + sum(cm_c29_tables[3, 21:30])), 6))
(k7c29_truescore <- round((2 * k7c29_tpr * k7c29_tnr) / (k7c29_tpr + k7c29_tnr), 6))

(k9c29_tpr <- round(sum(cm_c29_tables[1, 31:40]) / (sum(cm_c29_tables[1, 31:40]) + sum(cm_c29_tables[2, 31:40])), 6))
(k9c29_tnr <- round(sum(cm_c29_tables[4, 31:40]) / (sum(cm_c29_tables[4, 31:40]) + sum(cm_c29_tables[3, 31:40])), 6))
(k9c29_truescore <- round((2 * k9c29_tpr * k9c29_tnr) / (k9c29_tpr + k9c29_tnr), 6))

(k11c29_tpr <- round(sum(cm_c29_tables[1, 41:50]) / (sum(cm_c29_tables[1, 41:50]) + sum(cm_c29_tables[2, 41:50])), 6))
(k11c29_tnr <- round(sum(cm_c29_tables[4, 41:50]) / (sum(cm_c29_tables[4, 41:50]) + sum(cm_c29_tables[3, 41:50])), 6))
(k11c29_truescore <- round((2 * k11c29_tpr * k11c29_tnr) / (k11c29_tpr + k11c29_tnr), 6))

(k13c29_tpr <- round(sum(cm_c29_tables[1, 51:60]) / (sum(cm_c29_tables[1, 51:60]) + sum(cm_c29_tables[2, 51:60])), 6))
(k13c29_tnr <- round(sum(cm_c29_tables[4, 51:60]) / (sum(cm_c29_tables[4, 51:60]) + sum(cm_c29_tables[3, 51:60])), 6))
(k13c29_truescore <- round((2 * k13c29_tpr * k13c29_tnr) / (k13c29_tpr + k13c29_tnr), 6))

(k15c29_tpr <- round(sum(cm_c29_tables[1, 61:70]) / (sum(cm_c29_tables[1, 61:70]) + sum(cm_c29_tables[2, 61:70])), 6))
(k15c29_tnr <- round(sum(cm_c29_tables[4, 61:70]) / (sum(cm_c29_tables[4, 61:70]) + sum(cm_c29_tables[3, 61:70])), 6))
(k15c29_truescore <- round((2 * k15c29_tpr * k15c29_tnr) / (k15c29_tpr + k15c29_tnr), 6))

(k17c29_tpr <- round(sum(cm_c29_tables[1, 71:80]) / (sum(cm_c29_tables[1, 71:80]) + sum(cm_c29_tables[2, 71:80])), 6))
(k17c29_tnr <- round(sum(cm_c29_tables[4, 71:80]) / (sum(cm_c29_tables[4, 71:80]) + sum(cm_c29_tables[3, 71:80])), 6))
(k17c29_truescore <- round((2 * k17c29_tpr * k17c29_tnr) / (k17c29_tpr + k17c29_tnr), 6))

(k19c29_tpr <- round(sum(cm_c29_tables[1, 81:90]) / (sum(cm_c29_tables[1, 81:90]) + sum(cm_c29_tables[2, 81:90])), 6))
(k19c29_tnr <- round(sum(cm_c29_tables[4, 81:90]) / (sum(cm_c29_tables[4, 81:90]) + sum(cm_c29_tables[3, 81:90])), 6))
(k19c29_truescore <- round((2 * k19c29_tpr * k19c29_tnr) / (k19c29_tpr + k19c29_tnr), 6))

(k21c29_tpr <- round(sum(cm_c29_tables[1, 91:100]) / (sum(cm_c29_tables[1, 91:100]) + sum(cm_c29_tables[2, 91:100])), 6))
(k21c29_tnr <- round(sum(cm_c29_tables[4, 91:100]) / (sum(cm_c29_tables[4, 91:100]) + sum(cm_c29_tables[3, 91:100])), 6))
(k21c29_truescore <- round((2 * k21c29_tpr * k21c29_tnr) / (k21c29_tpr + k21c29_tnr), 6))

(k23c29_tpr <- round(sum(cm_c29_tables[1, 101:110]) / (sum(cm_c29_tables[1, 101:110]) + sum(cm_c29_tables[2, 101:110])), 6))
(k23c29_tnr <- round(sum(cm_c29_tables[4, 101:110]) / (sum(cm_c29_tables[4, 101:110]) + sum(cm_c29_tables[3, 101:110])), 6))
(k23c29_truescore <- round((2 * k23c29_tpr * k23c29_tnr) / (k23c29_tpr + k23c29_tnr), 6))

(k25c29_tpr <- round(sum(cm_c29_tables[1, 111:120]) / (sum(cm_c29_tables[1, 111:120]) + sum(cm_c29_tables[2, 111:120])), 6))
(k25c29_tnr <- round(sum(cm_c29_tables[4, 111:120]) / (sum(cm_c29_tables[4, 111:120]) + sum(cm_c29_tables[3, 111:120])), 6))
(k25c29_truescore <- round((2 * k25c29_tpr * k25c29_tnr) / (k25c29_tpr + k25c29_tnr), 6))

(k27c29_tpr <- round(sum(cm_c29_tables[1, 121:130]) / (sum(cm_c29_tables[1, 121:130]) + sum(cm_c29_tables[2, 121:130])), 6))
(k27c29_tnr <- round(sum(cm_c29_tables[4, 121:130]) / (sum(cm_c29_tables[4, 121:130]) + sum(cm_c29_tables[3, 121:130])), 6))
(k27c29_truescore <- round((2 * k27c29_tpr * k27c29_tnr) / (k27c29_tpr + k27c29_tnr), 6))

(k29c29_tpr <- round(sum(cm_c29_tables[1, 131:140]) / (sum(cm_c29_tables[1, 131:140]) + sum(cm_c29_tables[2, 131:140])), 6))
(k29c29_tnr <- round(sum(cm_c29_tables[4, 131:140]) / (sum(cm_c29_tables[4, 131:140]) + sum(cm_c29_tables[3, 131:140])), 6))
(k29c29_truescore <- round((2 * k29c29_tpr * k29c29_tnr) / (k29c29_tpr + k29c29_tnr), 6))

(k31c29_tpr <- round(sum(cm_c29_tables[1, 141:150]) / (sum(cm_c29_tables[1, 141:150]) + sum(cm_c29_tables[2, 141:150])), 6))
(k31c29_tnr <- round(sum(cm_c29_tables[4, 141:150]) / (sum(cm_c29_tables[4, 141:150]) + sum(cm_c29_tables[3, 141:150])), 6))
(k31c29_truescore <- round((2 * k31c29_tpr * k31c29_tnr) / (k31c29_tpr + k31c29_tnr), 6))

(k33c29_tpr <- round(sum(cm_c29_tables[1, 151:160]) / (sum(cm_c29_tables[1, 151:160]) + sum(cm_c29_tables[2, 151:160])), 6))
(k33c29_tnr <- round(sum(cm_c29_tables[4, 151:160]) / (sum(cm_c29_tables[4, 151:160]) + sum(cm_c29_tables[3, 151:160])), 6))
(k33c29_truescore <- round((2 * k33c29_tpr * k33c29_tnr) / (k33c29_tpr + k33c29_tnr), 6))

(k35c29_tpr <- round(sum(cm_c29_tables[1, 161:170]) / (sum(cm_c29_tables[1, 161:170]) + sum(cm_c29_tables[2, 161:170])), 6))
(k35c29_tnr <- round(sum(cm_c29_tables[4, 161:170]) / (sum(cm_c29_tables[4, 161:170]) + sum(cm_c29_tables[3, 161:170])), 6))
(k35c29_truescore <- round((2 * k35c29_tpr * k35c29_tnr) / (k35c29_tpr + k35c29_tnr), 6))

(k37c29_tpr <- round(sum(cm_c29_tables[1, 171:180]) / (sum(cm_c29_tables[1, 171:180]) + sum(cm_c29_tables[2, 171:180])), 6))
(k37c29_tnr <- round(sum(cm_c29_tables[4, 171:180]) / (sum(cm_c29_tables[4, 171:180]) + sum(cm_c29_tables[3, 171:180])), 6))
(k37c29_truescore <- round((2 * k37c29_tpr * k37c29_tnr) / (k37c29_tpr + k37c29_tnr), 6))

(k39c29_tpr <- round(sum(cm_c29_tables[1, 181:190]) / (sum(cm_c29_tables[1, 181:190]) + sum(cm_c29_tables[2, 181:190])), 6))
(k39c29_tnr <- round(sum(cm_c29_tables[4, 181:190]) / (sum(cm_c29_tables[4, 181:190]) + sum(cm_c29_tables[3, 181:190])), 6))
(k39c29_truescore <- round((2 * k39c29_tpr * k39c29_tnr) / (k39c29_tpr + k39c29_tnr), 6))

# Compile the 0.29 cutoff results in a table, and identify the value
# of k that maximizes the truescore.

c29_results <- tibble(k = seq(3, 39, 2), 
                      Cut = 0.29, 
                      TPR = c(k3c29_tpr, k5c29_tpr, k7c29_tpr, k9c29_tpr, k11c29_tpr, 
                              k13c29_tpr, k15c29_tpr, k17c29_tpr, k19c29_tpr, k21c29_tpr, 
                              k23c29_tpr, k25c29_tpr, k27c29_tpr, k29c29_tpr, k31c29_tpr, 
                              k33c29_tpr, k35c29_tpr, k37c29_tpr, k39c29_tpr), 
                      TNR = c(k3c29_tnr, k5c29_tnr, k7c29_tnr, k9c29_tnr, k11c29_tnr, 
                              k13c29_tnr, k15c29_tnr, k17c29_tnr, k19c29_tnr, k21c29_tnr, 
                              k23c29_tnr, k25c29_tnr, k27c29_tnr, k29c29_tnr, k31c29_tnr, 
                              k33c29_tnr, k35c29_tnr, k37c29_tnr, k39c29_tnr), 
                      Truescore = c(k3c29_truescore, k5c29_truescore, k7c29_truescore, 
                                    k9c29_truescore, k11c29_truescore, k13c29_truescore, 
                                    k15c29_truescore, k17c29_truescore, k19c29_truescore, 
                                    k21c29_truescore, k23c29_truescore, k25c29_truescore, 
                                    k27c29_truescore, k29c29_truescore, k31c29_truescore, 
                                    k33c29_truescore, k35c29_truescore, k37c29_truescore, 
                                    k39c29_truescore))

knitr::kable(c29_results[1:19, ], caption = "c29_results")

ggplot(c29_results, aes(k, Truescore)) + geom_point() + geom_line() + 
  labs(title = "Optimal k for Decision Cutoff 0.29")

############################
# 0.30 Cutoff
############################

# For the decision cutoff of 0.30, generate a confusion matrix for
# every combination of k (3:39 by two) and fold (1 to 10).

cm_c30 <- sapply(fk_dfs_l, function(x) {
  ss <- subset(x, select = c(pred30, obs))
  confusionMatrix(ss$pred30, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c30) <- fk_dfs_v

cm_c30_tables <- sapply(cm_c30, "[[", 2)
cm_c30_tables <- as_tibble(cm_c30_tables)

# For each value of k, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each value of k when the decision cutoff is 0.30.

(k3c30_tpr <- round(sum(cm_c30_tables[1, 1:10]) / (sum(cm_c30_tables[1, 1:10]) + sum(cm_c30_tables[2, 1:10])), 6))
(k3c30_tnr <- round(sum(cm_c30_tables[4, 1:10]) / (sum(cm_c30_tables[4, 1:10]) + sum(cm_c30_tables[3, 1:10])), 6))
(k3c30_truescore <- round((2 * k3c30_tpr * k3c30_tnr) / (k3c30_tpr + k3c30_tnr), 6))

(k5c30_tpr <- round(sum(cm_c30_tables[1, 11:20]) / (sum(cm_c30_tables[1, 11:20]) + sum(cm_c30_tables[2, 11:20])), 6))
(k5c30_tnr <- round(sum(cm_c30_tables[4, 11:20]) / (sum(cm_c30_tables[4, 11:20]) + sum(cm_c30_tables[3, 11:20])), 6))
(k5c30_truescore <- round((2 * k5c30_tpr * k5c30_tnr) / (k5c30_tpr + k5c30_tnr), 6))

(k7c30_tpr <- round(sum(cm_c30_tables[1, 21:30]) / (sum(cm_c30_tables[1, 21:30]) + sum(cm_c30_tables[2, 21:30])), 6))
(k7c30_tnr <- round(sum(cm_c30_tables[4, 21:30]) / (sum(cm_c30_tables[4, 21:30]) + sum(cm_c30_tables[3, 21:30])), 6))
(k7c30_truescore <- round((2 * k7c30_tpr * k7c30_tnr) / (k7c30_tpr + k7c30_tnr), 6))

(k9c30_tpr <- round(sum(cm_c30_tables[1, 31:40]) / (sum(cm_c30_tables[1, 31:40]) + sum(cm_c30_tables[2, 31:40])), 6))
(k9c30_tnr <- round(sum(cm_c30_tables[4, 31:40]) / (sum(cm_c30_tables[4, 31:40]) + sum(cm_c30_tables[3, 31:40])), 6))
(k9c30_truescore <- round((2 * k9c30_tpr * k9c30_tnr) / (k9c30_tpr + k9c30_tnr), 6))

(k11c30_tpr <- round(sum(cm_c30_tables[1, 41:50]) / (sum(cm_c30_tables[1, 41:50]) + sum(cm_c30_tables[2, 41:50])), 6))
(k11c30_tnr <- round(sum(cm_c30_tables[4, 41:50]) / (sum(cm_c30_tables[4, 41:50]) + sum(cm_c30_tables[3, 41:50])), 6))
(k11c30_truescore <- round((2 * k11c30_tpr * k11c30_tnr) / (k11c30_tpr + k11c30_tnr), 6))

(k13c30_tpr <- round(sum(cm_c30_tables[1, 51:60]) / (sum(cm_c30_tables[1, 51:60]) + sum(cm_c30_tables[2, 51:60])), 6))
(k13c30_tnr <- round(sum(cm_c30_tables[4, 51:60]) / (sum(cm_c30_tables[4, 51:60]) + sum(cm_c30_tables[3, 51:60])), 6))
(k13c30_truescore <- round((2 * k13c30_tpr * k13c30_tnr) / (k13c30_tpr + k13c30_tnr), 6))

(k15c30_tpr <- round(sum(cm_c30_tables[1, 61:70]) / (sum(cm_c30_tables[1, 61:70]) + sum(cm_c30_tables[2, 61:70])), 6))
(k15c30_tnr <- round(sum(cm_c30_tables[4, 61:70]) / (sum(cm_c30_tables[4, 61:70]) + sum(cm_c30_tables[3, 61:70])), 6))
(k15c30_truescore <- round((2 * k15c30_tpr * k15c30_tnr) / (k15c30_tpr + k15c30_tnr), 6))

(k17c30_tpr <- round(sum(cm_c30_tables[1, 71:80]) / (sum(cm_c30_tables[1, 71:80]) + sum(cm_c30_tables[2, 71:80])), 6))
(k17c30_tnr <- round(sum(cm_c30_tables[4, 71:80]) / (sum(cm_c30_tables[4, 71:80]) + sum(cm_c30_tables[3, 71:80])), 6))
(k17c30_truescore <- round((2 * k17c30_tpr * k17c30_tnr) / (k17c30_tpr + k17c30_tnr), 6))

(k19c30_tpr <- round(sum(cm_c30_tables[1, 81:90]) / (sum(cm_c30_tables[1, 81:90]) + sum(cm_c30_tables[2, 81:90])), 6))
(k19c30_tnr <- round(sum(cm_c30_tables[4, 81:90]) / (sum(cm_c30_tables[4, 81:90]) + sum(cm_c30_tables[3, 81:90])), 6))
(k19c30_truescore <- round((2 * k19c30_tpr * k19c30_tnr) / (k19c30_tpr + k19c30_tnr), 6))

(k21c30_tpr <- round(sum(cm_c30_tables[1, 91:100]) / (sum(cm_c30_tables[1, 91:100]) + sum(cm_c30_tables[2, 91:100])), 6))
(k21c30_tnr <- round(sum(cm_c30_tables[4, 91:100]) / (sum(cm_c30_tables[4, 91:100]) + sum(cm_c30_tables[3, 91:100])), 6))
(k21c30_truescore <- round((2 * k21c30_tpr * k21c30_tnr) / (k21c30_tpr + k21c30_tnr), 6))

(k23c30_tpr <- round(sum(cm_c30_tables[1, 101:110]) / (sum(cm_c30_tables[1, 101:110]) + sum(cm_c30_tables[2, 101:110])), 6))
(k23c30_tnr <- round(sum(cm_c30_tables[4, 101:110]) / (sum(cm_c30_tables[4, 101:110]) + sum(cm_c30_tables[3, 101:110])), 6))
(k23c30_truescore <- round((2 * k23c30_tpr * k23c30_tnr) / (k23c30_tpr + k23c30_tnr), 6))

(k25c30_tpr <- round(sum(cm_c30_tables[1, 111:120]) / (sum(cm_c30_tables[1, 111:120]) + sum(cm_c30_tables[2, 111:120])), 6))
(k25c30_tnr <- round(sum(cm_c30_tables[4, 111:120]) / (sum(cm_c30_tables[4, 111:120]) + sum(cm_c30_tables[3, 111:120])), 6))
(k25c30_truescore <- round((2 * k25c30_tpr * k25c30_tnr) / (k25c30_tpr + k25c30_tnr), 6))

(k27c30_tpr <- round(sum(cm_c30_tables[1, 121:130]) / (sum(cm_c30_tables[1, 121:130]) + sum(cm_c30_tables[2, 121:130])), 6))
(k27c30_tnr <- round(sum(cm_c30_tables[4, 121:130]) / (sum(cm_c30_tables[4, 121:130]) + sum(cm_c30_tables[3, 121:130])), 6))
(k27c30_truescore <- round((2 * k27c30_tpr * k27c30_tnr) / (k27c30_tpr + k27c30_tnr), 6))

(k29c30_tpr <- round(sum(cm_c30_tables[1, 131:140]) / (sum(cm_c30_tables[1, 131:140]) + sum(cm_c30_tables[2, 131:140])), 6))
(k29c30_tnr <- round(sum(cm_c30_tables[4, 131:140]) / (sum(cm_c30_tables[4, 131:140]) + sum(cm_c30_tables[3, 131:140])), 6))
(k29c30_truescore <- round((2 * k29c30_tpr * k29c30_tnr) / (k29c30_tpr + k29c30_tnr), 6))

(k31c30_tpr <- round(sum(cm_c30_tables[1, 141:150]) / (sum(cm_c30_tables[1, 141:150]) + sum(cm_c30_tables[2, 141:150])), 6))
(k31c30_tnr <- round(sum(cm_c30_tables[4, 141:150]) / (sum(cm_c30_tables[4, 141:150]) + sum(cm_c30_tables[3, 141:150])), 6))
(k31c30_truescore <- round((2 * k31c30_tpr * k31c30_tnr) / (k31c30_tpr + k31c30_tnr), 6))

(k33c30_tpr <- round(sum(cm_c30_tables[1, 151:160]) / (sum(cm_c30_tables[1, 151:160]) + sum(cm_c30_tables[2, 151:160])), 6))
(k33c30_tnr <- round(sum(cm_c30_tables[4, 151:160]) / (sum(cm_c30_tables[4, 151:160]) + sum(cm_c30_tables[3, 151:160])), 6))
(k33c30_truescore <- round((2 * k33c30_tpr * k33c30_tnr) / (k33c30_tpr + k33c30_tnr), 6))

(k35c30_tpr <- round(sum(cm_c30_tables[1, 161:170]) / (sum(cm_c30_tables[1, 161:170]) + sum(cm_c30_tables[2, 161:170])), 6))
(k35c30_tnr <- round(sum(cm_c30_tables[4, 161:170]) / (sum(cm_c30_tables[4, 161:170]) + sum(cm_c30_tables[3, 161:170])), 6))
(k35c30_truescore <- round((2 * k35c30_tpr * k35c30_tnr) / (k35c30_tpr + k35c30_tnr), 6))

(k37c30_tpr <- round(sum(cm_c30_tables[1, 171:180]) / (sum(cm_c30_tables[1, 171:180]) + sum(cm_c30_tables[2, 171:180])), 6))
(k37c30_tnr <- round(sum(cm_c30_tables[4, 171:180]) / (sum(cm_c30_tables[4, 171:180]) + sum(cm_c30_tables[3, 171:180])), 6))
(k37c30_truescore <- round((2 * k37c30_tpr * k37c30_tnr) / (k37c30_tpr + k37c30_tnr), 6))

(k39c30_tpr <- round(sum(cm_c30_tables[1, 181:190]) / (sum(cm_c30_tables[1, 181:190]) + sum(cm_c30_tables[2, 181:190])), 6))
(k39c30_tnr <- round(sum(cm_c30_tables[4, 181:190]) / (sum(cm_c30_tables[4, 181:190]) + sum(cm_c30_tables[3, 181:190])), 6))
(k39c30_truescore <- round((2 * k39c30_tpr * k39c30_tnr) / (k39c30_tpr + k39c30_tnr), 6))

# Compile the 0.30 cutoff results in a table, and identify the value
# of k that maximizes the truescore.

c30_results <- tibble(k = seq(3, 39, 2), 
                      Cut = 0.30, 
                      TPR = c(k3c30_tpr, k5c30_tpr, k7c30_tpr, k9c30_tpr, k11c30_tpr, 
                              k13c30_tpr, k15c30_tpr, k17c30_tpr, k19c30_tpr, k21c30_tpr, 
                              k23c30_tpr, k25c30_tpr, k27c30_tpr, k29c30_tpr, k31c30_tpr, 
                              k33c30_tpr, k35c30_tpr, k37c30_tpr, k39c30_tpr), 
                      TNR = c(k3c30_tnr, k5c30_tnr, k7c30_tnr, k9c30_tnr, k11c30_tnr, 
                              k13c30_tnr, k15c30_tnr, k17c30_tnr, k19c30_tnr, k21c30_tnr, 
                              k23c30_tnr, k25c30_tnr, k27c30_tnr, k29c30_tnr, k31c30_tnr, 
                              k33c30_tnr, k35c30_tnr, k37c30_tnr, k39c30_tnr), 
                      Truescore = c(k3c30_truescore, k5c30_truescore, k7c30_truescore, 
                                    k9c30_truescore, k11c30_truescore, k13c30_truescore, 
                                    k15c30_truescore, k17c30_truescore, k19c30_truescore, 
                                    k21c30_truescore, k23c30_truescore, k25c30_truescore, 
                                    k27c30_truescore, k29c30_truescore, k31c30_truescore, 
                                    k33c30_truescore, k35c30_truescore, k37c30_truescore, 
                                    k39c30_truescore))

knitr::kable(c30_results[1:19, ], caption = "c30_results")

ggplot(c30_results, aes(k, Truescore)) + geom_point() + geom_line() + 
  labs(title = "Optimal k for Decision Cutoff 0.30")

############################
# 0.31 Cutoff
############################

# For the decision cutoff of 0.31, generate a confusion matrix for
# every combination of k (3:39 by two) and fold (1 to 10).

cm_c31 <- sapply(fk_dfs_l, function(x) {
  ss <- subset(x, select = c(pred31, obs))
  confusionMatrix(ss$pred31, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c31) <- fk_dfs_v

cm_c31_tables <- sapply(cm_c31, "[[", 2)
cm_c31_tables <- as_tibble(cm_c31_tables)

# For each value of k, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each value of k when the decision cutoff is 0.31.

(k3c31_tpr <- round(sum(cm_c31_tables[1, 1:10]) / (sum(cm_c31_tables[1, 1:10]) + sum(cm_c31_tables[2, 1:10])), 6))
(k3c31_tnr <- round(sum(cm_c31_tables[4, 1:10]) / (sum(cm_c31_tables[4, 1:10]) + sum(cm_c31_tables[3, 1:10])), 6))
(k3c31_truescore <- round((2 * k3c31_tpr * k3c31_tnr) / (k3c31_tpr + k3c31_tnr), 6))

(k5c31_tpr <- round(sum(cm_c31_tables[1, 11:20]) / (sum(cm_c31_tables[1, 11:20]) + sum(cm_c31_tables[2, 11:20])), 6))
(k5c31_tnr <- round(sum(cm_c31_tables[4, 11:20]) / (sum(cm_c31_tables[4, 11:20]) + sum(cm_c31_tables[3, 11:20])), 6))
(k5c31_truescore <- round((2 * k5c31_tpr * k5c31_tnr) / (k5c31_tpr + k5c31_tnr), 6))

(k7c31_tpr <- round(sum(cm_c31_tables[1, 21:30]) / (sum(cm_c31_tables[1, 21:30]) + sum(cm_c31_tables[2, 21:30])), 6))
(k7c31_tnr <- round(sum(cm_c31_tables[4, 21:30]) / (sum(cm_c31_tables[4, 21:30]) + sum(cm_c31_tables[3, 21:30])), 6))
(k7c31_truescore <- round((2 * k7c31_tpr * k7c31_tnr) / (k7c31_tpr + k7c31_tnr), 6))

(k9c31_tpr <- round(sum(cm_c31_tables[1, 31:40]) / (sum(cm_c31_tables[1, 31:40]) + sum(cm_c31_tables[2, 31:40])), 6))
(k9c31_tnr <- round(sum(cm_c31_tables[4, 31:40]) / (sum(cm_c31_tables[4, 31:40]) + sum(cm_c31_tables[3, 31:40])), 6))
(k9c31_truescore <- round((2 * k9c31_tpr * k9c31_tnr) / (k9c31_tpr + k9c31_tnr), 6))

(k11c31_tpr <- round(sum(cm_c31_tables[1, 41:50]) / (sum(cm_c31_tables[1, 41:50]) + sum(cm_c31_tables[2, 41:50])), 6))
(k11c31_tnr <- round(sum(cm_c31_tables[4, 41:50]) / (sum(cm_c31_tables[4, 41:50]) + sum(cm_c31_tables[3, 41:50])), 6))
(k11c31_truescore <- round((2 * k11c31_tpr * k11c31_tnr) / (k11c31_tpr + k11c31_tnr), 6))

(k13c31_tpr <- round(sum(cm_c31_tables[1, 51:60]) / (sum(cm_c31_tables[1, 51:60]) + sum(cm_c31_tables[2, 51:60])), 6))
(k13c31_tnr <- round(sum(cm_c31_tables[4, 51:60]) / (sum(cm_c31_tables[4, 51:60]) + sum(cm_c31_tables[3, 51:60])), 6))
(k13c31_truescore <- round((2 * k13c31_tpr * k13c31_tnr) / (k13c31_tpr + k13c31_tnr), 6))

(k15c31_tpr <- round(sum(cm_c31_tables[1, 61:70]) / (sum(cm_c31_tables[1, 61:70]) + sum(cm_c31_tables[2, 61:70])), 6))
(k15c31_tnr <- round(sum(cm_c31_tables[4, 61:70]) / (sum(cm_c31_tables[4, 61:70]) + sum(cm_c31_tables[3, 61:70])), 6))
(k15c31_truescore <- round((2 * k15c31_tpr * k15c31_tnr) / (k15c31_tpr + k15c31_tnr), 6))

(k17c31_tpr <- round(sum(cm_c31_tables[1, 71:80]) / (sum(cm_c31_tables[1, 71:80]) + sum(cm_c31_tables[2, 71:80])), 6))
(k17c31_tnr <- round(sum(cm_c31_tables[4, 71:80]) / (sum(cm_c31_tables[4, 71:80]) + sum(cm_c31_tables[3, 71:80])), 6))
(k17c31_truescore <- round((2 * k17c31_tpr * k17c31_tnr) / (k17c31_tpr + k17c31_tnr), 6))

(k19c31_tpr <- round(sum(cm_c31_tables[1, 81:90]) / (sum(cm_c31_tables[1, 81:90]) + sum(cm_c31_tables[2, 81:90])), 6))
(k19c31_tnr <- round(sum(cm_c31_tables[4, 81:90]) / (sum(cm_c31_tables[4, 81:90]) + sum(cm_c31_tables[3, 81:90])), 6))
(k19c31_truescore <- round((2 * k19c31_tpr * k19c31_tnr) / (k19c31_tpr + k19c31_tnr), 6))

(k21c31_tpr <- round(sum(cm_c31_tables[1, 91:100]) / (sum(cm_c31_tables[1, 91:100]) + sum(cm_c31_tables[2, 91:100])), 6))
(k21c31_tnr <- round(sum(cm_c31_tables[4, 91:100]) / (sum(cm_c31_tables[4, 91:100]) + sum(cm_c31_tables[3, 91:100])), 6))
(k21c31_truescore <- round((2 * k21c31_tpr * k21c31_tnr) / (k21c31_tpr + k21c31_tnr), 6))

(k23c31_tpr <- round(sum(cm_c31_tables[1, 101:110]) / (sum(cm_c31_tables[1, 101:110]) + sum(cm_c31_tables[2, 101:110])), 6))
(k23c31_tnr <- round(sum(cm_c31_tables[4, 101:110]) / (sum(cm_c31_tables[4, 101:110]) + sum(cm_c31_tables[3, 101:110])), 6))
(k23c31_truescore <- round((2 * k23c31_tpr * k23c31_tnr) / (k23c31_tpr + k23c31_tnr), 6))

(k25c31_tpr <- round(sum(cm_c31_tables[1, 111:120]) / (sum(cm_c31_tables[1, 111:120]) + sum(cm_c31_tables[2, 111:120])), 6))
(k25c31_tnr <- round(sum(cm_c31_tables[4, 111:120]) / (sum(cm_c31_tables[4, 111:120]) + sum(cm_c31_tables[3, 111:120])), 6))
(k25c31_truescore <- round((2 * k25c31_tpr * k25c31_tnr) / (k25c31_tpr + k25c31_tnr), 6))

(k27c31_tpr <- round(sum(cm_c31_tables[1, 121:130]) / (sum(cm_c31_tables[1, 121:130]) + sum(cm_c31_tables[2, 121:130])), 6))
(k27c31_tnr <- round(sum(cm_c31_tables[4, 121:130]) / (sum(cm_c31_tables[4, 121:130]) + sum(cm_c31_tables[3, 121:130])), 6))
(k27c31_truescore <- round((2 * k27c31_tpr * k27c31_tnr) / (k27c31_tpr + k27c31_tnr), 6))

(k29c31_tpr <- round(sum(cm_c31_tables[1, 131:140]) / (sum(cm_c31_tables[1, 131:140]) + sum(cm_c31_tables[2, 131:140])), 6))
(k29c31_tnr <- round(sum(cm_c31_tables[4, 131:140]) / (sum(cm_c31_tables[4, 131:140]) + sum(cm_c31_tables[3, 131:140])), 6))
(k29c31_truescore <- round((2 * k29c31_tpr * k29c31_tnr) / (k29c31_tpr + k29c31_tnr), 6))

(k31c31_tpr <- round(sum(cm_c31_tables[1, 141:150]) / (sum(cm_c31_tables[1, 141:150]) + sum(cm_c31_tables[2, 141:150])), 6))
(k31c31_tnr <- round(sum(cm_c31_tables[4, 141:150]) / (sum(cm_c31_tables[4, 141:150]) + sum(cm_c31_tables[3, 141:150])), 6))
(k31c31_truescore <- round((2 * k31c31_tpr * k31c31_tnr) / (k31c31_tpr + k31c31_tnr), 6))

(k33c31_tpr <- round(sum(cm_c31_tables[1, 151:160]) / (sum(cm_c31_tables[1, 151:160]) + sum(cm_c31_tables[2, 151:160])), 6))
(k33c31_tnr <- round(sum(cm_c31_tables[4, 151:160]) / (sum(cm_c31_tables[4, 151:160]) + sum(cm_c31_tables[3, 151:160])), 6))
(k33c31_truescore <- round((2 * k33c31_tpr * k33c31_tnr) / (k33c31_tpr + k33c31_tnr), 6))

(k35c31_tpr <- round(sum(cm_c31_tables[1, 161:170]) / (sum(cm_c31_tables[1, 161:170]) + sum(cm_c31_tables[2, 161:170])), 6))
(k35c31_tnr <- round(sum(cm_c31_tables[4, 161:170]) / (sum(cm_c31_tables[4, 161:170]) + sum(cm_c31_tables[3, 161:170])), 6))
(k35c31_truescore <- round((2 * k35c31_tpr * k35c31_tnr) / (k35c31_tpr + k35c31_tnr), 6))

(k37c31_tpr <- round(sum(cm_c31_tables[1, 171:180]) / (sum(cm_c31_tables[1, 171:180]) + sum(cm_c31_tables[2, 171:180])), 6))
(k37c31_tnr <- round(sum(cm_c31_tables[4, 171:180]) / (sum(cm_c31_tables[4, 171:180]) + sum(cm_c31_tables[3, 171:180])), 6))
(k37c31_truescore <- round((2 * k37c31_tpr * k37c31_tnr) / (k37c31_tpr + k37c31_tnr), 6))

(k39c31_tpr <- round(sum(cm_c31_tables[1, 181:190]) / (sum(cm_c31_tables[1, 181:190]) + sum(cm_c31_tables[2, 181:190])), 6))
(k39c31_tnr <- round(sum(cm_c31_tables[4, 181:190]) / (sum(cm_c31_tables[4, 181:190]) + sum(cm_c31_tables[3, 181:190])), 6))
(k39c31_truescore <- round((2 * k39c31_tpr * k39c31_tnr) / (k39c31_tpr + k39c31_tnr), 6))

# Compile the 0.31 cutoff results in a table, and identify the value
# of k that maximizes the truescore.

c31_results <- tibble(k = seq(3, 39, 2), 
                      Cut = 0.31, 
                      TPR = c(k3c31_tpr, k5c31_tpr, k7c31_tpr, k9c31_tpr, k11c31_tpr, 
                              k13c31_tpr, k15c31_tpr, k17c31_tpr, k19c31_tpr, k21c31_tpr, 
                              k23c31_tpr, k25c31_tpr, k27c31_tpr, k29c31_tpr, k31c31_tpr, 
                              k33c31_tpr, k35c31_tpr, k37c31_tpr, k39c31_tpr), 
                      TNR = c(k3c31_tnr, k5c31_tnr, k7c31_tnr, k9c31_tnr, k11c31_tnr, 
                              k13c31_tnr, k15c31_tnr, k17c31_tnr, k19c31_tnr, k21c31_tnr, 
                              k23c31_tnr, k25c31_tnr, k27c31_tnr, k29c31_tnr, k31c31_tnr, 
                              k33c31_tnr, k35c31_tnr, k37c31_tnr, k39c31_tnr), 
                      Truescore = c(k3c31_truescore, k5c31_truescore, k7c31_truescore, 
                                    k9c31_truescore, k11c31_truescore, k13c31_truescore, 
                                    k15c31_truescore, k17c31_truescore, k19c31_truescore, 
                                    k21c31_truescore, k23c31_truescore, k25c31_truescore, 
                                    k27c31_truescore, k29c31_truescore, k31c31_truescore, 
                                    k33c31_truescore, k35c31_truescore, k37c31_truescore, 
                                    k39c31_truescore))

knitr::kable(c31_results[1:19, ], caption = "c31_results")

ggplot(c31_results, aes(k, Truescore)) + geom_point() + geom_line() + 
  labs(title = "Optimal k for Decision Cutoff 0.31")

############################
# 0.32 Cutoff
############################

# For the decision cutoff of 0.32, generate a confusion matrix for
# every combination of k (3:39 by two) and fold (1 to 10).

cm_c32 <- sapply(fk_dfs_l, function(x) {
  ss <- subset(x, select = c(pred32, obs))
  confusionMatrix(ss$pred32, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c32) <- fk_dfs_v

cm_c32_tables <- sapply(cm_c32, "[[", 2)
cm_c32_tables <- as_tibble(cm_c32_tables)

# For each value of k, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each value of k when the decision cutoff is 0.32.

(k3c32_tpr <- round(sum(cm_c32_tables[1, 1:10]) / (sum(cm_c32_tables[1, 1:10]) + sum(cm_c32_tables[2, 1:10])), 6))
(k3c32_tnr <- round(sum(cm_c32_tables[4, 1:10]) / (sum(cm_c32_tables[4, 1:10]) + sum(cm_c32_tables[3, 1:10])), 6))
(k3c32_truescore <- round((2 * k3c32_tpr * k3c32_tnr) / (k3c32_tpr + k3c32_tnr), 6))

(k5c32_tpr <- round(sum(cm_c32_tables[1, 11:20]) / (sum(cm_c32_tables[1, 11:20]) + sum(cm_c32_tables[2, 11:20])), 6))
(k5c32_tnr <- round(sum(cm_c32_tables[4, 11:20]) / (sum(cm_c32_tables[4, 11:20]) + sum(cm_c32_tables[3, 11:20])), 6))
(k5c32_truescore <- round((2 * k5c32_tpr * k5c32_tnr) / (k5c32_tpr + k5c32_tnr), 6))

(k7c32_tpr <- round(sum(cm_c32_tables[1, 21:30]) / (sum(cm_c32_tables[1, 21:30]) + sum(cm_c32_tables[2, 21:30])), 6))
(k7c32_tnr <- round(sum(cm_c32_tables[4, 21:30]) / (sum(cm_c32_tables[4, 21:30]) + sum(cm_c32_tables[3, 21:30])), 6))
(k7c32_truescore <- round((2 * k7c32_tpr * k7c32_tnr) / (k7c32_tpr + k7c32_tnr), 6))

(k9c32_tpr <- round(sum(cm_c32_tables[1, 31:40]) / (sum(cm_c32_tables[1, 31:40]) + sum(cm_c32_tables[2, 31:40])), 6))
(k9c32_tnr <- round(sum(cm_c32_tables[4, 31:40]) / (sum(cm_c32_tables[4, 31:40]) + sum(cm_c32_tables[3, 31:40])), 6))
(k9c32_truescore <- round((2 * k9c32_tpr * k9c32_tnr) / (k9c32_tpr + k9c32_tnr), 6))

(k11c32_tpr <- round(sum(cm_c32_tables[1, 41:50]) / (sum(cm_c32_tables[1, 41:50]) + sum(cm_c32_tables[2, 41:50])), 6))
(k11c32_tnr <- round(sum(cm_c32_tables[4, 41:50]) / (sum(cm_c32_tables[4, 41:50]) + sum(cm_c32_tables[3, 41:50])), 6))
(k11c32_truescore <- round((2 * k11c32_tpr * k11c32_tnr) / (k11c32_tpr + k11c32_tnr), 6))

(k13c32_tpr <- round(sum(cm_c32_tables[1, 51:60]) / (sum(cm_c32_tables[1, 51:60]) + sum(cm_c32_tables[2, 51:60])), 6))
(k13c32_tnr <- round(sum(cm_c32_tables[4, 51:60]) / (sum(cm_c32_tables[4, 51:60]) + sum(cm_c32_tables[3, 51:60])), 6))
(k13c32_truescore <- round((2 * k13c32_tpr * k13c32_tnr) / (k13c32_tpr + k13c32_tnr), 6))

(k15c32_tpr <- round(sum(cm_c32_tables[1, 61:70]) / (sum(cm_c32_tables[1, 61:70]) + sum(cm_c32_tables[2, 61:70])), 6))
(k15c32_tnr <- round(sum(cm_c32_tables[4, 61:70]) / (sum(cm_c32_tables[4, 61:70]) + sum(cm_c32_tables[3, 61:70])), 6))
(k15c32_truescore <- round((2 * k15c32_tpr * k15c32_tnr) / (k15c32_tpr + k15c32_tnr), 6))

(k17c32_tpr <- round(sum(cm_c32_tables[1, 71:80]) / (sum(cm_c32_tables[1, 71:80]) + sum(cm_c32_tables[2, 71:80])), 6))
(k17c32_tnr <- round(sum(cm_c32_tables[4, 71:80]) / (sum(cm_c32_tables[4, 71:80]) + sum(cm_c32_tables[3, 71:80])), 6))
(k17c32_truescore <- round((2 * k17c32_tpr * k17c32_tnr) / (k17c32_tpr + k17c32_tnr), 6))

(k19c32_tpr <- round(sum(cm_c32_tables[1, 81:90]) / (sum(cm_c32_tables[1, 81:90]) + sum(cm_c32_tables[2, 81:90])), 6))
(k19c32_tnr <- round(sum(cm_c32_tables[4, 81:90]) / (sum(cm_c32_tables[4, 81:90]) + sum(cm_c32_tables[3, 81:90])), 6))
(k19c32_truescore <- round((2 * k19c32_tpr * k19c32_tnr) / (k19c32_tpr + k19c32_tnr), 6))

(k21c32_tpr <- round(sum(cm_c32_tables[1, 91:100]) / (sum(cm_c32_tables[1, 91:100]) + sum(cm_c32_tables[2, 91:100])), 6))
(k21c32_tnr <- round(sum(cm_c32_tables[4, 91:100]) / (sum(cm_c32_tables[4, 91:100]) + sum(cm_c32_tables[3, 91:100])), 6))
(k21c32_truescore <- round((2 * k21c32_tpr * k21c32_tnr) / (k21c32_tpr + k21c32_tnr), 6))

(k23c32_tpr <- round(sum(cm_c32_tables[1, 101:110]) / (sum(cm_c32_tables[1, 101:110]) + sum(cm_c32_tables[2, 101:110])), 6))
(k23c32_tnr <- round(sum(cm_c32_tables[4, 101:110]) / (sum(cm_c32_tables[4, 101:110]) + sum(cm_c32_tables[3, 101:110])), 6))
(k23c32_truescore <- round((2 * k23c32_tpr * k23c32_tnr) / (k23c32_tpr + k23c32_tnr), 6))

(k25c32_tpr <- round(sum(cm_c32_tables[1, 111:120]) / (sum(cm_c32_tables[1, 111:120]) + sum(cm_c32_tables[2, 111:120])), 6))
(k25c32_tnr <- round(sum(cm_c32_tables[4, 111:120]) / (sum(cm_c32_tables[4, 111:120]) + sum(cm_c32_tables[3, 111:120])), 6))
(k25c32_truescore <- round((2 * k25c32_tpr * k25c32_tnr) / (k25c32_tpr + k25c32_tnr), 6))

(k27c32_tpr <- round(sum(cm_c32_tables[1, 121:130]) / (sum(cm_c32_tables[1, 121:130]) + sum(cm_c32_tables[2, 121:130])), 6))
(k27c32_tnr <- round(sum(cm_c32_tables[4, 121:130]) / (sum(cm_c32_tables[4, 121:130]) + sum(cm_c32_tables[3, 121:130])), 6))
(k27c32_truescore <- round((2 * k27c32_tpr * k27c32_tnr) / (k27c32_tpr + k27c32_tnr), 6))

(k29c32_tpr <- round(sum(cm_c32_tables[1, 131:140]) / (sum(cm_c32_tables[1, 131:140]) + sum(cm_c32_tables[2, 131:140])), 6))
(k29c32_tnr <- round(sum(cm_c32_tables[4, 131:140]) / (sum(cm_c32_tables[4, 131:140]) + sum(cm_c32_tables[3, 131:140])), 6))
(k29c32_truescore <- round((2 * k29c32_tpr * k29c32_tnr) / (k29c32_tpr + k29c32_tnr), 6))

(k31c32_tpr <- round(sum(cm_c32_tables[1, 141:150]) / (sum(cm_c32_tables[1, 141:150]) + sum(cm_c32_tables[2, 141:150])), 6))
(k31c32_tnr <- round(sum(cm_c32_tables[4, 141:150]) / (sum(cm_c32_tables[4, 141:150]) + sum(cm_c32_tables[3, 141:150])), 6))
(k31c32_truescore <- round((2 * k31c32_tpr * k31c32_tnr) / (k31c32_tpr + k31c32_tnr), 6))

(k33c32_tpr <- round(sum(cm_c32_tables[1, 151:160]) / (sum(cm_c32_tables[1, 151:160]) + sum(cm_c32_tables[2, 151:160])), 6))
(k33c32_tnr <- round(sum(cm_c32_tables[4, 151:160]) / (sum(cm_c32_tables[4, 151:160]) + sum(cm_c32_tables[3, 151:160])), 6))
(k33c32_truescore <- round((2 * k33c32_tpr * k33c32_tnr) / (k33c32_tpr + k33c32_tnr), 6))

(k35c32_tpr <- round(sum(cm_c32_tables[1, 161:170]) / (sum(cm_c32_tables[1, 161:170]) + sum(cm_c32_tables[2, 161:170])), 6))
(k35c32_tnr <- round(sum(cm_c32_tables[4, 161:170]) / (sum(cm_c32_tables[4, 161:170]) + sum(cm_c32_tables[3, 161:170])), 6))
(k35c32_truescore <- round((2 * k35c32_tpr * k35c32_tnr) / (k35c32_tpr + k35c32_tnr), 6))

(k37c32_tpr <- round(sum(cm_c32_tables[1, 171:180]) / (sum(cm_c32_tables[1, 171:180]) + sum(cm_c32_tables[2, 171:180])), 6))
(k37c32_tnr <- round(sum(cm_c32_tables[4, 171:180]) / (sum(cm_c32_tables[4, 171:180]) + sum(cm_c32_tables[3, 171:180])), 6))
(k37c32_truescore <- round((2 * k37c32_tpr * k37c32_tnr) / (k37c32_tpr + k37c32_tnr), 6))

(k39c32_tpr <- round(sum(cm_c32_tables[1, 181:190]) / (sum(cm_c32_tables[1, 181:190]) + sum(cm_c32_tables[2, 181:190])), 6))
(k39c32_tnr <- round(sum(cm_c32_tables[4, 181:190]) / (sum(cm_c32_tables[4, 181:190]) + sum(cm_c32_tables[3, 181:190])), 6))
(k39c32_truescore <- round((2 * k39c32_tpr * k39c32_tnr) / (k39c32_tpr + k39c32_tnr), 6))

# Compile the 0.32 cutoff results in a table, and identify the value
# of k that maximizes the truescore.

c32_results <- tibble(k = seq(3, 39, 2), 
                      Cut = 0.32, 
                      TPR = c(k3c32_tpr, k5c32_tpr, k7c32_tpr, k9c32_tpr, k11c32_tpr, 
                              k13c32_tpr, k15c32_tpr, k17c32_tpr, k19c32_tpr, k21c32_tpr, 
                              k23c32_tpr, k25c32_tpr, k27c32_tpr, k29c32_tpr, k31c32_tpr, 
                              k33c32_tpr, k35c32_tpr, k37c32_tpr, k39c32_tpr), 
                      TNR = c(k3c32_tnr, k5c32_tnr, k7c32_tnr, k9c32_tnr, k11c32_tnr, 
                              k13c32_tnr, k15c32_tnr, k17c32_tnr, k19c32_tnr, k21c32_tnr, 
                              k23c32_tnr, k25c32_tnr, k27c32_tnr, k29c32_tnr, k31c32_tnr, 
                              k33c32_tnr, k35c32_tnr, k37c32_tnr, k39c32_tnr), 
                      Truescore = c(k3c32_truescore, k5c32_truescore, k7c32_truescore, 
                                    k9c32_truescore, k11c32_truescore, k13c32_truescore, 
                                    k15c32_truescore, k17c32_truescore, k19c32_truescore, 
                                    k21c32_truescore, k23c32_truescore, k25c32_truescore, 
                                    k27c32_truescore, k29c32_truescore, k31c32_truescore, 
                                    k33c32_truescore, k35c32_truescore, k37c32_truescore, 
                                    k39c32_truescore))

knitr::kable(c32_results[1:19, ], caption = "c32_results")

ggplot(c32_results, aes(k, Truescore)) + geom_point() + geom_line() + 
  labs(title = "Optimal k for Decision Cutoff 0.32")

############################
# 0.33 Cutoff
############################

# For the decision cutoff of 0.33, generate a confusion matrix for
# every combination of k (3:39 by two) and fold (1 to 10).

cm_c33 <- sapply(fk_dfs_l, function(x) {
  ss <- subset(x, select = c(pred33, obs))
  confusionMatrix(ss$pred33, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c33) <- fk_dfs_v

cm_c33_tables <- sapply(cm_c33, "[[", 2)
cm_c33_tables <- as_tibble(cm_c33_tables)

# For each value of k, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each value of k when the decision cutoff is 0.33.

(k3c33_tpr <- round(sum(cm_c33_tables[1, 1:10]) / (sum(cm_c33_tables[1, 1:10]) + sum(cm_c33_tables[2, 1:10])), 6))
(k3c33_tnr <- round(sum(cm_c33_tables[4, 1:10]) / (sum(cm_c33_tables[4, 1:10]) + sum(cm_c33_tables[3, 1:10])), 6))
(k3c33_truescore <- round((2 * k3c33_tpr * k3c33_tnr) / (k3c33_tpr + k3c33_tnr), 6))

(k5c33_tpr <- round(sum(cm_c33_tables[1, 11:20]) / (sum(cm_c33_tables[1, 11:20]) + sum(cm_c33_tables[2, 11:20])), 6))
(k5c33_tnr <- round(sum(cm_c33_tables[4, 11:20]) / (sum(cm_c33_tables[4, 11:20]) + sum(cm_c33_tables[3, 11:20])), 6))
(k5c33_truescore <- round((2 * k5c33_tpr * k5c33_tnr) / (k5c33_tpr + k5c33_tnr), 6))

(k7c33_tpr <- round(sum(cm_c33_tables[1, 21:30]) / (sum(cm_c33_tables[1, 21:30]) + sum(cm_c33_tables[2, 21:30])), 6))
(k7c33_tnr <- round(sum(cm_c33_tables[4, 21:30]) / (sum(cm_c33_tables[4, 21:30]) + sum(cm_c33_tables[3, 21:30])), 6))
(k7c33_truescore <- round((2 * k7c33_tpr * k7c33_tnr) / (k7c33_tpr + k7c33_tnr), 6))

(k9c33_tpr <- round(sum(cm_c33_tables[1, 31:40]) / (sum(cm_c33_tables[1, 31:40]) + sum(cm_c33_tables[2, 31:40])), 6))
(k9c33_tnr <- round(sum(cm_c33_tables[4, 31:40]) / (sum(cm_c33_tables[4, 31:40]) + sum(cm_c33_tables[3, 31:40])), 6))
(k9c33_truescore <- round((2 * k9c33_tpr * k9c33_tnr) / (k9c33_tpr + k9c33_tnr), 6))

(k11c33_tpr <- round(sum(cm_c33_tables[1, 41:50]) / (sum(cm_c33_tables[1, 41:50]) + sum(cm_c33_tables[2, 41:50])), 6))
(k11c33_tnr <- round(sum(cm_c33_tables[4, 41:50]) / (sum(cm_c33_tables[4, 41:50]) + sum(cm_c33_tables[3, 41:50])), 6))
(k11c33_truescore <- round((2 * k11c33_tpr * k11c33_tnr) / (k11c33_tpr + k11c33_tnr), 6))

(k13c33_tpr <- round(sum(cm_c33_tables[1, 51:60]) / (sum(cm_c33_tables[1, 51:60]) + sum(cm_c33_tables[2, 51:60])), 6))
(k13c33_tnr <- round(sum(cm_c33_tables[4, 51:60]) / (sum(cm_c33_tables[4, 51:60]) + sum(cm_c33_tables[3, 51:60])), 6))
(k13c33_truescore <- round((2 * k13c33_tpr * k13c33_tnr) / (k13c33_tpr + k13c33_tnr), 6))

(k15c33_tpr <- round(sum(cm_c33_tables[1, 61:70]) / (sum(cm_c33_tables[1, 61:70]) + sum(cm_c33_tables[2, 61:70])), 6))
(k15c33_tnr <- round(sum(cm_c33_tables[4, 61:70]) / (sum(cm_c33_tables[4, 61:70]) + sum(cm_c33_tables[3, 61:70])), 6))
(k15c33_truescore <- round((2 * k15c33_tpr * k15c33_tnr) / (k15c33_tpr + k15c33_tnr), 6))

(k17c33_tpr <- round(sum(cm_c33_tables[1, 71:80]) / (sum(cm_c33_tables[1, 71:80]) + sum(cm_c33_tables[2, 71:80])), 6))
(k17c33_tnr <- round(sum(cm_c33_tables[4, 71:80]) / (sum(cm_c33_tables[4, 71:80]) + sum(cm_c33_tables[3, 71:80])), 6))
(k17c33_truescore <- round((2 * k17c33_tpr * k17c33_tnr) / (k17c33_tpr + k17c33_tnr), 6))

(k19c33_tpr <- round(sum(cm_c33_tables[1, 81:90]) / (sum(cm_c33_tables[1, 81:90]) + sum(cm_c33_tables[2, 81:90])), 6))
(k19c33_tnr <- round(sum(cm_c33_tables[4, 81:90]) / (sum(cm_c33_tables[4, 81:90]) + sum(cm_c33_tables[3, 81:90])), 6))
(k19c33_truescore <- round((2 * k19c33_tpr * k19c33_tnr) / (k19c33_tpr + k19c33_tnr), 6))

(k21c33_tpr <- round(sum(cm_c33_tables[1, 91:100]) / (sum(cm_c33_tables[1, 91:100]) + sum(cm_c33_tables[2, 91:100])), 6))
(k21c33_tnr <- round(sum(cm_c33_tables[4, 91:100]) / (sum(cm_c33_tables[4, 91:100]) + sum(cm_c33_tables[3, 91:100])), 6))
(k21c33_truescore <- round((2 * k21c33_tpr * k21c33_tnr) / (k21c33_tpr + k21c33_tnr), 6))

(k23c33_tpr <- round(sum(cm_c33_tables[1, 101:110]) / (sum(cm_c33_tables[1, 101:110]) + sum(cm_c33_tables[2, 101:110])), 6))
(k23c33_tnr <- round(sum(cm_c33_tables[4, 101:110]) / (sum(cm_c33_tables[4, 101:110]) + sum(cm_c33_tables[3, 101:110])), 6))
(k23c33_truescore <- round((2 * k23c33_tpr * k23c33_tnr) / (k23c33_tpr + k23c33_tnr), 6))

(k25c33_tpr <- round(sum(cm_c33_tables[1, 111:120]) / (sum(cm_c33_tables[1, 111:120]) + sum(cm_c33_tables[2, 111:120])), 6))
(k25c33_tnr <- round(sum(cm_c33_tables[4, 111:120]) / (sum(cm_c33_tables[4, 111:120]) + sum(cm_c33_tables[3, 111:120])), 6))
(k25c33_truescore <- round((2 * k25c33_tpr * k25c33_tnr) / (k25c33_tpr + k25c33_tnr), 6))

(k27c33_tpr <- round(sum(cm_c33_tables[1, 121:130]) / (sum(cm_c33_tables[1, 121:130]) + sum(cm_c33_tables[2, 121:130])), 6))
(k27c33_tnr <- round(sum(cm_c33_tables[4, 121:130]) / (sum(cm_c33_tables[4, 121:130]) + sum(cm_c33_tables[3, 121:130])), 6))
(k27c33_truescore <- round((2 * k27c33_tpr * k27c33_tnr) / (k27c33_tpr + k27c33_tnr), 6))

(k29c33_tpr <- round(sum(cm_c33_tables[1, 131:140]) / (sum(cm_c33_tables[1, 131:140]) + sum(cm_c33_tables[2, 131:140])), 6))
(k29c33_tnr <- round(sum(cm_c33_tables[4, 131:140]) / (sum(cm_c33_tables[4, 131:140]) + sum(cm_c33_tables[3, 131:140])), 6))
(k29c33_truescore <- round((2 * k29c33_tpr * k29c33_tnr) / (k29c33_tpr + k29c33_tnr), 6))

(k31c33_tpr <- round(sum(cm_c33_tables[1, 141:150]) / (sum(cm_c33_tables[1, 141:150]) + sum(cm_c33_tables[2, 141:150])), 6))
(k31c33_tnr <- round(sum(cm_c33_tables[4, 141:150]) / (sum(cm_c33_tables[4, 141:150]) + sum(cm_c33_tables[3, 141:150])), 6))
(k31c33_truescore <- round((2 * k31c33_tpr * k31c33_tnr) / (k31c33_tpr + k31c33_tnr), 6))

(k33c33_tpr <- round(sum(cm_c33_tables[1, 151:160]) / (sum(cm_c33_tables[1, 151:160]) + sum(cm_c33_tables[2, 151:160])), 6))
(k33c33_tnr <- round(sum(cm_c33_tables[4, 151:160]) / (sum(cm_c33_tables[4, 151:160]) + sum(cm_c33_tables[3, 151:160])), 6))
(k33c33_truescore <- round((2 * k33c33_tpr * k33c33_tnr) / (k33c33_tpr + k33c33_tnr), 6))

(k35c33_tpr <- round(sum(cm_c33_tables[1, 161:170]) / (sum(cm_c33_tables[1, 161:170]) + sum(cm_c33_tables[2, 161:170])), 6))
(k35c33_tnr <- round(sum(cm_c33_tables[4, 161:170]) / (sum(cm_c33_tables[4, 161:170]) + sum(cm_c33_tables[3, 161:170])), 6))
(k35c33_truescore <- round((2 * k35c33_tpr * k35c33_tnr) / (k35c33_tpr + k35c33_tnr), 6))

(k37c33_tpr <- round(sum(cm_c33_tables[1, 171:180]) / (sum(cm_c33_tables[1, 171:180]) + sum(cm_c33_tables[2, 171:180])), 6))
(k37c33_tnr <- round(sum(cm_c33_tables[4, 171:180]) / (sum(cm_c33_tables[4, 171:180]) + sum(cm_c33_tables[3, 171:180])), 6))
(k37c33_truescore <- round((2 * k37c33_tpr * k37c33_tnr) / (k37c33_tpr + k37c33_tnr), 6))

(k39c33_tpr <- round(sum(cm_c33_tables[1, 181:190]) / (sum(cm_c33_tables[1, 181:190]) + sum(cm_c33_tables[2, 181:190])), 6))
(k39c33_tnr <- round(sum(cm_c33_tables[4, 181:190]) / (sum(cm_c33_tables[4, 181:190]) + sum(cm_c33_tables[3, 181:190])), 6))
(k39c33_truescore <- round((2 * k39c33_tpr * k39c33_tnr) / (k39c33_tpr + k39c33_tnr), 6))

# Compile the 0.33 cutoff results in a table, and identify the value
# of k that maximizes the truescore.

c33_results <- tibble(k = seq(3, 39, 2), 
                      Cut = 0.33, 
                      TPR = c(k3c33_tpr, k5c33_tpr, k7c33_tpr, k9c33_tpr, k11c33_tpr, 
                              k13c33_tpr, k15c33_tpr, k17c33_tpr, k19c33_tpr, k21c33_tpr, 
                              k23c33_tpr, k25c33_tpr, k27c33_tpr, k29c33_tpr, k31c33_tpr, 
                              k33c33_tpr, k35c33_tpr, k37c33_tpr, k39c33_tpr), 
                      TNR = c(k3c33_tnr, k5c33_tnr, k7c33_tnr, k9c33_tnr, k11c33_tnr, 
                              k13c33_tnr, k15c33_tnr, k17c33_tnr, k19c33_tnr, k21c33_tnr, 
                              k23c33_tnr, k25c33_tnr, k27c33_tnr, k29c33_tnr, k31c33_tnr, 
                              k33c33_tnr, k35c33_tnr, k37c33_tnr, k39c33_tnr), 
                      Truescore = c(k3c33_truescore, k5c33_truescore, k7c33_truescore, 
                                    k9c33_truescore, k11c33_truescore, k13c33_truescore, 
                                    k15c33_truescore, k17c33_truescore, k19c33_truescore, 
                                    k21c33_truescore, k23c33_truescore, k25c33_truescore, 
                                    k27c33_truescore, k29c33_truescore, k31c33_truescore, 
                                    k33c33_truescore, k35c33_truescore, k37c33_truescore, 
                                    k39c33_truescore))

knitr::kable(c33_results[1:19, ], caption = "c33_results")

ggplot(c33_results, aes(k, Truescore)) + geom_point() + geom_line() + 
  labs(title = "Optimal k for Decision Cutoff 0.33")

############################
# 0.34 Cutoff
############################

# For the decision cutoff of 0.34, generate a confusion matrix for
# every combination of k (3:39 by two) and fold (1 to 10).

cm_c34 <- sapply(fk_dfs_l, function(x) {
  ss <- subset(x, select = c(pred34, obs))
  confusionMatrix(ss$pred34, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c34) <- fk_dfs_v

cm_c34_tables <- sapply(cm_c34, "[[", 2)
cm_c34_tables <- as_tibble(cm_c34_tables)

# For each value of k, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each value of k when the decision cutoff is 0.34.

(k3c34_tpr <- round(sum(cm_c34_tables[1, 1:10]) / (sum(cm_c34_tables[1, 1:10]) + sum(cm_c34_tables[2, 1:10])), 6))
(k3c34_tnr <- round(sum(cm_c34_tables[4, 1:10]) / (sum(cm_c34_tables[4, 1:10]) + sum(cm_c34_tables[3, 1:10])), 6))
(k3c34_truescore <- round((2 * k3c34_tpr * k3c34_tnr) / (k3c34_tpr + k3c34_tnr), 6))

(k5c34_tpr <- round(sum(cm_c34_tables[1, 11:20]) / (sum(cm_c34_tables[1, 11:20]) + sum(cm_c34_tables[2, 11:20])), 6))
(k5c34_tnr <- round(sum(cm_c34_tables[4, 11:20]) / (sum(cm_c34_tables[4, 11:20]) + sum(cm_c34_tables[3, 11:20])), 6))
(k5c34_truescore <- round((2 * k5c34_tpr * k5c34_tnr) / (k5c34_tpr + k5c34_tnr), 6))

(k7c34_tpr <- round(sum(cm_c34_tables[1, 21:30]) / (sum(cm_c34_tables[1, 21:30]) + sum(cm_c34_tables[2, 21:30])), 6))
(k7c34_tnr <- round(sum(cm_c34_tables[4, 21:30]) / (sum(cm_c34_tables[4, 21:30]) + sum(cm_c34_tables[3, 21:30])), 6))
(k7c34_truescore <- round((2 * k7c34_tpr * k7c34_tnr) / (k7c34_tpr + k7c34_tnr), 6))

(k9c34_tpr <- round(sum(cm_c34_tables[1, 31:40]) / (sum(cm_c34_tables[1, 31:40]) + sum(cm_c34_tables[2, 31:40])), 6))
(k9c34_tnr <- round(sum(cm_c34_tables[4, 31:40]) / (sum(cm_c34_tables[4, 31:40]) + sum(cm_c34_tables[3, 31:40])), 6))
(k9c34_truescore <- round((2 * k9c34_tpr * k9c34_tnr) / (k9c34_tpr + k9c34_tnr), 6))

(k11c34_tpr <- round(sum(cm_c34_tables[1, 41:50]) / (sum(cm_c34_tables[1, 41:50]) + sum(cm_c34_tables[2, 41:50])), 6))
(k11c34_tnr <- round(sum(cm_c34_tables[4, 41:50]) / (sum(cm_c34_tables[4, 41:50]) + sum(cm_c34_tables[3, 41:50])), 6))
(k11c34_truescore <- round((2 * k11c34_tpr * k11c34_tnr) / (k11c34_tpr + k11c34_tnr), 6))

(k13c34_tpr <- round(sum(cm_c34_tables[1, 51:60]) / (sum(cm_c34_tables[1, 51:60]) + sum(cm_c34_tables[2, 51:60])), 6))
(k13c34_tnr <- round(sum(cm_c34_tables[4, 51:60]) / (sum(cm_c34_tables[4, 51:60]) + sum(cm_c34_tables[3, 51:60])), 6))
(k13c34_truescore <- round((2 * k13c34_tpr * k13c34_tnr) / (k13c34_tpr + k13c34_tnr), 6))

(k15c34_tpr <- round(sum(cm_c34_tables[1, 61:70]) / (sum(cm_c34_tables[1, 61:70]) + sum(cm_c34_tables[2, 61:70])), 6))
(k15c34_tnr <- round(sum(cm_c34_tables[4, 61:70]) / (sum(cm_c34_tables[4, 61:70]) + sum(cm_c34_tables[3, 61:70])), 6))
(k15c34_truescore <- round((2 * k15c34_tpr * k15c34_tnr) / (k15c34_tpr + k15c34_tnr), 6))

(k17c34_tpr <- round(sum(cm_c34_tables[1, 71:80]) / (sum(cm_c34_tables[1, 71:80]) + sum(cm_c34_tables[2, 71:80])), 6))
(k17c34_tnr <- round(sum(cm_c34_tables[4, 71:80]) / (sum(cm_c34_tables[4, 71:80]) + sum(cm_c34_tables[3, 71:80])), 6))
(k17c34_truescore <- round((2 * k17c34_tpr * k17c34_tnr) / (k17c34_tpr + k17c34_tnr), 6))

(k19c34_tpr <- round(sum(cm_c34_tables[1, 81:90]) / (sum(cm_c34_tables[1, 81:90]) + sum(cm_c34_tables[2, 81:90])), 6))
(k19c34_tnr <- round(sum(cm_c34_tables[4, 81:90]) / (sum(cm_c34_tables[4, 81:90]) + sum(cm_c34_tables[3, 81:90])), 6))
(k19c34_truescore <- round((2 * k19c34_tpr * k19c34_tnr) / (k19c34_tpr + k19c34_tnr), 6))

(k21c34_tpr <- round(sum(cm_c34_tables[1, 91:100]) / (sum(cm_c34_tables[1, 91:100]) + sum(cm_c34_tables[2, 91:100])), 6))
(k21c34_tnr <- round(sum(cm_c34_tables[4, 91:100]) / (sum(cm_c34_tables[4, 91:100]) + sum(cm_c34_tables[3, 91:100])), 6))
(k21c34_truescore <- round((2 * k21c34_tpr * k21c34_tnr) / (k21c34_tpr + k21c34_tnr), 6))

(k23c34_tpr <- round(sum(cm_c34_tables[1, 101:110]) / (sum(cm_c34_tables[1, 101:110]) + sum(cm_c34_tables[2, 101:110])), 6))
(k23c34_tnr <- round(sum(cm_c34_tables[4, 101:110]) / (sum(cm_c34_tables[4, 101:110]) + sum(cm_c34_tables[3, 101:110])), 6))
(k23c34_truescore <- round((2 * k23c34_tpr * k23c34_tnr) / (k23c34_tpr + k23c34_tnr), 6))

(k25c34_tpr <- round(sum(cm_c34_tables[1, 111:120]) / (sum(cm_c34_tables[1, 111:120]) + sum(cm_c34_tables[2, 111:120])), 6))
(k25c34_tnr <- round(sum(cm_c34_tables[4, 111:120]) / (sum(cm_c34_tables[4, 111:120]) + sum(cm_c34_tables[3, 111:120])), 6))
(k25c34_truescore <- round((2 * k25c34_tpr * k25c34_tnr) / (k25c34_tpr + k25c34_tnr), 6))

(k27c34_tpr <- round(sum(cm_c34_tables[1, 121:130]) / (sum(cm_c34_tables[1, 121:130]) + sum(cm_c34_tables[2, 121:130])), 6))
(k27c34_tnr <- round(sum(cm_c34_tables[4, 121:130]) / (sum(cm_c34_tables[4, 121:130]) + sum(cm_c34_tables[3, 121:130])), 6))
(k27c34_truescore <- round((2 * k27c34_tpr * k27c34_tnr) / (k27c34_tpr + k27c34_tnr), 6))

(k29c34_tpr <- round(sum(cm_c34_tables[1, 131:140]) / (sum(cm_c34_tables[1, 131:140]) + sum(cm_c34_tables[2, 131:140])), 6))
(k29c34_tnr <- round(sum(cm_c34_tables[4, 131:140]) / (sum(cm_c34_tables[4, 131:140]) + sum(cm_c34_tables[3, 131:140])), 6))
(k29c34_truescore <- round((2 * k29c34_tpr * k29c34_tnr) / (k29c34_tpr + k29c34_tnr), 6))

(k31c34_tpr <- round(sum(cm_c34_tables[1, 141:150]) / (sum(cm_c34_tables[1, 141:150]) + sum(cm_c34_tables[2, 141:150])), 6))
(k31c34_tnr <- round(sum(cm_c34_tables[4, 141:150]) / (sum(cm_c34_tables[4, 141:150]) + sum(cm_c34_tables[3, 141:150])), 6))
(k31c34_truescore <- round((2 * k31c34_tpr * k31c34_tnr) / (k31c34_tpr + k31c34_tnr), 6))

(k33c34_tpr <- round(sum(cm_c34_tables[1, 151:160]) / (sum(cm_c34_tables[1, 151:160]) + sum(cm_c34_tables[2, 151:160])), 6))
(k33c34_tnr <- round(sum(cm_c34_tables[4, 151:160]) / (sum(cm_c34_tables[4, 151:160]) + sum(cm_c34_tables[3, 151:160])), 6))
(k33c34_truescore <- round((2 * k33c34_tpr * k33c34_tnr) / (k33c34_tpr + k33c34_tnr), 6))

(k35c34_tpr <- round(sum(cm_c34_tables[1, 161:170]) / (sum(cm_c34_tables[1, 161:170]) + sum(cm_c34_tables[2, 161:170])), 6))
(k35c34_tnr <- round(sum(cm_c34_tables[4, 161:170]) / (sum(cm_c34_tables[4, 161:170]) + sum(cm_c34_tables[3, 161:170])), 6))
(k35c34_truescore <- round((2 * k35c34_tpr * k35c34_tnr) / (k35c34_tpr + k35c34_tnr), 6))

(k37c34_tpr <- round(sum(cm_c34_tables[1, 171:180]) / (sum(cm_c34_tables[1, 171:180]) + sum(cm_c34_tables[2, 171:180])), 6))
(k37c34_tnr <- round(sum(cm_c34_tables[4, 171:180]) / (sum(cm_c34_tables[4, 171:180]) + sum(cm_c34_tables[3, 171:180])), 6))
(k37c34_truescore <- round((2 * k37c34_tpr * k37c34_tnr) / (k37c34_tpr + k37c34_tnr), 6))

(k39c34_tpr <- round(sum(cm_c34_tables[1, 181:190]) / (sum(cm_c34_tables[1, 181:190]) + sum(cm_c34_tables[2, 181:190])), 6))
(k39c34_tnr <- round(sum(cm_c34_tables[4, 181:190]) / (sum(cm_c34_tables[4, 181:190]) + sum(cm_c34_tables[3, 181:190])), 6))
(k39c34_truescore <- round((2 * k39c34_tpr * k39c34_tnr) / (k39c34_tpr + k39c34_tnr), 6))

# Compile the 0.34 cutoff results in a table, and identify the value
# of k that maximizes the truescore.

c34_results <- tibble(k = seq(3, 39, 2), 
                      Cut = 0.34, 
                      TPR = c(k3c34_tpr, k5c34_tpr, k7c34_tpr, k9c34_tpr, k11c34_tpr, 
                              k13c34_tpr, k15c34_tpr, k17c34_tpr, k19c34_tpr, k21c34_tpr, 
                              k23c34_tpr, k25c34_tpr, k27c34_tpr, k29c34_tpr, k31c34_tpr, 
                              k33c34_tpr, k35c34_tpr, k37c34_tpr, k39c34_tpr), 
                      TNR = c(k3c34_tnr, k5c34_tnr, k7c34_tnr, k9c34_tnr, k11c34_tnr, 
                              k13c34_tnr, k15c34_tnr, k17c34_tnr, k19c34_tnr, k21c34_tnr, 
                              k23c34_tnr, k25c34_tnr, k27c34_tnr, k29c34_tnr, k31c34_tnr, 
                              k33c34_tnr, k35c34_tnr, k37c34_tnr, k39c34_tnr), 
                      Truescore = c(k3c34_truescore, k5c34_truescore, k7c34_truescore, 
                                    k9c34_truescore, k11c34_truescore, k13c34_truescore, 
                                    k15c34_truescore, k17c34_truescore, k19c34_truescore, 
                                    k21c34_truescore, k23c34_truescore, k25c34_truescore, 
                                    k27c34_truescore, k29c34_truescore, k31c34_truescore, 
                                    k33c34_truescore, k35c34_truescore, k37c34_truescore, 
                                    k39c34_truescore))

knitr::kable(c34_results[1:19, ], caption = "c34_results")

ggplot(c34_results, aes(k, Truescore)) + geom_point() + geom_line() + 
  labs(title = "Optimal k for Decision Cutoff 0.34")

############################
# 0.35 Cutoff
############################

# For the decision cutoff of 0.35, generate a confusion matrix for
# every combination of k (3:39 by two) and fold (1 to 10).

cm_c35 <- sapply(fk_dfs_l, function(x) {
  ss <- subset(x, select = c(pred35, obs))
  confusionMatrix(ss$pred35, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c35) <- fk_dfs_v

cm_c35_tables <- sapply(cm_c35, "[[", 2)
cm_c35_tables <- as_tibble(cm_c35_tables)

# For each value of k, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each value of k when the decision cutoff is 0.35.

(k3c35_tpr <- round(sum(cm_c35_tables[1, 1:10]) / (sum(cm_c35_tables[1, 1:10]) + sum(cm_c35_tables[2, 1:10])), 6))
(k3c35_tnr <- round(sum(cm_c35_tables[4, 1:10]) / (sum(cm_c35_tables[4, 1:10]) + sum(cm_c35_tables[3, 1:10])), 6))
(k3c35_truescore <- round((2 * k3c35_tpr * k3c35_tnr) / (k3c35_tpr + k3c35_tnr), 6))

(k5c35_tpr <- round(sum(cm_c35_tables[1, 11:20]) / (sum(cm_c35_tables[1, 11:20]) + sum(cm_c35_tables[2, 11:20])), 6))
(k5c35_tnr <- round(sum(cm_c35_tables[4, 11:20]) / (sum(cm_c35_tables[4, 11:20]) + sum(cm_c35_tables[3, 11:20])), 6))
(k5c35_truescore <- round((2 * k5c35_tpr * k5c35_tnr) / (k5c35_tpr + k5c35_tnr), 6))

(k7c35_tpr <- round(sum(cm_c35_tables[1, 21:30]) / (sum(cm_c35_tables[1, 21:30]) + sum(cm_c35_tables[2, 21:30])), 6))
(k7c35_tnr <- round(sum(cm_c35_tables[4, 21:30]) / (sum(cm_c35_tables[4, 21:30]) + sum(cm_c35_tables[3, 21:30])), 6))
(k7c35_truescore <- round((2 * k7c35_tpr * k7c35_tnr) / (k7c35_tpr + k7c35_tnr), 6))

(k9c35_tpr <- round(sum(cm_c35_tables[1, 31:40]) / (sum(cm_c35_tables[1, 31:40]) + sum(cm_c35_tables[2, 31:40])), 6))
(k9c35_tnr <- round(sum(cm_c35_tables[4, 31:40]) / (sum(cm_c35_tables[4, 31:40]) + sum(cm_c35_tables[3, 31:40])), 6))
(k9c35_truescore <- round((2 * k9c35_tpr * k9c35_tnr) / (k9c35_tpr + k9c35_tnr), 6))

(k11c35_tpr <- round(sum(cm_c35_tables[1, 41:50]) / (sum(cm_c35_tables[1, 41:50]) + sum(cm_c35_tables[2, 41:50])), 6))
(k11c35_tnr <- round(sum(cm_c35_tables[4, 41:50]) / (sum(cm_c35_tables[4, 41:50]) + sum(cm_c35_tables[3, 41:50])), 6))
(k11c35_truescore <- round((2 * k11c35_tpr * k11c35_tnr) / (k11c35_tpr + k11c35_tnr), 6))

(k13c35_tpr <- round(sum(cm_c35_tables[1, 51:60]) / (sum(cm_c35_tables[1, 51:60]) + sum(cm_c35_tables[2, 51:60])), 6))
(k13c35_tnr <- round(sum(cm_c35_tables[4, 51:60]) / (sum(cm_c35_tables[4, 51:60]) + sum(cm_c35_tables[3, 51:60])), 6))
(k13c35_truescore <- round((2 * k13c35_tpr * k13c35_tnr) / (k13c35_tpr + k13c35_tnr), 6))

(k15c35_tpr <- round(sum(cm_c35_tables[1, 61:70]) / (sum(cm_c35_tables[1, 61:70]) + sum(cm_c35_tables[2, 61:70])), 6))
(k15c35_tnr <- round(sum(cm_c35_tables[4, 61:70]) / (sum(cm_c35_tables[4, 61:70]) + sum(cm_c35_tables[3, 61:70])), 6))
(k15c35_truescore <- round((2 * k15c35_tpr * k15c35_tnr) / (k15c35_tpr + k15c35_tnr), 6))

(k17c35_tpr <- round(sum(cm_c35_tables[1, 71:80]) / (sum(cm_c35_tables[1, 71:80]) + sum(cm_c35_tables[2, 71:80])), 6))
(k17c35_tnr <- round(sum(cm_c35_tables[4, 71:80]) / (sum(cm_c35_tables[4, 71:80]) + sum(cm_c35_tables[3, 71:80])), 6))
(k17c35_truescore <- round((2 * k17c35_tpr * k17c35_tnr) / (k17c35_tpr + k17c35_tnr), 6))

(k19c35_tpr <- round(sum(cm_c35_tables[1, 81:90]) / (sum(cm_c35_tables[1, 81:90]) + sum(cm_c35_tables[2, 81:90])), 6))
(k19c35_tnr <- round(sum(cm_c35_tables[4, 81:90]) / (sum(cm_c35_tables[4, 81:90]) + sum(cm_c35_tables[3, 81:90])), 6))
(k19c35_truescore <- round((2 * k19c35_tpr * k19c35_tnr) / (k19c35_tpr + k19c35_tnr), 6))

(k21c35_tpr <- round(sum(cm_c35_tables[1, 91:100]) / (sum(cm_c35_tables[1, 91:100]) + sum(cm_c35_tables[2, 91:100])), 6))
(k21c35_tnr <- round(sum(cm_c35_tables[4, 91:100]) / (sum(cm_c35_tables[4, 91:100]) + sum(cm_c35_tables[3, 91:100])), 6))
(k21c35_truescore <- round((2 * k21c35_tpr * k21c35_tnr) / (k21c35_tpr + k21c35_tnr), 6))

(k23c35_tpr <- round(sum(cm_c35_tables[1, 101:110]) / (sum(cm_c35_tables[1, 101:110]) + sum(cm_c35_tables[2, 101:110])), 6))
(k23c35_tnr <- round(sum(cm_c35_tables[4, 101:110]) / (sum(cm_c35_tables[4, 101:110]) + sum(cm_c35_tables[3, 101:110])), 6))
(k23c35_truescore <- round((2 * k23c35_tpr * k23c35_tnr) / (k23c35_tpr + k23c35_tnr), 6))

(k25c35_tpr <- round(sum(cm_c35_tables[1, 111:120]) / (sum(cm_c35_tables[1, 111:120]) + sum(cm_c35_tables[2, 111:120])), 6))
(k25c35_tnr <- round(sum(cm_c35_tables[4, 111:120]) / (sum(cm_c35_tables[4, 111:120]) + sum(cm_c35_tables[3, 111:120])), 6))
(k25c35_truescore <- round((2 * k25c35_tpr * k25c35_tnr) / (k25c35_tpr + k25c35_tnr), 6))

(k27c35_tpr <- round(sum(cm_c35_tables[1, 121:130]) / (sum(cm_c35_tables[1, 121:130]) + sum(cm_c35_tables[2, 121:130])), 6))
(k27c35_tnr <- round(sum(cm_c35_tables[4, 121:130]) / (sum(cm_c35_tables[4, 121:130]) + sum(cm_c35_tables[3, 121:130])), 6))
(k27c35_truescore <- round((2 * k27c35_tpr * k27c35_tnr) / (k27c35_tpr + k27c35_tnr), 6))

(k29c35_tpr <- round(sum(cm_c35_tables[1, 131:140]) / (sum(cm_c35_tables[1, 131:140]) + sum(cm_c35_tables[2, 131:140])), 6))
(k29c35_tnr <- round(sum(cm_c35_tables[4, 131:140]) / (sum(cm_c35_tables[4, 131:140]) + sum(cm_c35_tables[3, 131:140])), 6))
(k29c35_truescore <- round((2 * k29c35_tpr * k29c35_tnr) / (k29c35_tpr + k29c35_tnr), 6))

(k31c35_tpr <- round(sum(cm_c35_tables[1, 141:150]) / (sum(cm_c35_tables[1, 141:150]) + sum(cm_c35_tables[2, 141:150])), 6))
(k31c35_tnr <- round(sum(cm_c35_tables[4, 141:150]) / (sum(cm_c35_tables[4, 141:150]) + sum(cm_c35_tables[3, 141:150])), 6))
(k31c35_truescore <- round((2 * k31c35_tpr * k31c35_tnr) / (k31c35_tpr + k31c35_tnr), 6))

(k33c35_tpr <- round(sum(cm_c35_tables[1, 151:160]) / (sum(cm_c35_tables[1, 151:160]) + sum(cm_c35_tables[2, 151:160])), 6))
(k33c35_tnr <- round(sum(cm_c35_tables[4, 151:160]) / (sum(cm_c35_tables[4, 151:160]) + sum(cm_c35_tables[3, 151:160])), 6))
(k33c35_truescore <- round((2 * k33c35_tpr * k33c35_tnr) / (k33c35_tpr + k33c35_tnr), 6))

(k35c35_tpr <- round(sum(cm_c35_tables[1, 161:170]) / (sum(cm_c35_tables[1, 161:170]) + sum(cm_c35_tables[2, 161:170])), 6))
(k35c35_tnr <- round(sum(cm_c35_tables[4, 161:170]) / (sum(cm_c35_tables[4, 161:170]) + sum(cm_c35_tables[3, 161:170])), 6))
(k35c35_truescore <- round((2 * k35c35_tpr * k35c35_tnr) / (k35c35_tpr + k35c35_tnr), 6))

(k37c35_tpr <- round(sum(cm_c35_tables[1, 171:180]) / (sum(cm_c35_tables[1, 171:180]) + sum(cm_c35_tables[2, 171:180])), 6))
(k37c35_tnr <- round(sum(cm_c35_tables[4, 171:180]) / (sum(cm_c35_tables[4, 171:180]) + sum(cm_c35_tables[3, 171:180])), 6))
(k37c35_truescore <- round((2 * k37c35_tpr * k37c35_tnr) / (k37c35_tpr + k37c35_tnr), 6))

(k39c35_tpr <- round(sum(cm_c35_tables[1, 181:190]) / (sum(cm_c35_tables[1, 181:190]) + sum(cm_c35_tables[2, 181:190])), 6))
(k39c35_tnr <- round(sum(cm_c35_tables[4, 181:190]) / (sum(cm_c35_tables[4, 181:190]) + sum(cm_c35_tables[3, 181:190])), 6))
(k39c35_truescore <- round((2 * k39c35_tpr * k39c35_tnr) / (k39c35_tpr + k39c35_tnr), 6))

# Compile the 0.35 cutoff results in a table, and identify the value
# of k that maximizes the truescore.

c35_results <- tibble(k = seq(3, 39, 2), 
                      Cut = 0.35, 
                      TPR = c(k3c35_tpr, k5c35_tpr, k7c35_tpr, k9c35_tpr, k11c35_tpr, 
                              k13c35_tpr, k15c35_tpr, k17c35_tpr, k19c35_tpr, k21c35_tpr, 
                              k23c35_tpr, k25c35_tpr, k27c35_tpr, k29c35_tpr, k31c35_tpr, 
                              k33c35_tpr, k35c35_tpr, k37c35_tpr, k39c35_tpr), 
                      TNR = c(k3c35_tnr, k5c35_tnr, k7c35_tnr, k9c35_tnr, k11c35_tnr, 
                              k13c35_tnr, k15c35_tnr, k17c35_tnr, k19c35_tnr, k21c35_tnr, 
                              k23c35_tnr, k25c35_tnr, k27c35_tnr, k29c35_tnr, k31c35_tnr, 
                              k33c35_tnr, k35c35_tnr, k37c35_tnr, k39c35_tnr), 
                      Truescore = c(k3c35_truescore, k5c35_truescore, k7c35_truescore, 
                                    k9c35_truescore, k11c35_truescore, k13c35_truescore, 
                                    k15c35_truescore, k17c35_truescore, k19c35_truescore, 
                                    k21c35_truescore, k23c35_truescore, k25c35_truescore, 
                                    k27c35_truescore, k29c35_truescore, k31c35_truescore, 
                                    k33c35_truescore, k35c35_truescore, k37c35_truescore, 
                                    k39c35_truescore))

knitr::kable(c35_results[1:19, ], caption = "c35_results")

ggplot(c35_results, aes(k, Truescore)) + geom_point() + geom_line() + 
  labs(title = "Optimal k for Decision Cutoff 0.35")

############################
# 0.36 Cutoff
############################

# For the decision cutoff of 0.36, generate a confusion matrix for
# every combination of k (3:39 by two) and fold (1 to 10).

cm_c36 <- sapply(fk_dfs_l, function(x) {
  ss <- subset(x, select = c(pred36, obs))
  confusionMatrix(ss$pred36, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c36) <- fk_dfs_v

cm_c36_tables <- sapply(cm_c36, "[[", 2)
cm_c36_tables <- as_tibble(cm_c36_tables)

# For each value of k, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each value of k when the decision cutoff is 0.36.

(k3c36_tpr <- round(sum(cm_c36_tables[1, 1:10]) / (sum(cm_c36_tables[1, 1:10]) + sum(cm_c36_tables[2, 1:10])), 6))
(k3c36_tnr <- round(sum(cm_c36_tables[4, 1:10]) / (sum(cm_c36_tables[4, 1:10]) + sum(cm_c36_tables[3, 1:10])), 6))
(k3c36_truescore <- round((2 * k3c36_tpr * k3c36_tnr) / (k3c36_tpr + k3c36_tnr), 6))

(k5c36_tpr <- round(sum(cm_c36_tables[1, 11:20]) / (sum(cm_c36_tables[1, 11:20]) + sum(cm_c36_tables[2, 11:20])), 6))
(k5c36_tnr <- round(sum(cm_c36_tables[4, 11:20]) / (sum(cm_c36_tables[4, 11:20]) + sum(cm_c36_tables[3, 11:20])), 6))
(k5c36_truescore <- round((2 * k5c36_tpr * k5c36_tnr) / (k5c36_tpr + k5c36_tnr), 6))

(k7c36_tpr <- round(sum(cm_c36_tables[1, 21:30]) / (sum(cm_c36_tables[1, 21:30]) + sum(cm_c36_tables[2, 21:30])), 6))
(k7c36_tnr <- round(sum(cm_c36_tables[4, 21:30]) / (sum(cm_c36_tables[4, 21:30]) + sum(cm_c36_tables[3, 21:30])), 6))
(k7c36_truescore <- round((2 * k7c36_tpr * k7c36_tnr) / (k7c36_tpr + k7c36_tnr), 6))

(k9c36_tpr <- round(sum(cm_c36_tables[1, 31:40]) / (sum(cm_c36_tables[1, 31:40]) + sum(cm_c36_tables[2, 31:40])), 6))
(k9c36_tnr <- round(sum(cm_c36_tables[4, 31:40]) / (sum(cm_c36_tables[4, 31:40]) + sum(cm_c36_tables[3, 31:40])), 6))
(k9c36_truescore <- round((2 * k9c36_tpr * k9c36_tnr) / (k9c36_tpr + k9c36_tnr), 6))

(k11c36_tpr <- round(sum(cm_c36_tables[1, 41:50]) / (sum(cm_c36_tables[1, 41:50]) + sum(cm_c36_tables[2, 41:50])), 6))
(k11c36_tnr <- round(sum(cm_c36_tables[4, 41:50]) / (sum(cm_c36_tables[4, 41:50]) + sum(cm_c36_tables[3, 41:50])), 6))
(k11c36_truescore <- round((2 * k11c36_tpr * k11c36_tnr) / (k11c36_tpr + k11c36_tnr), 6))

(k13c36_tpr <- round(sum(cm_c36_tables[1, 51:60]) / (sum(cm_c36_tables[1, 51:60]) + sum(cm_c36_tables[2, 51:60])), 6))
(k13c36_tnr <- round(sum(cm_c36_tables[4, 51:60]) / (sum(cm_c36_tables[4, 51:60]) + sum(cm_c36_tables[3, 51:60])), 6))
(k13c36_truescore <- round((2 * k13c36_tpr * k13c36_tnr) / (k13c36_tpr + k13c36_tnr), 6))

(k15c36_tpr <- round(sum(cm_c36_tables[1, 61:70]) / (sum(cm_c36_tables[1, 61:70]) + sum(cm_c36_tables[2, 61:70])), 6))
(k15c36_tnr <- round(sum(cm_c36_tables[4, 61:70]) / (sum(cm_c36_tables[4, 61:70]) + sum(cm_c36_tables[3, 61:70])), 6))
(k15c36_truescore <- round((2 * k15c36_tpr * k15c36_tnr) / (k15c36_tpr + k15c36_tnr), 6))

(k17c36_tpr <- round(sum(cm_c36_tables[1, 71:80]) / (sum(cm_c36_tables[1, 71:80]) + sum(cm_c36_tables[2, 71:80])), 6))
(k17c36_tnr <- round(sum(cm_c36_tables[4, 71:80]) / (sum(cm_c36_tables[4, 71:80]) + sum(cm_c36_tables[3, 71:80])), 6))
(k17c36_truescore <- round((2 * k17c36_tpr * k17c36_tnr) / (k17c36_tpr + k17c36_tnr), 6))

(k19c36_tpr <- round(sum(cm_c36_tables[1, 81:90]) / (sum(cm_c36_tables[1, 81:90]) + sum(cm_c36_tables[2, 81:90])), 6))
(k19c36_tnr <- round(sum(cm_c36_tables[4, 81:90]) / (sum(cm_c36_tables[4, 81:90]) + sum(cm_c36_tables[3, 81:90])), 6))
(k19c36_truescore <- round((2 * k19c36_tpr * k19c36_tnr) / (k19c36_tpr + k19c36_tnr), 6))

(k21c36_tpr <- round(sum(cm_c36_tables[1, 91:100]) / (sum(cm_c36_tables[1, 91:100]) + sum(cm_c36_tables[2, 91:100])), 6))
(k21c36_tnr <- round(sum(cm_c36_tables[4, 91:100]) / (sum(cm_c36_tables[4, 91:100]) + sum(cm_c36_tables[3, 91:100])), 6))
(k21c36_truescore <- round((2 * k21c36_tpr * k21c36_tnr) / (k21c36_tpr + k21c36_tnr), 6))

(k23c36_tpr <- round(sum(cm_c36_tables[1, 101:110]) / (sum(cm_c36_tables[1, 101:110]) + sum(cm_c36_tables[2, 101:110])), 6))
(k23c36_tnr <- round(sum(cm_c36_tables[4, 101:110]) / (sum(cm_c36_tables[4, 101:110]) + sum(cm_c36_tables[3, 101:110])), 6))
(k23c36_truescore <- round((2 * k23c36_tpr * k23c36_tnr) / (k23c36_tpr + k23c36_tnr), 6))

(k25c36_tpr <- round(sum(cm_c36_tables[1, 111:120]) / (sum(cm_c36_tables[1, 111:120]) + sum(cm_c36_tables[2, 111:120])), 6))
(k25c36_tnr <- round(sum(cm_c36_tables[4, 111:120]) / (sum(cm_c36_tables[4, 111:120]) + sum(cm_c36_tables[3, 111:120])), 6))
(k25c36_truescore <- round((2 * k25c36_tpr * k25c36_tnr) / (k25c36_tpr + k25c36_tnr), 6))

(k27c36_tpr <- round(sum(cm_c36_tables[1, 121:130]) / (sum(cm_c36_tables[1, 121:130]) + sum(cm_c36_tables[2, 121:130])), 6))
(k27c36_tnr <- round(sum(cm_c36_tables[4, 121:130]) / (sum(cm_c36_tables[4, 121:130]) + sum(cm_c36_tables[3, 121:130])), 6))
(k27c36_truescore <- round((2 * k27c36_tpr * k27c36_tnr) / (k27c36_tpr + k27c36_tnr), 6))

(k29c36_tpr <- round(sum(cm_c36_tables[1, 131:140]) / (sum(cm_c36_tables[1, 131:140]) + sum(cm_c36_tables[2, 131:140])), 6))
(k29c36_tnr <- round(sum(cm_c36_tables[4, 131:140]) / (sum(cm_c36_tables[4, 131:140]) + sum(cm_c36_tables[3, 131:140])), 6))
(k29c36_truescore <- round((2 * k29c36_tpr * k29c36_tnr) / (k29c36_tpr + k29c36_tnr), 6))

(k31c36_tpr <- round(sum(cm_c36_tables[1, 141:150]) / (sum(cm_c36_tables[1, 141:150]) + sum(cm_c36_tables[2, 141:150])), 6))
(k31c36_tnr <- round(sum(cm_c36_tables[4, 141:150]) / (sum(cm_c36_tables[4, 141:150]) + sum(cm_c36_tables[3, 141:150])), 6))
(k31c36_truescore <- round((2 * k31c36_tpr * k31c36_tnr) / (k31c36_tpr + k31c36_tnr), 6))

(k33c36_tpr <- round(sum(cm_c36_tables[1, 151:160]) / (sum(cm_c36_tables[1, 151:160]) + sum(cm_c36_tables[2, 151:160])), 6))
(k33c36_tnr <- round(sum(cm_c36_tables[4, 151:160]) / (sum(cm_c36_tables[4, 151:160]) + sum(cm_c36_tables[3, 151:160])), 6))
(k33c36_truescore <- round((2 * k33c36_tpr * k33c36_tnr) / (k33c36_tpr + k33c36_tnr), 6))

(k35c36_tpr <- round(sum(cm_c36_tables[1, 161:170]) / (sum(cm_c36_tables[1, 161:170]) + sum(cm_c36_tables[2, 161:170])), 6))
(k35c36_tnr <- round(sum(cm_c36_tables[4, 161:170]) / (sum(cm_c36_tables[4, 161:170]) + sum(cm_c36_tables[3, 161:170])), 6))
(k35c36_truescore <- round((2 * k35c36_tpr * k35c36_tnr) / (k35c36_tpr + k35c36_tnr), 6))

(k37c36_tpr <- round(sum(cm_c36_tables[1, 171:180]) / (sum(cm_c36_tables[1, 171:180]) + sum(cm_c36_tables[2, 171:180])), 6))
(k37c36_tnr <- round(sum(cm_c36_tables[4, 171:180]) / (sum(cm_c36_tables[4, 171:180]) + sum(cm_c36_tables[3, 171:180])), 6))
(k37c36_truescore <- round((2 * k37c36_tpr * k37c36_tnr) / (k37c36_tpr + k37c36_tnr), 6))

(k39c36_tpr <- round(sum(cm_c36_tables[1, 181:190]) / (sum(cm_c36_tables[1, 181:190]) + sum(cm_c36_tables[2, 181:190])), 6))
(k39c36_tnr <- round(sum(cm_c36_tables[4, 181:190]) / (sum(cm_c36_tables[4, 181:190]) + sum(cm_c36_tables[3, 181:190])), 6))
(k39c36_truescore <- round((2 * k39c36_tpr * k39c36_tnr) / (k39c36_tpr + k39c36_tnr), 6))

# Compile the 0.36 cutoff results in a table, and identify the value
# of k that maximizes the truescore.

c36_results <- tibble(k = seq(3, 39, 2), 
                      Cut = 0.36, 
                      TPR = c(k3c36_tpr, k5c36_tpr, k7c36_tpr, k9c36_tpr, k11c36_tpr, 
                              k13c36_tpr, k15c36_tpr, k17c36_tpr, k19c36_tpr, k21c36_tpr, 
                              k23c36_tpr, k25c36_tpr, k27c36_tpr, k29c36_tpr, k31c36_tpr, 
                              k33c36_tpr, k35c36_tpr, k37c36_tpr, k39c36_tpr), 
                      TNR = c(k3c36_tnr, k5c36_tnr, k7c36_tnr, k9c36_tnr, k11c36_tnr, 
                              k13c36_tnr, k15c36_tnr, k17c36_tnr, k19c36_tnr, k21c36_tnr, 
                              k23c36_tnr, k25c36_tnr, k27c36_tnr, k29c36_tnr, k31c36_tnr, 
                              k33c36_tnr, k35c36_tnr, k37c36_tnr, k39c36_tnr), 
                      Truescore = c(k3c36_truescore, k5c36_truescore, k7c36_truescore, 
                                    k9c36_truescore, k11c36_truescore, k13c36_truescore, 
                                    k15c36_truescore, k17c36_truescore, k19c36_truescore, 
                                    k21c36_truescore, k23c36_truescore, k25c36_truescore, 
                                    k27c36_truescore, k29c36_truescore, k31c36_truescore, 
                                    k33c36_truescore, k35c36_truescore, k37c36_truescore, 
                                    k39c36_truescore))

knitr::kable(c36_results[1:19, ], caption = "c36_results")

ggplot(c36_results, aes(k, Truescore)) + geom_point() + geom_line() + 
  labs(title = "Optimal k for Decision Cutoff 0.36")

############################
# 0.37 Cutoff
############################

# For the decision cutoff of 0.37, generate a confusion matrix for
# every combination of k (3:39 by two) and fold (1 to 10).

cm_c37 <- sapply(fk_dfs_l, function(x) {
  ss <- subset(x, select = c(pred37, obs))
  confusionMatrix(ss$pred37, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c37) <- fk_dfs_v

cm_c37_tables <- sapply(cm_c37, "[[", 2)
cm_c37_tables <- as_tibble(cm_c37_tables)

# For each value of k, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each value of k when the decision cutoff is 0.37.

(k3c37_tpr <- round(sum(cm_c37_tables[1, 1:10]) / (sum(cm_c37_tables[1, 1:10]) + sum(cm_c37_tables[2, 1:10])), 6))
(k3c37_tnr <- round(sum(cm_c37_tables[4, 1:10]) / (sum(cm_c37_tables[4, 1:10]) + sum(cm_c37_tables[3, 1:10])), 6))
(k3c37_truescore <- round((2 * k3c37_tpr * k3c37_tnr) / (k3c37_tpr + k3c37_tnr), 6))

(k5c37_tpr <- round(sum(cm_c37_tables[1, 11:20]) / (sum(cm_c37_tables[1, 11:20]) + sum(cm_c37_tables[2, 11:20])), 6))
(k5c37_tnr <- round(sum(cm_c37_tables[4, 11:20]) / (sum(cm_c37_tables[4, 11:20]) + sum(cm_c37_tables[3, 11:20])), 6))
(k5c37_truescore <- round((2 * k5c37_tpr * k5c37_tnr) / (k5c37_tpr + k5c37_tnr), 6))

(k7c37_tpr <- round(sum(cm_c37_tables[1, 21:30]) / (sum(cm_c37_tables[1, 21:30]) + sum(cm_c37_tables[2, 21:30])), 6))
(k7c37_tnr <- round(sum(cm_c37_tables[4, 21:30]) / (sum(cm_c37_tables[4, 21:30]) + sum(cm_c37_tables[3, 21:30])), 6))
(k7c37_truescore <- round((2 * k7c37_tpr * k7c37_tnr) / (k7c37_tpr + k7c37_tnr), 6))

(k9c37_tpr <- round(sum(cm_c37_tables[1, 31:40]) / (sum(cm_c37_tables[1, 31:40]) + sum(cm_c37_tables[2, 31:40])), 6))
(k9c37_tnr <- round(sum(cm_c37_tables[4, 31:40]) / (sum(cm_c37_tables[4, 31:40]) + sum(cm_c37_tables[3, 31:40])), 6))
(k9c37_truescore <- round((2 * k9c37_tpr * k9c37_tnr) / (k9c37_tpr + k9c37_tnr), 6))

(k11c37_tpr <- round(sum(cm_c37_tables[1, 41:50]) / (sum(cm_c37_tables[1, 41:50]) + sum(cm_c37_tables[2, 41:50])), 6))
(k11c37_tnr <- round(sum(cm_c37_tables[4, 41:50]) / (sum(cm_c37_tables[4, 41:50]) + sum(cm_c37_tables[3, 41:50])), 6))
(k11c37_truescore <- round((2 * k11c37_tpr * k11c37_tnr) / (k11c37_tpr + k11c37_tnr), 6))

(k13c37_tpr <- round(sum(cm_c37_tables[1, 51:60]) / (sum(cm_c37_tables[1, 51:60]) + sum(cm_c37_tables[2, 51:60])), 6))
(k13c37_tnr <- round(sum(cm_c37_tables[4, 51:60]) / (sum(cm_c37_tables[4, 51:60]) + sum(cm_c37_tables[3, 51:60])), 6))
(k13c37_truescore <- round((2 * k13c37_tpr * k13c37_tnr) / (k13c37_tpr + k13c37_tnr), 6))

(k15c37_tpr <- round(sum(cm_c37_tables[1, 61:70]) / (sum(cm_c37_tables[1, 61:70]) + sum(cm_c37_tables[2, 61:70])), 6))
(k15c37_tnr <- round(sum(cm_c37_tables[4, 61:70]) / (sum(cm_c37_tables[4, 61:70]) + sum(cm_c37_tables[3, 61:70])), 6))
(k15c37_truescore <- round((2 * k15c37_tpr * k15c37_tnr) / (k15c37_tpr + k15c37_tnr), 6))

(k17c37_tpr <- round(sum(cm_c37_tables[1, 71:80]) / (sum(cm_c37_tables[1, 71:80]) + sum(cm_c37_tables[2, 71:80])), 6))
(k17c37_tnr <- round(sum(cm_c37_tables[4, 71:80]) / (sum(cm_c37_tables[4, 71:80]) + sum(cm_c37_tables[3, 71:80])), 6))
(k17c37_truescore <- round((2 * k17c37_tpr * k17c37_tnr) / (k17c37_tpr + k17c37_tnr), 6))

(k19c37_tpr <- round(sum(cm_c37_tables[1, 81:90]) / (sum(cm_c37_tables[1, 81:90]) + sum(cm_c37_tables[2, 81:90])), 6))
(k19c37_tnr <- round(sum(cm_c37_tables[4, 81:90]) / (sum(cm_c37_tables[4, 81:90]) + sum(cm_c37_tables[3, 81:90])), 6))
(k19c37_truescore <- round((2 * k19c37_tpr * k19c37_tnr) / (k19c37_tpr + k19c37_tnr), 6))

(k21c37_tpr <- round(sum(cm_c37_tables[1, 91:100]) / (sum(cm_c37_tables[1, 91:100]) + sum(cm_c37_tables[2, 91:100])), 6))
(k21c37_tnr <- round(sum(cm_c37_tables[4, 91:100]) / (sum(cm_c37_tables[4, 91:100]) + sum(cm_c37_tables[3, 91:100])), 6))
(k21c37_truescore <- round((2 * k21c37_tpr * k21c37_tnr) / (k21c37_tpr + k21c37_tnr), 6))

(k23c37_tpr <- round(sum(cm_c37_tables[1, 101:110]) / (sum(cm_c37_tables[1, 101:110]) + sum(cm_c37_tables[2, 101:110])), 6))
(k23c37_tnr <- round(sum(cm_c37_tables[4, 101:110]) / (sum(cm_c37_tables[4, 101:110]) + sum(cm_c37_tables[3, 101:110])), 6))
(k23c37_truescore <- round((2 * k23c37_tpr * k23c37_tnr) / (k23c37_tpr + k23c37_tnr), 6))

(k25c37_tpr <- round(sum(cm_c37_tables[1, 111:120]) / (sum(cm_c37_tables[1, 111:120]) + sum(cm_c37_tables[2, 111:120])), 6))
(k25c37_tnr <- round(sum(cm_c37_tables[4, 111:120]) / (sum(cm_c37_tables[4, 111:120]) + sum(cm_c37_tables[3, 111:120])), 6))
(k25c37_truescore <- round((2 * k25c37_tpr * k25c37_tnr) / (k25c37_tpr + k25c37_tnr), 6))

(k27c37_tpr <- round(sum(cm_c37_tables[1, 121:130]) / (sum(cm_c37_tables[1, 121:130]) + sum(cm_c37_tables[2, 121:130])), 6))
(k27c37_tnr <- round(sum(cm_c37_tables[4, 121:130]) / (sum(cm_c37_tables[4, 121:130]) + sum(cm_c37_tables[3, 121:130])), 6))
(k27c37_truescore <- round((2 * k27c37_tpr * k27c37_tnr) / (k27c37_tpr + k27c37_tnr), 6))

(k29c37_tpr <- round(sum(cm_c37_tables[1, 131:140]) / (sum(cm_c37_tables[1, 131:140]) + sum(cm_c37_tables[2, 131:140])), 6))
(k29c37_tnr <- round(sum(cm_c37_tables[4, 131:140]) / (sum(cm_c37_tables[4, 131:140]) + sum(cm_c37_tables[3, 131:140])), 6))
(k29c37_truescore <- round((2 * k29c37_tpr * k29c37_tnr) / (k29c37_tpr + k29c37_tnr), 6))

(k31c37_tpr <- round(sum(cm_c37_tables[1, 141:150]) / (sum(cm_c37_tables[1, 141:150]) + sum(cm_c37_tables[2, 141:150])), 6))
(k31c37_tnr <- round(sum(cm_c37_tables[4, 141:150]) / (sum(cm_c37_tables[4, 141:150]) + sum(cm_c37_tables[3, 141:150])), 6))
(k31c37_truescore <- round((2 * k31c37_tpr * k31c37_tnr) / (k31c37_tpr + k31c37_tnr), 6))

(k33c37_tpr <- round(sum(cm_c37_tables[1, 151:160]) / (sum(cm_c37_tables[1, 151:160]) + sum(cm_c37_tables[2, 151:160])), 6))
(k33c37_tnr <- round(sum(cm_c37_tables[4, 151:160]) / (sum(cm_c37_tables[4, 151:160]) + sum(cm_c37_tables[3, 151:160])), 6))
(k33c37_truescore <- round((2 * k33c37_tpr * k33c37_tnr) / (k33c37_tpr + k33c37_tnr), 6))

(k35c37_tpr <- round(sum(cm_c37_tables[1, 161:170]) / (sum(cm_c37_tables[1, 161:170]) + sum(cm_c37_tables[2, 161:170])), 6))
(k35c37_tnr <- round(sum(cm_c37_tables[4, 161:170]) / (sum(cm_c37_tables[4, 161:170]) + sum(cm_c37_tables[3, 161:170])), 6))
(k35c37_truescore <- round((2 * k35c37_tpr * k35c37_tnr) / (k35c37_tpr + k35c37_tnr), 6))

(k37c37_tpr <- round(sum(cm_c37_tables[1, 171:180]) / (sum(cm_c37_tables[1, 171:180]) + sum(cm_c37_tables[2, 171:180])), 6))
(k37c37_tnr <- round(sum(cm_c37_tables[4, 171:180]) / (sum(cm_c37_tables[4, 171:180]) + sum(cm_c37_tables[3, 171:180])), 6))
(k37c37_truescore <- round((2 * k37c37_tpr * k37c37_tnr) / (k37c37_tpr + k37c37_tnr), 6))

(k39c37_tpr <- round(sum(cm_c37_tables[1, 181:190]) / (sum(cm_c37_tables[1, 181:190]) + sum(cm_c37_tables[2, 181:190])), 6))
(k39c37_tnr <- round(sum(cm_c37_tables[4, 181:190]) / (sum(cm_c37_tables[4, 181:190]) + sum(cm_c37_tables[3, 181:190])), 6))
(k39c37_truescore <- round((2 * k39c37_tpr * k39c37_tnr) / (k39c37_tpr + k39c37_tnr), 6))

# Compile the 0.37 cutoff results in a table, and identify the value
# of k that maximizes the truescore.

c37_results <- tibble(k = seq(3, 39, 2), 
                      Cut = 0.37, 
                      TPR = c(k3c37_tpr, k5c37_tpr, k7c37_tpr, k9c37_tpr, k11c37_tpr, 
                              k13c37_tpr, k15c37_tpr, k17c37_tpr, k19c37_tpr, k21c37_tpr, 
                              k23c37_tpr, k25c37_tpr, k27c37_tpr, k29c37_tpr, k31c37_tpr, 
                              k33c37_tpr, k35c37_tpr, k37c37_tpr, k39c37_tpr), 
                      TNR = c(k3c37_tnr, k5c37_tnr, k7c37_tnr, k9c37_tnr, k11c37_tnr, 
                              k13c37_tnr, k15c37_tnr, k17c37_tnr, k19c37_tnr, k21c37_tnr, 
                              k23c37_tnr, k25c37_tnr, k27c37_tnr, k29c37_tnr, k31c37_tnr, 
                              k33c37_tnr, k35c37_tnr, k37c37_tnr, k39c37_tnr), 
                      Truescore = c(k3c37_truescore, k5c37_truescore, k7c37_truescore, 
                                    k9c37_truescore, k11c37_truescore, k13c37_truescore, 
                                    k15c37_truescore, k17c37_truescore, k19c37_truescore, 
                                    k21c37_truescore, k23c37_truescore, k25c37_truescore, 
                                    k27c37_truescore, k29c37_truescore, k31c37_truescore, 
                                    k33c37_truescore, k35c37_truescore, k37c37_truescore, 
                                    k39c37_truescore))

knitr::kable(c37_results[1:19, ], caption = "c37_results")

ggplot(c37_results, aes(k, Truescore)) + geom_point() + geom_line() + 
  labs(title = "Optimal k for Decision Cutoff 0.37")

############################
# 0.38 Cutoff
############################

# For the decision cutoff of 0.38, generate a confusion matrix for
# every combination of k (3:39 by two) and fold (1 to 10).

cm_c38 <- sapply(fk_dfs_l, function(x) {
  ss <- subset(x, select = c(pred38, obs))
  confusionMatrix(ss$pred38, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c38) <- fk_dfs_v

cm_c38_tables <- sapply(cm_c38, "[[", 2)
cm_c38_tables <- as_tibble(cm_c38_tables)

# For each value of k, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each value of k when the decision cutoff is 0.38.

(k3c38_tpr <- round(sum(cm_c38_tables[1, 1:10]) / (sum(cm_c38_tables[1, 1:10]) + sum(cm_c38_tables[2, 1:10])), 6))
(k3c38_tnr <- round(sum(cm_c38_tables[4, 1:10]) / (sum(cm_c38_tables[4, 1:10]) + sum(cm_c38_tables[3, 1:10])), 6))
(k3c38_truescore <- round((2 * k3c38_tpr * k3c38_tnr) / (k3c38_tpr + k3c38_tnr), 6))

(k5c38_tpr <- round(sum(cm_c38_tables[1, 11:20]) / (sum(cm_c38_tables[1, 11:20]) + sum(cm_c38_tables[2, 11:20])), 6))
(k5c38_tnr <- round(sum(cm_c38_tables[4, 11:20]) / (sum(cm_c38_tables[4, 11:20]) + sum(cm_c38_tables[3, 11:20])), 6))
(k5c38_truescore <- round((2 * k5c38_tpr * k5c38_tnr) / (k5c38_tpr + k5c38_tnr), 6))

(k7c38_tpr <- round(sum(cm_c38_tables[1, 21:30]) / (sum(cm_c38_tables[1, 21:30]) + sum(cm_c38_tables[2, 21:30])), 6))
(k7c38_tnr <- round(sum(cm_c38_tables[4, 21:30]) / (sum(cm_c38_tables[4, 21:30]) + sum(cm_c38_tables[3, 21:30])), 6))
(k7c38_truescore <- round((2 * k7c38_tpr * k7c38_tnr) / (k7c38_tpr + k7c38_tnr), 6))

(k9c38_tpr <- round(sum(cm_c38_tables[1, 31:40]) / (sum(cm_c38_tables[1, 31:40]) + sum(cm_c38_tables[2, 31:40])), 6))
(k9c38_tnr <- round(sum(cm_c38_tables[4, 31:40]) / (sum(cm_c38_tables[4, 31:40]) + sum(cm_c38_tables[3, 31:40])), 6))
(k9c38_truescore <- round((2 * k9c38_tpr * k9c38_tnr) / (k9c38_tpr + k9c38_tnr), 6))

(k11c38_tpr <- round(sum(cm_c38_tables[1, 41:50]) / (sum(cm_c38_tables[1, 41:50]) + sum(cm_c38_tables[2, 41:50])), 6))
(k11c38_tnr <- round(sum(cm_c38_tables[4, 41:50]) / (sum(cm_c38_tables[4, 41:50]) + sum(cm_c38_tables[3, 41:50])), 6))
(k11c38_truescore <- round((2 * k11c38_tpr * k11c38_tnr) / (k11c38_tpr + k11c38_tnr), 6))

(k13c38_tpr <- round(sum(cm_c38_tables[1, 51:60]) / (sum(cm_c38_tables[1, 51:60]) + sum(cm_c38_tables[2, 51:60])), 6))
(k13c38_tnr <- round(sum(cm_c38_tables[4, 51:60]) / (sum(cm_c38_tables[4, 51:60]) + sum(cm_c38_tables[3, 51:60])), 6))
(k13c38_truescore <- round((2 * k13c38_tpr * k13c38_tnr) / (k13c38_tpr + k13c38_tnr), 6))

(k15c38_tpr <- round(sum(cm_c38_tables[1, 61:70]) / (sum(cm_c38_tables[1, 61:70]) + sum(cm_c38_tables[2, 61:70])), 6))
(k15c38_tnr <- round(sum(cm_c38_tables[4, 61:70]) / (sum(cm_c38_tables[4, 61:70]) + sum(cm_c38_tables[3, 61:70])), 6))
(k15c38_truescore <- round((2 * k15c38_tpr * k15c38_tnr) / (k15c38_tpr + k15c38_tnr), 6))

(k17c38_tpr <- round(sum(cm_c38_tables[1, 71:80]) / (sum(cm_c38_tables[1, 71:80]) + sum(cm_c38_tables[2, 71:80])), 6))
(k17c38_tnr <- round(sum(cm_c38_tables[4, 71:80]) / (sum(cm_c38_tables[4, 71:80]) + sum(cm_c38_tables[3, 71:80])), 6))
(k17c38_truescore <- round((2 * k17c38_tpr * k17c38_tnr) / (k17c38_tpr + k17c38_tnr), 6))

(k19c38_tpr <- round(sum(cm_c38_tables[1, 81:90]) / (sum(cm_c38_tables[1, 81:90]) + sum(cm_c38_tables[2, 81:90])), 6))
(k19c38_tnr <- round(sum(cm_c38_tables[4, 81:90]) / (sum(cm_c38_tables[4, 81:90]) + sum(cm_c38_tables[3, 81:90])), 6))
(k19c38_truescore <- round((2 * k19c38_tpr * k19c38_tnr) / (k19c38_tpr + k19c38_tnr), 6))

(k21c38_tpr <- round(sum(cm_c38_tables[1, 91:100]) / (sum(cm_c38_tables[1, 91:100]) + sum(cm_c38_tables[2, 91:100])), 6))
(k21c38_tnr <- round(sum(cm_c38_tables[4, 91:100]) / (sum(cm_c38_tables[4, 91:100]) + sum(cm_c38_tables[3, 91:100])), 6))
(k21c38_truescore <- round((2 * k21c38_tpr * k21c38_tnr) / (k21c38_tpr + k21c38_tnr), 6))

(k23c38_tpr <- round(sum(cm_c38_tables[1, 101:110]) / (sum(cm_c38_tables[1, 101:110]) + sum(cm_c38_tables[2, 101:110])), 6))
(k23c38_tnr <- round(sum(cm_c38_tables[4, 101:110]) / (sum(cm_c38_tables[4, 101:110]) + sum(cm_c38_tables[3, 101:110])), 6))
(k23c38_truescore <- round((2 * k23c38_tpr * k23c38_tnr) / (k23c38_tpr + k23c38_tnr), 6))

(k25c38_tpr <- round(sum(cm_c38_tables[1, 111:120]) / (sum(cm_c38_tables[1, 111:120]) + sum(cm_c38_tables[2, 111:120])), 6))
(k25c38_tnr <- round(sum(cm_c38_tables[4, 111:120]) / (sum(cm_c38_tables[4, 111:120]) + sum(cm_c38_tables[3, 111:120])), 6))
(k25c38_truescore <- round((2 * k25c38_tpr * k25c38_tnr) / (k25c38_tpr + k25c38_tnr), 6))

(k27c38_tpr <- round(sum(cm_c38_tables[1, 121:130]) / (sum(cm_c38_tables[1, 121:130]) + sum(cm_c38_tables[2, 121:130])), 6))
(k27c38_tnr <- round(sum(cm_c38_tables[4, 121:130]) / (sum(cm_c38_tables[4, 121:130]) + sum(cm_c38_tables[3, 121:130])), 6))
(k27c38_truescore <- round((2 * k27c38_tpr * k27c38_tnr) / (k27c38_tpr + k27c38_tnr), 6))

(k29c38_tpr <- round(sum(cm_c38_tables[1, 131:140]) / (sum(cm_c38_tables[1, 131:140]) + sum(cm_c38_tables[2, 131:140])), 6))
(k29c38_tnr <- round(sum(cm_c38_tables[4, 131:140]) / (sum(cm_c38_tables[4, 131:140]) + sum(cm_c38_tables[3, 131:140])), 6))
(k29c38_truescore <- round((2 * k29c38_tpr * k29c38_tnr) / (k29c38_tpr + k29c38_tnr), 6))

(k31c38_tpr <- round(sum(cm_c38_tables[1, 141:150]) / (sum(cm_c38_tables[1, 141:150]) + sum(cm_c38_tables[2, 141:150])), 6))
(k31c38_tnr <- round(sum(cm_c38_tables[4, 141:150]) / (sum(cm_c38_tables[4, 141:150]) + sum(cm_c38_tables[3, 141:150])), 6))
(k31c38_truescore <- round((2 * k31c38_tpr * k31c38_tnr) / (k31c38_tpr + k31c38_tnr), 6))

(k33c38_tpr <- round(sum(cm_c38_tables[1, 151:160]) / (sum(cm_c38_tables[1, 151:160]) + sum(cm_c38_tables[2, 151:160])), 6))
(k33c38_tnr <- round(sum(cm_c38_tables[4, 151:160]) / (sum(cm_c38_tables[4, 151:160]) + sum(cm_c38_tables[3, 151:160])), 6))
(k33c38_truescore <- round((2 * k33c38_tpr * k33c38_tnr) / (k33c38_tpr + k33c38_tnr), 6))

(k35c38_tpr <- round(sum(cm_c38_tables[1, 161:170]) / (sum(cm_c38_tables[1, 161:170]) + sum(cm_c38_tables[2, 161:170])), 6))
(k35c38_tnr <- round(sum(cm_c38_tables[4, 161:170]) / (sum(cm_c38_tables[4, 161:170]) + sum(cm_c38_tables[3, 161:170])), 6))
(k35c38_truescore <- round((2 * k35c38_tpr * k35c38_tnr) / (k35c38_tpr + k35c38_tnr), 6))

(k37c38_tpr <- round(sum(cm_c38_tables[1, 171:180]) / (sum(cm_c38_tables[1, 171:180]) + sum(cm_c38_tables[2, 171:180])), 6))
(k37c38_tnr <- round(sum(cm_c38_tables[4, 171:180]) / (sum(cm_c38_tables[4, 171:180]) + sum(cm_c38_tables[3, 171:180])), 6))
(k37c38_truescore <- round((2 * k37c38_tpr * k37c38_tnr) / (k37c38_tpr + k37c38_tnr), 6))

(k39c38_tpr <- round(sum(cm_c38_tables[1, 181:190]) / (sum(cm_c38_tables[1, 181:190]) + sum(cm_c38_tables[2, 181:190])), 6))
(k39c38_tnr <- round(sum(cm_c38_tables[4, 181:190]) / (sum(cm_c38_tables[4, 181:190]) + sum(cm_c38_tables[3, 181:190])), 6))
(k39c38_truescore <- round((2 * k39c38_tpr * k39c38_tnr) / (k39c38_tpr + k39c38_tnr), 6))

# Compile the 0.38 cutoff results in a table, and identify the value
# of k that maximizes the truescore.

c38_results <- tibble(k = seq(3, 39, 2), 
                      Cut = 0.38, 
                      TPR = c(k3c38_tpr, k5c38_tpr, k7c38_tpr, k9c38_tpr, k11c38_tpr, 
                              k13c38_tpr, k15c38_tpr, k17c38_tpr, k19c38_tpr, k21c38_tpr, 
                              k23c38_tpr, k25c38_tpr, k27c38_tpr, k29c38_tpr, k31c38_tpr, 
                              k33c38_tpr, k35c38_tpr, k37c38_tpr, k39c38_tpr), 
                      TNR = c(k3c38_tnr, k5c38_tnr, k7c38_tnr, k9c38_tnr, k11c38_tnr, 
                              k13c38_tnr, k15c38_tnr, k17c38_tnr, k19c38_tnr, k21c38_tnr, 
                              k23c38_tnr, k25c38_tnr, k27c38_tnr, k29c38_tnr, k31c38_tnr, 
                              k33c38_tnr, k35c38_tnr, k37c38_tnr, k39c38_tnr), 
                      Truescore = c(k3c38_truescore, k5c38_truescore, k7c38_truescore, 
                                    k9c38_truescore, k11c38_truescore, k13c38_truescore, 
                                    k15c38_truescore, k17c38_truescore, k19c38_truescore, 
                                    k21c38_truescore, k23c38_truescore, k25c38_truescore, 
                                    k27c38_truescore, k29c38_truescore, k31c38_truescore, 
                                    k33c38_truescore, k35c38_truescore, k37c38_truescore, 
                                    k39c38_truescore))

knitr::kable(c38_results[1:19, ], caption = "c38_results")

ggplot(c38_results, aes(k, Truescore)) + geom_point() + geom_line() + 
  labs(title = "Optimal k for Decision Cutoff 0.38")

############################
# 0.39 Cutoff
############################

# For the decision cutoff of 0.39, generate a confusion matrix for
# every combination of k (3:39 by two) and fold (1 to 10).

cm_c39 <- sapply(fk_dfs_l, function(x) {
  ss <- subset(x, select = c(pred39, obs))
  confusionMatrix(ss$pred39, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

rm(fk_dfs_l)
gc(reset = TRUE)

names(cm_c39) <- fk_dfs_v

cm_c39_tables <- sapply(cm_c39, "[[", 2)
cm_c39_tables <- as_tibble(cm_c39_tables)

# For each value of k, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each value of k when the decision cutoff is 0.39.

(k3c39_tpr <- round(sum(cm_c39_tables[1, 1:10]) / (sum(cm_c39_tables[1, 1:10]) + sum(cm_c39_tables[2, 1:10])), 6))
(k3c39_tnr <- round(sum(cm_c39_tables[4, 1:10]) / (sum(cm_c39_tables[4, 1:10]) + sum(cm_c39_tables[3, 1:10])), 6))
(k3c39_truescore <- round((2 * k3c39_tpr * k3c39_tnr) / (k3c39_tpr + k3c39_tnr), 6))

(k5c39_tpr <- round(sum(cm_c39_tables[1, 11:20]) / (sum(cm_c39_tables[1, 11:20]) + sum(cm_c39_tables[2, 11:20])), 6))
(k5c39_tnr <- round(sum(cm_c39_tables[4, 11:20]) / (sum(cm_c39_tables[4, 11:20]) + sum(cm_c39_tables[3, 11:20])), 6))
(k5c39_truescore <- round((2 * k5c39_tpr * k5c39_tnr) / (k5c39_tpr + k5c39_tnr), 6))

(k7c39_tpr <- round(sum(cm_c39_tables[1, 21:30]) / (sum(cm_c39_tables[1, 21:30]) + sum(cm_c39_tables[2, 21:30])), 6))
(k7c39_tnr <- round(sum(cm_c39_tables[4, 21:30]) / (sum(cm_c39_tables[4, 21:30]) + sum(cm_c39_tables[3, 21:30])), 6))
(k7c39_truescore <- round((2 * k7c39_tpr * k7c39_tnr) / (k7c39_tpr + k7c39_tnr), 6))

(k9c39_tpr <- round(sum(cm_c39_tables[1, 31:40]) / (sum(cm_c39_tables[1, 31:40]) + sum(cm_c39_tables[2, 31:40])), 6))
(k9c39_tnr <- round(sum(cm_c39_tables[4, 31:40]) / (sum(cm_c39_tables[4, 31:40]) + sum(cm_c39_tables[3, 31:40])), 6))
(k9c39_truescore <- round((2 * k9c39_tpr * k9c39_tnr) / (k9c39_tpr + k9c39_tnr), 6))

(k11c39_tpr <- round(sum(cm_c39_tables[1, 41:50]) / (sum(cm_c39_tables[1, 41:50]) + sum(cm_c39_tables[2, 41:50])), 6))
(k11c39_tnr <- round(sum(cm_c39_tables[4, 41:50]) / (sum(cm_c39_tables[4, 41:50]) + sum(cm_c39_tables[3, 41:50])), 6))
(k11c39_truescore <- round((2 * k11c39_tpr * k11c39_tnr) / (k11c39_tpr + k11c39_tnr), 6))

(k13c39_tpr <- round(sum(cm_c39_tables[1, 51:60]) / (sum(cm_c39_tables[1, 51:60]) + sum(cm_c39_tables[2, 51:60])), 6))
(k13c39_tnr <- round(sum(cm_c39_tables[4, 51:60]) / (sum(cm_c39_tables[4, 51:60]) + sum(cm_c39_tables[3, 51:60])), 6))
(k13c39_truescore <- round((2 * k13c39_tpr * k13c39_tnr) / (k13c39_tpr + k13c39_tnr), 6))

(k15c39_tpr <- round(sum(cm_c39_tables[1, 61:70]) / (sum(cm_c39_tables[1, 61:70]) + sum(cm_c39_tables[2, 61:70])), 6))
(k15c39_tnr <- round(sum(cm_c39_tables[4, 61:70]) / (sum(cm_c39_tables[4, 61:70]) + sum(cm_c39_tables[3, 61:70])), 6))
(k15c39_truescore <- round((2 * k15c39_tpr * k15c39_tnr) / (k15c39_tpr + k15c39_tnr), 6))

(k17c39_tpr <- round(sum(cm_c39_tables[1, 71:80]) / (sum(cm_c39_tables[1, 71:80]) + sum(cm_c39_tables[2, 71:80])), 6))
(k17c39_tnr <- round(sum(cm_c39_tables[4, 71:80]) / (sum(cm_c39_tables[4, 71:80]) + sum(cm_c39_tables[3, 71:80])), 6))
(k17c39_truescore <- round((2 * k17c39_tpr * k17c39_tnr) / (k17c39_tpr + k17c39_tnr), 6))

(k19c39_tpr <- round(sum(cm_c39_tables[1, 81:90]) / (sum(cm_c39_tables[1, 81:90]) + sum(cm_c39_tables[2, 81:90])), 6))
(k19c39_tnr <- round(sum(cm_c39_tables[4, 81:90]) / (sum(cm_c39_tables[4, 81:90]) + sum(cm_c39_tables[3, 81:90])), 6))
(k19c39_truescore <- round((2 * k19c39_tpr * k19c39_tnr) / (k19c39_tpr + k19c39_tnr), 6))

(k21c39_tpr <- round(sum(cm_c39_tables[1, 91:100]) / (sum(cm_c39_tables[1, 91:100]) + sum(cm_c39_tables[2, 91:100])), 6))
(k21c39_tnr <- round(sum(cm_c39_tables[4, 91:100]) / (sum(cm_c39_tables[4, 91:100]) + sum(cm_c39_tables[3, 91:100])), 6))
(k21c39_truescore <- round((2 * k21c39_tpr * k21c39_tnr) / (k21c39_tpr + k21c39_tnr), 6))

(k23c39_tpr <- round(sum(cm_c39_tables[1, 101:110]) / (sum(cm_c39_tables[1, 101:110]) + sum(cm_c39_tables[2, 101:110])), 6))
(k23c39_tnr <- round(sum(cm_c39_tables[4, 101:110]) / (sum(cm_c39_tables[4, 101:110]) + sum(cm_c39_tables[3, 101:110])), 6))
(k23c39_truescore <- round((2 * k23c39_tpr * k23c39_tnr) / (k23c39_tpr + k23c39_tnr), 6))

(k25c39_tpr <- round(sum(cm_c39_tables[1, 111:120]) / (sum(cm_c39_tables[1, 111:120]) + sum(cm_c39_tables[2, 111:120])), 6))
(k25c39_tnr <- round(sum(cm_c39_tables[4, 111:120]) / (sum(cm_c39_tables[4, 111:120]) + sum(cm_c39_tables[3, 111:120])), 6))
(k25c39_truescore <- round((2 * k25c39_tpr * k25c39_tnr) / (k25c39_tpr + k25c39_tnr), 6))

(k27c39_tpr <- round(sum(cm_c39_tables[1, 121:130]) / (sum(cm_c39_tables[1, 121:130]) + sum(cm_c39_tables[2, 121:130])), 6))
(k27c39_tnr <- round(sum(cm_c39_tables[4, 121:130]) / (sum(cm_c39_tables[4, 121:130]) + sum(cm_c39_tables[3, 121:130])), 6))
(k27c39_truescore <- round((2 * k27c39_tpr * k27c39_tnr) / (k27c39_tpr + k27c39_tnr), 6))

(k29c39_tpr <- round(sum(cm_c39_tables[1, 131:140]) / (sum(cm_c39_tables[1, 131:140]) + sum(cm_c39_tables[2, 131:140])), 6))
(k29c39_tnr <- round(sum(cm_c39_tables[4, 131:140]) / (sum(cm_c39_tables[4, 131:140]) + sum(cm_c39_tables[3, 131:140])), 6))
(k29c39_truescore <- round((2 * k29c39_tpr * k29c39_tnr) / (k29c39_tpr + k29c39_tnr), 6))

(k31c39_tpr <- round(sum(cm_c39_tables[1, 141:150]) / (sum(cm_c39_tables[1, 141:150]) + sum(cm_c39_tables[2, 141:150])), 6))
(k31c39_tnr <- round(sum(cm_c39_tables[4, 141:150]) / (sum(cm_c39_tables[4, 141:150]) + sum(cm_c39_tables[3, 141:150])), 6))
(k31c39_truescore <- round((2 * k31c39_tpr * k31c39_tnr) / (k31c39_tpr + k31c39_tnr), 6))

(k33c39_tpr <- round(sum(cm_c39_tables[1, 151:160]) / (sum(cm_c39_tables[1, 151:160]) + sum(cm_c39_tables[2, 151:160])), 6))
(k33c39_tnr <- round(sum(cm_c39_tables[4, 151:160]) / (sum(cm_c39_tables[4, 151:160]) + sum(cm_c39_tables[3, 151:160])), 6))
(k33c39_truescore <- round((2 * k33c39_tpr * k33c39_tnr) / (k33c39_tpr + k33c39_tnr), 6))

(k35c39_tpr <- round(sum(cm_c39_tables[1, 161:170]) / (sum(cm_c39_tables[1, 161:170]) + sum(cm_c39_tables[2, 161:170])), 6))
(k35c39_tnr <- round(sum(cm_c39_tables[4, 161:170]) / (sum(cm_c39_tables[4, 161:170]) + sum(cm_c39_tables[3, 161:170])), 6))
(k35c39_truescore <- round((2 * k35c39_tpr * k35c39_tnr) / (k35c39_tpr + k35c39_tnr), 6))

(k37c39_tpr <- round(sum(cm_c39_tables[1, 171:180]) / (sum(cm_c39_tables[1, 171:180]) + sum(cm_c39_tables[2, 171:180])), 6))
(k37c39_tnr <- round(sum(cm_c39_tables[4, 171:180]) / (sum(cm_c39_tables[4, 171:180]) + sum(cm_c39_tables[3, 171:180])), 6))
(k37c39_truescore <- round((2 * k37c39_tpr * k37c39_tnr) / (k37c39_tpr + k37c39_tnr), 6))

(k39c39_tpr <- round(sum(cm_c39_tables[1, 181:190]) / (sum(cm_c39_tables[1, 181:190]) + sum(cm_c39_tables[2, 181:190])), 6))
(k39c39_tnr <- round(sum(cm_c39_tables[4, 181:190]) / (sum(cm_c39_tables[4, 181:190]) + sum(cm_c39_tables[3, 181:190])), 6))
(k39c39_truescore <- round((2 * k39c39_tpr * k39c39_tnr) / (k39c39_tpr + k39c39_tnr), 6))

# Compile the 0.39 cutoff results in a table, and identify the value
# of k that maximizes the truescore.

c39_results <- tibble(k = seq(3, 39, 2), 
                      Cut = 0.39, 
                      TPR = c(k3c39_tpr, k5c39_tpr, k7c39_tpr, k9c39_tpr, k11c39_tpr, 
                              k13c39_tpr, k15c39_tpr, k17c39_tpr, k19c39_tpr, k21c39_tpr, 
                              k23c39_tpr, k25c39_tpr, k27c39_tpr, k29c39_tpr, k31c39_tpr, 
                              k33c39_tpr, k35c39_tpr, k37c39_tpr, k39c39_tpr), 
                      TNR = c(k3c39_tnr, k5c39_tnr, k7c39_tnr, k9c39_tnr, k11c39_tnr, 
                              k13c39_tnr, k15c39_tnr, k17c39_tnr, k19c39_tnr, k21c39_tnr, 
                              k23c39_tnr, k25c39_tnr, k27c39_tnr, k29c39_tnr, k31c39_tnr, 
                              k33c39_tnr, k35c39_tnr, k37c39_tnr, k39c39_tnr), 
                      Truescore = c(k3c39_truescore, k5c39_truescore, k7c39_truescore, 
                                    k9c39_truescore, k11c39_truescore, k13c39_truescore, 
                                    k15c39_truescore, k17c39_truescore, k19c39_truescore, 
                                    k21c39_truescore, k23c39_truescore, k25c39_truescore, 
                                    k27c39_truescore, k29c39_truescore, k31c39_truescore, 
                                    k33c39_truescore, k35c39_truescore, k37c39_truescore, 
                                    k39c39_truescore))

knitr::kable(c39_results[1:19, ], caption = "c39_results")

ggplot(c39_results, aes(k, Truescore)) + geom_point() + geom_line() + 
  labs(title = "Optimal k for Decision Cutoff 0.39")

#################

# Combine the results for every combination of k and cutoff that was tested.

br1 <- bind_rows(c25_results, c26_results, c27_results, c28_results, c29_results)
br2 <- bind_rows(c30_results, c31_results, c32_results, c33_results, c34_results)
br3 <- bind_rows(c35_results, c36_results, c37_results, c38_results, c39_results)

results_optkcut <- bind_rows(br1, br2, br3)

# Compute the Minimum Distance to (0, 1) for each combination of k and cutoff 
# that was tested.

results_optkcut <- results_optkcut %>% mutate(Distance = sqrt((1 - TPR)^2 + (1 - TNR)^2))

# Identify the optimal combination of values for k and the cutoff based on each of our 
# assessment methods, Truescore and Mimimum Distance to (0, 1).

results_optkcut %>% ggplot(aes(x = Cut, y = Truescore, color = k)) + geom_point()
results_optkcut %>% ggplot(aes(x = k, y = Truescore, color = Cut)) + geom_point()

max(results_optkcut$Truescore)
(knn_opt_k <- results_optkcut$k[which.max(results_optkcut$Truescore)])
(knn_opt_cut <- results_optkcut$Cut[which.max(results_optkcut$Truescore)])

min(results_optkcut$Distance)
results_optkcut$k[which.min(results_optkcut$Distance)]
results_optkcut$Cut[which.min(results_optkcut$Distance)]

# Fit an optimized knn model (k = 23) to the entire predictor matrix and outcome vector.

knn_fit_k23 <- knn3(x, y, k = 23)

# Apply our latest knn model (knn_fit_k23) and optimal cutoff of 0.31 to predict 
# outcomes based on the predictors in test_set2.

knn_y_hat_prob <- predict(knn_fit_k23, z, type = "prob")
knn_y_hat_prob_tbl <- as_tibble(knn_y_hat_prob)
knn_y_hat_prob_tbl <- knn_y_hat_prob_tbl %>% 
  mutate(preds = ifelse(knn_y_hat_prob_tbl$single > 0.31, "single", "field_out"))
knn_y_hat_prob_tbl$preds <- factor(knn_y_hat_prob_tbl$preds, levels = c("single", "field_out"))

# Assess the predictive ability of our latest knn model (knn_fit_k23).

confusionMatrix(data = knn_y_hat_prob_tbl$preds, reference = test_set2$events)
(knn_k23c31_tpr <- round(sensitivity(knn_y_hat_prob_tbl$preds, reference = factor(test_set2$events)), 6))
(knn_k23c31_tnr <- round(specificity(knn_y_hat_prob_tbl$preds, reference = factor(test_set2$events)), 6))
(knn_k23c31_fpr <- round(1 - knn_k23c31_tnr, 6))
(knn_k23c31_truescore <- round((2 * knn_k23c31_tpr * knn_k23c31_tnr) / (knn_k23c31_tpr + knn_k23c31_tnr) , 6))
(knn_k23c31_distance <- round(sqrt((1 - knn_k23c31_tpr)^2 + (knn_k23c31_fpr)^2), 6))

assessment_results <- tibble(Model = c("Baseline", "Logistic Regression", "Logistic Regression", 
                                       "KNN_k5", "KNN_k7", "KNN_k7c40", "KNN_k23c31"), 
                             `Cutoff Method` = c(NA, "Truescore", "ROC Distance to (0,1)", 
                                                 "default (0.50)", "default (0.50)", "both", "both"),
                             `Truescore` = c(bl_truescore, max_truescore, min_distance_truescore, 
                                             knn_k5_truescore, knn_k7_truescore, knn_k7c40_truescore, 
                                             knn_k23c31_truescore), 
                             TPR = c(bl_tpr, max_truescore_tpr, min_distance_tpr, 
                                     knn_k5_tpr, knn_k7_tpr, knn_k7c40_tpr, knn_k23c31_tpr),
                             TNR = c(bl_tnr, max_truescore_tnr, min_distance_tnr, 
                                     knn_k5_tnr, knn_k7_tnr, knn_k7c40_tnr, knn_k23c31_tnr), 
                             ROC_Distance = c(NA, max_truescore_distance, min_distance, 
                                              knn_k5_distance, knn_k7_distance, knn_k7c40_distance, 
                                              knn_k23c31_distance), 
                             FPR = c((1 - bl_tnr), max_truescore_fpr, min_distance_fpr, 
                                     knn_k5_fpr, knn_k7_fpr, knn_k7c40_fpr, knn_k23c31_fpr), 
                             `Best Cutoff` = c(NA, max_truescore_cutoff, min_distance_cutoff, 
                                               "default", "default", knn_k7c40_cutoff, knn_opt_cut))
knitr::kable(assessment_results[1:7, ], caption = "Assessment Results")

# ###############################################################
# # Manage Memory in R Before Proceeding
# ###############################################################
# 
# # Published in Jeromy Anglim's Blog: Psychology and Statistics, 
# # http://jeromyanglim.blogspot.com/2009/11/memory-management-in-r-few-tips-and.html
# 
# .ls.objects <- function (pos = 1, pattern, order.by = "Size", decreasing=TRUE, head = TRUE, n = 20) {
#   # based on postings by Petr Pikal and David Hinds to the r-help list in 2004
#   # modified by: Dirk Eddelbuettel (http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session) 
#   # I then gave it a few tweaks (show size as megabytes and use defaults that I like)
#   # a data frame of the objects and their associated storage needs.
#   napply <- function(names, fn) sapply(names, function(x)
#     fn(get(x, pos = pos)))
#   names <- ls(pos = pos, pattern = pattern)
#   obj.class <- napply(names, function(x) as.character(class(x))[1])
#   obj.mode <- napply(names, mode)
#   obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
#   obj.size <- napply(names, object.size) / 10^6 # megabytes
#   obj.dim <- t(napply(names, function(x)
#     as.numeric(dim(x))[1:2]))
#   vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
#   obj.dim[vec, 1] <- napply(names, length)[vec]
#   out <- data.frame(obj.type, obj.size, obj.dim)
#   names(out) <- c("Type", "Size", "Rows", "Columns")
#   out <- out[order(out[[order.by]], decreasing=decreasing), ]
#   if (head)
#     out <- head(out, n)
#   out
# }
# 
# .ls.objects()
# 
# rm(knn_train_pred_tib)
# rm(knn_train_pred_tib_f)
# rm(fk_dfs_l)
# rm(bb_outcome_by_launch_angle)
# rm(bb_outcome_by_launch_speed)
# rm(la_density)
# rm(ls_density)
# 
# gc(reset = TRUE)

###############################################################
# Build and Assess a Weighted K-Nearest Neighbors (kknn) Model
###############################################################

# Use caret::train() to develop a weighted knn algorithm with an optimal 
# combination of values for k, kernel, and the decision threshold. 
# The optimal combination of k, kernel, and the decision threshold should 
# maximize the truescore of the model.

train_set2 <- dplyr::select(train_set, -(hit_distance_sc))
test_set2 <- dplyr::select(test_set, -(hit_distance_sc))

x <- as.matrix(train_set2[, c("n_launch_angle", "n_launch_speed", "n_spray_angle_Kolp", "n_spray_angle_adj", 
                              "n_hp_to_1b", "n_if_alignment")])
y <- train_set2$events

# Use the kknn method with the train function and cross-validation to 
# compute the probabilities of "single" for a range of k's and kernels.

set.seed(1)
fitControl <- trainControl(method = "cv", number = 10, p = 0.8, returnData = TRUE,
                           returnResamp = "all", savePredictions = "all",
                           summaryFunction = twoClassSummary, classProbs = TRUE, 
                           verboseIter = TRUE)
tuneGrid <- expand.grid(kmax = seq(11, 39, 2), 
                        distance = 2, 
                        kernel = c("triangular", "gaussian", "optimal"))

kknn_train <- train(x, y, method = "kknn", 
                    tuneGrid = tuneGrid, trControl = fitControl)

# Generate predictions by applying a range of thresholds (cutoffs) to the 
# probabilities for each combination of k and kernel.

kknn_train_pred_tib <- as_tibble(kknn_train$pred)
kknn_train_pred_tib <- kknn_train_pred_tib %>% 
  mutate(pred25 = ifelse(kknn_train_pred_tib$single > 0.25, "single", "field_out")) 
kknn_train_pred_tib <- kknn_train_pred_tib %>% 
  mutate(pred26 = ifelse(kknn_train_pred_tib$single > 0.26, "single", "field_out"))
kknn_train_pred_tib <- kknn_train_pred_tib %>% 
  mutate(pred27 = ifelse(kknn_train_pred_tib$single > 0.27, "single", "field_out"))
kknn_train_pred_tib <- kknn_train_pred_tib %>% 
  mutate(pred28 = ifelse(kknn_train_pred_tib$single > 0.28, "single", "field_out"))
kknn_train_pred_tib <- kknn_train_pred_tib %>% 
  mutate(pred29 = ifelse(kknn_train_pred_tib$single > 0.29, "single", "field_out"))
kknn_train_pred_tib <- kknn_train_pred_tib %>% 
  mutate(pred30 = ifelse(kknn_train_pred_tib$single > 0.30, "single", "field_out"))
kknn_train_pred_tib <- kknn_train_pred_tib %>% 
  mutate(pred31 = ifelse(kknn_train_pred_tib$single > 0.31, "single", "field_out"))
kknn_train_pred_tib <- kknn_train_pred_tib %>% 
  mutate(pred32 = ifelse(kknn_train_pred_tib$single > 0.32, "single", "field_out"))
kknn_train_pred_tib <- kknn_train_pred_tib %>% 
  mutate(pred33 = ifelse(kknn_train_pred_tib$single > 0.33, "single", "field_out"))
kknn_train_pred_tib <- kknn_train_pred_tib %>% 
  mutate(pred34 = ifelse(kknn_train_pred_tib$single > 0.34, "single", "field_out"))
kknn_train_pred_tib <- kknn_train_pred_tib %>% 
  mutate(pred35 = ifelse(kknn_train_pred_tib$single > 0.35, "single", "field_out"))
kknn_train_pred_tib <- kknn_train_pred_tib %>% 
  mutate(pred36 = ifelse(kknn_train_pred_tib$single > 0.36, "single", "field_out"))
kknn_train_pred_tib <- kknn_train_pred_tib %>% 
  mutate(pred37 = ifelse(kknn_train_pred_tib$single > 0.37, "single", "field_out"))
kknn_train_pred_tib <- kknn_train_pred_tib %>% 
  mutate(pred38 = ifelse(kknn_train_pred_tib$single > 0.38, "single", "field_out"))
kknn_train_pred_tib <- kknn_train_pred_tib %>% 
  mutate(pred39 = ifelse(kknn_train_pred_tib$single > 0.39, "single", "field_out"))


# Convert all character columns to factors using dplyr package 
# (https://gist.github.com/ramhiser/character2factor.r).

kknn_train_pred_tib_f <- kknn_train_pred_tib %>% 
  mutate_if(sapply(kknn_train_pred_tib, is.character), as.factor)

rm(kknn_train_pred_tib)
gc(reset = TRUE)

sapply(kknn_train_pred_tib_f, class)

# Make sure all of the factor outcomes have the same levels in the same order.

kknn_train_pred_tib_f$pred25 <- factor(kknn_train_pred_tib_f$pred25, levels = c("single", "field_out"))
kknn_train_pred_tib_f$pred26 <- factor(kknn_train_pred_tib_f$pred26, levels = c("single", "field_out"))
kknn_train_pred_tib_f$pred27 <- factor(kknn_train_pred_tib_f$pred27, levels = c("single", "field_out"))
kknn_train_pred_tib_f$pred28 <- factor(kknn_train_pred_tib_f$pred28, levels = c("single", "field_out"))                                      
kknn_train_pred_tib_f$pred29 <- factor(kknn_train_pred_tib_f$pred29, levels = c("single", "field_out"))
kknn_train_pred_tib_f$pred30 <- factor(kknn_train_pred_tib_f$pred30, levels = c("single", "field_out"))   
kknn_train_pred_tib_f$pred31 <- factor(kknn_train_pred_tib_f$pred31, levels = c("single", "field_out"))                                      
kknn_train_pred_tib_f$pred32 <- factor(kknn_train_pred_tib_f$pred32, levels = c("single", "field_out"))
kknn_train_pred_tib_f$pred33 <- factor(kknn_train_pred_tib_f$pred33, levels = c("single", "field_out"))                                      
kknn_train_pred_tib_f$pred34 <- factor(kknn_train_pred_tib_f$pred34, levels = c("single", "field_out"))                                      
kknn_train_pred_tib_f$pred35 <- factor(kknn_train_pred_tib_f$pred35, levels = c("single", "field_out"))                                      
kknn_train_pred_tib_f$pred36 <- factor(kknn_train_pred_tib_f$pred36, levels = c("single", "field_out"))                                      
kknn_train_pred_tib_f$pred37 <- factor(kknn_train_pred_tib_f$pred37, levels = c("single", "field_out"))                                      
kknn_train_pred_tib_f$pred38 <- factor(kknn_train_pred_tib_f$pred38, levels = c("single", "field_out"))                                      
kknn_train_pred_tib_f$pred39 <- factor(kknn_train_pred_tib_f$pred39, levels = c("single", "field_out"))                                      
sapply(kknn_train_pred_tib_f, levels)

# Convert kknn_train_pred_tib_f$Resample into a numeric type.

kknn_train_pred_tib_f$Resample <- as.numeric(kknn_train_pred_tib_f$Resample)

# Add a column for a numeric version of kknn_train_pred_tib_f$wt_kernel.

kknn_train_pred_tib_f <- kknn_train_pred_tib_f %>% mutate(wt_kernel_n = as.numeric(kknn_train_pred_tib_f$kernel))

# Rename "kernel" column to "wt_kernel".

kknn_train_pred_tib_f <- rename(kknn_train_pred_tib_f, wt_kernel = kernel)

# Reorder columns in kknn_train_pred_tib_f.

kknn_train_pred_tib_f <- dplyr::select(kknn_train_pred_tib_f, pred, obs, 
                                       single, field_out, rowIndex, distance, 
                                       kmax, wt_kernel, wt_kernel_n, 
                                       everything())

# Form separate tibbles for each combination of k (kmax), kernel (wt_kernel_n), 
# and fold (Resample).

k11w1f1  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 1, Resample == 1)
k11w1f2  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 1, Resample == 2)
k11w1f3  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 1, Resample == 3)
k11w1f4  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 1, Resample == 4)
k11w1f5  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 1, Resample == 5)
k11w1f6  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 1, Resample == 6)
k11w1f7  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 1, Resample == 7)
k11w1f8  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 1, Resample == 8)
k11w1f9  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 1, Resample == 9)
k11w1f10  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 1, Resample == 10)

k11w2f1  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 2, Resample == 1)
k11w2f2  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 2, Resample == 2)
k11w2f3  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 2, Resample == 3)
k11w2f4  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 2, Resample == 4)
k11w2f5  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 2, Resample == 5)
k11w2f6  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 2, Resample == 6)
k11w2f7  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 2, Resample == 7)
k11w2f8  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 2, Resample == 8)
k11w2f9  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 2, Resample == 9)
k11w2f10  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 2, Resample == 10)

k11w3f1  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 3, Resample == 1)
k11w3f2  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 3, Resample == 2)
k11w3f3  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 3, Resample == 3)
k11w3f4  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 3, Resample == 4)
k11w3f5  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 3, Resample == 5)
k11w3f6  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 3, Resample == 6)
k11w3f7  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 3, Resample == 7)
k11w3f8  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 3, Resample == 8)
k11w3f9  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 3, Resample == 9)
k11w3f10  <- kknn_train_pred_tib_f %>% filter(kmax == 11, wt_kernel_n == 3, Resample == 10)


k13w1f1  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 1, Resample == 1)
k13w1f2  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 1, Resample == 2)
k13w1f3  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 1, Resample == 3)
k13w1f4  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 1, Resample == 4)
k13w1f5  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 1, Resample == 5)
k13w1f6  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 1, Resample == 6)
k13w1f7  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 1, Resample == 7)
k13w1f8  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 1, Resample == 8)
k13w1f9  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 1, Resample == 9)
k13w1f10  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 1, Resample == 10)

k13w2f1  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 2, Resample == 1)
k13w2f2  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 2, Resample == 2)
k13w2f3  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 2, Resample == 3)
k13w2f4  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 2, Resample == 4)
k13w2f5  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 2, Resample == 5)
k13w2f6  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 2, Resample == 6)
k13w2f7  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 2, Resample == 7)
k13w2f8  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 2, Resample == 8)
k13w2f9  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 2, Resample == 9)
k13w2f10  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 2, Resample == 10)

k13w3f1  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 3, Resample == 1)
k13w3f2  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 3, Resample == 2)
k13w3f3  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 3, Resample == 3)
k13w3f4  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 3, Resample == 4)
k13w3f5  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 3, Resample == 5)
k13w3f6  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 3, Resample == 6)
k13w3f7  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 3, Resample == 7)
k13w3f8  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 3, Resample == 8)
k13w3f9  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 3, Resample == 9)
k13w3f10  <- kknn_train_pred_tib_f %>% filter(kmax == 13, wt_kernel_n == 3, Resample == 10)


k15w1f1  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 1, Resample == 1)
k15w1f2  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 1, Resample == 2)
k15w1f3  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 1, Resample == 3)
k15w1f4  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 1, Resample == 4)
k15w1f5  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 1, Resample == 5)
k15w1f6  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 1, Resample == 6)
k15w1f7  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 1, Resample == 7)
k15w1f8  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 1, Resample == 8)
k15w1f9  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 1, Resample == 9)
k15w1f10  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 1, Resample == 10)

k15w2f1  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 2, Resample == 1)
k15w2f2  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 2, Resample == 2)
k15w2f3  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 2, Resample == 3)
k15w2f4  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 2, Resample == 4)
k15w2f5  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 2, Resample == 5)
k15w2f6  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 2, Resample == 6)
k15w2f7  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 2, Resample == 7)
k15w2f8  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 2, Resample == 8)
k15w2f9  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 2, Resample == 9)
k15w2f10  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 2, Resample == 10)

k15w3f1  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 3, Resample == 1)
k15w3f2  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 3, Resample == 2)
k15w3f3  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 3, Resample == 3)
k15w3f4  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 3, Resample == 4)
k15w3f5  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 3, Resample == 5)
k15w3f6  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 3, Resample == 6)
k15w3f7  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 3, Resample == 7)
k15w3f8  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 3, Resample == 8)
k15w3f9  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 3, Resample == 9)
k15w3f10  <- kknn_train_pred_tib_f %>% filter(kmax == 15, wt_kernel_n == 3, Resample == 10)


k17w1f1  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 1, Resample == 1)
k17w1f2  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 1, Resample == 2)
k17w1f3  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 1, Resample == 3)
k17w1f4  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 1, Resample == 4)
k17w1f5  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 1, Resample == 5)
k17w1f6  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 1, Resample == 6)
k17w1f7  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 1, Resample == 7)
k17w1f8  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 1, Resample == 8)
k17w1f9  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 1, Resample == 9)
k17w1f10  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 1, Resample == 10)

k17w2f1  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 2, Resample == 1)
k17w2f2  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 2, Resample == 2)
k17w2f3  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 2, Resample == 3)
k17w2f4  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 2, Resample == 4)
k17w2f5  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 2, Resample == 5)
k17w2f6  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 2, Resample == 6)
k17w2f7  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 2, Resample == 7)
k17w2f8  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 2, Resample == 8)
k17w2f9  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 2, Resample == 9)
k17w2f10  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 2, Resample == 10)

k17w3f1  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 3, Resample == 1)
k17w3f2  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 3, Resample == 2)
k17w3f3  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 3, Resample == 3)
k17w3f4  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 3, Resample == 4)
k17w3f5  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 3, Resample == 5)
k17w3f6  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 3, Resample == 6)
k17w3f7  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 3, Resample == 7)
k17w3f8  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 3, Resample == 8)
k17w3f9  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 3, Resample == 9)
k17w3f10  <- kknn_train_pred_tib_f %>% filter(kmax == 17, wt_kernel_n == 3, Resample == 10)


k19w1f1  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 1, Resample == 1)
k19w1f2  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 1, Resample == 2)
k19w1f3  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 1, Resample == 3)
k19w1f4  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 1, Resample == 4)
k19w1f5  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 1, Resample == 5)
k19w1f6  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 1, Resample == 6)
k19w1f7  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 1, Resample == 7)
k19w1f8  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 1, Resample == 8)
k19w1f9  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 1, Resample == 9)
k19w1f10  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 1, Resample == 10)

k19w2f1  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 2, Resample == 1)
k19w2f2  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 2, Resample == 2)
k19w2f3  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 2, Resample == 3)
k19w2f4  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 2, Resample == 4)
k19w2f5  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 2, Resample == 5)
k19w2f6  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 2, Resample == 6)
k19w2f7  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 2, Resample == 7)
k19w2f8  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 2, Resample == 8)
k19w2f9  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 2, Resample == 9)
k19w2f10  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 2, Resample == 10)

k19w3f1  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 3, Resample == 1)
k19w3f2  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 3, Resample == 2)
k19w3f3  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 3, Resample == 3)
k19w3f4  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 3, Resample == 4)
k19w3f5  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 3, Resample == 5)
k19w3f6  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 3, Resample == 6)
k19w3f7  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 3, Resample == 7)
k19w3f8  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 3, Resample == 8)
k19w3f9  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 3, Resample == 9)
k19w3f10  <- kknn_train_pred_tib_f %>% filter(kmax == 19, wt_kernel_n == 3, Resample == 10)


k21w1f1  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 1, Resample == 1)
k21w1f2  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 1, Resample == 2)
k21w1f3  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 1, Resample == 3)
k21w1f4  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 1, Resample == 4)
k21w1f5  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 1, Resample == 5)
k21w1f6  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 1, Resample == 6)
k21w1f7  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 1, Resample == 7)
k21w1f8  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 1, Resample == 8)
k21w1f9  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 1, Resample == 9)
k21w1f10  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 1, Resample == 10)

k21w2f1  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 2, Resample == 1)
k21w2f2  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 2, Resample == 2)
k21w2f3  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 2, Resample == 3)
k21w2f4  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 2, Resample == 4)
k21w2f5  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 2, Resample == 5)
k21w2f6  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 2, Resample == 6)
k21w2f7  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 2, Resample == 7)
k21w2f8  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 2, Resample == 8)
k21w2f9  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 2, Resample == 9)
k21w2f10  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 2, Resample == 10)

k21w3f1  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 3, Resample == 1)
k21w3f2  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 3, Resample == 2)
k21w3f3  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 3, Resample == 3)
k21w3f4  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 3, Resample == 4)
k21w3f5  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 3, Resample == 5)
k21w3f6  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 3, Resample == 6)
k21w3f7  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 3, Resample == 7)
k21w3f8  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 3, Resample == 8)
k21w3f9  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 3, Resample == 9)
k21w3f10  <- kknn_train_pred_tib_f %>% filter(kmax == 21, wt_kernel_n == 3, Resample == 10)


k23w1f1  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 1, Resample == 1)
k23w1f2  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 1, Resample == 2)
k23w1f3  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 1, Resample == 3)
k23w1f4  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 1, Resample == 4)
k23w1f5  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 1, Resample == 5)
k23w1f6  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 1, Resample == 6)
k23w1f7  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 1, Resample == 7)
k23w1f8  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 1, Resample == 8)
k23w1f9  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 1, Resample == 9)
k23w1f10  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 1, Resample == 10)

k23w2f1  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 2, Resample == 1)
k23w2f2  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 2, Resample == 2)
k23w2f3  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 2, Resample == 3)
k23w2f4  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 2, Resample == 4)
k23w2f5  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 2, Resample == 5)
k23w2f6  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 2, Resample == 6)
k23w2f7  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 2, Resample == 7)
k23w2f8  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 2, Resample == 8)
k23w2f9  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 2, Resample == 9)
k23w2f10  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 2, Resample == 10)

k23w3f1  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 3, Resample == 1)
k23w3f2  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 3, Resample == 2)
k23w3f3  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 3, Resample == 3)
k23w3f4  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 3, Resample == 4)
k23w3f5  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 3, Resample == 5)
k23w3f6  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 3, Resample == 6)
k23w3f7  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 3, Resample == 7)
k23w3f8  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 3, Resample == 8)
k23w3f9  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 3, Resample == 9)
k23w3f10  <- kknn_train_pred_tib_f %>% filter(kmax == 23, wt_kernel_n == 3, Resample == 10)


k25w1f1  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 1, Resample == 1)
k25w1f2  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 1, Resample == 2)
k25w1f3  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 1, Resample == 3)
k25w1f4  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 1, Resample == 4)
k25w1f5  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 1, Resample == 5)
k25w1f6  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 1, Resample == 6)
k25w1f7  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 1, Resample == 7)
k25w1f8  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 1, Resample == 8)
k25w1f9  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 1, Resample == 9)
k25w1f10  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 1, Resample == 10)

k25w2f1  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 2, Resample == 1)
k25w2f2  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 2, Resample == 2)
k25w2f3  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 2, Resample == 3)
k25w2f4  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 2, Resample == 4)
k25w2f5  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 2, Resample == 5)
k25w2f6  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 2, Resample == 6)
k25w2f7  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 2, Resample == 7)
k25w2f8  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 2, Resample == 8)
k25w2f9  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 2, Resample == 9)
k25w2f10  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 2, Resample == 10)

k25w3f1  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 3, Resample == 1)
k25w3f2  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 3, Resample == 2)
k25w3f3  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 3, Resample == 3)
k25w3f4  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 3, Resample == 4)
k25w3f5  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 3, Resample == 5)
k25w3f6  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 3, Resample == 6)
k25w3f7  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 3, Resample == 7)
k25w3f8  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 3, Resample == 8)
k25w3f9  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 3, Resample == 9)
k25w3f10  <- kknn_train_pred_tib_f %>% filter(kmax == 25, wt_kernel_n == 3, Resample == 10)


k27w1f1  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 1, Resample == 1)
k27w1f2  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 1, Resample == 2)
k27w1f3  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 1, Resample == 3)
k27w1f4  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 1, Resample == 4)
k27w1f5  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 1, Resample == 5)
k27w1f6  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 1, Resample == 6)
k27w1f7  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 1, Resample == 7)
k27w1f8  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 1, Resample == 8)
k27w1f9  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 1, Resample == 9)
k27w1f10  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 1, Resample == 10)

k27w2f1  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 2, Resample == 1)
k27w2f2  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 2, Resample == 2)
k27w2f3  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 2, Resample == 3)
k27w2f4  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 2, Resample == 4)
k27w2f5  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 2, Resample == 5)
k27w2f6  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 2, Resample == 6)
k27w2f7  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 2, Resample == 7)
k27w2f8  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 2, Resample == 8)
k27w2f9  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 2, Resample == 9)
k27w2f10  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 2, Resample == 10)

k27w3f1  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 3, Resample == 1)
k27w3f2  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 3, Resample == 2)
k27w3f3  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 3, Resample == 3)
k27w3f4  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 3, Resample == 4)
k27w3f5  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 3, Resample == 5)
k27w3f6  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 3, Resample == 6)
k27w3f7  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 3, Resample == 7)
k27w3f8  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 3, Resample == 8)
k27w3f9  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 3, Resample == 9)
k27w3f10  <- kknn_train_pred_tib_f %>% filter(kmax == 27, wt_kernel_n == 3, Resample == 10)


k29w1f1  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 1, Resample == 1)
k29w1f2  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 1, Resample == 2)
k29w1f3  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 1, Resample == 3)
k29w1f4  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 1, Resample == 4)
k29w1f5  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 1, Resample == 5)
k29w1f6  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 1, Resample == 6)
k29w1f7  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 1, Resample == 7)
k29w1f8  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 1, Resample == 8)
k29w1f9  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 1, Resample == 9)
k29w1f10  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 1, Resample == 10)

k29w2f1  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 2, Resample == 1)
k29w2f2  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 2, Resample == 2)
k29w2f3  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 2, Resample == 3)
k29w2f4  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 2, Resample == 4)
k29w2f5  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 2, Resample == 5)
k29w2f6  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 2, Resample == 6)
k29w2f7  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 2, Resample == 7)
k29w2f8  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 2, Resample == 8)
k29w2f9  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 2, Resample == 9)
k29w2f10  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 2, Resample == 10)

k29w3f1  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 3, Resample == 1)
k29w3f2  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 3, Resample == 2)
k29w3f3  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 3, Resample == 3)
k29w3f4  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 3, Resample == 4)
k29w3f5  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 3, Resample == 5)
k29w3f6  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 3, Resample == 6)
k29w3f7  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 3, Resample == 7)
k29w3f8  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 3, Resample == 8)
k29w3f9  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 3, Resample == 9)
k29w3f10  <- kknn_train_pred_tib_f %>% filter(kmax == 29, wt_kernel_n == 3, Resample == 10)


k31w1f1  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 1, Resample == 1)
k31w1f2  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 1, Resample == 2)
k31w1f3  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 1, Resample == 3)
k31w1f4  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 1, Resample == 4)
k31w1f5  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 1, Resample == 5)
k31w1f6  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 1, Resample == 6)
k31w1f7  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 1, Resample == 7)
k31w1f8  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 1, Resample == 8)
k31w1f9  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 1, Resample == 9)
k31w1f10  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 1, Resample == 10)

k31w2f1  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 2, Resample == 1)
k31w2f2  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 2, Resample == 2)
k31w2f3  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 2, Resample == 3)
k31w2f4  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 2, Resample == 4)
k31w2f5  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 2, Resample == 5)
k31w2f6  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 2, Resample == 6)
k31w2f7  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 2, Resample == 7)
k31w2f8  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 2, Resample == 8)
k31w2f9  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 2, Resample == 9)
k31w2f10  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 2, Resample == 10)

k31w3f1  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 3, Resample == 1)
k31w3f2  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 3, Resample == 2)
k31w3f3  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 3, Resample == 3)
k31w3f4  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 3, Resample == 4)
k31w3f5  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 3, Resample == 5)
k31w3f6  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 3, Resample == 6)
k31w3f7  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 3, Resample == 7)
k31w3f8  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 3, Resample == 8)
k31w3f9  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 3, Resample == 9)
k31w3f10  <- kknn_train_pred_tib_f %>% filter(kmax == 31, wt_kernel_n == 3, Resample == 10)


k33w1f1  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 1, Resample == 1)
k33w1f2  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 1, Resample == 2)
k33w1f3  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 1, Resample == 3)
k33w1f4  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 1, Resample == 4)
k33w1f5  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 1, Resample == 5)
k33w1f6  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 1, Resample == 6)
k33w1f7  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 1, Resample == 7)
k33w1f8  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 1, Resample == 8)
k33w1f9  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 1, Resample == 9)
k33w1f10  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 1, Resample == 10)

k33w2f1  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 2, Resample == 1)
k33w2f2  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 2, Resample == 2)
k33w2f3  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 2, Resample == 3)
k33w2f4  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 2, Resample == 4)
k33w2f5  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 2, Resample == 5)
k33w2f6  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 2, Resample == 6)
k33w2f7  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 2, Resample == 7)
k33w2f8  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 2, Resample == 8)
k33w2f9  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 2, Resample == 9)
k33w2f10  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 2, Resample == 10)

k33w3f1  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 3, Resample == 1)
k33w3f2  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 3, Resample == 2)
k33w3f3  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 3, Resample == 3)
k33w3f4  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 3, Resample == 4)
k33w3f5  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 3, Resample == 5)
k33w3f6  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 3, Resample == 6)
k33w3f7  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 3, Resample == 7)
k33w3f8  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 3, Resample == 8)
k33w3f9  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 3, Resample == 9)
k33w3f10  <- kknn_train_pred_tib_f %>% filter(kmax == 33, wt_kernel_n == 3, Resample == 10)


k35w1f1  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 1, Resample == 1)
k35w1f2  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 1, Resample == 2)
k35w1f3  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 1, Resample == 3)
k35w1f4  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 1, Resample == 4)
k35w1f5  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 1, Resample == 5)
k35w1f6  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 1, Resample == 6)
k35w1f7  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 1, Resample == 7)
k35w1f8  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 1, Resample == 8)
k35w1f9  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 1, Resample == 9)
k35w1f10  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 1, Resample == 10)

k35w2f1  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 2, Resample == 1)
k35w2f2  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 2, Resample == 2)
k35w2f3  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 2, Resample == 3)
k35w2f4  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 2, Resample == 4)
k35w2f5  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 2, Resample == 5)
k35w2f6  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 2, Resample == 6)
k35w2f7  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 2, Resample == 7)
k35w2f8  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 2, Resample == 8)
k35w2f9  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 2, Resample == 9)
k35w2f10  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 2, Resample == 10)

k35w3f1  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 3, Resample == 1)
k35w3f2  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 3, Resample == 2)
k35w3f3  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 3, Resample == 3)
k35w3f4  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 3, Resample == 4)
k35w3f5  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 3, Resample == 5)
k35w3f6  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 3, Resample == 6)
k35w3f7  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 3, Resample == 7)
k35w3f8  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 3, Resample == 8)
k35w3f9  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 3, Resample == 9)
k35w3f10  <- kknn_train_pred_tib_f %>% filter(kmax == 35, wt_kernel_n == 3, Resample == 10)


k37w1f1  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 1, Resample == 1)
k37w1f2  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 1, Resample == 2)
k37w1f3  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 1, Resample == 3)
k37w1f4  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 1, Resample == 4)
k37w1f5  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 1, Resample == 5)
k37w1f6  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 1, Resample == 6)
k37w1f7  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 1, Resample == 7)
k37w1f8  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 1, Resample == 8)
k37w1f9  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 1, Resample == 9)
k37w1f10  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 1, Resample == 10)

k37w2f1  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 2, Resample == 1)
k37w2f2  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 2, Resample == 2)
k37w2f3  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 2, Resample == 3)
k37w2f4  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 2, Resample == 4)
k37w2f5  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 2, Resample == 5)
k37w2f6  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 2, Resample == 6)
k37w2f7  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 2, Resample == 7)
k37w2f8  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 2, Resample == 8)
k37w2f9  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 2, Resample == 9)
k37w2f10  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 2, Resample == 10)

k37w3f1  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 3, Resample == 1)
k37w3f2  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 3, Resample == 2)
k37w3f3  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 3, Resample == 3)
k37w3f4  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 3, Resample == 4)
k37w3f5  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 3, Resample == 5)
k37w3f6  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 3, Resample == 6)
k37w3f7  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 3, Resample == 7)
k37w3f8  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 3, Resample == 8)
k37w3f9  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 3, Resample == 9)
k37w3f10  <- kknn_train_pred_tib_f %>% filter(kmax == 37, wt_kernel_n == 3, Resample == 10)


k39w1f1  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 1, Resample == 1)
k39w1f2  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 1, Resample == 2)
k39w1f3  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 1, Resample == 3)
k39w1f4  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 1, Resample == 4)
k39w1f5  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 1, Resample == 5)
k39w1f6  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 1, Resample == 6)
k39w1f7  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 1, Resample == 7)
k39w1f8  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 1, Resample == 8)
k39w1f9  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 1, Resample == 9)
k39w1f10  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 1, Resample == 10)

k39w2f1  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 2, Resample == 1)
k39w2f2  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 2, Resample == 2)
k39w2f3  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 2, Resample == 3)
k39w2f4  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 2, Resample == 4)
k39w2f5  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 2, Resample == 5)
k39w2f6  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 2, Resample == 6)
k39w2f7  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 2, Resample == 7)
k39w2f8  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 2, Resample == 8)
k39w2f9  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 2, Resample == 9)
k39w2f10  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 2, Resample == 10)

k39w3f1  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 3, Resample == 1)
k39w3f2  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 3, Resample == 2)
k39w3f3  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 3, Resample == 3)
k39w3f4  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 3, Resample == 4)
k39w3f5  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 3, Resample == 5)
k39w3f6  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 3, Resample == 6)
k39w3f7  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 3, Resample == 7)
k39w3f8  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 3, Resample == 8)
k39w3f9  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 3, Resample == 9)
k39w3f10  <- kknn_train_pred_tib_f %>% filter(kmax == 39, wt_kernel_n == 3, Resample == 10)

rm(kknn_train_pred_tib_f)
gc(reset = TRUE)


# Form a list and vector of these tibbles.

kwf_dfs_l <- list(k11w1f1, k11w1f2, k11w1f3, k11w1f4, k11w1f5, 
                  k11w1f6, k11w1f7, k11w1f8, k11w1f9, k11w1f10, 
                  k11w2f1, k11w2f2, k11w2f3, k11w2f4, k11w2f5, 
                  k11w2f6, k11w2f7, k11w2f8, k11w2f9, k11w2f10, 
                  k11w3f1, k11w3f2, k11w3f3, k11w3f4, k11w3f5, 
                  k11w3f6, k11w3f7, k11w3f8, k11w3f9, k11w3f10, 
                  k13w1f1, k13w1f2, k13w1f3, k13w1f4, k13w1f5, 
                  k13w1f6, k13w1f7, k13w1f8, k13w1f9, k13w1f10, 
                  k13w2f1, k13w2f2, k13w2f3, k13w2f4, k13w2f5, 
                  k13w2f6, k13w2f7, k13w2f8, k13w2f9, k13w2f10, 
                  k13w3f1, k13w3f2, k13w3f3, k13w3f4, k13w3f5, 
                  k13w3f6, k13w3f7, k13w3f8, k13w3f9, k13w3f10, 
                  k15w1f1, k15w1f2, k15w1f3, k15w1f4, k15w1f5, 
                  k15w1f6, k15w1f7, k15w1f8, k15w1f9, k15w1f10, 
                  k15w2f1, k15w2f2, k15w2f3, k15w2f4, k15w2f5, 
                  k15w2f6, k15w2f7, k15w2f8, k15w2f9, k15w2f10, 
                  k15w3f1, k15w3f2, k15w3f3, k15w3f4, k15w3f5, 
                  k15w3f6, k15w3f7, k15w3f8, k15w3f9, k15w3f10, 
                  k17w1f1, k17w1f2, k17w1f3, k17w1f4, k17w1f5, 
                  k17w1f6, k17w1f7, k17w1f8, k17w1f9, k17w1f10, 
                  k17w2f1, k17w2f2, k17w2f3, k17w2f4, k17w2f5, 
                  k17w2f6, k17w2f7, k17w2f8, k17w2f9, k17w2f10, 
                  k17w3f1, k17w3f2, k17w3f3, k17w3f4, k17w3f5, 
                  k17w3f6, k17w3f7, k17w3f8, k17w3f9, k17w3f10, 
                  k19w1f1, k19w1f2, k19w1f3, k19w1f4, k19w1f5, 
                  k19w1f6, k19w1f7, k19w1f8, k19w1f9, k19w1f10, 
                  k19w2f1, k19w2f2, k19w2f3, k19w2f4, k19w2f5, 
                  k19w2f6, k19w2f7, k19w2f8, k19w2f9, k19w2f10, 
                  k19w3f1, k19w3f2, k19w3f3, k19w3f4, k19w3f5, 
                  k19w3f6, k19w3f7, k19w3f8, k19w3f9, k19w3f10, 
                  k21w1f1, k21w1f2, k21w1f3, k21w1f4, k21w1f5, 
                  k21w1f6, k21w1f7, k21w1f8, k21w1f9, k21w1f10, 
                  k21w2f1, k21w2f2, k21w2f3, k21w2f4, k21w2f5, 
                  k21w2f6, k21w2f7, k21w2f8, k21w2f9, k21w2f10, 
                  k21w3f1, k21w3f2, k21w3f3, k21w3f4, k21w3f5, 
                  k21w3f6, k21w3f7, k21w3f8, k21w3f9, k21w3f10, 
                  k23w1f1, k23w1f2, k23w1f3, k23w1f4, k23w1f5, 
                  k23w1f6, k23w1f7, k23w1f8, k23w1f9, k23w1f10, 
                  k23w2f1, k23w2f2, k23w2f3, k23w2f4, k23w2f5, 
                  k23w2f6, k23w2f7, k23w2f8, k23w2f9, k23w2f10, 
                  k23w3f1, k23w3f2, k23w3f3, k23w3f4, k23w3f5, 
                  k23w3f6, k23w3f7, k23w3f8, k23w3f9, k23w3f10, 
                  k25w1f1, k25w1f2, k25w1f3, k25w1f4, k25w1f5, 
                  k25w1f6, k25w1f7, k25w1f8, k25w1f9, k25w1f10, 
                  k25w2f1, k25w2f2, k25w2f3, k25w2f4, k25w2f5, 
                  k25w2f6, k25w2f7, k25w2f8, k25w2f9, k25w2f10, 
                  k25w3f1, k25w3f2, k25w3f3, k25w3f4, k25w3f5, 
                  k25w3f6, k25w3f7, k25w3f8, k25w3f9, k25w3f10, 
                  k27w1f1, k27w1f2, k27w1f3, k27w1f4, k27w1f5, 
                  k27w1f6, k27w1f7, k27w1f8, k27w1f9, k27w1f10, 
                  k27w2f1, k27w2f2, k27w2f3, k27w2f4, k27w2f5, 
                  k27w2f6, k27w2f7, k27w2f8, k27w2f9, k27w2f10, 
                  k27w3f1, k27w3f2, k27w3f3, k27w3f4, k27w3f5, 
                  k27w3f6, k27w3f7, k27w3f8, k27w3f9, k27w3f10, 
                  k29w1f1, k29w1f2, k29w1f3, k29w1f4, k29w1f5, 
                  k29w1f6, k29w1f7, k29w1f8, k29w1f9, k29w1f10, 
                  k29w2f1, k29w2f2, k29w2f3, k29w2f4, k29w2f5, 
                  k29w2f6, k29w2f7, k29w2f8, k29w2f9, k29w2f10, 
                  k29w3f1, k29w3f2, k29w3f3, k29w3f4, k29w3f5, 
                  k29w3f6, k29w3f7, k29w3f8, k29w3f9, k29w3f10, 
                  k31w1f1, k31w1f2, k31w1f3, k31w1f4, k31w1f5, 
                  k31w1f6, k31w1f7, k31w1f8, k31w1f9, k31w1f10, 
                  k31w2f1, k31w2f2, k31w2f3, k31w2f4, k31w2f5, 
                  k31w2f6, k31w2f7, k31w2f8, k31w2f9, k31w2f10, 
                  k31w3f1, k31w3f2, k31w3f3, k31w3f4, k31w3f5, 
                  k31w3f6, k31w3f7, k31w3f8, k31w3f9, k31w3f10, 
                  k33w1f1, k33w1f2, k33w1f3, k33w1f4, k33w1f5, 
                  k33w1f6, k33w1f7, k33w1f8, k33w1f9, k33w1f10, 
                  k33w2f1, k33w2f2, k33w2f3, k33w2f4, k33w2f5, 
                  k33w2f6, k33w2f7, k33w2f8, k33w2f9, k33w2f10, 
                  k33w3f1, k33w3f2, k33w3f3, k33w3f4, k33w3f5, 
                  k33w3f6, k33w3f7, k33w3f8, k33w3f9, k33w3f10, 
                  k35w1f1, k35w1f2, k35w1f3, k35w1f4, k35w1f5, 
                  k35w1f6, k35w1f7, k35w1f8, k35w1f9, k35w1f10, 
                  k35w2f1, k35w2f2, k35w2f3, k35w2f4, k35w2f5, 
                  k35w2f6, k35w2f7, k35w2f8, k35w2f9, k35w2f10, 
                  k35w3f1, k35w3f2, k35w3f3, k35w3f4, k35w3f5, 
                  k35w3f6, k35w3f7, k35w3f8, k35w3f9, k35w3f10, 
                  k37w1f1, k37w1f2, k37w1f3, k37w1f4, k37w1f5, 
                  k37w1f6, k37w1f7, k37w1f8, k37w1f9, k37w1f10, 
                  k37w2f1, k37w2f2, k37w2f3, k37w2f4, k37w2f5, 
                  k37w2f6, k37w2f7, k37w2f8, k37w2f9, k37w2f10, 
                  k37w3f1, k37w3f2, k37w3f3, k37w3f4, k37w3f5, 
                  k37w3f6, k37w3f7, k37w3f8, k37w3f9, k37w3f10, 
                  k39w1f1, k39w1f2, k39w1f3, k39w1f4, k39w1f5, 
                  k39w1f6, k39w1f7, k39w1f8, k39w1f9, k39w1f10, 
                  k39w2f1, k39w2f2, k39w2f3, k39w2f4, k39w2f5, 
                  k39w2f6, k39w2f7, k39w2f8, k39w2f9, k39w2f10, 
                  k39w3f1, k39w3f2, k39w3f3, k39w3f4, k39w3f5, 
                  k39w3f6, k39w3f7, k39w3f8, k39w3f9, k39w3f10)

kwf_dfs_v <- c("k11w1f1", "k11w1f2", "k11w1f3", "k11w1f4", "k11w1f5", 
               "k11w1f6", "k11w1f7", "k11w1f8", "k11w1f9", "k11w1f10", 
               "k11w2f1", "k11w2f2", "k11w2f3", "k11w2f4", "k11w2f5", 
               "k11w2f6", "k11w2f7", "k11w2f8", "k11w2f9", "k11w2f10", 
               "k11w3f1", "k11w3f2", "k11w3f3", "k11w3f4", "k11w3f5", 
               "k11w3f6", "k11w3f7", "k11w3f8", "k11w3f9", "k11w3f10", 
               "k13w1f1", "k13w1f2", "k13w1f3", "k13w1f4", "k13w1f5", 
               "k13w1f6", "k13w1f7", "k13w1f8", "k13w1f9", "k13w1f10", 
               "k13w2f1", "k13w2f2", "k13w2f3", "k13w2f4", "k13w2f5", 
               "k13w2f6", "k13w2f7", "k13w2f8", "k13w2f9", "k13w2f10", 
               "k13w3f1", "k13w3f2", "k13w3f3", "k13w3f4", "k13w3f5", 
               "k13w3f6", "k13w3f7", "k13w3f8", "k13w3f9", "k13w3f10", 
               "k15w1f1", "k15w1f2", "k15w1f3", "k15w1f4", "k15w1f5", 
               "k15w1f6", "k15w1f7", "k15w1f8", "k15w1f9", "k15w1f10", 
               "k15w2f1", "k15w2f2", "k15w2f3", "k15w2f4", "k15w2f5", 
               "k15w2f6", "k15w2f7", "k15w2f8", "k15w2f9", "k15w2f10", 
               "k15w3f1", "k15w3f2", "k15w3f3", "k15w3f4", "k15w3f5", 
               "k15w3f6", "k15w3f7", "k15w3f8", "k15w3f9", "k15w3f10", 
               "k17w1f1", "k17w1f2", "k17w1f3", "k17w1f4", "k17w1f5", 
               "k17w1f6", "k17w1f7", "k17w1f8", "k17w1f9", "k17w1f10", 
               "k17w2f1", "k17w2f2", "k17w2f3", "k17w2f4", "k17w2f5", 
               "k17w2f6", "k17w2f7", "k17w2f8", "k17w2f9", "k17w2f10", 
               "k17w3f1", "k17w3f2", "k17w3f3", "k17w3f4", "k17w3f5", 
               "k17w3f6", "k17w3f7", "k17w3f8", "k17w3f9", "k17w3f10", 
               "k19w1f1", "k19w1f2", "k19w1f3", "k19w1f4", "k19w1f5", 
               "k19w1f6", "k19w1f7", "k19w1f8", "k19w1f9", "k19w1f10", 
               "k19w2f1", "k19w2f2", "k19w2f3", "k19w2f4", "k19w2f5", 
               "k19w2f6", "k19w2f7", "k19w2f8", "k19w2f9", "k19w2f10", 
               "k19w3f1", "k19w3f2", "k19w3f3", "k19w3f4", "k19w3f5", 
               "k19w3f6", "k19w3f7", "k19w3f8", "k19w3f9", "k19w3f10", 
               "k21w1f1", "k21w1f2", "k21w1f3", "k21w1f4", "k21w1f5", 
               "k21w1f6", "k21w1f7", "k21w1f8", "k21w1f9", "k21w1f10", 
               "k21w2f1", "k21w2f2", "k21w2f3", "k21w2f4", "k21w2f5", 
               "k21w2f6", "k21w2f7", "k21w2f8", "k21w2f9", "k21w2f10", 
               "k21w3f1", "k21w3f2", "k21w3f3", "k21w3f4", "k21w3f5", 
               "k21w3f6", "k21w3f7", "k21w3f8", "k21w3f9", "k21w3f10", 
               "k23w1f1", "k23w1f2", "k23w1f3", "k23w1f4", "k23w1f5", 
               "k23w1f6", "k23w1f7", "k23w1f8", "k23w1f9", "k23w1f10", 
               "k23w2f1", "k23w2f2", "k23w2f3", "k23w2f4", "k23w2f5", 
               "k23w2f6", "k23w2f7", "k23w2f8", "k23w2f9", "k23w2f10", 
               "k23w3f1", "k23w3f2", "k23w3f3", "k23w3f4", "k23w3f5", 
               "k23w3f6", "k23w3f7", "k23w3f8", "k23w3f9", "k23w3f10", 
               "k25w1f1", "k25w1f2", "k25w1f3", "k25w1f4", "k25w1f5", 
               "k25w1f6", "k25w1f7", "k25w1f8", "k25w1f9", "k25w1f10", 
               "k25w2f1", "k25w2f2", "k25w2f3", "k25w2f4", "k25w2f5", 
               "k25w2f6", "k25w2f7", "k25w2f8", "k25w2f9", "k25w2f10", 
               "k25w3f1", "k25w3f2", "k25w3f3", "k25w3f4", "k25w3f5", 
               "k25w3f6", "k25w3f7", "k25w3f8", "k25w3f9", "k25w3f10", 
               "k27w1f1", "k27w1f2", "k27w1f3", "k27w1f4", "k27w1f5", 
               "k27w1f6", "k27w1f7", "k27w1f8", "k27w1f9", "k27w1f10", 
               "k27w2f1", "k27w2f2", "k27w2f3", "k27w2f4", "k27w2f5", 
               "k27w2f6", "k27w2f7", "k27w2f8", "k27w2f9", "k27w2f10", 
               "k27w3f1", "k27w3f2", "k27w3f3", "k27w3f4", "k27w3f5", 
               "k27w3f6", "k27w3f7", "k27w3f8", "k27w3f9", "k27w3f10", 
               "k29w1f1", "k29w1f2", "k29w1f3", "k29w1f4", "k29w1f5", 
               "k29w1f6", "k29w1f7", "k29w1f8", "k29w1f9", "k29w1f10", 
               "k29w2f1", "k29w2f2", "k29w2f3", "k29w2f4", "k29w2f5", 
               "k29w2f6", "k29w2f7", "k29w2f8", "k29w2f9", "k29w2f10", 
               "k29w3f1", "k29w3f2", "k29w3f3", "k29w3f4", "k29w3f5", 
               "k29w3f6", "k29w3f7", "k29w3f8", "k29w3f9", "k29w3f10", 
               "k31w1f1", "k31w1f2", "k31w1f3", "k31w1f4", "k31w1f5", 
               "k31w1f6", "k31w1f7", "k31w1f8", "k31w1f9", "k31w1f10", 
               "k31w2f1", "k31w2f2", "k31w2f3", "k31w2f4", "k31w2f5", 
               "k31w2f6", "k31w2f7", "k31w2f8", "k31w2f9", "k31w2f10", 
               "k31w3f1", "k31w3f2", "k31w3f3", "k31w3f4", "k31w3f5", 
               "k31w3f6", "k31w3f7", "k31w3f8", "k31w3f9", "k31w3f10", 
               "k33w1f1", "k33w1f2", "k33w1f3", "k33w1f4", "k33w1f5", 
               "k33w1f6", "k33w1f7", "k33w1f8", "k33w1f9", "k33w1f10", 
               "k33w2f1", "k33w2f2", "k33w2f3", "k33w2f4", "k33w2f5", 
               "k33w2f6", "k33w2f7", "k33w2f8", "k33w2f9", "k33w2f10", 
               "k33w3f1", "k33w3f2", "k33w3f3", "k33w3f4", "k33w3f5", 
               "k33w3f6", "k33w3f7", "k33w3f8", "k33w3f9", "k33w3f10", 
               "k35w1f1", "k35w1f2", "k35w1f3", "k35w1f4", "k35w1f5", 
               "k35w1f6", "k35w1f7", "k35w1f8", "k35w1f9", "k35w1f10", 
               "k35w2f1", "k35w2f2", "k35w2f3", "k35w2f4", "k35w2f5", 
               "k35w2f6", "k35w2f7", "k35w2f8", "k35w2f9", "k35w2f10", 
               "k35w3f1", "k35w3f2", "k35w3f3", "k35w3f4", "k35w3f5", 
               "k35w3f6", "k35w3f7", "k35w3f8", "k35w3f9", "k35w3f10", 
               "k37w1f1", "k37w1f2", "k37w1f3", "k37w1f4", "k37w1f5", 
               "k37w1f6", "k37w1f7", "k37w1f8", "k37w1f9", "k37w1f10", 
               "k37w2f1", "k37w2f2", "k37w2f3", "k37w2f4", "k37w2f5", 
               "k37w2f6", "k37w2f7", "k37w2f8", "k37w2f9", "k37w2f10", 
               "k37w3f1", "k37w3f2", "k37w3f3", "k37w3f4", "k37w3f5", 
               "k37w3f6", "k37w3f7", "k37w3f8", "k37w3f9", "k37w3f10", 
               "k39w1f1", "k39w1f2", "k39w1f3", "k39w1f4", "k39w1f5", 
               "k39w1f6", "k39w1f7", "k39w1f8", "k39w1f9", "k39w1f10", 
               "k39w2f1", "k39w2f2", "k39w2f3", "k39w2f4", "k39w2f5", 
               "k39w2f6", "k39w2f7", "k39w2f8", "k39w2f9", "k39w2f10", 
               "k39w3f1", "k39w3f2", "k39w3f3", "k39w3f4", "k39w3f5", 
               "k39w3f6", "k39w3f7", "k39w3f8", "k39w3f9", "k39w3f10")


############################
# 0.25 Cutoff
############################

# For the decision cutoff of 0.25, generate a confusion matrix for
# every combination of k (11:39 by two), kernel (1:3) and fold (1:10).

cm_c25 <- sapply(kwf_dfs_l, function(x) {
  ss <- subset(x, select = c(pred25, obs))
  confusionMatrix(ss$pred25, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c25) <- kwf_dfs_v

cm_c25_tables <- sapply(cm_c25, "[[", 2)
cm_c25_tables <- as_tibble(cm_c25_tables)

# For each combination of k and kernel, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each combination of k and kernel when the decision cutoff is 0.25.

(k11w1c25_tpr <- round(sum(cm_c25_tables[1, 1:10]) / (sum(cm_c25_tables[1, 1:10]) + sum(cm_c25_tables[2, 1:10])), 6))
(k11w1c25_tnr <- round(sum(cm_c25_tables[4, 1:10]) / (sum(cm_c25_tables[4, 1:10]) + sum(cm_c25_tables[3, 1:10])), 6))
(k11w1c25_truescore <- round((2 * k11w1c25_tpr * k11w1c25_tnr) / (k11w1c25_tpr + k11w1c25_tnr), 6))

(k11w2c25_tpr <- round(sum(cm_c25_tables[1, 11:20]) / (sum(cm_c25_tables[1, 11:20]) + sum(cm_c25_tables[2, 11:20])), 6))
(k11w2c25_tnr <- round(sum(cm_c25_tables[4, 11:20]) / (sum(cm_c25_tables[4, 11:20]) + sum(cm_c25_tables[3, 11:20])), 6))
(k11w2c25_truescore <- round((2 * k11w2c25_tpr * k11w2c25_tnr) / (k11w2c25_tpr + k11w2c25_tnr), 6))

(k11w3c25_tpr <- round(sum(cm_c25_tables[1, 21:30]) / (sum(cm_c25_tables[1, 21:30]) + sum(cm_c25_tables[2, 21:30])), 6))
(k11w3c25_tnr <- round(sum(cm_c25_tables[4, 21:30]) / (sum(cm_c25_tables[4, 21:30]) + sum(cm_c25_tables[3, 21:30])), 6))
(k11w3c25_truescore <- round((2 * k11w3c25_tpr * k11w3c25_tnr) / (k11w3c25_tpr + k11w3c25_tnr), 6))


(k13w1c25_tpr <- round(sum(cm_c25_tables[1, 31:40]) / (sum(cm_c25_tables[1, 31:40]) + sum(cm_c25_tables[2, 31:40])), 6))
(k13w1c25_tnr <- round(sum(cm_c25_tables[4, 31:40]) / (sum(cm_c25_tables[4, 31:40]) + sum(cm_c25_tables[3, 31:40])), 6))
(k13w1c25_truescore <- round((2 * k13w1c25_tpr * k13w1c25_tnr) / (k13w1c25_tpr + k13w1c25_tnr), 6))

(k13w2c25_tpr <- round(sum(cm_c25_tables[1, 41:50]) / (sum(cm_c25_tables[1, 41:50]) + sum(cm_c25_tables[2, 41:50])), 6))
(k13w2c25_tnr <- round(sum(cm_c25_tables[4, 41:50]) / (sum(cm_c25_tables[4, 41:50]) + sum(cm_c25_tables[3, 41:50])), 6))
(k13w2c25_truescore <- round((2 * k13w2c25_tpr * k13w2c25_tnr) / (k13w2c25_tpr + k13w2c25_tnr), 6))

(k13w3c25_tpr <- round(sum(cm_c25_tables[1, 51:60]) / (sum(cm_c25_tables[1, 51:60]) + sum(cm_c25_tables[2, 51:60])), 6))
(k13w3c25_tnr <- round(sum(cm_c25_tables[4, 51:60]) / (sum(cm_c25_tables[4, 51:60]) + sum(cm_c25_tables[3, 51:60])), 6))
(k13w3c25_truescore <- round((2 * k13w3c25_tpr * k13w3c25_tnr) / (k13w3c25_tpr + k13w3c25_tnr), 6))


(k15w1c25_tpr <- round(sum(cm_c25_tables[1, 61:70]) / (sum(cm_c25_tables[1, 61:70]) + sum(cm_c25_tables[2, 61:70])), 6))
(k15w1c25_tnr <- round(sum(cm_c25_tables[4, 61:70]) / (sum(cm_c25_tables[4, 61:70]) + sum(cm_c25_tables[3, 61:70])), 6))
(k15w1c25_truescore <- round((2 * k15w1c25_tpr * k15w1c25_tnr) / (k15w1c25_tpr + k15w1c25_tnr), 6))

(k15w2c25_tpr <- round(sum(cm_c25_tables[1, 71:80]) / (sum(cm_c25_tables[1, 71:80]) + sum(cm_c25_tables[2, 71:80])), 6))
(k15w2c25_tnr <- round(sum(cm_c25_tables[4, 71:80]) / (sum(cm_c25_tables[4, 71:80]) + sum(cm_c25_tables[3, 71:80])), 6))
(k15w2c25_truescore <- round((2 * k15w2c25_tpr * k15w2c25_tnr) / (k15w2c25_tpr + k15w2c25_tnr), 6))

(k15w3c25_tpr <- round(sum(cm_c25_tables[1, 81:90]) / (sum(cm_c25_tables[1, 81:90]) + sum(cm_c25_tables[2, 81:90])), 6))
(k15w3c25_tnr <- round(sum(cm_c25_tables[4, 81:90]) / (sum(cm_c25_tables[4, 81:90]) + sum(cm_c25_tables[3, 81:90])), 6))
(k15w3c25_truescore <- round((2 * k15w3c25_tpr * k15w3c25_tnr) / (k15w3c25_tpr + k15w3c25_tnr), 6))


(k17w1c25_tpr <- round(sum(cm_c25_tables[1, 91:100]) / (sum(cm_c25_tables[1, 91:100]) + sum(cm_c25_tables[2, 91:100])), 6))
(k17w1c25_tnr <- round(sum(cm_c25_tables[4, 91:100]) / (sum(cm_c25_tables[4, 91:100]) + sum(cm_c25_tables[3, 91:100])), 6))
(k17w1c25_truescore <- round((2 * k17w1c25_tpr * k17w1c25_tnr) / (k17w1c25_tpr + k17w1c25_tnr), 6))

(k17w2c25_tpr <- round(sum(cm_c25_tables[1, 101:110]) / (sum(cm_c25_tables[1, 101:110]) + sum(cm_c25_tables[2, 101:110])), 6))
(k17w2c25_tnr <- round(sum(cm_c25_tables[4, 101:110]) / (sum(cm_c25_tables[4, 101:110]) + sum(cm_c25_tables[3, 101:110])), 6))
(k17w2c25_truescore <- round((2 * k17w2c25_tpr * k17w2c25_tnr) / (k17w2c25_tpr + k17w2c25_tnr), 6))

(k17w3c25_tpr <- round(sum(cm_c25_tables[1, 111:120]) / (sum(cm_c25_tables[1, 111:120]) + sum(cm_c25_tables[2, 111:120])), 6))
(k17w3c25_tnr <- round(sum(cm_c25_tables[4, 111:120]) / (sum(cm_c25_tables[4, 111:120]) + sum(cm_c25_tables[3, 111:120])), 6))
(k17w3c25_truescore <- round((2 * k17w3c25_tpr * k17w3c25_tnr) / (k17w3c25_tpr + k17w3c25_tnr), 6))


(k19w1c25_tpr <- round(sum(cm_c25_tables[1, 121:130]) / (sum(cm_c25_tables[1, 121:130]) + sum(cm_c25_tables[2, 121:130])), 6))
(k19w1c25_tnr <- round(sum(cm_c25_tables[4, 121:130]) / (sum(cm_c25_tables[4, 121:130]) + sum(cm_c25_tables[3, 121:130])), 6))
(k19w1c25_truescore <- round((2 * k19w1c25_tpr * k19w1c25_tnr) / (k19w1c25_tpr + k19w1c25_tnr), 6))

(k19w2c25_tpr <- round(sum(cm_c25_tables[1, 131:140]) / (sum(cm_c25_tables[1, 131:140]) + sum(cm_c25_tables[2, 131:140])), 6))
(k19w2c25_tnr <- round(sum(cm_c25_tables[4, 131:140]) / (sum(cm_c25_tables[4, 131:140]) + sum(cm_c25_tables[3, 131:140])), 6))
(k19w2c25_truescore <- round((2 * k19w2c25_tpr * k19w2c25_tnr) / (k19w2c25_tpr + k19w2c25_tnr), 6))

(k19w3c25_tpr <- round(sum(cm_c25_tables[1, 141:150]) / (sum(cm_c25_tables[1, 141:150]) + sum(cm_c25_tables[2, 141:150])), 6))
(k19w3c25_tnr <- round(sum(cm_c25_tables[4, 141:150]) / (sum(cm_c25_tables[4, 141:150]) + sum(cm_c25_tables[3, 141:150])), 6))
(k19w3c25_truescore <- round((2 * k19w3c25_tpr * k19w3c25_tnr) / (k19w3c25_tpr + k19w3c25_tnr), 6))


(k21w1c25_tpr <- round(sum(cm_c25_tables[1, 151:160]) / (sum(cm_c25_tables[1, 151:160]) + sum(cm_c25_tables[2, 151:160])), 6))
(k21w1c25_tnr <- round(sum(cm_c25_tables[4, 151:160]) / (sum(cm_c25_tables[4, 151:160]) + sum(cm_c25_tables[3, 151:160])), 6))
(k21w1c25_truescore <- round((2 * k21w1c25_tpr * k21w1c25_tnr) / (k21w1c25_tpr + k21w1c25_tnr), 6))

(k21w2c25_tpr <- round(sum(cm_c25_tables[1, 161:170]) / (sum(cm_c25_tables[1, 161:170]) + sum(cm_c25_tables[2, 161:170])), 6))
(k21w2c25_tnr <- round(sum(cm_c25_tables[4, 161:170]) / (sum(cm_c25_tables[4, 161:170]) + sum(cm_c25_tables[3, 161:170])), 6))
(k21w2c25_truescore <- round((2 * k21w2c25_tpr * k21w2c25_tnr) / (k21w2c25_tpr + k21w2c25_tnr), 6))

(k21w3c25_tpr <- round(sum(cm_c25_tables[1, 171:180]) / (sum(cm_c25_tables[1, 171:180]) + sum(cm_c25_tables[2, 171:180])), 6))
(k21w3c25_tnr <- round(sum(cm_c25_tables[4, 171:180]) / (sum(cm_c25_tables[4, 171:180]) + sum(cm_c25_tables[3, 171:180])), 6))
(k21w3c25_truescore <- round((2 * k21w3c25_tpr * k21w3c25_tnr) / (k21w3c25_tpr + k21w3c25_tnr), 6))


(k23w1c25_tpr <- round(sum(cm_c25_tables[1, 181:190]) / (sum(cm_c25_tables[1, 181:190]) + sum(cm_c25_tables[2, 181:190])), 6))
(k23w1c25_tnr <- round(sum(cm_c25_tables[4, 181:190]) / (sum(cm_c25_tables[4, 181:190]) + sum(cm_c25_tables[3, 181:190])), 6))
(k23w1c25_truescore <- round((2 * k23w1c25_tpr * k23w1c25_tnr) / (k23w1c25_tpr + k23w1c25_tnr), 6))

(k23w2c25_tpr <- round(sum(cm_c25_tables[1, 191:200]) / (sum(cm_c25_tables[1, 191:200]) + sum(cm_c25_tables[2, 191:200])), 6))
(k23w2c25_tnr <- round(sum(cm_c25_tables[4, 191:200]) / (sum(cm_c25_tables[4, 191:200]) + sum(cm_c25_tables[3, 191:200])), 6))
(k23w2c25_truescore <- round((2 * k23w2c25_tpr * k23w2c25_tnr) / (k23w2c25_tpr + k23w2c25_tnr), 6))

(k23w3c25_tpr <- round(sum(cm_c25_tables[1, 201:210]) / (sum(cm_c25_tables[1, 201:210]) + sum(cm_c25_tables[2, 201:210])), 6))
(k23w3c25_tnr <- round(sum(cm_c25_tables[4, 201:210]) / (sum(cm_c25_tables[4, 201:210]) + sum(cm_c25_tables[3, 201:210])), 6))
(k23w3c25_truescore <- round((2 * k23w3c25_tpr * k23w3c25_tnr) / (k23w3c25_tpr + k23w3c25_tnr), 6))


(k25w1c25_tpr <- round(sum(cm_c25_tables[1, 211:220]) / (sum(cm_c25_tables[1, 211:220]) + sum(cm_c25_tables[2, 211:220])), 6))
(k25w1c25_tnr <- round(sum(cm_c25_tables[4, 211:220]) / (sum(cm_c25_tables[4, 211:220]) + sum(cm_c25_tables[3, 211:220])), 6))
(k25w1c25_truescore <- round((2 * k25w1c25_tpr * k25w1c25_tnr) / (k25w1c25_tpr + k25w1c25_tnr), 6))

(k25w2c25_tpr <- round(sum(cm_c25_tables[1, 221:230]) / (sum(cm_c25_tables[1, 221:230]) + sum(cm_c25_tables[2, 221:230])), 6))
(k25w2c25_tnr <- round(sum(cm_c25_tables[4, 221:230]) / (sum(cm_c25_tables[4, 221:230]) + sum(cm_c25_tables[3, 221:230])), 6))
(k25w2c25_truescore <- round((2 * k25w2c25_tpr * k25w2c25_tnr) / (k25w2c25_tpr + k25w2c25_tnr), 6))

(k25w3c25_tpr <- round(sum(cm_c25_tables[1, 231:240]) / (sum(cm_c25_tables[1, 231:240]) + sum(cm_c25_tables[2, 231:240])), 6))
(k25w3c25_tnr <- round(sum(cm_c25_tables[4, 231:240]) / (sum(cm_c25_tables[4, 231:240]) + sum(cm_c25_tables[3, 231:240])), 6))
(k25w3c25_truescore <- round((2 * k25w3c25_tpr * k25w3c25_tnr) / (k25w3c25_tpr + k25w3c25_tnr), 6))


(k27w1c25_tpr <- round(sum(cm_c25_tables[1, 241:250]) / (sum(cm_c25_tables[1, 241:250]) + sum(cm_c25_tables[2, 241:250])), 6))
(k27w1c25_tnr <- round(sum(cm_c25_tables[4, 241:250]) / (sum(cm_c25_tables[4, 241:250]) + sum(cm_c25_tables[3, 241:250])), 6))
(k27w1c25_truescore <- round((2 * k27w1c25_tpr * k27w1c25_tnr) / (k27w1c25_tpr + k27w1c25_tnr), 6))

(k27w2c25_tpr <- round(sum(cm_c25_tables[1, 251:260]) / (sum(cm_c25_tables[1, 251:260]) + sum(cm_c25_tables[2, 251:260])), 6))
(k27w2c25_tnr <- round(sum(cm_c25_tables[4, 251:260]) / (sum(cm_c25_tables[4, 251:260]) + sum(cm_c25_tables[3, 251:260])), 6))
(k27w2c25_truescore <- round((2 * k27w2c25_tpr * k27w2c25_tnr) / (k27w2c25_tpr + k27w2c25_tnr), 6))

(k27w3c25_tpr <- round(sum(cm_c25_tables[1, 261:270]) / (sum(cm_c25_tables[1, 261:270]) + sum(cm_c25_tables[2, 261:270])), 6))
(k27w3c25_tnr <- round(sum(cm_c25_tables[4, 261:270]) / (sum(cm_c25_tables[4, 261:270]) + sum(cm_c25_tables[3, 261:270])), 6))
(k27w3c25_truescore <- round((2 * k27w3c25_tpr * k27w3c25_tnr) / (k27w3c25_tpr + k27w3c25_tnr), 6))


(k29w1c25_tpr <- round(sum(cm_c25_tables[1, 271:280]) / (sum(cm_c25_tables[1, 271:280]) + sum(cm_c25_tables[2, 271:280])), 6))
(k29w1c25_tnr <- round(sum(cm_c25_tables[4, 271:280]) / (sum(cm_c25_tables[4, 271:280]) + sum(cm_c25_tables[3, 271:280])), 6))
(k29w1c25_truescore <- round((2 * k29w1c25_tpr * k29w1c25_tnr) / (k29w1c25_tpr + k29w1c25_tnr), 6))

(k29w2c25_tpr <- round(sum(cm_c25_tables[1, 281:290]) / (sum(cm_c25_tables[1, 281:290]) + sum(cm_c25_tables[2, 281:290])), 6))
(k29w2c25_tnr <- round(sum(cm_c25_tables[4, 281:290]) / (sum(cm_c25_tables[4, 281:290]) + sum(cm_c25_tables[3, 281:290])), 6))
(k29w2c25_truescore <- round((2 * k29w2c25_tpr * k29w2c25_tnr) / (k29w2c25_tpr + k29w2c25_tnr), 6))

(k29w3c25_tpr <- round(sum(cm_c25_tables[1, 291:300]) / (sum(cm_c25_tables[1, 291:300]) + sum(cm_c25_tables[2, 291:300])), 6))
(k29w3c25_tnr <- round(sum(cm_c25_tables[4, 291:300]) / (sum(cm_c25_tables[4, 291:300]) + sum(cm_c25_tables[3, 291:300])), 6))
(k29w3c25_truescore <- round((2 * k29w3c25_tpr * k29w3c25_tnr) / (k29w3c25_tpr + k29w3c25_tnr), 6))


(k31w1c25_tpr <- round(sum(cm_c25_tables[1, 301:310]) / (sum(cm_c25_tables[1, 301:310]) + sum(cm_c25_tables[2, 301:310])), 6))
(k31w1c25_tnr <- round(sum(cm_c25_tables[4, 301:310]) / (sum(cm_c25_tables[4, 301:310]) + sum(cm_c25_tables[3, 301:310])), 6))
(k31w1c25_truescore <- round((2 * k31w1c25_tpr * k31w1c25_tnr) / (k31w1c25_tpr + k31w1c25_tnr), 6))

(k31w2c25_tpr <- round(sum(cm_c25_tables[1, 311:320]) / (sum(cm_c25_tables[1, 311:320]) + sum(cm_c25_tables[2, 311:320])), 6))
(k31w2c25_tnr <- round(sum(cm_c25_tables[4, 311:320]) / (sum(cm_c25_tables[4, 311:320]) + sum(cm_c25_tables[3, 311:320])), 6))
(k31w2c25_truescore <- round((2 * k31w2c25_tpr * k31w2c25_tnr) / (k31w2c25_tpr + k31w2c25_tnr), 6))

(k31w3c25_tpr <- round(sum(cm_c25_tables[1, 321:330]) / (sum(cm_c25_tables[1, 321:330]) + sum(cm_c25_tables[2, 321:330])), 6))
(k31w3c25_tnr <- round(sum(cm_c25_tables[4, 321:330]) / (sum(cm_c25_tables[4, 321:330]) + sum(cm_c25_tables[3, 321:330])), 6))
(k31w3c25_truescore <- round((2 * k31w3c25_tpr * k31w3c25_tnr) / (k31w3c25_tpr + k31w3c25_tnr), 6))


(k33w1c25_tpr <- round(sum(cm_c25_tables[1, 331:340]) / (sum(cm_c25_tables[1, 331:340]) + sum(cm_c25_tables[2, 331:340])), 6))
(k33w1c25_tnr <- round(sum(cm_c25_tables[4, 331:340]) / (sum(cm_c25_tables[4, 331:340]) + sum(cm_c25_tables[3, 331:340])), 6))
(k33w1c25_truescore <- round((2 * k33w1c25_tpr * k33w1c25_tnr) / (k33w1c25_tpr + k33w1c25_tnr), 6))

(k33w2c25_tpr <- round(sum(cm_c25_tables[1, 341:350]) / (sum(cm_c25_tables[1, 341:350]) + sum(cm_c25_tables[2, 341:350])), 6))
(k33w2c25_tnr <- round(sum(cm_c25_tables[4, 341:350]) / (sum(cm_c25_tables[4, 341:350]) + sum(cm_c25_tables[3, 341:350])), 6))
(k33w2c25_truescore <- round((2 * k33w2c25_tpr * k33w2c25_tnr) / (k33w2c25_tpr + k33w2c25_tnr), 6))

(k33w3c25_tpr <- round(sum(cm_c25_tables[1, 351:360]) / (sum(cm_c25_tables[1, 351:360]) + sum(cm_c25_tables[2, 351:360])), 6))
(k33w3c25_tnr <- round(sum(cm_c25_tables[4, 351:360]) / (sum(cm_c25_tables[4, 351:360]) + sum(cm_c25_tables[3, 351:360])), 6))
(k33w3c25_truescore <- round((2 * k33w3c25_tpr * k33w3c25_tnr) / (k33w3c25_tpr + k33w3c25_tnr), 6))


(k35w1c25_tpr <- round(sum(cm_c25_tables[1, 361:370]) / (sum(cm_c25_tables[1, 361:370]) + sum(cm_c25_tables[2, 361:370])), 6))
(k35w1c25_tnr <- round(sum(cm_c25_tables[4, 361:370]) / (sum(cm_c25_tables[4, 361:370]) + sum(cm_c25_tables[3, 361:370])), 6))
(k35w1c25_truescore <- round((2 * k35w1c25_tpr * k35w1c25_tnr) / (k35w1c25_tpr + k35w1c25_tnr), 6))

(k35w2c25_tpr <- round(sum(cm_c25_tables[1, 371:380]) / (sum(cm_c25_tables[1, 371:380]) + sum(cm_c25_tables[2, 371:380])), 6))
(k35w2c25_tnr <- round(sum(cm_c25_tables[4, 371:380]) / (sum(cm_c25_tables[4, 371:380]) + sum(cm_c25_tables[3, 371:380])), 6))
(k35w2c25_truescore <- round((2 * k35w2c25_tpr * k35w2c25_tnr) / (k35w2c25_tpr + k35w2c25_tnr), 6))

(k35w3c25_tpr <- round(sum(cm_c25_tables[1, 381:390]) / (sum(cm_c25_tables[1, 381:390]) + sum(cm_c25_tables[2, 381:390])), 6))
(k35w3c25_tnr <- round(sum(cm_c25_tables[4, 381:390]) / (sum(cm_c25_tables[4, 381:390]) + sum(cm_c25_tables[3, 381:390])), 6))
(k35w3c25_truescore <- round((2 * k35w3c25_tpr * k35w3c25_tnr) / (k35w3c25_tpr + k35w3c25_tnr), 6))


(k37w1c25_tpr <- round(sum(cm_c25_tables[1, 391:400]) / (sum(cm_c25_tables[1, 391:400]) + sum(cm_c25_tables[2, 391:400])), 6))
(k37w1c25_tnr <- round(sum(cm_c25_tables[4, 391:400]) / (sum(cm_c25_tables[4, 391:400]) + sum(cm_c25_tables[3, 391:400])), 6))
(k37w1c25_truescore <- round((2 * k37w1c25_tpr * k37w1c25_tnr) / (k37w1c25_tpr + k37w1c25_tnr), 6))

(k37w2c25_tpr <- round(sum(cm_c25_tables[1, 401:410]) / (sum(cm_c25_tables[1, 401:410]) + sum(cm_c25_tables[2, 401:410])), 6))
(k37w2c25_tnr <- round(sum(cm_c25_tables[4, 401:410]) / (sum(cm_c25_tables[4, 401:410]) + sum(cm_c25_tables[3, 401:410])), 6))
(k37w2c25_truescore <- round((2 * k37w2c25_tpr * k37w2c25_tnr) / (k37w2c25_tpr + k37w2c25_tnr), 6))

(k37w3c25_tpr <- round(sum(cm_c25_tables[1, 411:420]) / (sum(cm_c25_tables[1, 411:420]) + sum(cm_c25_tables[2, 411:420])), 6))
(k37w3c25_tnr <- round(sum(cm_c25_tables[4, 411:420]) / (sum(cm_c25_tables[4, 411:420]) + sum(cm_c25_tables[3, 411:420])), 6))
(k37w3c25_truescore <- round((2 * k37w3c25_tpr * k37w3c25_tnr) / (k37w3c25_tpr + k37w3c25_tnr), 6))


(k39w1c25_tpr <- round(sum(cm_c25_tables[1, 421:430]) / (sum(cm_c25_tables[1, 421:430]) + sum(cm_c25_tables[2, 421:430])), 6))
(k39w1c25_tnr <- round(sum(cm_c25_tables[4, 421:430]) / (sum(cm_c25_tables[4, 421:430]) + sum(cm_c25_tables[3, 421:430])), 6))
(k39w1c25_truescore <- round((2 * k39w1c25_tpr * k39w1c25_tnr) / (k39w1c25_tpr + k39w1c25_tnr), 6))

(k39w2c25_tpr <- round(sum(cm_c25_tables[1, 431:440]) / (sum(cm_c25_tables[1, 431:440]) + sum(cm_c25_tables[2, 431:440])), 6))
(k39w2c25_tnr <- round(sum(cm_c25_tables[4, 431:440]) / (sum(cm_c25_tables[4, 431:440]) + sum(cm_c25_tables[3, 431:440])), 6))
(k39w2c25_truescore <- round((2 * k39w2c25_tpr * k39w2c25_tnr) / (k39w2c25_tpr + k39w2c25_tnr), 6))

(k39w3c25_tpr <- round(sum(cm_c25_tables[1, 441:450]) / (sum(cm_c25_tables[1, 441:450]) + sum(cm_c25_tables[2, 441:450])), 6))
(k39w3c25_tnr <- round(sum(cm_c25_tables[4, 441:450]) / (sum(cm_c25_tables[4, 441:450]) + sum(cm_c25_tables[3, 441:450])), 6))
(k39w3c25_truescore <- round((2 * k39w3c25_tpr * k39w3c25_tnr) / (k39w3c25_tpr + k39w3c25_tnr), 6))


# Compile the 0.25 cutoff results in a table, and identify the combination of 
# of k and kernel that maximizes the Truescore.

c25_results <- tibble(k = c(11, 11, 11, 13, 13, 13, 15, 15, 15, 
                            17, 17, 17, 19, 19, 19, 21, 21, 21, 
                            23, 23, 23, 25, 25, 25, 27, 27, 27, 
                            29, 29, 29, 31, 31, 31, 33, 33, 33, 
                            35, 35, 35, 37, 37, 37, 39, 39, 39), 
                      Kernel = rep(c("triangular", "gaussian", "optimal"), 15), 
                      Cut = 0.25, 
                      TPR = c(k11w1c25_tpr, k11w2c25_tpr, k11w3c25_tpr, k13w1c25_tpr, 
                              k13w2c25_tpr, k13w3c25_tpr, k15w1c25_tpr, k15w2c25_tpr, 
                              k15w3c25_tpr, k17w1c25_tpr, k17w2c25_tpr, k17w3c25_tpr, 
                              k19w1c25_tpr, k19w2c25_tpr, k19w3c25_tpr, k21w1c25_tpr, 
                              k21w2c25_tpr, k21w3c25_tpr, k23w1c25_tpr, k23w2c25_tpr, 
                              k23w3c25_tpr, k25w1c25_tpr, k25w2c25_tpr, k25w3c25_tpr, 
                              k27w1c25_tpr, k27w2c25_tpr, k27w3c25_tpr, k29w1c25_tpr, 
                              k29w2c25_tpr, k29w3c25_tpr, k31w1c25_tpr, k31w2c25_tpr, 
                              k31w3c25_tpr, k33w1c25_tpr, k33w2c25_tpr, k33w3c25_tpr, 
                              k35w1c25_tpr, k35w2c25_tpr, k35w3c25_tpr, k37w1c25_tpr, 
                              k37w2c25_tpr, k37w3c25_tpr, k39w1c25_tpr, k39w2c25_tpr, 
                              k39w3c25_tpr), 
                      TNR = c(k11w1c25_tnr, k11w2c25_tnr, k11w3c25_tnr, k13w1c25_tnr, 
                              k13w2c25_tnr, k13w3c25_tnr, k15w1c25_tnr, k15w2c25_tnr, 
                              k15w3c25_tnr, k17w1c25_tnr, k17w2c25_tnr, k17w3c25_tnr, 
                              k19w1c25_tnr, k19w2c25_tnr, k19w3c25_tnr, k21w1c25_tnr, 
                              k21w2c25_tnr, k21w3c25_tnr, k23w1c25_tnr, k23w2c25_tnr, 
                              k23w3c25_tnr, k25w1c25_tnr, k25w2c25_tnr, k25w3c25_tnr, 
                              k27w1c25_tnr, k27w2c25_tnr, k27w3c25_tnr, k29w1c25_tnr, 
                              k29w2c25_tnr, k29w3c25_tnr, k31w1c25_tnr, k31w2c25_tnr, 
                              k31w3c25_tnr, k33w1c25_tnr, k33w2c25_tnr, k33w3c25_tnr, 
                              k35w1c25_tnr, k35w2c25_tnr, k35w3c25_tnr, k37w1c25_tnr, 
                              k37w2c25_tnr, k37w3c25_tnr, k39w1c25_tnr, k39w2c25_tnr, 
                              k39w3c25_tnr), 
                      Truescore = c(k11w1c25_truescore, k11w2c25_truescore, 
                                    k11w3c25_truescore, k13w1c25_truescore, 
                                    k13w2c25_truescore, k13w3c25_truescore, 
                                    k15w1c25_truescore, k15w2c25_truescore, 
                                    k15w3c25_truescore, k17w1c25_truescore, 
                                    k17w2c25_truescore, k17w3c25_truescore, 
                                    k19w1c25_truescore, k19w2c25_truescore, 
                                    k19w3c25_truescore, k21w1c25_truescore, 
                                    k21w2c25_truescore, k21w3c25_truescore, 
                                    k23w1c25_truescore, k23w2c25_truescore, 
                                    k23w3c25_truescore, k25w1c25_truescore, 
                                    k25w2c25_truescore, k25w3c25_truescore, 
                                    k27w1c25_truescore, k27w2c25_truescore, 
                                    k27w3c25_truescore, k29w1c25_truescore, 
                                    k29w2c25_truescore, k29w3c25_truescore, 
                                    k31w1c25_truescore, k31w2c25_truescore, 
                                    k31w3c25_truescore, k33w1c25_truescore, 
                                    k33w2c25_truescore, k33w3c25_truescore, 
                                    k35w1c25_truescore, k35w2c25_truescore, 
                                    k35w3c25_truescore, k37w1c25_truescore, 
                                    k37w2c25_truescore, k37w3c25_truescore, 
                                    k39w1c25_truescore, k39w2c25_truescore, 
                                    k39w3c25_truescore))

knitr::kable(c25_results[1:45, ], caption = "c25_results")

ggplot(c25_results, aes(k, Truescore, shape = Kernel, color = Kernel)) + 
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.25 (Truescore)")

# For the Cutoff of 0.25, compute the Minimum Distance to (0, 1) for each 
# combination of k and Kernel. 

c25_results <- c25_results %>% mutate(Distance = sqrt((1 - TPR)^2 + (1 - TNR)^2))

ggplot(c25_results, aes(k, Distance, shape = Kernel, color = Kernel)) +
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.25 (Distance)")

# For the Cutoff of 0.25, identify the optimal combination of values for k and Kernel 
# based on each of our assessment methods, Truescore and Mimimum Distance to (0, 1).

max(c25_results$Truescore)
(c25_opt_k_ts <- c25_results$k[which.max(c25_results$Truescore)])
(c25_opt_kernel_ts <- c25_results$Kernel[which.max(c25_results$Truescore)])
(c25_opt_cut_ts <- c25_results$Cut[which.max(c25_results$Truescore)])
(c25_opt_tpr_ts <- c25_results$TPR[which.max(c25_results$Truescore)])
(c25_opt_tnr_ts <- c25_results$TNR[which.max(c25_results$Truescore)])
(c25_opt_d_ts <- c25_results$Distance[which.max(c25_results$Truescore)])

min(c25_results$Distance)
(c25_opt_k_dist <- c25_results$k[which.min(c25_results$Distance)])
(c25_opt_kernel_dist <- c25_results$Kernel[which.min(c25_results$Distance)])
(c25_opt_cut_dist <- c25_results$Cut[which.min(c25_results$Distance)])
(c25_opt_tpr_dist <- c25_results$TPR[which.min(c25_results$Distance)])
(c25_opt_tnr_dist <- c25_results$TNR[which.min(c25_results$Distance)])
(c25_opt_t_dist <- c25_results$Truescore[which.min(c25_results$Distance)])

############################
# 0.26 Cutoff
############################

# For the decision cutoff of 0.26, generate a confusion matrix for
# every combination of k (11:39 by two), kernel (1:3) and fold (1:10).

cm_c26 <- sapply(kwf_dfs_l, function(x) {
  ss <- subset(x, select = c(pred26, obs))
  confusionMatrix(ss$pred26, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c26) <- kwf_dfs_v

cm_c26_tables <- sapply(cm_c26, "[[", 2)
cm_c26_tables <- as_tibble(cm_c26_tables)

# For each combination of k and kernel, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each combination of k and kernel when the decision cutoff is 0.26.

(k11w1c26_tpr <- round(sum(cm_c26_tables[1, 1:10]) / (sum(cm_c26_tables[1, 1:10]) + sum(cm_c26_tables[2, 1:10])), 6))
(k11w1c26_tnr <- round(sum(cm_c26_tables[4, 1:10]) / (sum(cm_c26_tables[4, 1:10]) + sum(cm_c26_tables[3, 1:10])), 6))
(k11w1c26_truescore <- round((2 * k11w1c26_tpr * k11w1c26_tnr) / (k11w1c26_tpr + k11w1c26_tnr), 6))

(k11w2c26_tpr <- round(sum(cm_c26_tables[1, 11:20]) / (sum(cm_c26_tables[1, 11:20]) + sum(cm_c26_tables[2, 11:20])), 6))
(k11w2c26_tnr <- round(sum(cm_c26_tables[4, 11:20]) / (sum(cm_c26_tables[4, 11:20]) + sum(cm_c26_tables[3, 11:20])), 6))
(k11w2c26_truescore <- round((2 * k11w2c26_tpr * k11w2c26_tnr) / (k11w2c26_tpr + k11w2c26_tnr), 6))

(k11w3c26_tpr <- round(sum(cm_c26_tables[1, 21:30]) / (sum(cm_c26_tables[1, 21:30]) + sum(cm_c26_tables[2, 21:30])), 6))
(k11w3c26_tnr <- round(sum(cm_c26_tables[4, 21:30]) / (sum(cm_c26_tables[4, 21:30]) + sum(cm_c26_tables[3, 21:30])), 6))
(k11w3c26_truescore <- round((2 * k11w3c26_tpr * k11w3c26_tnr) / (k11w3c26_tpr + k11w3c26_tnr), 6))


(k13w1c26_tpr <- round(sum(cm_c26_tables[1, 31:40]) / (sum(cm_c26_tables[1, 31:40]) + sum(cm_c26_tables[2, 31:40])), 6))
(k13w1c26_tnr <- round(sum(cm_c26_tables[4, 31:40]) / (sum(cm_c26_tables[4, 31:40]) + sum(cm_c26_tables[3, 31:40])), 6))
(k13w1c26_truescore <- round((2 * k13w1c26_tpr * k13w1c26_tnr) / (k13w1c26_tpr + k13w1c26_tnr), 6))

(k13w2c26_tpr <- round(sum(cm_c26_tables[1, 41:50]) / (sum(cm_c26_tables[1, 41:50]) + sum(cm_c26_tables[2, 41:50])), 6))
(k13w2c26_tnr <- round(sum(cm_c26_tables[4, 41:50]) / (sum(cm_c26_tables[4, 41:50]) + sum(cm_c26_tables[3, 41:50])), 6))
(k13w2c26_truescore <- round((2 * k13w2c26_tpr * k13w2c26_tnr) / (k13w2c26_tpr + k13w2c26_tnr), 6))

(k13w3c26_tpr <- round(sum(cm_c26_tables[1, 51:60]) / (sum(cm_c26_tables[1, 51:60]) + sum(cm_c26_tables[2, 51:60])), 6))
(k13w3c26_tnr <- round(sum(cm_c26_tables[4, 51:60]) / (sum(cm_c26_tables[4, 51:60]) + sum(cm_c26_tables[3, 51:60])), 6))
(k13w3c26_truescore <- round((2 * k13w3c26_tpr * k13w3c26_tnr) / (k13w3c26_tpr + k13w3c26_tnr), 6))


(k15w1c26_tpr <- round(sum(cm_c26_tables[1, 61:70]) / (sum(cm_c26_tables[1, 61:70]) + sum(cm_c26_tables[2, 61:70])), 6))
(k15w1c26_tnr <- round(sum(cm_c26_tables[4, 61:70]) / (sum(cm_c26_tables[4, 61:70]) + sum(cm_c26_tables[3, 61:70])), 6))
(k15w1c26_truescore <- round((2 * k15w1c26_tpr * k15w1c26_tnr) / (k15w1c26_tpr + k15w1c26_tnr), 6))

(k15w2c26_tpr <- round(sum(cm_c26_tables[1, 71:80]) / (sum(cm_c26_tables[1, 71:80]) + sum(cm_c26_tables[2, 71:80])), 6))
(k15w2c26_tnr <- round(sum(cm_c26_tables[4, 71:80]) / (sum(cm_c26_tables[4, 71:80]) + sum(cm_c26_tables[3, 71:80])), 6))
(k15w2c26_truescore <- round((2 * k15w2c26_tpr * k15w2c26_tnr) / (k15w2c26_tpr + k15w2c26_tnr), 6))

(k15w3c26_tpr <- round(sum(cm_c26_tables[1, 81:90]) / (sum(cm_c26_tables[1, 81:90]) + sum(cm_c26_tables[2, 81:90])), 6))
(k15w3c26_tnr <- round(sum(cm_c26_tables[4, 81:90]) / (sum(cm_c26_tables[4, 81:90]) + sum(cm_c26_tables[3, 81:90])), 6))
(k15w3c26_truescore <- round((2 * k15w3c26_tpr * k15w3c26_tnr) / (k15w3c26_tpr + k15w3c26_tnr), 6))


(k17w1c26_tpr <- round(sum(cm_c26_tables[1, 91:100]) / (sum(cm_c26_tables[1, 91:100]) + sum(cm_c26_tables[2, 91:100])), 6))
(k17w1c26_tnr <- round(sum(cm_c26_tables[4, 91:100]) / (sum(cm_c26_tables[4, 91:100]) + sum(cm_c26_tables[3, 91:100])), 6))
(k17w1c26_truescore <- round((2 * k17w1c26_tpr * k17w1c26_tnr) / (k17w1c26_tpr + k17w1c26_tnr), 6))

(k17w2c26_tpr <- round(sum(cm_c26_tables[1, 101:110]) / (sum(cm_c26_tables[1, 101:110]) + sum(cm_c26_tables[2, 101:110])), 6))
(k17w2c26_tnr <- round(sum(cm_c26_tables[4, 101:110]) / (sum(cm_c26_tables[4, 101:110]) + sum(cm_c26_tables[3, 101:110])), 6))
(k17w2c26_truescore <- round((2 * k17w2c26_tpr * k17w2c26_tnr) / (k17w2c26_tpr + k17w2c26_tnr), 6))

(k17w3c26_tpr <- round(sum(cm_c26_tables[1, 111:120]) / (sum(cm_c26_tables[1, 111:120]) + sum(cm_c26_tables[2, 111:120])), 6))
(k17w3c26_tnr <- round(sum(cm_c26_tables[4, 111:120]) / (sum(cm_c26_tables[4, 111:120]) + sum(cm_c26_tables[3, 111:120])), 6))
(k17w3c26_truescore <- round((2 * k17w3c26_tpr * k17w3c26_tnr) / (k17w3c26_tpr + k17w3c26_tnr), 6))


(k19w1c26_tpr <- round(sum(cm_c26_tables[1, 121:130]) / (sum(cm_c26_tables[1, 121:130]) + sum(cm_c26_tables[2, 121:130])), 6))
(k19w1c26_tnr <- round(sum(cm_c26_tables[4, 121:130]) / (sum(cm_c26_tables[4, 121:130]) + sum(cm_c26_tables[3, 121:130])), 6))
(k19w1c26_truescore <- round((2 * k19w1c26_tpr * k19w1c26_tnr) / (k19w1c26_tpr + k19w1c26_tnr), 6))

(k19w2c26_tpr <- round(sum(cm_c26_tables[1, 131:140]) / (sum(cm_c26_tables[1, 131:140]) + sum(cm_c26_tables[2, 131:140])), 6))
(k19w2c26_tnr <- round(sum(cm_c26_tables[4, 131:140]) / (sum(cm_c26_tables[4, 131:140]) + sum(cm_c26_tables[3, 131:140])), 6))
(k19w2c26_truescore <- round((2 * k19w2c26_tpr * k19w2c26_tnr) / (k19w2c26_tpr + k19w2c26_tnr), 6))

(k19w3c26_tpr <- round(sum(cm_c26_tables[1, 141:150]) / (sum(cm_c26_tables[1, 141:150]) + sum(cm_c26_tables[2, 141:150])), 6))
(k19w3c26_tnr <- round(sum(cm_c26_tables[4, 141:150]) / (sum(cm_c26_tables[4, 141:150]) + sum(cm_c26_tables[3, 141:150])), 6))
(k19w3c26_truescore <- round((2 * k19w3c26_tpr * k19w3c26_tnr) / (k19w3c26_tpr + k19w3c26_tnr), 6))


(k21w1c26_tpr <- round(sum(cm_c26_tables[1, 151:160]) / (sum(cm_c26_tables[1, 151:160]) + sum(cm_c26_tables[2, 151:160])), 6))
(k21w1c26_tnr <- round(sum(cm_c26_tables[4, 151:160]) / (sum(cm_c26_tables[4, 151:160]) + sum(cm_c26_tables[3, 151:160])), 6))
(k21w1c26_truescore <- round((2 * k21w1c26_tpr * k21w1c26_tnr) / (k21w1c26_tpr + k21w1c26_tnr), 6))

(k21w2c26_tpr <- round(sum(cm_c26_tables[1, 161:170]) / (sum(cm_c26_tables[1, 161:170]) + sum(cm_c26_tables[2, 161:170])), 6))
(k21w2c26_tnr <- round(sum(cm_c26_tables[4, 161:170]) / (sum(cm_c26_tables[4, 161:170]) + sum(cm_c26_tables[3, 161:170])), 6))
(k21w2c26_truescore <- round((2 * k21w2c26_tpr * k21w2c26_tnr) / (k21w2c26_tpr + k21w2c26_tnr), 6))

(k21w3c26_tpr <- round(sum(cm_c26_tables[1, 171:180]) / (sum(cm_c26_tables[1, 171:180]) + sum(cm_c26_tables[2, 171:180])), 6))
(k21w3c26_tnr <- round(sum(cm_c26_tables[4, 171:180]) / (sum(cm_c26_tables[4, 171:180]) + sum(cm_c26_tables[3, 171:180])), 6))
(k21w3c26_truescore <- round((2 * k21w3c26_tpr * k21w3c26_tnr) / (k21w3c26_tpr + k21w3c26_tnr), 6))


(k23w1c26_tpr <- round(sum(cm_c26_tables[1, 181:190]) / (sum(cm_c26_tables[1, 181:190]) + sum(cm_c26_tables[2, 181:190])), 6))
(k23w1c26_tnr <- round(sum(cm_c26_tables[4, 181:190]) / (sum(cm_c26_tables[4, 181:190]) + sum(cm_c26_tables[3, 181:190])), 6))
(k23w1c26_truescore <- round((2 * k23w1c26_tpr * k23w1c26_tnr) / (k23w1c26_tpr + k23w1c26_tnr), 6))

(k23w2c26_tpr <- round(sum(cm_c26_tables[1, 191:200]) / (sum(cm_c26_tables[1, 191:200]) + sum(cm_c26_tables[2, 191:200])), 6))
(k23w2c26_tnr <- round(sum(cm_c26_tables[4, 191:200]) / (sum(cm_c26_tables[4, 191:200]) + sum(cm_c26_tables[3, 191:200])), 6))
(k23w2c26_truescore <- round((2 * k23w2c26_tpr * k23w2c26_tnr) / (k23w2c26_tpr + k23w2c26_tnr), 6))

(k23w3c26_tpr <- round(sum(cm_c26_tables[1, 201:210]) / (sum(cm_c26_tables[1, 201:210]) + sum(cm_c26_tables[2, 201:210])), 6))
(k23w3c26_tnr <- round(sum(cm_c26_tables[4, 201:210]) / (sum(cm_c26_tables[4, 201:210]) + sum(cm_c26_tables[3, 201:210])), 6))
(k23w3c26_truescore <- round((2 * k23w3c26_tpr * k23w3c26_tnr) / (k23w3c26_tpr + k23w3c26_tnr), 6))


(k25w1c26_tpr <- round(sum(cm_c26_tables[1, 211:220]) / (sum(cm_c26_tables[1, 211:220]) + sum(cm_c26_tables[2, 211:220])), 6))
(k25w1c26_tnr <- round(sum(cm_c26_tables[4, 211:220]) / (sum(cm_c26_tables[4, 211:220]) + sum(cm_c26_tables[3, 211:220])), 6))
(k25w1c26_truescore <- round((2 * k25w1c26_tpr * k25w1c26_tnr) / (k25w1c26_tpr + k25w1c26_tnr), 6))

(k25w2c26_tpr <- round(sum(cm_c26_tables[1, 221:230]) / (sum(cm_c26_tables[1, 221:230]) + sum(cm_c26_tables[2, 221:230])), 6))
(k25w2c26_tnr <- round(sum(cm_c26_tables[4, 221:230]) / (sum(cm_c26_tables[4, 221:230]) + sum(cm_c26_tables[3, 221:230])), 6))
(k25w2c26_truescore <- round((2 * k25w2c26_tpr * k25w2c26_tnr) / (k25w2c26_tpr + k25w2c26_tnr), 6))

(k25w3c26_tpr <- round(sum(cm_c26_tables[1, 231:240]) / (sum(cm_c26_tables[1, 231:240]) + sum(cm_c26_tables[2, 231:240])), 6))
(k25w3c26_tnr <- round(sum(cm_c26_tables[4, 231:240]) / (sum(cm_c26_tables[4, 231:240]) + sum(cm_c26_tables[3, 231:240])), 6))
(k25w3c26_truescore <- round((2 * k25w3c26_tpr * k25w3c26_tnr) / (k25w3c26_tpr + k25w3c26_tnr), 6))


(k27w1c26_tpr <- round(sum(cm_c26_tables[1, 241:250]) / (sum(cm_c26_tables[1, 241:250]) + sum(cm_c26_tables[2, 241:250])), 6))
(k27w1c26_tnr <- round(sum(cm_c26_tables[4, 241:250]) / (sum(cm_c26_tables[4, 241:250]) + sum(cm_c26_tables[3, 241:250])), 6))
(k27w1c26_truescore <- round((2 * k27w1c26_tpr * k27w1c26_tnr) / (k27w1c26_tpr + k27w1c26_tnr), 6))

(k27w2c26_tpr <- round(sum(cm_c26_tables[1, 251:260]) / (sum(cm_c26_tables[1, 251:260]) + sum(cm_c26_tables[2, 251:260])), 6))
(k27w2c26_tnr <- round(sum(cm_c26_tables[4, 251:260]) / (sum(cm_c26_tables[4, 251:260]) + sum(cm_c26_tables[3, 251:260])), 6))
(k27w2c26_truescore <- round((2 * k27w2c26_tpr * k27w2c26_tnr) / (k27w2c26_tpr + k27w2c26_tnr), 6))

(k27w3c26_tpr <- round(sum(cm_c26_tables[1, 261:270]) / (sum(cm_c26_tables[1, 261:270]) + sum(cm_c26_tables[2, 261:270])), 6))
(k27w3c26_tnr <- round(sum(cm_c26_tables[4, 261:270]) / (sum(cm_c26_tables[4, 261:270]) + sum(cm_c26_tables[3, 261:270])), 6))
(k27w3c26_truescore <- round((2 * k27w3c26_tpr * k27w3c26_tnr) / (k27w3c26_tpr + k27w3c26_tnr), 6))


(k29w1c26_tpr <- round(sum(cm_c26_tables[1, 271:280]) / (sum(cm_c26_tables[1, 271:280]) + sum(cm_c26_tables[2, 271:280])), 6))
(k29w1c26_tnr <- round(sum(cm_c26_tables[4, 271:280]) / (sum(cm_c26_tables[4, 271:280]) + sum(cm_c26_tables[3, 271:280])), 6))
(k29w1c26_truescore <- round((2 * k29w1c26_tpr * k29w1c26_tnr) / (k29w1c26_tpr + k29w1c26_tnr), 6))

(k29w2c26_tpr <- round(sum(cm_c26_tables[1, 281:290]) / (sum(cm_c26_tables[1, 281:290]) + sum(cm_c26_tables[2, 281:290])), 6))
(k29w2c26_tnr <- round(sum(cm_c26_tables[4, 281:290]) / (sum(cm_c26_tables[4, 281:290]) + sum(cm_c26_tables[3, 281:290])), 6))
(k29w2c26_truescore <- round((2 * k29w2c26_tpr * k29w2c26_tnr) / (k29w2c26_tpr + k29w2c26_tnr), 6))

(k29w3c26_tpr <- round(sum(cm_c26_tables[1, 291:300]) / (sum(cm_c26_tables[1, 291:300]) + sum(cm_c26_tables[2, 291:300])), 6))
(k29w3c26_tnr <- round(sum(cm_c26_tables[4, 291:300]) / (sum(cm_c26_tables[4, 291:300]) + sum(cm_c26_tables[3, 291:300])), 6))
(k29w3c26_truescore <- round((2 * k29w3c26_tpr * k29w3c26_tnr) / (k29w3c26_tpr + k29w3c26_tnr), 6))


(k31w1c26_tpr <- round(sum(cm_c26_tables[1, 301:310]) / (sum(cm_c26_tables[1, 301:310]) + sum(cm_c26_tables[2, 301:310])), 6))
(k31w1c26_tnr <- round(sum(cm_c26_tables[4, 301:310]) / (sum(cm_c26_tables[4, 301:310]) + sum(cm_c26_tables[3, 301:310])), 6))
(k31w1c26_truescore <- round((2 * k31w1c26_tpr * k31w1c26_tnr) / (k31w1c26_tpr + k31w1c26_tnr), 6))

(k31w2c26_tpr <- round(sum(cm_c26_tables[1, 311:320]) / (sum(cm_c26_tables[1, 311:320]) + sum(cm_c26_tables[2, 311:320])), 6))
(k31w2c26_tnr <- round(sum(cm_c26_tables[4, 311:320]) / (sum(cm_c26_tables[4, 311:320]) + sum(cm_c26_tables[3, 311:320])), 6))
(k31w2c26_truescore <- round((2 * k31w2c26_tpr * k31w2c26_tnr) / (k31w2c26_tpr + k31w2c26_tnr), 6))

(k31w3c26_tpr <- round(sum(cm_c26_tables[1, 321:330]) / (sum(cm_c26_tables[1, 321:330]) + sum(cm_c26_tables[2, 321:330])), 6))
(k31w3c26_tnr <- round(sum(cm_c26_tables[4, 321:330]) / (sum(cm_c26_tables[4, 321:330]) + sum(cm_c26_tables[3, 321:330])), 6))
(k31w3c26_truescore <- round((2 * k31w3c26_tpr * k31w3c26_tnr) / (k31w3c26_tpr + k31w3c26_tnr), 6))


(k33w1c26_tpr <- round(sum(cm_c26_tables[1, 331:340]) / (sum(cm_c26_tables[1, 331:340]) + sum(cm_c26_tables[2, 331:340])), 6))
(k33w1c26_tnr <- round(sum(cm_c26_tables[4, 331:340]) / (sum(cm_c26_tables[4, 331:340]) + sum(cm_c26_tables[3, 331:340])), 6))
(k33w1c26_truescore <- round((2 * k33w1c26_tpr * k33w1c26_tnr) / (k33w1c26_tpr + k33w1c26_tnr), 6))

(k33w2c26_tpr <- round(sum(cm_c26_tables[1, 341:350]) / (sum(cm_c26_tables[1, 341:350]) + sum(cm_c26_tables[2, 341:350])), 6))
(k33w2c26_tnr <- round(sum(cm_c26_tables[4, 341:350]) / (sum(cm_c26_tables[4, 341:350]) + sum(cm_c26_tables[3, 341:350])), 6))
(k33w2c26_truescore <- round((2 * k33w2c26_tpr * k33w2c26_tnr) / (k33w2c26_tpr + k33w2c26_tnr), 6))

(k33w3c26_tpr <- round(sum(cm_c26_tables[1, 351:360]) / (sum(cm_c26_tables[1, 351:360]) + sum(cm_c26_tables[2, 351:360])), 6))
(k33w3c26_tnr <- round(sum(cm_c26_tables[4, 351:360]) / (sum(cm_c26_tables[4, 351:360]) + sum(cm_c26_tables[3, 351:360])), 6))
(k33w3c26_truescore <- round((2 * k33w3c26_tpr * k33w3c26_tnr) / (k33w3c26_tpr + k33w3c26_tnr), 6))


(k35w1c26_tpr <- round(sum(cm_c26_tables[1, 361:370]) / (sum(cm_c26_tables[1, 361:370]) + sum(cm_c26_tables[2, 361:370])), 6))
(k35w1c26_tnr <- round(sum(cm_c26_tables[4, 361:370]) / (sum(cm_c26_tables[4, 361:370]) + sum(cm_c26_tables[3, 361:370])), 6))
(k35w1c26_truescore <- round((2 * k35w1c26_tpr * k35w1c26_tnr) / (k35w1c26_tpr + k35w1c26_tnr), 6))

(k35w2c26_tpr <- round(sum(cm_c26_tables[1, 371:380]) / (sum(cm_c26_tables[1, 371:380]) + sum(cm_c26_tables[2, 371:380])), 6))
(k35w2c26_tnr <- round(sum(cm_c26_tables[4, 371:380]) / (sum(cm_c26_tables[4, 371:380]) + sum(cm_c26_tables[3, 371:380])), 6))
(k35w2c26_truescore <- round((2 * k35w2c26_tpr * k35w2c26_tnr) / (k35w2c26_tpr + k35w2c26_tnr), 6))

(k35w3c26_tpr <- round(sum(cm_c26_tables[1, 381:390]) / (sum(cm_c26_tables[1, 381:390]) + sum(cm_c26_tables[2, 381:390])), 6))
(k35w3c26_tnr <- round(sum(cm_c26_tables[4, 381:390]) / (sum(cm_c26_tables[4, 381:390]) + sum(cm_c26_tables[3, 381:390])), 6))
(k35w3c26_truescore <- round((2 * k35w3c26_tpr * k35w3c26_tnr) / (k35w3c26_tpr + k35w3c26_tnr), 6))


(k37w1c26_tpr <- round(sum(cm_c26_tables[1, 391:400]) / (sum(cm_c26_tables[1, 391:400]) + sum(cm_c26_tables[2, 391:400])), 6))
(k37w1c26_tnr <- round(sum(cm_c26_tables[4, 391:400]) / (sum(cm_c26_tables[4, 391:400]) + sum(cm_c26_tables[3, 391:400])), 6))
(k37w1c26_truescore <- round((2 * k37w1c26_tpr * k37w1c26_tnr) / (k37w1c26_tpr + k37w1c26_tnr), 6))

(k37w2c26_tpr <- round(sum(cm_c26_tables[1, 401:410]) / (sum(cm_c26_tables[1, 401:410]) + sum(cm_c26_tables[2, 401:410])), 6))
(k37w2c26_tnr <- round(sum(cm_c26_tables[4, 401:410]) / (sum(cm_c26_tables[4, 401:410]) + sum(cm_c26_tables[3, 401:410])), 6))
(k37w2c26_truescore <- round((2 * k37w2c26_tpr * k37w2c26_tnr) / (k37w2c26_tpr + k37w2c26_tnr), 6))

(k37w3c26_tpr <- round(sum(cm_c26_tables[1, 411:420]) / (sum(cm_c26_tables[1, 411:420]) + sum(cm_c26_tables[2, 411:420])), 6))
(k37w3c26_tnr <- round(sum(cm_c26_tables[4, 411:420]) / (sum(cm_c26_tables[4, 411:420]) + sum(cm_c26_tables[3, 411:420])), 6))
(k37w3c26_truescore <- round((2 * k37w3c26_tpr * k37w3c26_tnr) / (k37w3c26_tpr + k37w3c26_tnr), 6))


(k39w1c26_tpr <- round(sum(cm_c26_tables[1, 421:430]) / (sum(cm_c26_tables[1, 421:430]) + sum(cm_c26_tables[2, 421:430])), 6))
(k39w1c26_tnr <- round(sum(cm_c26_tables[4, 421:430]) / (sum(cm_c26_tables[4, 421:430]) + sum(cm_c26_tables[3, 421:430])), 6))
(k39w1c26_truescore <- round((2 * k39w1c26_tpr * k39w1c26_tnr) / (k39w1c26_tpr + k39w1c26_tnr), 6))

(k39w2c26_tpr <- round(sum(cm_c26_tables[1, 431:440]) / (sum(cm_c26_tables[1, 431:440]) + sum(cm_c26_tables[2, 431:440])), 6))
(k39w2c26_tnr <- round(sum(cm_c26_tables[4, 431:440]) / (sum(cm_c26_tables[4, 431:440]) + sum(cm_c26_tables[3, 431:440])), 6))
(k39w2c26_truescore <- round((2 * k39w2c26_tpr * k39w2c26_tnr) / (k39w2c26_tpr + k39w2c26_tnr), 6))

(k39w3c26_tpr <- round(sum(cm_c26_tables[1, 441:450]) / (sum(cm_c26_tables[1, 441:450]) + sum(cm_c26_tables[2, 441:450])), 6))
(k39w3c26_tnr <- round(sum(cm_c26_tables[4, 441:450]) / (sum(cm_c26_tables[4, 441:450]) + sum(cm_c26_tables[3, 441:450])), 6))
(k39w3c26_truescore <- round((2 * k39w3c26_tpr * k39w3c26_tnr) / (k39w3c26_tpr + k39w3c26_tnr), 6))


# Compile the 0.26 cutoff results in a table, and identify the combination of 
# of k and kernel that maximizes the Truescore.

c26_results <- tibble(k = c(11, 11, 11, 13, 13, 13, 15, 15, 15, 
                            17, 17, 17, 19, 19, 19, 21, 21, 21, 
                            23, 23, 23, 25, 25, 25, 27, 27, 27, 
                            29, 29, 29, 31, 31, 31, 33, 33, 33, 
                            35, 35, 35, 37, 37, 37, 39, 39, 39), 
                      Kernel = rep(c("triangular", "gaussian", "optimal"), 15), 
                      Cut = 0.26, 
                      TPR = c(k11w1c26_tpr, k11w2c26_tpr, k11w3c26_tpr, k13w1c26_tpr, 
                              k13w2c26_tpr, k13w3c26_tpr, k15w1c26_tpr, k15w2c26_tpr, 
                              k15w3c26_tpr, k17w1c26_tpr, k17w2c26_tpr, k17w3c26_tpr, 
                              k19w1c26_tpr, k19w2c26_tpr, k19w3c26_tpr, k21w1c26_tpr, 
                              k21w2c26_tpr, k21w3c26_tpr, k23w1c26_tpr, k23w2c26_tpr, 
                              k23w3c26_tpr, k25w1c26_tpr, k25w2c26_tpr, k25w3c26_tpr, 
                              k27w1c26_tpr, k27w2c26_tpr, k27w3c26_tpr, k29w1c26_tpr, 
                              k29w2c26_tpr, k29w3c26_tpr, k31w1c26_tpr, k31w2c26_tpr, 
                              k31w3c26_tpr, k33w1c26_tpr, k33w2c26_tpr, k33w3c26_tpr, 
                              k35w1c26_tpr, k35w2c26_tpr, k35w3c26_tpr, k37w1c26_tpr, 
                              k37w2c26_tpr, k37w3c26_tpr, k39w1c26_tpr, k39w2c26_tpr, 
                              k39w3c26_tpr), 
                      TNR = c(k11w1c26_tnr, k11w2c26_tnr, k11w3c26_tnr, k13w1c26_tnr, 
                              k13w2c26_tnr, k13w3c26_tnr, k15w1c26_tnr, k15w2c26_tnr, 
                              k15w3c26_tnr, k17w1c26_tnr, k17w2c26_tnr, k17w3c26_tnr, 
                              k19w1c26_tnr, k19w2c26_tnr, k19w3c26_tnr, k21w1c26_tnr, 
                              k21w2c26_tnr, k21w3c26_tnr, k23w1c26_tnr, k23w2c26_tnr, 
                              k23w3c26_tnr, k25w1c26_tnr, k25w2c26_tnr, k25w3c26_tnr, 
                              k27w1c26_tnr, k27w2c26_tnr, k27w3c26_tnr, k29w1c26_tnr, 
                              k29w2c26_tnr, k29w3c26_tnr, k31w1c26_tnr, k31w2c26_tnr, 
                              k31w3c26_tnr, k33w1c26_tnr, k33w2c26_tnr, k33w3c26_tnr, 
                              k35w1c26_tnr, k35w2c26_tnr, k35w3c26_tnr, k37w1c26_tnr, 
                              k37w2c26_tnr, k37w3c26_tnr, k39w1c26_tnr, k39w2c26_tnr, 
                              k39w3c26_tnr), 
                      Truescore = c(k11w1c26_truescore, k11w2c26_truescore, 
                                    k11w3c26_truescore, k13w1c26_truescore, 
                                    k13w2c26_truescore, k13w3c26_truescore, 
                                    k15w1c26_truescore, k15w2c26_truescore, 
                                    k15w3c26_truescore, k17w1c26_truescore, 
                                    k17w2c26_truescore, k17w3c26_truescore, 
                                    k19w1c26_truescore, k19w2c26_truescore, 
                                    k19w3c26_truescore, k21w1c26_truescore, 
                                    k21w2c26_truescore, k21w3c26_truescore, 
                                    k23w1c26_truescore, k23w2c26_truescore, 
                                    k23w3c26_truescore, k25w1c26_truescore, 
                                    k25w2c26_truescore, k25w3c26_truescore, 
                                    k27w1c26_truescore, k27w2c26_truescore, 
                                    k27w3c26_truescore, k29w1c26_truescore, 
                                    k29w2c26_truescore, k29w3c26_truescore, 
                                    k31w1c26_truescore, k31w2c26_truescore, 
                                    k31w3c26_truescore, k33w1c26_truescore, 
                                    k33w2c26_truescore, k33w3c26_truescore, 
                                    k35w1c26_truescore, k35w2c26_truescore, 
                                    k35w3c26_truescore, k37w1c26_truescore, 
                                    k37w2c26_truescore, k37w3c26_truescore, 
                                    k39w1c26_truescore, k39w2c26_truescore, 
                                    k39w3c26_truescore))

knitr::kable(c26_results[1:45, ], caption = "c26_results")

ggplot(c26_results, aes(k, Truescore, shape = Kernel, color = Kernel)) + 
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.26 (Truescore)")

# For the Cutoff of 0.26, compute the Minimum Distance to (0, 1) for each 
# combination of k and Kernel. 

c26_results <- c26_results %>% mutate(Distance = sqrt((1 - TPR)^2 + (1 - TNR)^2))

ggplot(c26_results, aes(k, Distance, shape = Kernel, color = Kernel)) +
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.26 (Distance)")

# For the Cutoff of 0.26, identify the optimal combination of values for k and Kernel 
# based on each of our assessment methods, Truescore and Mimimum Distance to (0, 1).

max(c26_results$Truescore)
(c26_opt_k_ts <- c26_results$k[which.max(c26_results$Truescore)])
(c26_opt_kernel_ts <- c26_results$Kernel[which.max(c26_results$Truescore)])
(c26_opt_cut_ts <- c26_results$Cut[which.max(c26_results$Truescore)])
(c26_opt_tpr_ts <- c26_results$TPR[which.max(c26_results$Truescore)])
(c26_opt_tnr_ts <- c26_results$TNR[which.max(c26_results$Truescore)])
(c26_opt_d_ts <- c26_results$Distance[which.max(c26_results$Truescore)])

min(c26_results$Distance)
(c26_opt_k_dist <- c26_results$k[which.min(c26_results$Distance)])
(c26_opt_kernel_dist <- c26_results$Kernel[which.min(c26_results$Distance)])
(c26_opt_cut_dist <- c26_results$Cut[which.min(c26_results$Distance)])
(c26_opt_tpr_dist <- c26_results$TPR[which.min(c26_results$Distance)])
(c26_opt_tnr_dist <- c26_results$TNR[which.min(c26_results$Distance)])
(c26_opt_t_dist <- c26_results$Truescore[which.min(c26_results$Distance)])

############################
# 0.27 Cutoff
############################

# For the decision cutoff of 0.27, generate a confusion matrix for
# every combination of k (11:39 by two), kernel (1:3) and fold (1:10).

cm_c27 <- sapply(kwf_dfs_l, function(x) {
  ss <- subset(x, select = c(pred27, obs))
  confusionMatrix(ss$pred27, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c27) <- kwf_dfs_v

cm_c27_tables <- sapply(cm_c27, "[[", 2)
cm_c27_tables <- as_tibble(cm_c27_tables)

# For each combination of k and kernel, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each combination of k and kernel when the decision cutoff is 0.27.

(k11w1c27_tpr <- round(sum(cm_c27_tables[1, 1:10]) / (sum(cm_c27_tables[1, 1:10]) + sum(cm_c27_tables[2, 1:10])), 6))
(k11w1c27_tnr <- round(sum(cm_c27_tables[4, 1:10]) / (sum(cm_c27_tables[4, 1:10]) + sum(cm_c27_tables[3, 1:10])), 6))
(k11w1c27_truescore <- round((2 * k11w1c27_tpr * k11w1c27_tnr) / (k11w1c27_tpr + k11w1c27_tnr), 6))

(k11w2c27_tpr <- round(sum(cm_c27_tables[1, 11:20]) / (sum(cm_c27_tables[1, 11:20]) + sum(cm_c27_tables[2, 11:20])), 6))
(k11w2c27_tnr <- round(sum(cm_c27_tables[4, 11:20]) / (sum(cm_c27_tables[4, 11:20]) + sum(cm_c27_tables[3, 11:20])), 6))
(k11w2c27_truescore <- round((2 * k11w2c27_tpr * k11w2c27_tnr) / (k11w2c27_tpr + k11w2c27_tnr), 6))

(k11w3c27_tpr <- round(sum(cm_c27_tables[1, 21:30]) / (sum(cm_c27_tables[1, 21:30]) + sum(cm_c27_tables[2, 21:30])), 6))
(k11w3c27_tnr <- round(sum(cm_c27_tables[4, 21:30]) / (sum(cm_c27_tables[4, 21:30]) + sum(cm_c27_tables[3, 21:30])), 6))
(k11w3c27_truescore <- round((2 * k11w3c27_tpr * k11w3c27_tnr) / (k11w3c27_tpr + k11w3c27_tnr), 6))


(k13w1c27_tpr <- round(sum(cm_c27_tables[1, 31:40]) / (sum(cm_c27_tables[1, 31:40]) + sum(cm_c27_tables[2, 31:40])), 6))
(k13w1c27_tnr <- round(sum(cm_c27_tables[4, 31:40]) / (sum(cm_c27_tables[4, 31:40]) + sum(cm_c27_tables[3, 31:40])), 6))
(k13w1c27_truescore <- round((2 * k13w1c27_tpr * k13w1c27_tnr) / (k13w1c27_tpr + k13w1c27_tnr), 6))

(k13w2c27_tpr <- round(sum(cm_c27_tables[1, 41:50]) / (sum(cm_c27_tables[1, 41:50]) + sum(cm_c27_tables[2, 41:50])), 6))
(k13w2c27_tnr <- round(sum(cm_c27_tables[4, 41:50]) / (sum(cm_c27_tables[4, 41:50]) + sum(cm_c27_tables[3, 41:50])), 6))
(k13w2c27_truescore <- round((2 * k13w2c27_tpr * k13w2c27_tnr) / (k13w2c27_tpr + k13w2c27_tnr), 6))

(k13w3c27_tpr <- round(sum(cm_c27_tables[1, 51:60]) / (sum(cm_c27_tables[1, 51:60]) + sum(cm_c27_tables[2, 51:60])), 6))
(k13w3c27_tnr <- round(sum(cm_c27_tables[4, 51:60]) / (sum(cm_c27_tables[4, 51:60]) + sum(cm_c27_tables[3, 51:60])), 6))
(k13w3c27_truescore <- round((2 * k13w3c27_tpr * k13w3c27_tnr) / (k13w3c27_tpr + k13w3c27_tnr), 6))


(k15w1c27_tpr <- round(sum(cm_c27_tables[1, 61:70]) / (sum(cm_c27_tables[1, 61:70]) + sum(cm_c27_tables[2, 61:70])), 6))
(k15w1c27_tnr <- round(sum(cm_c27_tables[4, 61:70]) / (sum(cm_c27_tables[4, 61:70]) + sum(cm_c27_tables[3, 61:70])), 6))
(k15w1c27_truescore <- round((2 * k15w1c27_tpr * k15w1c27_tnr) / (k15w1c27_tpr + k15w1c27_tnr), 6))

(k15w2c27_tpr <- round(sum(cm_c27_tables[1, 71:80]) / (sum(cm_c27_tables[1, 71:80]) + sum(cm_c27_tables[2, 71:80])), 6))
(k15w2c27_tnr <- round(sum(cm_c27_tables[4, 71:80]) / (sum(cm_c27_tables[4, 71:80]) + sum(cm_c27_tables[3, 71:80])), 6))
(k15w2c27_truescore <- round((2 * k15w2c27_tpr * k15w2c27_tnr) / (k15w2c27_tpr + k15w2c27_tnr), 6))

(k15w3c27_tpr <- round(sum(cm_c27_tables[1, 81:90]) / (sum(cm_c27_tables[1, 81:90]) + sum(cm_c27_tables[2, 81:90])), 6))
(k15w3c27_tnr <- round(sum(cm_c27_tables[4, 81:90]) / (sum(cm_c27_tables[4, 81:90]) + sum(cm_c27_tables[3, 81:90])), 6))
(k15w3c27_truescore <- round((2 * k15w3c27_tpr * k15w3c27_tnr) / (k15w3c27_tpr + k15w3c27_tnr), 6))


(k17w1c27_tpr <- round(sum(cm_c27_tables[1, 91:100]) / (sum(cm_c27_tables[1, 91:100]) + sum(cm_c27_tables[2, 91:100])), 6))
(k17w1c27_tnr <- round(sum(cm_c27_tables[4, 91:100]) / (sum(cm_c27_tables[4, 91:100]) + sum(cm_c27_tables[3, 91:100])), 6))
(k17w1c27_truescore <- round((2 * k17w1c27_tpr * k17w1c27_tnr) / (k17w1c27_tpr + k17w1c27_tnr), 6))

(k17w2c27_tpr <- round(sum(cm_c27_tables[1, 101:110]) / (sum(cm_c27_tables[1, 101:110]) + sum(cm_c27_tables[2, 101:110])), 6))
(k17w2c27_tnr <- round(sum(cm_c27_tables[4, 101:110]) / (sum(cm_c27_tables[4, 101:110]) + sum(cm_c27_tables[3, 101:110])), 6))
(k17w2c27_truescore <- round((2 * k17w2c27_tpr * k17w2c27_tnr) / (k17w2c27_tpr + k17w2c27_tnr), 6))

(k17w3c27_tpr <- round(sum(cm_c27_tables[1, 111:120]) / (sum(cm_c27_tables[1, 111:120]) + sum(cm_c27_tables[2, 111:120])), 6))
(k17w3c27_tnr <- round(sum(cm_c27_tables[4, 111:120]) / (sum(cm_c27_tables[4, 111:120]) + sum(cm_c27_tables[3, 111:120])), 6))
(k17w3c27_truescore <- round((2 * k17w3c27_tpr * k17w3c27_tnr) / (k17w3c27_tpr + k17w3c27_tnr), 6))


(k19w1c27_tpr <- round(sum(cm_c27_tables[1, 121:130]) / (sum(cm_c27_tables[1, 121:130]) + sum(cm_c27_tables[2, 121:130])), 6))
(k19w1c27_tnr <- round(sum(cm_c27_tables[4, 121:130]) / (sum(cm_c27_tables[4, 121:130]) + sum(cm_c27_tables[3, 121:130])), 6))
(k19w1c27_truescore <- round((2 * k19w1c27_tpr * k19w1c27_tnr) / (k19w1c27_tpr + k19w1c27_tnr), 6))

(k19w2c27_tpr <- round(sum(cm_c27_tables[1, 131:140]) / (sum(cm_c27_tables[1, 131:140]) + sum(cm_c27_tables[2, 131:140])), 6))
(k19w2c27_tnr <- round(sum(cm_c27_tables[4, 131:140]) / (sum(cm_c27_tables[4, 131:140]) + sum(cm_c27_tables[3, 131:140])), 6))
(k19w2c27_truescore <- round((2 * k19w2c27_tpr * k19w2c27_tnr) / (k19w2c27_tpr + k19w2c27_tnr), 6))

(k19w3c27_tpr <- round(sum(cm_c27_tables[1, 141:150]) / (sum(cm_c27_tables[1, 141:150]) + sum(cm_c27_tables[2, 141:150])), 6))
(k19w3c27_tnr <- round(sum(cm_c27_tables[4, 141:150]) / (sum(cm_c27_tables[4, 141:150]) + sum(cm_c27_tables[3, 141:150])), 6))
(k19w3c27_truescore <- round((2 * k19w3c27_tpr * k19w3c27_tnr) / (k19w3c27_tpr + k19w3c27_tnr), 6))


(k21w1c27_tpr <- round(sum(cm_c27_tables[1, 151:160]) / (sum(cm_c27_tables[1, 151:160]) + sum(cm_c27_tables[2, 151:160])), 6))
(k21w1c27_tnr <- round(sum(cm_c27_tables[4, 151:160]) / (sum(cm_c27_tables[4, 151:160]) + sum(cm_c27_tables[3, 151:160])), 6))
(k21w1c27_truescore <- round((2 * k21w1c27_tpr * k21w1c27_tnr) / (k21w1c27_tpr + k21w1c27_tnr), 6))

(k21w2c27_tpr <- round(sum(cm_c27_tables[1, 161:170]) / (sum(cm_c27_tables[1, 161:170]) + sum(cm_c27_tables[2, 161:170])), 6))
(k21w2c27_tnr <- round(sum(cm_c27_tables[4, 161:170]) / (sum(cm_c27_tables[4, 161:170]) + sum(cm_c27_tables[3, 161:170])), 6))
(k21w2c27_truescore <- round((2 * k21w2c27_tpr * k21w2c27_tnr) / (k21w2c27_tpr + k21w2c27_tnr), 6))

(k21w3c27_tpr <- round(sum(cm_c27_tables[1, 171:180]) / (sum(cm_c27_tables[1, 171:180]) + sum(cm_c27_tables[2, 171:180])), 6))
(k21w3c27_tnr <- round(sum(cm_c27_tables[4, 171:180]) / (sum(cm_c27_tables[4, 171:180]) + sum(cm_c27_tables[3, 171:180])), 6))
(k21w3c27_truescore <- round((2 * k21w3c27_tpr * k21w3c27_tnr) / (k21w3c27_tpr + k21w3c27_tnr), 6))


(k23w1c27_tpr <- round(sum(cm_c27_tables[1, 181:190]) / (sum(cm_c27_tables[1, 181:190]) + sum(cm_c27_tables[2, 181:190])), 6))
(k23w1c27_tnr <- round(sum(cm_c27_tables[4, 181:190]) / (sum(cm_c27_tables[4, 181:190]) + sum(cm_c27_tables[3, 181:190])), 6))
(k23w1c27_truescore <- round((2 * k23w1c27_tpr * k23w1c27_tnr) / (k23w1c27_tpr + k23w1c27_tnr), 6))

(k23w2c27_tpr <- round(sum(cm_c27_tables[1, 191:200]) / (sum(cm_c27_tables[1, 191:200]) + sum(cm_c27_tables[2, 191:200])), 6))
(k23w2c27_tnr <- round(sum(cm_c27_tables[4, 191:200]) / (sum(cm_c27_tables[4, 191:200]) + sum(cm_c27_tables[3, 191:200])), 6))
(k23w2c27_truescore <- round((2 * k23w2c27_tpr * k23w2c27_tnr) / (k23w2c27_tpr + k23w2c27_tnr), 6))

(k23w3c27_tpr <- round(sum(cm_c27_tables[1, 201:210]) / (sum(cm_c27_tables[1, 201:210]) + sum(cm_c27_tables[2, 201:210])), 6))
(k23w3c27_tnr <- round(sum(cm_c27_tables[4, 201:210]) / (sum(cm_c27_tables[4, 201:210]) + sum(cm_c27_tables[3, 201:210])), 6))
(k23w3c27_truescore <- round((2 * k23w3c27_tpr * k23w3c27_tnr) / (k23w3c27_tpr + k23w3c27_tnr), 6))


(k25w1c27_tpr <- round(sum(cm_c27_tables[1, 211:220]) / (sum(cm_c27_tables[1, 211:220]) + sum(cm_c27_tables[2, 211:220])), 6))
(k25w1c27_tnr <- round(sum(cm_c27_tables[4, 211:220]) / (sum(cm_c27_tables[4, 211:220]) + sum(cm_c27_tables[3, 211:220])), 6))
(k25w1c27_truescore <- round((2 * k25w1c27_tpr * k25w1c27_tnr) / (k25w1c27_tpr + k25w1c27_tnr), 6))

(k25w2c27_tpr <- round(sum(cm_c27_tables[1, 221:230]) / (sum(cm_c27_tables[1, 221:230]) + sum(cm_c27_tables[2, 221:230])), 6))
(k25w2c27_tnr <- round(sum(cm_c27_tables[4, 221:230]) / (sum(cm_c27_tables[4, 221:230]) + sum(cm_c27_tables[3, 221:230])), 6))
(k25w2c27_truescore <- round((2 * k25w2c27_tpr * k25w2c27_tnr) / (k25w2c27_tpr + k25w2c27_tnr), 6))

(k25w3c27_tpr <- round(sum(cm_c27_tables[1, 231:240]) / (sum(cm_c27_tables[1, 231:240]) + sum(cm_c27_tables[2, 231:240])), 6))
(k25w3c27_tnr <- round(sum(cm_c27_tables[4, 231:240]) / (sum(cm_c27_tables[4, 231:240]) + sum(cm_c27_tables[3, 231:240])), 6))
(k25w3c27_truescore <- round((2 * k25w3c27_tpr * k25w3c27_tnr) / (k25w3c27_tpr + k25w3c27_tnr), 6))


(k27w1c27_tpr <- round(sum(cm_c27_tables[1, 241:250]) / (sum(cm_c27_tables[1, 241:250]) + sum(cm_c27_tables[2, 241:250])), 6))
(k27w1c27_tnr <- round(sum(cm_c27_tables[4, 241:250]) / (sum(cm_c27_tables[4, 241:250]) + sum(cm_c27_tables[3, 241:250])), 6))
(k27w1c27_truescore <- round((2 * k27w1c27_tpr * k27w1c27_tnr) / (k27w1c27_tpr + k27w1c27_tnr), 6))

(k27w2c27_tpr <- round(sum(cm_c27_tables[1, 251:260]) / (sum(cm_c27_tables[1, 251:260]) + sum(cm_c27_tables[2, 251:260])), 6))
(k27w2c27_tnr <- round(sum(cm_c27_tables[4, 251:260]) / (sum(cm_c27_tables[4, 251:260]) + sum(cm_c27_tables[3, 251:260])), 6))
(k27w2c27_truescore <- round((2 * k27w2c27_tpr * k27w2c27_tnr) / (k27w2c27_tpr + k27w2c27_tnr), 6))

(k27w3c27_tpr <- round(sum(cm_c27_tables[1, 261:270]) / (sum(cm_c27_tables[1, 261:270]) + sum(cm_c27_tables[2, 261:270])), 6))
(k27w3c27_tnr <- round(sum(cm_c27_tables[4, 261:270]) / (sum(cm_c27_tables[4, 261:270]) + sum(cm_c27_tables[3, 261:270])), 6))
(k27w3c27_truescore <- round((2 * k27w3c27_tpr * k27w3c27_tnr) / (k27w3c27_tpr + k27w3c27_tnr), 6))


(k29w1c27_tpr <- round(sum(cm_c27_tables[1, 271:280]) / (sum(cm_c27_tables[1, 271:280]) + sum(cm_c27_tables[2, 271:280])), 6))
(k29w1c27_tnr <- round(sum(cm_c27_tables[4, 271:280]) / (sum(cm_c27_tables[4, 271:280]) + sum(cm_c27_tables[3, 271:280])), 6))
(k29w1c27_truescore <- round((2 * k29w1c27_tpr * k29w1c27_tnr) / (k29w1c27_tpr + k29w1c27_tnr), 6))

(k29w2c27_tpr <- round(sum(cm_c27_tables[1, 281:290]) / (sum(cm_c27_tables[1, 281:290]) + sum(cm_c27_tables[2, 281:290])), 6))
(k29w2c27_tnr <- round(sum(cm_c27_tables[4, 281:290]) / (sum(cm_c27_tables[4, 281:290]) + sum(cm_c27_tables[3, 281:290])), 6))
(k29w2c27_truescore <- round((2 * k29w2c27_tpr * k29w2c27_tnr) / (k29w2c27_tpr + k29w2c27_tnr), 6))

(k29w3c27_tpr <- round(sum(cm_c27_tables[1, 291:300]) / (sum(cm_c27_tables[1, 291:300]) + sum(cm_c27_tables[2, 291:300])), 6))
(k29w3c27_tnr <- round(sum(cm_c27_tables[4, 291:300]) / (sum(cm_c27_tables[4, 291:300]) + sum(cm_c27_tables[3, 291:300])), 6))
(k29w3c27_truescore <- round((2 * k29w3c27_tpr * k29w3c27_tnr) / (k29w3c27_tpr + k29w3c27_tnr), 6))


(k31w1c27_tpr <- round(sum(cm_c27_tables[1, 301:310]) / (sum(cm_c27_tables[1, 301:310]) + sum(cm_c27_tables[2, 301:310])), 6))
(k31w1c27_tnr <- round(sum(cm_c27_tables[4, 301:310]) / (sum(cm_c27_tables[4, 301:310]) + sum(cm_c27_tables[3, 301:310])), 6))
(k31w1c27_truescore <- round((2 * k31w1c27_tpr * k31w1c27_tnr) / (k31w1c27_tpr + k31w1c27_tnr), 6))

(k31w2c27_tpr <- round(sum(cm_c27_tables[1, 311:320]) / (sum(cm_c27_tables[1, 311:320]) + sum(cm_c27_tables[2, 311:320])), 6))
(k31w2c27_tnr <- round(sum(cm_c27_tables[4, 311:320]) / (sum(cm_c27_tables[4, 311:320]) + sum(cm_c27_tables[3, 311:320])), 6))
(k31w2c27_truescore <- round((2 * k31w2c27_tpr * k31w2c27_tnr) / (k31w2c27_tpr + k31w2c27_tnr), 6))

(k31w3c27_tpr <- round(sum(cm_c27_tables[1, 321:330]) / (sum(cm_c27_tables[1, 321:330]) + sum(cm_c27_tables[2, 321:330])), 6))
(k31w3c27_tnr <- round(sum(cm_c27_tables[4, 321:330]) / (sum(cm_c27_tables[4, 321:330]) + sum(cm_c27_tables[3, 321:330])), 6))
(k31w3c27_truescore <- round((2 * k31w3c27_tpr * k31w3c27_tnr) / (k31w3c27_tpr + k31w3c27_tnr), 6))


(k33w1c27_tpr <- round(sum(cm_c27_tables[1, 331:340]) / (sum(cm_c27_tables[1, 331:340]) + sum(cm_c27_tables[2, 331:340])), 6))
(k33w1c27_tnr <- round(sum(cm_c27_tables[4, 331:340]) / (sum(cm_c27_tables[4, 331:340]) + sum(cm_c27_tables[3, 331:340])), 6))
(k33w1c27_truescore <- round((2 * k33w1c27_tpr * k33w1c27_tnr) / (k33w1c27_tpr + k33w1c27_tnr), 6))

(k33w2c27_tpr <- round(sum(cm_c27_tables[1, 341:350]) / (sum(cm_c27_tables[1, 341:350]) + sum(cm_c27_tables[2, 341:350])), 6))
(k33w2c27_tnr <- round(sum(cm_c27_tables[4, 341:350]) / (sum(cm_c27_tables[4, 341:350]) + sum(cm_c27_tables[3, 341:350])), 6))
(k33w2c27_truescore <- round((2 * k33w2c27_tpr * k33w2c27_tnr) / (k33w2c27_tpr + k33w2c27_tnr), 6))

(k33w3c27_tpr <- round(sum(cm_c27_tables[1, 351:360]) / (sum(cm_c27_tables[1, 351:360]) + sum(cm_c27_tables[2, 351:360])), 6))
(k33w3c27_tnr <- round(sum(cm_c27_tables[4, 351:360]) / (sum(cm_c27_tables[4, 351:360]) + sum(cm_c27_tables[3, 351:360])), 6))
(k33w3c27_truescore <- round((2 * k33w3c27_tpr * k33w3c27_tnr) / (k33w3c27_tpr + k33w3c27_tnr), 6))


(k35w1c27_tpr <- round(sum(cm_c27_tables[1, 361:370]) / (sum(cm_c27_tables[1, 361:370]) + sum(cm_c27_tables[2, 361:370])), 6))
(k35w1c27_tnr <- round(sum(cm_c27_tables[4, 361:370]) / (sum(cm_c27_tables[4, 361:370]) + sum(cm_c27_tables[3, 361:370])), 6))
(k35w1c27_truescore <- round((2 * k35w1c27_tpr * k35w1c27_tnr) / (k35w1c27_tpr + k35w1c27_tnr), 6))

(k35w2c27_tpr <- round(sum(cm_c27_tables[1, 371:380]) / (sum(cm_c27_tables[1, 371:380]) + sum(cm_c27_tables[2, 371:380])), 6))
(k35w2c27_tnr <- round(sum(cm_c27_tables[4, 371:380]) / (sum(cm_c27_tables[4, 371:380]) + sum(cm_c27_tables[3, 371:380])), 6))
(k35w2c27_truescore <- round((2 * k35w2c27_tpr * k35w2c27_tnr) / (k35w2c27_tpr + k35w2c27_tnr), 6))

(k35w3c27_tpr <- round(sum(cm_c27_tables[1, 381:390]) / (sum(cm_c27_tables[1, 381:390]) + sum(cm_c27_tables[2, 381:390])), 6))
(k35w3c27_tnr <- round(sum(cm_c27_tables[4, 381:390]) / (sum(cm_c27_tables[4, 381:390]) + sum(cm_c27_tables[3, 381:390])), 6))
(k35w3c27_truescore <- round((2 * k35w3c27_tpr * k35w3c27_tnr) / (k35w3c27_tpr + k35w3c27_tnr), 6))


(k37w1c27_tpr <- round(sum(cm_c27_tables[1, 391:400]) / (sum(cm_c27_tables[1, 391:400]) + sum(cm_c27_tables[2, 391:400])), 6))
(k37w1c27_tnr <- round(sum(cm_c27_tables[4, 391:400]) / (sum(cm_c27_tables[4, 391:400]) + sum(cm_c27_tables[3, 391:400])), 6))
(k37w1c27_truescore <- round((2 * k37w1c27_tpr * k37w1c27_tnr) / (k37w1c27_tpr + k37w1c27_tnr), 6))

(k37w2c27_tpr <- round(sum(cm_c27_tables[1, 401:410]) / (sum(cm_c27_tables[1, 401:410]) + sum(cm_c27_tables[2, 401:410])), 6))
(k37w2c27_tnr <- round(sum(cm_c27_tables[4, 401:410]) / (sum(cm_c27_tables[4, 401:410]) + sum(cm_c27_tables[3, 401:410])), 6))
(k37w2c27_truescore <- round((2 * k37w2c27_tpr * k37w2c27_tnr) / (k37w2c27_tpr + k37w2c27_tnr), 6))

(k37w3c27_tpr <- round(sum(cm_c27_tables[1, 411:420]) / (sum(cm_c27_tables[1, 411:420]) + sum(cm_c27_tables[2, 411:420])), 6))
(k37w3c27_tnr <- round(sum(cm_c27_tables[4, 411:420]) / (sum(cm_c27_tables[4, 411:420]) + sum(cm_c27_tables[3, 411:420])), 6))
(k37w3c27_truescore <- round((2 * k37w3c27_tpr * k37w3c27_tnr) / (k37w3c27_tpr + k37w3c27_tnr), 6))


(k39w1c27_tpr <- round(sum(cm_c27_tables[1, 421:430]) / (sum(cm_c27_tables[1, 421:430]) + sum(cm_c27_tables[2, 421:430])), 6))
(k39w1c27_tnr <- round(sum(cm_c27_tables[4, 421:430]) / (sum(cm_c27_tables[4, 421:430]) + sum(cm_c27_tables[3, 421:430])), 6))
(k39w1c27_truescore <- round((2 * k39w1c27_tpr * k39w1c27_tnr) / (k39w1c27_tpr + k39w1c27_tnr), 6))

(k39w2c27_tpr <- round(sum(cm_c27_tables[1, 431:440]) / (sum(cm_c27_tables[1, 431:440]) + sum(cm_c27_tables[2, 431:440])), 6))
(k39w2c27_tnr <- round(sum(cm_c27_tables[4, 431:440]) / (sum(cm_c27_tables[4, 431:440]) + sum(cm_c27_tables[3, 431:440])), 6))
(k39w2c27_truescore <- round((2 * k39w2c27_tpr * k39w2c27_tnr) / (k39w2c27_tpr + k39w2c27_tnr), 6))

(k39w3c27_tpr <- round(sum(cm_c27_tables[1, 441:450]) / (sum(cm_c27_tables[1, 441:450]) + sum(cm_c27_tables[2, 441:450])), 6))
(k39w3c27_tnr <- round(sum(cm_c27_tables[4, 441:450]) / (sum(cm_c27_tables[4, 441:450]) + sum(cm_c27_tables[3, 441:450])), 6))
(k39w3c27_truescore <- round((2 * k39w3c27_tpr * k39w3c27_tnr) / (k39w3c27_tpr + k39w3c27_tnr), 6))


# Compile the 0.27 cutoff results in a table, and identify the combination of 
# of k and kernel that maximizes the Truescore.

c27_results <- tibble(k = c(11, 11, 11, 13, 13, 13, 15, 15, 15, 
                            17, 17, 17, 19, 19, 19, 21, 21, 21, 
                            23, 23, 23, 25, 25, 25, 27, 27, 27, 
                            29, 29, 29, 31, 31, 31, 33, 33, 33, 
                            35, 35, 35, 37, 37, 37, 39, 39, 39), 
                      Kernel = rep(c("triangular", "gaussian", "optimal"), 15), 
                      Cut = 0.27, 
                      TPR = c(k11w1c27_tpr, k11w2c27_tpr, k11w3c27_tpr, k13w1c27_tpr, 
                              k13w2c27_tpr, k13w3c27_tpr, k15w1c27_tpr, k15w2c27_tpr, 
                              k15w3c27_tpr, k17w1c27_tpr, k17w2c27_tpr, k17w3c27_tpr, 
                              k19w1c27_tpr, k19w2c27_tpr, k19w3c27_tpr, k21w1c27_tpr, 
                              k21w2c27_tpr, k21w3c27_tpr, k23w1c27_tpr, k23w2c27_tpr, 
                              k23w3c27_tpr, k25w1c27_tpr, k25w2c27_tpr, k25w3c27_tpr, 
                              k27w1c27_tpr, k27w2c27_tpr, k27w3c27_tpr, k29w1c27_tpr, 
                              k29w2c27_tpr, k29w3c27_tpr, k31w1c27_tpr, k31w2c27_tpr, 
                              k31w3c27_tpr, k33w1c27_tpr, k33w2c27_tpr, k33w3c27_tpr, 
                              k35w1c27_tpr, k35w2c27_tpr, k35w3c27_tpr, k37w1c27_tpr, 
                              k37w2c27_tpr, k37w3c27_tpr, k39w1c27_tpr, k39w2c27_tpr, 
                              k39w3c27_tpr), 
                      TNR = c(k11w1c27_tnr, k11w2c27_tnr, k11w3c27_tnr, k13w1c27_tnr, 
                              k13w2c27_tnr, k13w3c27_tnr, k15w1c27_tnr, k15w2c27_tnr, 
                              k15w3c27_tnr, k17w1c27_tnr, k17w2c27_tnr, k17w3c27_tnr, 
                              k19w1c27_tnr, k19w2c27_tnr, k19w3c27_tnr, k21w1c27_tnr, 
                              k21w2c27_tnr, k21w3c27_tnr, k23w1c27_tnr, k23w2c27_tnr, 
                              k23w3c27_tnr, k25w1c27_tnr, k25w2c27_tnr, k25w3c27_tnr, 
                              k27w1c27_tnr, k27w2c27_tnr, k27w3c27_tnr, k29w1c27_tnr, 
                              k29w2c27_tnr, k29w3c27_tnr, k31w1c27_tnr, k31w2c27_tnr, 
                              k31w3c27_tnr, k33w1c27_tnr, k33w2c27_tnr, k33w3c27_tnr, 
                              k35w1c27_tnr, k35w2c27_tnr, k35w3c27_tnr, k37w1c27_tnr, 
                              k37w2c27_tnr, k37w3c27_tnr, k39w1c27_tnr, k39w2c27_tnr, 
                              k39w3c27_tnr), 
                      Truescore = c(k11w1c27_truescore, k11w2c27_truescore, 
                                    k11w3c27_truescore, k13w1c27_truescore, 
                                    k13w2c27_truescore, k13w3c27_truescore, 
                                    k15w1c27_truescore, k15w2c27_truescore, 
                                    k15w3c27_truescore, k17w1c27_truescore, 
                                    k17w2c27_truescore, k17w3c27_truescore, 
                                    k19w1c27_truescore, k19w2c27_truescore, 
                                    k19w3c27_truescore, k21w1c27_truescore, 
                                    k21w2c27_truescore, k21w3c27_truescore, 
                                    k23w1c27_truescore, k23w2c27_truescore, 
                                    k23w3c27_truescore, k25w1c27_truescore, 
                                    k25w2c27_truescore, k25w3c27_truescore, 
                                    k27w1c27_truescore, k27w2c27_truescore, 
                                    k27w3c27_truescore, k29w1c27_truescore, 
                                    k29w2c27_truescore, k29w3c27_truescore, 
                                    k31w1c27_truescore, k31w2c27_truescore, 
                                    k31w3c27_truescore, k33w1c27_truescore, 
                                    k33w2c27_truescore, k33w3c27_truescore, 
                                    k35w1c27_truescore, k35w2c27_truescore, 
                                    k35w3c27_truescore, k37w1c27_truescore, 
                                    k37w2c27_truescore, k37w3c27_truescore, 
                                    k39w1c27_truescore, k39w2c27_truescore, 
                                    k39w3c27_truescore))

knitr::kable(c27_results[1:45, ], caption = "c27_results")

ggplot(c27_results, aes(k, Truescore, shape = Kernel, color = Kernel)) + 
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.27 (Truescore)")

# For the Cutoff of 0.27, compute the Minimum Distance to (0, 1) for each 
# combination of k and Kernel. 

c27_results <- c27_results %>% mutate(Distance = sqrt((1 - TPR)^2 + (1 - TNR)^2))

ggplot(c27_results, aes(k, Distance, shape = Kernel, color = Kernel)) +
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.27 (Distance)")

# For the Cutoff of 0.27, identify the optimal combination of values for k and Kernel 
# based on each of our assessment methods, Truescore and Mimimum Distance to (0, 1).

max(c27_results$Truescore)
(c27_opt_k_ts <- c27_results$k[which.max(c27_results$Truescore)])
(c27_opt_kernel_ts <- c27_results$Kernel[which.max(c27_results$Truescore)])
(c27_opt_cut_ts <- c27_results$Cut[which.max(c27_results$Truescore)])
(c27_opt_tpr_ts <- c27_results$TPR[which.max(c27_results$Truescore)])
(c27_opt_tnr_ts <- c27_results$TNR[which.max(c27_results$Truescore)])
(c27_opt_d_ts <- c27_results$Distance[which.max(c27_results$Truescore)])

min(c27_results$Distance)
(c27_opt_k_dist <- c27_results$k[which.min(c27_results$Distance)])
(c27_opt_kernel_dist <- c27_results$Kernel[which.min(c27_results$Distance)])
(c27_opt_cut_dist <- c27_results$Cut[which.min(c27_results$Distance)])
(c27_opt_tpr_dist <- c27_results$TPR[which.min(c27_results$Distance)])
(c27_opt_tnr_dist <- c27_results$TNR[which.min(c27_results$Distance)])
(c27_opt_t_dist <- c27_results$Truescore[which.min(c27_results$Distance)])

############################
# 0.28 Cutoff
############################

# For the decision cutoff of 0.28, generate a confusion matrix for
# every combination of k (11:39 by two), kernel (1:3) and fold (1:10).

cm_c28 <- sapply(kwf_dfs_l, function(x) {
  ss <- subset(x, select = c(pred28, obs))
  confusionMatrix(ss$pred28, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c28) <- kwf_dfs_v

cm_c28_tables <- sapply(cm_c28, "[[", 2)
cm_c28_tables <- as_tibble(cm_c28_tables)

# For each combination of k and kernel, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each combination of k and kernel when the decision cutoff is 0.28.

(k11w1c28_tpr <- round(sum(cm_c28_tables[1, 1:10]) / (sum(cm_c28_tables[1, 1:10]) + sum(cm_c28_tables[2, 1:10])), 6))
(k11w1c28_tnr <- round(sum(cm_c28_tables[4, 1:10]) / (sum(cm_c28_tables[4, 1:10]) + sum(cm_c28_tables[3, 1:10])), 6))
(k11w1c28_truescore <- round((2 * k11w1c28_tpr * k11w1c28_tnr) / (k11w1c28_tpr + k11w1c28_tnr), 6))

(k11w2c28_tpr <- round(sum(cm_c28_tables[1, 11:20]) / (sum(cm_c28_tables[1, 11:20]) + sum(cm_c28_tables[2, 11:20])), 6))
(k11w2c28_tnr <- round(sum(cm_c28_tables[4, 11:20]) / (sum(cm_c28_tables[4, 11:20]) + sum(cm_c28_tables[3, 11:20])), 6))
(k11w2c28_truescore <- round((2 * k11w2c28_tpr * k11w2c28_tnr) / (k11w2c28_tpr + k11w2c28_tnr), 6))

(k11w3c28_tpr <- round(sum(cm_c28_tables[1, 21:30]) / (sum(cm_c28_tables[1, 21:30]) + sum(cm_c28_tables[2, 21:30])), 6))
(k11w3c28_tnr <- round(sum(cm_c28_tables[4, 21:30]) / (sum(cm_c28_tables[4, 21:30]) + sum(cm_c28_tables[3, 21:30])), 6))
(k11w3c28_truescore <- round((2 * k11w3c28_tpr * k11w3c28_tnr) / (k11w3c28_tpr + k11w3c28_tnr), 6))


(k13w1c28_tpr <- round(sum(cm_c28_tables[1, 31:40]) / (sum(cm_c28_tables[1, 31:40]) + sum(cm_c28_tables[2, 31:40])), 6))
(k13w1c28_tnr <- round(sum(cm_c28_tables[4, 31:40]) / (sum(cm_c28_tables[4, 31:40]) + sum(cm_c28_tables[3, 31:40])), 6))
(k13w1c28_truescore <- round((2 * k13w1c28_tpr * k13w1c28_tnr) / (k13w1c28_tpr + k13w1c28_tnr), 6))

(k13w2c28_tpr <- round(sum(cm_c28_tables[1, 41:50]) / (sum(cm_c28_tables[1, 41:50]) + sum(cm_c28_tables[2, 41:50])), 6))
(k13w2c28_tnr <- round(sum(cm_c28_tables[4, 41:50]) / (sum(cm_c28_tables[4, 41:50]) + sum(cm_c28_tables[3, 41:50])), 6))
(k13w2c28_truescore <- round((2 * k13w2c28_tpr * k13w2c28_tnr) / (k13w2c28_tpr + k13w2c28_tnr), 6))

(k13w3c28_tpr <- round(sum(cm_c28_tables[1, 51:60]) / (sum(cm_c28_tables[1, 51:60]) + sum(cm_c28_tables[2, 51:60])), 6))
(k13w3c28_tnr <- round(sum(cm_c28_tables[4, 51:60]) / (sum(cm_c28_tables[4, 51:60]) + sum(cm_c28_tables[3, 51:60])), 6))
(k13w3c28_truescore <- round((2 * k13w3c28_tpr * k13w3c28_tnr) / (k13w3c28_tpr + k13w3c28_tnr), 6))


(k15w1c28_tpr <- round(sum(cm_c28_tables[1, 61:70]) / (sum(cm_c28_tables[1, 61:70]) + sum(cm_c28_tables[2, 61:70])), 6))
(k15w1c28_tnr <- round(sum(cm_c28_tables[4, 61:70]) / (sum(cm_c28_tables[4, 61:70]) + sum(cm_c28_tables[3, 61:70])), 6))
(k15w1c28_truescore <- round((2 * k15w1c28_tpr * k15w1c28_tnr) / (k15w1c28_tpr + k15w1c28_tnr), 6))

(k15w2c28_tpr <- round(sum(cm_c28_tables[1, 71:80]) / (sum(cm_c28_tables[1, 71:80]) + sum(cm_c28_tables[2, 71:80])), 6))
(k15w2c28_tnr <- round(sum(cm_c28_tables[4, 71:80]) / (sum(cm_c28_tables[4, 71:80]) + sum(cm_c28_tables[3, 71:80])), 6))
(k15w2c28_truescore <- round((2 * k15w2c28_tpr * k15w2c28_tnr) / (k15w2c28_tpr + k15w2c28_tnr), 6))

(k15w3c28_tpr <- round(sum(cm_c28_tables[1, 81:90]) / (sum(cm_c28_tables[1, 81:90]) + sum(cm_c28_tables[2, 81:90])), 6))
(k15w3c28_tnr <- round(sum(cm_c28_tables[4, 81:90]) / (sum(cm_c28_tables[4, 81:90]) + sum(cm_c28_tables[3, 81:90])), 6))
(k15w3c28_truescore <- round((2 * k15w3c28_tpr * k15w3c28_tnr) / (k15w3c28_tpr + k15w3c28_tnr), 6))


(k17w1c28_tpr <- round(sum(cm_c28_tables[1, 91:100]) / (sum(cm_c28_tables[1, 91:100]) + sum(cm_c28_tables[2, 91:100])), 6))
(k17w1c28_tnr <- round(sum(cm_c28_tables[4, 91:100]) / (sum(cm_c28_tables[4, 91:100]) + sum(cm_c28_tables[3, 91:100])), 6))
(k17w1c28_truescore <- round((2 * k17w1c28_tpr * k17w1c28_tnr) / (k17w1c28_tpr + k17w1c28_tnr), 6))

(k17w2c28_tpr <- round(sum(cm_c28_tables[1, 101:110]) / (sum(cm_c28_tables[1, 101:110]) + sum(cm_c28_tables[2, 101:110])), 6))
(k17w2c28_tnr <- round(sum(cm_c28_tables[4, 101:110]) / (sum(cm_c28_tables[4, 101:110]) + sum(cm_c28_tables[3, 101:110])), 6))
(k17w2c28_truescore <- round((2 * k17w2c28_tpr * k17w2c28_tnr) / (k17w2c28_tpr + k17w2c28_tnr), 6))

(k17w3c28_tpr <- round(sum(cm_c28_tables[1, 111:120]) / (sum(cm_c28_tables[1, 111:120]) + sum(cm_c28_tables[2, 111:120])), 6))
(k17w3c28_tnr <- round(sum(cm_c28_tables[4, 111:120]) / (sum(cm_c28_tables[4, 111:120]) + sum(cm_c28_tables[3, 111:120])), 6))
(k17w3c28_truescore <- round((2 * k17w3c28_tpr * k17w3c28_tnr) / (k17w3c28_tpr + k17w3c28_tnr), 6))


(k19w1c28_tpr <- round(sum(cm_c28_tables[1, 121:130]) / (sum(cm_c28_tables[1, 121:130]) + sum(cm_c28_tables[2, 121:130])), 6))
(k19w1c28_tnr <- round(sum(cm_c28_tables[4, 121:130]) / (sum(cm_c28_tables[4, 121:130]) + sum(cm_c28_tables[3, 121:130])), 6))
(k19w1c28_truescore <- round((2 * k19w1c28_tpr * k19w1c28_tnr) / (k19w1c28_tpr + k19w1c28_tnr), 6))

(k19w2c28_tpr <- round(sum(cm_c28_tables[1, 131:140]) / (sum(cm_c28_tables[1, 131:140]) + sum(cm_c28_tables[2, 131:140])), 6))
(k19w2c28_tnr <- round(sum(cm_c28_tables[4, 131:140]) / (sum(cm_c28_tables[4, 131:140]) + sum(cm_c28_tables[3, 131:140])), 6))
(k19w2c28_truescore <- round((2 * k19w2c28_tpr * k19w2c28_tnr) / (k19w2c28_tpr + k19w2c28_tnr), 6))

(k19w3c28_tpr <- round(sum(cm_c28_tables[1, 141:150]) / (sum(cm_c28_tables[1, 141:150]) + sum(cm_c28_tables[2, 141:150])), 6))
(k19w3c28_tnr <- round(sum(cm_c28_tables[4, 141:150]) / (sum(cm_c28_tables[4, 141:150]) + sum(cm_c28_tables[3, 141:150])), 6))
(k19w3c28_truescore <- round((2 * k19w3c28_tpr * k19w3c28_tnr) / (k19w3c28_tpr + k19w3c28_tnr), 6))


(k21w1c28_tpr <- round(sum(cm_c28_tables[1, 151:160]) / (sum(cm_c28_tables[1, 151:160]) + sum(cm_c28_tables[2, 151:160])), 6))
(k21w1c28_tnr <- round(sum(cm_c28_tables[4, 151:160]) / (sum(cm_c28_tables[4, 151:160]) + sum(cm_c28_tables[3, 151:160])), 6))
(k21w1c28_truescore <- round((2 * k21w1c28_tpr * k21w1c28_tnr) / (k21w1c28_tpr + k21w1c28_tnr), 6))

(k21w2c28_tpr <- round(sum(cm_c28_tables[1, 161:170]) / (sum(cm_c28_tables[1, 161:170]) + sum(cm_c28_tables[2, 161:170])), 6))
(k21w2c28_tnr <- round(sum(cm_c28_tables[4, 161:170]) / (sum(cm_c28_tables[4, 161:170]) + sum(cm_c28_tables[3, 161:170])), 6))
(k21w2c28_truescore <- round((2 * k21w2c28_tpr * k21w2c28_tnr) / (k21w2c28_tpr + k21w2c28_tnr), 6))

(k21w3c28_tpr <- round(sum(cm_c28_tables[1, 171:180]) / (sum(cm_c28_tables[1, 171:180]) + sum(cm_c28_tables[2, 171:180])), 6))
(k21w3c28_tnr <- round(sum(cm_c28_tables[4, 171:180]) / (sum(cm_c28_tables[4, 171:180]) + sum(cm_c28_tables[3, 171:180])), 6))
(k21w3c28_truescore <- round((2 * k21w3c28_tpr * k21w3c28_tnr) / (k21w3c28_tpr + k21w3c28_tnr), 6))


(k23w1c28_tpr <- round(sum(cm_c28_tables[1, 181:190]) / (sum(cm_c28_tables[1, 181:190]) + sum(cm_c28_tables[2, 181:190])), 6))
(k23w1c28_tnr <- round(sum(cm_c28_tables[4, 181:190]) / (sum(cm_c28_tables[4, 181:190]) + sum(cm_c28_tables[3, 181:190])), 6))
(k23w1c28_truescore <- round((2 * k23w1c28_tpr * k23w1c28_tnr) / (k23w1c28_tpr + k23w1c28_tnr), 6))

(k23w2c28_tpr <- round(sum(cm_c28_tables[1, 191:200]) / (sum(cm_c28_tables[1, 191:200]) + sum(cm_c28_tables[2, 191:200])), 6))
(k23w2c28_tnr <- round(sum(cm_c28_tables[4, 191:200]) / (sum(cm_c28_tables[4, 191:200]) + sum(cm_c28_tables[3, 191:200])), 6))
(k23w2c28_truescore <- round((2 * k23w2c28_tpr * k23w2c28_tnr) / (k23w2c28_tpr + k23w2c28_tnr), 6))

(k23w3c28_tpr <- round(sum(cm_c28_tables[1, 201:210]) / (sum(cm_c28_tables[1, 201:210]) + sum(cm_c28_tables[2, 201:210])), 6))
(k23w3c28_tnr <- round(sum(cm_c28_tables[4, 201:210]) / (sum(cm_c28_tables[4, 201:210]) + sum(cm_c28_tables[3, 201:210])), 6))
(k23w3c28_truescore <- round((2 * k23w3c28_tpr * k23w3c28_tnr) / (k23w3c28_tpr + k23w3c28_tnr), 6))


(k25w1c28_tpr <- round(sum(cm_c28_tables[1, 211:220]) / (sum(cm_c28_tables[1, 211:220]) + sum(cm_c28_tables[2, 211:220])), 6))
(k25w1c28_tnr <- round(sum(cm_c28_tables[4, 211:220]) / (sum(cm_c28_tables[4, 211:220]) + sum(cm_c28_tables[3, 211:220])), 6))
(k25w1c28_truescore <- round((2 * k25w1c28_tpr * k25w1c28_tnr) / (k25w1c28_tpr + k25w1c28_tnr), 6))

(k25w2c28_tpr <- round(sum(cm_c28_tables[1, 221:230]) / (sum(cm_c28_tables[1, 221:230]) + sum(cm_c28_tables[2, 221:230])), 6))
(k25w2c28_tnr <- round(sum(cm_c28_tables[4, 221:230]) / (sum(cm_c28_tables[4, 221:230]) + sum(cm_c28_tables[3, 221:230])), 6))
(k25w2c28_truescore <- round((2 * k25w2c28_tpr * k25w2c28_tnr) / (k25w2c28_tpr + k25w2c28_tnr), 6))

(k25w3c28_tpr <- round(sum(cm_c28_tables[1, 231:240]) / (sum(cm_c28_tables[1, 231:240]) + sum(cm_c28_tables[2, 231:240])), 6))
(k25w3c28_tnr <- round(sum(cm_c28_tables[4, 231:240]) / (sum(cm_c28_tables[4, 231:240]) + sum(cm_c28_tables[3, 231:240])), 6))
(k25w3c28_truescore <- round((2 * k25w3c28_tpr * k25w3c28_tnr) / (k25w3c28_tpr + k25w3c28_tnr), 6))


(k27w1c28_tpr <- round(sum(cm_c28_tables[1, 241:250]) / (sum(cm_c28_tables[1, 241:250]) + sum(cm_c28_tables[2, 241:250])), 6))
(k27w1c28_tnr <- round(sum(cm_c28_tables[4, 241:250]) / (sum(cm_c28_tables[4, 241:250]) + sum(cm_c28_tables[3, 241:250])), 6))
(k27w1c28_truescore <- round((2 * k27w1c28_tpr * k27w1c28_tnr) / (k27w1c28_tpr + k27w1c28_tnr), 6))

(k27w2c28_tpr <- round(sum(cm_c28_tables[1, 251:260]) / (sum(cm_c28_tables[1, 251:260]) + sum(cm_c28_tables[2, 251:260])), 6))
(k27w2c28_tnr <- round(sum(cm_c28_tables[4, 251:260]) / (sum(cm_c28_tables[4, 251:260]) + sum(cm_c28_tables[3, 251:260])), 6))
(k27w2c28_truescore <- round((2 * k27w2c28_tpr * k27w2c28_tnr) / (k27w2c28_tpr + k27w2c28_tnr), 6))

(k27w3c28_tpr <- round(sum(cm_c28_tables[1, 261:270]) / (sum(cm_c28_tables[1, 261:270]) + sum(cm_c28_tables[2, 261:270])), 6))
(k27w3c28_tnr <- round(sum(cm_c28_tables[4, 261:270]) / (sum(cm_c28_tables[4, 261:270]) + sum(cm_c28_tables[3, 261:270])), 6))
(k27w3c28_truescore <- round((2 * k27w3c28_tpr * k27w3c28_tnr) / (k27w3c28_tpr + k27w3c28_tnr), 6))


(k29w1c28_tpr <- round(sum(cm_c28_tables[1, 271:280]) / (sum(cm_c28_tables[1, 271:280]) + sum(cm_c28_tables[2, 271:280])), 6))
(k29w1c28_tnr <- round(sum(cm_c28_tables[4, 271:280]) / (sum(cm_c28_tables[4, 271:280]) + sum(cm_c28_tables[3, 271:280])), 6))
(k29w1c28_truescore <- round((2 * k29w1c28_tpr * k29w1c28_tnr) / (k29w1c28_tpr + k29w1c28_tnr), 6))

(k29w2c28_tpr <- round(sum(cm_c28_tables[1, 281:290]) / (sum(cm_c28_tables[1, 281:290]) + sum(cm_c28_tables[2, 281:290])), 6))
(k29w2c28_tnr <- round(sum(cm_c28_tables[4, 281:290]) / (sum(cm_c28_tables[4, 281:290]) + sum(cm_c28_tables[3, 281:290])), 6))
(k29w2c28_truescore <- round((2 * k29w2c28_tpr * k29w2c28_tnr) / (k29w2c28_tpr + k29w2c28_tnr), 6))

(k29w3c28_tpr <- round(sum(cm_c28_tables[1, 291:300]) / (sum(cm_c28_tables[1, 291:300]) + sum(cm_c28_tables[2, 291:300])), 6))
(k29w3c28_tnr <- round(sum(cm_c28_tables[4, 291:300]) / (sum(cm_c28_tables[4, 291:300]) + sum(cm_c28_tables[3, 291:300])), 6))
(k29w3c28_truescore <- round((2 * k29w3c28_tpr * k29w3c28_tnr) / (k29w3c28_tpr + k29w3c28_tnr), 6))


(k31w1c28_tpr <- round(sum(cm_c28_tables[1, 301:310]) / (sum(cm_c28_tables[1, 301:310]) + sum(cm_c28_tables[2, 301:310])), 6))
(k31w1c28_tnr <- round(sum(cm_c28_tables[4, 301:310]) / (sum(cm_c28_tables[4, 301:310]) + sum(cm_c28_tables[3, 301:310])), 6))
(k31w1c28_truescore <- round((2 * k31w1c28_tpr * k31w1c28_tnr) / (k31w1c28_tpr + k31w1c28_tnr), 6))

(k31w2c28_tpr <- round(sum(cm_c28_tables[1, 311:320]) / (sum(cm_c28_tables[1, 311:320]) + sum(cm_c28_tables[2, 311:320])), 6))
(k31w2c28_tnr <- round(sum(cm_c28_tables[4, 311:320]) / (sum(cm_c28_tables[4, 311:320]) + sum(cm_c28_tables[3, 311:320])), 6))
(k31w2c28_truescore <- round((2 * k31w2c28_tpr * k31w2c28_tnr) / (k31w2c28_tpr + k31w2c28_tnr), 6))

(k31w3c28_tpr <- round(sum(cm_c28_tables[1, 321:330]) / (sum(cm_c28_tables[1, 321:330]) + sum(cm_c28_tables[2, 321:330])), 6))
(k31w3c28_tnr <- round(sum(cm_c28_tables[4, 321:330]) / (sum(cm_c28_tables[4, 321:330]) + sum(cm_c28_tables[3, 321:330])), 6))
(k31w3c28_truescore <- round((2 * k31w3c28_tpr * k31w3c28_tnr) / (k31w3c28_tpr + k31w3c28_tnr), 6))


(k33w1c28_tpr <- round(sum(cm_c28_tables[1, 331:340]) / (sum(cm_c28_tables[1, 331:340]) + sum(cm_c28_tables[2, 331:340])), 6))
(k33w1c28_tnr <- round(sum(cm_c28_tables[4, 331:340]) / (sum(cm_c28_tables[4, 331:340]) + sum(cm_c28_tables[3, 331:340])), 6))
(k33w1c28_truescore <- round((2 * k33w1c28_tpr * k33w1c28_tnr) / (k33w1c28_tpr + k33w1c28_tnr), 6))

(k33w2c28_tpr <- round(sum(cm_c28_tables[1, 341:350]) / (sum(cm_c28_tables[1, 341:350]) + sum(cm_c28_tables[2, 341:350])), 6))
(k33w2c28_tnr <- round(sum(cm_c28_tables[4, 341:350]) / (sum(cm_c28_tables[4, 341:350]) + sum(cm_c28_tables[3, 341:350])), 6))
(k33w2c28_truescore <- round((2 * k33w2c28_tpr * k33w2c28_tnr) / (k33w2c28_tpr + k33w2c28_tnr), 6))

(k33w3c28_tpr <- round(sum(cm_c28_tables[1, 351:360]) / (sum(cm_c28_tables[1, 351:360]) + sum(cm_c28_tables[2, 351:360])), 6))
(k33w3c28_tnr <- round(sum(cm_c28_tables[4, 351:360]) / (sum(cm_c28_tables[4, 351:360]) + sum(cm_c28_tables[3, 351:360])), 6))
(k33w3c28_truescore <- round((2 * k33w3c28_tpr * k33w3c28_tnr) / (k33w3c28_tpr + k33w3c28_tnr), 6))


(k35w1c28_tpr <- round(sum(cm_c28_tables[1, 361:370]) / (sum(cm_c28_tables[1, 361:370]) + sum(cm_c28_tables[2, 361:370])), 6))
(k35w1c28_tnr <- round(sum(cm_c28_tables[4, 361:370]) / (sum(cm_c28_tables[4, 361:370]) + sum(cm_c28_tables[3, 361:370])), 6))
(k35w1c28_truescore <- round((2 * k35w1c28_tpr * k35w1c28_tnr) / (k35w1c28_tpr + k35w1c28_tnr), 6))

(k35w2c28_tpr <- round(sum(cm_c28_tables[1, 371:380]) / (sum(cm_c28_tables[1, 371:380]) + sum(cm_c28_tables[2, 371:380])), 6))
(k35w2c28_tnr <- round(sum(cm_c28_tables[4, 371:380]) / (sum(cm_c28_tables[4, 371:380]) + sum(cm_c28_tables[3, 371:380])), 6))
(k35w2c28_truescore <- round((2 * k35w2c28_tpr * k35w2c28_tnr) / (k35w2c28_tpr + k35w2c28_tnr), 6))

(k35w3c28_tpr <- round(sum(cm_c28_tables[1, 381:390]) / (sum(cm_c28_tables[1, 381:390]) + sum(cm_c28_tables[2, 381:390])), 6))
(k35w3c28_tnr <- round(sum(cm_c28_tables[4, 381:390]) / (sum(cm_c28_tables[4, 381:390]) + sum(cm_c28_tables[3, 381:390])), 6))
(k35w3c28_truescore <- round((2 * k35w3c28_tpr * k35w3c28_tnr) / (k35w3c28_tpr + k35w3c28_tnr), 6))


(k37w1c28_tpr <- round(sum(cm_c28_tables[1, 391:400]) / (sum(cm_c28_tables[1, 391:400]) + sum(cm_c28_tables[2, 391:400])), 6))
(k37w1c28_tnr <- round(sum(cm_c28_tables[4, 391:400]) / (sum(cm_c28_tables[4, 391:400]) + sum(cm_c28_tables[3, 391:400])), 6))
(k37w1c28_truescore <- round((2 * k37w1c28_tpr * k37w1c28_tnr) / (k37w1c28_tpr + k37w1c28_tnr), 6))

(k37w2c28_tpr <- round(sum(cm_c28_tables[1, 401:410]) / (sum(cm_c28_tables[1, 401:410]) + sum(cm_c28_tables[2, 401:410])), 6))
(k37w2c28_tnr <- round(sum(cm_c28_tables[4, 401:410]) / (sum(cm_c28_tables[4, 401:410]) + sum(cm_c28_tables[3, 401:410])), 6))
(k37w2c28_truescore <- round((2 * k37w2c28_tpr * k37w2c28_tnr) / (k37w2c28_tpr + k37w2c28_tnr), 6))

(k37w3c28_tpr <- round(sum(cm_c28_tables[1, 411:420]) / (sum(cm_c28_tables[1, 411:420]) + sum(cm_c28_tables[2, 411:420])), 6))
(k37w3c28_tnr <- round(sum(cm_c28_tables[4, 411:420]) / (sum(cm_c28_tables[4, 411:420]) + sum(cm_c28_tables[3, 411:420])), 6))
(k37w3c28_truescore <- round((2 * k37w3c28_tpr * k37w3c28_tnr) / (k37w3c28_tpr + k37w3c28_tnr), 6))


(k39w1c28_tpr <- round(sum(cm_c28_tables[1, 421:430]) / (sum(cm_c28_tables[1, 421:430]) + sum(cm_c28_tables[2, 421:430])), 6))
(k39w1c28_tnr <- round(sum(cm_c28_tables[4, 421:430]) / (sum(cm_c28_tables[4, 421:430]) + sum(cm_c28_tables[3, 421:430])), 6))
(k39w1c28_truescore <- round((2 * k39w1c28_tpr * k39w1c28_tnr) / (k39w1c28_tpr + k39w1c28_tnr), 6))

(k39w2c28_tpr <- round(sum(cm_c28_tables[1, 431:440]) / (sum(cm_c28_tables[1, 431:440]) + sum(cm_c28_tables[2, 431:440])), 6))
(k39w2c28_tnr <- round(sum(cm_c28_tables[4, 431:440]) / (sum(cm_c28_tables[4, 431:440]) + sum(cm_c28_tables[3, 431:440])), 6))
(k39w2c28_truescore <- round((2 * k39w2c28_tpr * k39w2c28_tnr) / (k39w2c28_tpr + k39w2c28_tnr), 6))

(k39w3c28_tpr <- round(sum(cm_c28_tables[1, 441:450]) / (sum(cm_c28_tables[1, 441:450]) + sum(cm_c28_tables[2, 441:450])), 6))
(k39w3c28_tnr <- round(sum(cm_c28_tables[4, 441:450]) / (sum(cm_c28_tables[4, 441:450]) + sum(cm_c28_tables[3, 441:450])), 6))
(k39w3c28_truescore <- round((2 * k39w3c28_tpr * k39w3c28_tnr) / (k39w3c28_tpr + k39w3c28_tnr), 6))


# Compile the 0.28 cutoff results in a table, and identify the combination of 
# of k and kernel that maximizes the Truescore.

c28_results <- tibble(k = c(11, 11, 11, 13, 13, 13, 15, 15, 15, 
                            17, 17, 17, 19, 19, 19, 21, 21, 21, 
                            23, 23, 23, 25, 25, 25, 27, 27, 27, 
                            29, 29, 29, 31, 31, 31, 33, 33, 33, 
                            35, 35, 35, 37, 37, 37, 39, 39, 39), 
                      Kernel = rep(c("triangular", "gaussian", "optimal"), 15), 
                      Cut = 0.28, 
                      TPR = c(k11w1c28_tpr, k11w2c28_tpr, k11w3c28_tpr, k13w1c28_tpr, 
                              k13w2c28_tpr, k13w3c28_tpr, k15w1c28_tpr, k15w2c28_tpr, 
                              k15w3c28_tpr, k17w1c28_tpr, k17w2c28_tpr, k17w3c28_tpr, 
                              k19w1c28_tpr, k19w2c28_tpr, k19w3c28_tpr, k21w1c28_tpr, 
                              k21w2c28_tpr, k21w3c28_tpr, k23w1c28_tpr, k23w2c28_tpr, 
                              k23w3c28_tpr, k25w1c28_tpr, k25w2c28_tpr, k25w3c28_tpr, 
                              k27w1c28_tpr, k27w2c28_tpr, k27w3c28_tpr, k29w1c28_tpr, 
                              k29w2c28_tpr, k29w3c28_tpr, k31w1c28_tpr, k31w2c28_tpr, 
                              k31w3c28_tpr, k33w1c28_tpr, k33w2c28_tpr, k33w3c28_tpr, 
                              k35w1c28_tpr, k35w2c28_tpr, k35w3c28_tpr, k37w1c28_tpr, 
                              k37w2c28_tpr, k37w3c28_tpr, k39w1c28_tpr, k39w2c28_tpr, 
                              k39w3c28_tpr), 
                      TNR = c(k11w1c28_tnr, k11w2c28_tnr, k11w3c28_tnr, k13w1c28_tnr, 
                              k13w2c28_tnr, k13w3c28_tnr, k15w1c28_tnr, k15w2c28_tnr, 
                              k15w3c28_tnr, k17w1c28_tnr, k17w2c28_tnr, k17w3c28_tnr, 
                              k19w1c28_tnr, k19w2c28_tnr, k19w3c28_tnr, k21w1c28_tnr, 
                              k21w2c28_tnr, k21w3c28_tnr, k23w1c28_tnr, k23w2c28_tnr, 
                              k23w3c28_tnr, k25w1c28_tnr, k25w2c28_tnr, k25w3c28_tnr, 
                              k27w1c28_tnr, k27w2c28_tnr, k27w3c28_tnr, k29w1c28_tnr, 
                              k29w2c28_tnr, k29w3c28_tnr, k31w1c28_tnr, k31w2c28_tnr, 
                              k31w3c28_tnr, k33w1c28_tnr, k33w2c28_tnr, k33w3c28_tnr, 
                              k35w1c28_tnr, k35w2c28_tnr, k35w3c28_tnr, k37w1c28_tnr, 
                              k37w2c28_tnr, k37w3c28_tnr, k39w1c28_tnr, k39w2c28_tnr, 
                              k39w3c28_tnr), 
                      Truescore = c(k11w1c28_truescore, k11w2c28_truescore, 
                                    k11w3c28_truescore, k13w1c28_truescore, 
                                    k13w2c28_truescore, k13w3c28_truescore, 
                                    k15w1c28_truescore, k15w2c28_truescore, 
                                    k15w3c28_truescore, k17w1c28_truescore, 
                                    k17w2c28_truescore, k17w3c28_truescore, 
                                    k19w1c28_truescore, k19w2c28_truescore, 
                                    k19w3c28_truescore, k21w1c28_truescore, 
                                    k21w2c28_truescore, k21w3c28_truescore, 
                                    k23w1c28_truescore, k23w2c28_truescore, 
                                    k23w3c28_truescore, k25w1c28_truescore, 
                                    k25w2c28_truescore, k25w3c28_truescore, 
                                    k27w1c28_truescore, k27w2c28_truescore, 
                                    k27w3c28_truescore, k29w1c28_truescore, 
                                    k29w2c28_truescore, k29w3c28_truescore, 
                                    k31w1c28_truescore, k31w2c28_truescore, 
                                    k31w3c28_truescore, k33w1c28_truescore, 
                                    k33w2c28_truescore, k33w3c28_truescore, 
                                    k35w1c28_truescore, k35w2c28_truescore, 
                                    k35w3c28_truescore, k37w1c28_truescore, 
                                    k37w2c28_truescore, k37w3c28_truescore, 
                                    k39w1c28_truescore, k39w2c28_truescore, 
                                    k39w3c28_truescore))

knitr::kable(c28_results[1:45, ], caption = "c28_results")

ggplot(c28_results, aes(k, Truescore, shape = Kernel, color = Kernel)) + 
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.28 (Truescore)")

# For the Cutoff of 0.28, compute the Minimum Distance to (0, 1) for each 
# combination of k and Kernel. 

c28_results <- c28_results %>% mutate(Distance = sqrt((1 - TPR)^2 + (1 - TNR)^2))

ggplot(c28_results, aes(k, Distance, shape = Kernel, color = Kernel)) +
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.28 (Distance)")

# For the Cutoff of 0.28, identify the optimal combination of values for k and Kernel 
# based on each of our assessment methods, Truescore and Mimimum Distance to (0, 1).

max(c28_results$Truescore)
(c28_opt_k_ts <- c28_results$k[which.max(c28_results$Truescore)])
(c28_opt_kernel_ts <- c28_results$Kernel[which.max(c28_results$Truescore)])
(c28_opt_cut_ts <- c28_results$Cut[which.max(c28_results$Truescore)])
(c28_opt_tpr_ts <- c28_results$TPR[which.max(c28_results$Truescore)])
(c28_opt_tnr_ts <- c28_results$TNR[which.max(c28_results$Truescore)])
(c28_opt_d_ts <- c28_results$Distance[which.max(c28_results$Truescore)])

min(c28_results$Distance)
(c28_opt_k_dist <- c28_results$k[which.min(c28_results$Distance)])
(c28_opt_kernel_dist <- c28_results$Kernel[which.min(c28_results$Distance)])
(c28_opt_cut_dist <- c28_results$Cut[which.min(c28_results$Distance)])  
(c28_opt_tpr_dist <- c28_results$TPR[which.min(c28_results$Distance)])
(c28_opt_tnr_dist <- c28_results$TNR[which.min(c28_results$Distance)])
(c28_opt_t_dist <- c28_results$Truescore[which.min(c28_results$Distance)])

############################
# 0.29 Cutoff
############################

# For the decision cutoff of 0.29, generate a confusion matrix for
# every combination of k (11:39 by two), kernel (1:3) and fold (1:10).

cm_c29 <- sapply(kwf_dfs_l, function(x) {
  ss <- subset(x, select = c(pred29, obs))
  confusionMatrix(ss$pred29, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c29) <- kwf_dfs_v

cm_c29_tables <- sapply(cm_c29, "[[", 2)
cm_c29_tables <- as_tibble(cm_c29_tables)

# For each combination of k and kernel, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each combination of k and kernel when the decision cutoff is 0.29.

(k11w1c29_tpr <- round(sum(cm_c29_tables[1, 1:10]) / (sum(cm_c29_tables[1, 1:10]) + sum(cm_c29_tables[2, 1:10])), 6))
(k11w1c29_tnr <- round(sum(cm_c29_tables[4, 1:10]) / (sum(cm_c29_tables[4, 1:10]) + sum(cm_c29_tables[3, 1:10])), 6))
(k11w1c29_truescore <- round((2 * k11w1c29_tpr * k11w1c29_tnr) / (k11w1c29_tpr + k11w1c29_tnr), 6))

(k11w2c29_tpr <- round(sum(cm_c29_tables[1, 11:20]) / (sum(cm_c29_tables[1, 11:20]) + sum(cm_c29_tables[2, 11:20])), 6))
(k11w2c29_tnr <- round(sum(cm_c29_tables[4, 11:20]) / (sum(cm_c29_tables[4, 11:20]) + sum(cm_c29_tables[3, 11:20])), 6))
(k11w2c29_truescore <- round((2 * k11w2c29_tpr * k11w2c29_tnr) / (k11w2c29_tpr + k11w2c29_tnr), 6))

(k11w3c29_tpr <- round(sum(cm_c29_tables[1, 21:30]) / (sum(cm_c29_tables[1, 21:30]) + sum(cm_c29_tables[2, 21:30])), 6))
(k11w3c29_tnr <- round(sum(cm_c29_tables[4, 21:30]) / (sum(cm_c29_tables[4, 21:30]) + sum(cm_c29_tables[3, 21:30])), 6))
(k11w3c29_truescore <- round((2 * k11w3c29_tpr * k11w3c29_tnr) / (k11w3c29_tpr + k11w3c29_tnr), 6))


(k13w1c29_tpr <- round(sum(cm_c29_tables[1, 31:40]) / (sum(cm_c29_tables[1, 31:40]) + sum(cm_c29_tables[2, 31:40])), 6))
(k13w1c29_tnr <- round(sum(cm_c29_tables[4, 31:40]) / (sum(cm_c29_tables[4, 31:40]) + sum(cm_c29_tables[3, 31:40])), 6))
(k13w1c29_truescore <- round((2 * k13w1c29_tpr * k13w1c29_tnr) / (k13w1c29_tpr + k13w1c29_tnr), 6))

(k13w2c29_tpr <- round(sum(cm_c29_tables[1, 41:50]) / (sum(cm_c29_tables[1, 41:50]) + sum(cm_c29_tables[2, 41:50])), 6))
(k13w2c29_tnr <- round(sum(cm_c29_tables[4, 41:50]) / (sum(cm_c29_tables[4, 41:50]) + sum(cm_c29_tables[3, 41:50])), 6))
(k13w2c29_truescore <- round((2 * k13w2c29_tpr * k13w2c29_tnr) / (k13w2c29_tpr + k13w2c29_tnr), 6))

(k13w3c29_tpr <- round(sum(cm_c29_tables[1, 51:60]) / (sum(cm_c29_tables[1, 51:60]) + sum(cm_c29_tables[2, 51:60])), 6))
(k13w3c29_tnr <- round(sum(cm_c29_tables[4, 51:60]) / (sum(cm_c29_tables[4, 51:60]) + sum(cm_c29_tables[3, 51:60])), 6))
(k13w3c29_truescore <- round((2 * k13w3c29_tpr * k13w3c29_tnr) / (k13w3c29_tpr + k13w3c29_tnr), 6))


(k15w1c29_tpr <- round(sum(cm_c29_tables[1, 61:70]) / (sum(cm_c29_tables[1, 61:70]) + sum(cm_c29_tables[2, 61:70])), 6))
(k15w1c29_tnr <- round(sum(cm_c29_tables[4, 61:70]) / (sum(cm_c29_tables[4, 61:70]) + sum(cm_c29_tables[3, 61:70])), 6))
(k15w1c29_truescore <- round((2 * k15w1c29_tpr * k15w1c29_tnr) / (k15w1c29_tpr + k15w1c29_tnr), 6))

(k15w2c29_tpr <- round(sum(cm_c29_tables[1, 71:80]) / (sum(cm_c29_tables[1, 71:80]) + sum(cm_c29_tables[2, 71:80])), 6))
(k15w2c29_tnr <- round(sum(cm_c29_tables[4, 71:80]) / (sum(cm_c29_tables[4, 71:80]) + sum(cm_c29_tables[3, 71:80])), 6))
(k15w2c29_truescore <- round((2 * k15w2c29_tpr * k15w2c29_tnr) / (k15w2c29_tpr + k15w2c29_tnr), 6))

(k15w3c29_tpr <- round(sum(cm_c29_tables[1, 81:90]) / (sum(cm_c29_tables[1, 81:90]) + sum(cm_c29_tables[2, 81:90])), 6))
(k15w3c29_tnr <- round(sum(cm_c29_tables[4, 81:90]) / (sum(cm_c29_tables[4, 81:90]) + sum(cm_c29_tables[3, 81:90])), 6))
(k15w3c29_truescore <- round((2 * k15w3c29_tpr * k15w3c29_tnr) / (k15w3c29_tpr + k15w3c29_tnr), 6))


(k17w1c29_tpr <- round(sum(cm_c29_tables[1, 91:100]) / (sum(cm_c29_tables[1, 91:100]) + sum(cm_c29_tables[2, 91:100])), 6))
(k17w1c29_tnr <- round(sum(cm_c29_tables[4, 91:100]) / (sum(cm_c29_tables[4, 91:100]) + sum(cm_c29_tables[3, 91:100])), 6))
(k17w1c29_truescore <- round((2 * k17w1c29_tpr * k17w1c29_tnr) / (k17w1c29_tpr + k17w1c29_tnr), 6))

(k17w2c29_tpr <- round(sum(cm_c29_tables[1, 101:110]) / (sum(cm_c29_tables[1, 101:110]) + sum(cm_c29_tables[2, 101:110])), 6))
(k17w2c29_tnr <- round(sum(cm_c29_tables[4, 101:110]) / (sum(cm_c29_tables[4, 101:110]) + sum(cm_c29_tables[3, 101:110])), 6))
(k17w2c29_truescore <- round((2 * k17w2c29_tpr * k17w2c29_tnr) / (k17w2c29_tpr + k17w2c29_tnr), 6))

(k17w3c29_tpr <- round(sum(cm_c29_tables[1, 111:120]) / (sum(cm_c29_tables[1, 111:120]) + sum(cm_c29_tables[2, 111:120])), 6))
(k17w3c29_tnr <- round(sum(cm_c29_tables[4, 111:120]) / (sum(cm_c29_tables[4, 111:120]) + sum(cm_c29_tables[3, 111:120])), 6))
(k17w3c29_truescore <- round((2 * k17w3c29_tpr * k17w3c29_tnr) / (k17w3c29_tpr + k17w3c29_tnr), 6))


(k19w1c29_tpr <- round(sum(cm_c29_tables[1, 121:130]) / (sum(cm_c29_tables[1, 121:130]) + sum(cm_c29_tables[2, 121:130])), 6))
(k19w1c29_tnr <- round(sum(cm_c29_tables[4, 121:130]) / (sum(cm_c29_tables[4, 121:130]) + sum(cm_c29_tables[3, 121:130])), 6))
(k19w1c29_truescore <- round((2 * k19w1c29_tpr * k19w1c29_tnr) / (k19w1c29_tpr + k19w1c29_tnr), 6))

(k19w2c29_tpr <- round(sum(cm_c29_tables[1, 131:140]) / (sum(cm_c29_tables[1, 131:140]) + sum(cm_c29_tables[2, 131:140])), 6))
(k19w2c29_tnr <- round(sum(cm_c29_tables[4, 131:140]) / (sum(cm_c29_tables[4, 131:140]) + sum(cm_c29_tables[3, 131:140])), 6))
(k19w2c29_truescore <- round((2 * k19w2c29_tpr * k19w2c29_tnr) / (k19w2c29_tpr + k19w2c29_tnr), 6))

(k19w3c29_tpr <- round(sum(cm_c29_tables[1, 141:150]) / (sum(cm_c29_tables[1, 141:150]) + sum(cm_c29_tables[2, 141:150])), 6))
(k19w3c29_tnr <- round(sum(cm_c29_tables[4, 141:150]) / (sum(cm_c29_tables[4, 141:150]) + sum(cm_c29_tables[3, 141:150])), 6))
(k19w3c29_truescore <- round((2 * k19w3c29_tpr * k19w3c29_tnr) / (k19w3c29_tpr + k19w3c29_tnr), 6))


(k21w1c29_tpr <- round(sum(cm_c29_tables[1, 151:160]) / (sum(cm_c29_tables[1, 151:160]) + sum(cm_c29_tables[2, 151:160])), 6))
(k21w1c29_tnr <- round(sum(cm_c29_tables[4, 151:160]) / (sum(cm_c29_tables[4, 151:160]) + sum(cm_c29_tables[3, 151:160])), 6))
(k21w1c29_truescore <- round((2 * k21w1c29_tpr * k21w1c29_tnr) / (k21w1c29_tpr + k21w1c29_tnr), 6))

(k21w2c29_tpr <- round(sum(cm_c29_tables[1, 161:170]) / (sum(cm_c29_tables[1, 161:170]) + sum(cm_c29_tables[2, 161:170])), 6))
(k21w2c29_tnr <- round(sum(cm_c29_tables[4, 161:170]) / (sum(cm_c29_tables[4, 161:170]) + sum(cm_c29_tables[3, 161:170])), 6))
(k21w2c29_truescore <- round((2 * k21w2c29_tpr * k21w2c29_tnr) / (k21w2c29_tpr + k21w2c29_tnr), 6))

(k21w3c29_tpr <- round(sum(cm_c29_tables[1, 171:180]) / (sum(cm_c29_tables[1, 171:180]) + sum(cm_c29_tables[2, 171:180])), 6))
(k21w3c29_tnr <- round(sum(cm_c29_tables[4, 171:180]) / (sum(cm_c29_tables[4, 171:180]) + sum(cm_c29_tables[3, 171:180])), 6))
(k21w3c29_truescore <- round((2 * k21w3c29_tpr * k21w3c29_tnr) / (k21w3c29_tpr + k21w3c29_tnr), 6))


(k23w1c29_tpr <- round(sum(cm_c29_tables[1, 181:190]) / (sum(cm_c29_tables[1, 181:190]) + sum(cm_c29_tables[2, 181:190])), 6))
(k23w1c29_tnr <- round(sum(cm_c29_tables[4, 181:190]) / (sum(cm_c29_tables[4, 181:190]) + sum(cm_c29_tables[3, 181:190])), 6))
(k23w1c29_truescore <- round((2 * k23w1c29_tpr * k23w1c29_tnr) / (k23w1c29_tpr + k23w1c29_tnr), 6))

(k23w2c29_tpr <- round(sum(cm_c29_tables[1, 191:200]) / (sum(cm_c29_tables[1, 191:200]) + sum(cm_c29_tables[2, 191:200])), 6))
(k23w2c29_tnr <- round(sum(cm_c29_tables[4, 191:200]) / (sum(cm_c29_tables[4, 191:200]) + sum(cm_c29_tables[3, 191:200])), 6))
(k23w2c29_truescore <- round((2 * k23w2c29_tpr * k23w2c29_tnr) / (k23w2c29_tpr + k23w2c29_tnr), 6))

(k23w3c29_tpr <- round(sum(cm_c29_tables[1, 201:210]) / (sum(cm_c29_tables[1, 201:210]) + sum(cm_c29_tables[2, 201:210])), 6))
(k23w3c29_tnr <- round(sum(cm_c29_tables[4, 201:210]) / (sum(cm_c29_tables[4, 201:210]) + sum(cm_c29_tables[3, 201:210])), 6))
(k23w3c29_truescore <- round((2 * k23w3c29_tpr * k23w3c29_tnr) / (k23w3c29_tpr + k23w3c29_tnr), 6))


(k25w1c29_tpr <- round(sum(cm_c29_tables[1, 211:220]) / (sum(cm_c29_tables[1, 211:220]) + sum(cm_c29_tables[2, 211:220])), 6))
(k25w1c29_tnr <- round(sum(cm_c29_tables[4, 211:220]) / (sum(cm_c29_tables[4, 211:220]) + sum(cm_c29_tables[3, 211:220])), 6))
(k25w1c29_truescore <- round((2 * k25w1c29_tpr * k25w1c29_tnr) / (k25w1c29_tpr + k25w1c29_tnr), 6))

(k25w2c29_tpr <- round(sum(cm_c29_tables[1, 221:230]) / (sum(cm_c29_tables[1, 221:230]) + sum(cm_c29_tables[2, 221:230])), 6))
(k25w2c29_tnr <- round(sum(cm_c29_tables[4, 221:230]) / (sum(cm_c29_tables[4, 221:230]) + sum(cm_c29_tables[3, 221:230])), 6))
(k25w2c29_truescore <- round((2 * k25w2c29_tpr * k25w2c29_tnr) / (k25w2c29_tpr + k25w2c29_tnr), 6))

(k25w3c29_tpr <- round(sum(cm_c29_tables[1, 231:240]) / (sum(cm_c29_tables[1, 231:240]) + sum(cm_c29_tables[2, 231:240])), 6))
(k25w3c29_tnr <- round(sum(cm_c29_tables[4, 231:240]) / (sum(cm_c29_tables[4, 231:240]) + sum(cm_c29_tables[3, 231:240])), 6))
(k25w3c29_truescore <- round((2 * k25w3c29_tpr * k25w3c29_tnr) / (k25w3c29_tpr + k25w3c29_tnr), 6))


(k27w1c29_tpr <- round(sum(cm_c29_tables[1, 241:250]) / (sum(cm_c29_tables[1, 241:250]) + sum(cm_c29_tables[2, 241:250])), 6))
(k27w1c29_tnr <- round(sum(cm_c29_tables[4, 241:250]) / (sum(cm_c29_tables[4, 241:250]) + sum(cm_c29_tables[3, 241:250])), 6))
(k27w1c29_truescore <- round((2 * k27w1c29_tpr * k27w1c29_tnr) / (k27w1c29_tpr + k27w1c29_tnr), 6))

(k27w2c29_tpr <- round(sum(cm_c29_tables[1, 251:260]) / (sum(cm_c29_tables[1, 251:260]) + sum(cm_c29_tables[2, 251:260])), 6))
(k27w2c29_tnr <- round(sum(cm_c29_tables[4, 251:260]) / (sum(cm_c29_tables[4, 251:260]) + sum(cm_c29_tables[3, 251:260])), 6))
(k27w2c29_truescore <- round((2 * k27w2c29_tpr * k27w2c29_tnr) / (k27w2c29_tpr + k27w2c29_tnr), 6))

(k27w3c29_tpr <- round(sum(cm_c29_tables[1, 261:270]) / (sum(cm_c29_tables[1, 261:270]) + sum(cm_c29_tables[2, 261:270])), 6))
(k27w3c29_tnr <- round(sum(cm_c29_tables[4, 261:270]) / (sum(cm_c29_tables[4, 261:270]) + sum(cm_c29_tables[3, 261:270])), 6))
(k27w3c29_truescore <- round((2 * k27w3c29_tpr * k27w3c29_tnr) / (k27w3c29_tpr + k27w3c29_tnr), 6))


(k29w1c29_tpr <- round(sum(cm_c29_tables[1, 271:280]) / (sum(cm_c29_tables[1, 271:280]) + sum(cm_c29_tables[2, 271:280])), 6))
(k29w1c29_tnr <- round(sum(cm_c29_tables[4, 271:280]) / (sum(cm_c29_tables[4, 271:280]) + sum(cm_c29_tables[3, 271:280])), 6))
(k29w1c29_truescore <- round((2 * k29w1c29_tpr * k29w1c29_tnr) / (k29w1c29_tpr + k29w1c29_tnr), 6))

(k29w2c29_tpr <- round(sum(cm_c29_tables[1, 281:290]) / (sum(cm_c29_tables[1, 281:290]) + sum(cm_c29_tables[2, 281:290])), 6))
(k29w2c29_tnr <- round(sum(cm_c29_tables[4, 281:290]) / (sum(cm_c29_tables[4, 281:290]) + sum(cm_c29_tables[3, 281:290])), 6))
(k29w2c29_truescore <- round((2 * k29w2c29_tpr * k29w2c29_tnr) / (k29w2c29_tpr + k29w2c29_tnr), 6))

(k29w3c29_tpr <- round(sum(cm_c29_tables[1, 291:300]) / (sum(cm_c29_tables[1, 291:300]) + sum(cm_c29_tables[2, 291:300])), 6))
(k29w3c29_tnr <- round(sum(cm_c29_tables[4, 291:300]) / (sum(cm_c29_tables[4, 291:300]) + sum(cm_c29_tables[3, 291:300])), 6))
(k29w3c29_truescore <- round((2 * k29w3c29_tpr * k29w3c29_tnr) / (k29w3c29_tpr + k29w3c29_tnr), 6))


(k31w1c29_tpr <- round(sum(cm_c29_tables[1, 301:310]) / (sum(cm_c29_tables[1, 301:310]) + sum(cm_c29_tables[2, 301:310])), 6))
(k31w1c29_tnr <- round(sum(cm_c29_tables[4, 301:310]) / (sum(cm_c29_tables[4, 301:310]) + sum(cm_c29_tables[3, 301:310])), 6))
(k31w1c29_truescore <- round((2 * k31w1c29_tpr * k31w1c29_tnr) / (k31w1c29_tpr + k31w1c29_tnr), 6))

(k31w2c29_tpr <- round(sum(cm_c29_tables[1, 311:320]) / (sum(cm_c29_tables[1, 311:320]) + sum(cm_c29_tables[2, 311:320])), 6))
(k31w2c29_tnr <- round(sum(cm_c29_tables[4, 311:320]) / (sum(cm_c29_tables[4, 311:320]) + sum(cm_c29_tables[3, 311:320])), 6))
(k31w2c29_truescore <- round((2 * k31w2c29_tpr * k31w2c29_tnr) / (k31w2c29_tpr + k31w2c29_tnr), 6))

(k31w3c29_tpr <- round(sum(cm_c29_tables[1, 321:330]) / (sum(cm_c29_tables[1, 321:330]) + sum(cm_c29_tables[2, 321:330])), 6))
(k31w3c29_tnr <- round(sum(cm_c29_tables[4, 321:330]) / (sum(cm_c29_tables[4, 321:330]) + sum(cm_c29_tables[3, 321:330])), 6))
(k31w3c29_truescore <- round((2 * k31w3c29_tpr * k31w3c29_tnr) / (k31w3c29_tpr + k31w3c29_tnr), 6))


(k33w1c29_tpr <- round(sum(cm_c29_tables[1, 331:340]) / (sum(cm_c29_tables[1, 331:340]) + sum(cm_c29_tables[2, 331:340])), 6))
(k33w1c29_tnr <- round(sum(cm_c29_tables[4, 331:340]) / (sum(cm_c29_tables[4, 331:340]) + sum(cm_c29_tables[3, 331:340])), 6))
(k33w1c29_truescore <- round((2 * k33w1c29_tpr * k33w1c29_tnr) / (k33w1c29_tpr + k33w1c29_tnr), 6))

(k33w2c29_tpr <- round(sum(cm_c29_tables[1, 341:350]) / (sum(cm_c29_tables[1, 341:350]) + sum(cm_c29_tables[2, 341:350])), 6))
(k33w2c29_tnr <- round(sum(cm_c29_tables[4, 341:350]) / (sum(cm_c29_tables[4, 341:350]) + sum(cm_c29_tables[3, 341:350])), 6))
(k33w2c29_truescore <- round((2 * k33w2c29_tpr * k33w2c29_tnr) / (k33w2c29_tpr + k33w2c29_tnr), 6))

(k33w3c29_tpr <- round(sum(cm_c29_tables[1, 351:360]) / (sum(cm_c29_tables[1, 351:360]) + sum(cm_c29_tables[2, 351:360])), 6))
(k33w3c29_tnr <- round(sum(cm_c29_tables[4, 351:360]) / (sum(cm_c29_tables[4, 351:360]) + sum(cm_c29_tables[3, 351:360])), 6))
(k33w3c29_truescore <- round((2 * k33w3c29_tpr * k33w3c29_tnr) / (k33w3c29_tpr + k33w3c29_tnr), 6))


(k35w1c29_tpr <- round(sum(cm_c29_tables[1, 361:370]) / (sum(cm_c29_tables[1, 361:370]) + sum(cm_c29_tables[2, 361:370])), 6))
(k35w1c29_tnr <- round(sum(cm_c29_tables[4, 361:370]) / (sum(cm_c29_tables[4, 361:370]) + sum(cm_c29_tables[3, 361:370])), 6))
(k35w1c29_truescore <- round((2 * k35w1c29_tpr * k35w1c29_tnr) / (k35w1c29_tpr + k35w1c29_tnr), 6))

(k35w2c29_tpr <- round(sum(cm_c29_tables[1, 371:380]) / (sum(cm_c29_tables[1, 371:380]) + sum(cm_c29_tables[2, 371:380])), 6))
(k35w2c29_tnr <- round(sum(cm_c29_tables[4, 371:380]) / (sum(cm_c29_tables[4, 371:380]) + sum(cm_c29_tables[3, 371:380])), 6))
(k35w2c29_truescore <- round((2 * k35w2c29_tpr * k35w2c29_tnr) / (k35w2c29_tpr + k35w2c29_tnr), 6))

(k35w3c29_tpr <- round(sum(cm_c29_tables[1, 381:390]) / (sum(cm_c29_tables[1, 381:390]) + sum(cm_c29_tables[2, 381:390])), 6))
(k35w3c29_tnr <- round(sum(cm_c29_tables[4, 381:390]) / (sum(cm_c29_tables[4, 381:390]) + sum(cm_c29_tables[3, 381:390])), 6))
(k35w3c29_truescore <- round((2 * k35w3c29_tpr * k35w3c29_tnr) / (k35w3c29_tpr + k35w3c29_tnr), 6))


(k37w1c29_tpr <- round(sum(cm_c29_tables[1, 391:400]) / (sum(cm_c29_tables[1, 391:400]) + sum(cm_c29_tables[2, 391:400])), 6))
(k37w1c29_tnr <- round(sum(cm_c29_tables[4, 391:400]) / (sum(cm_c29_tables[4, 391:400]) + sum(cm_c29_tables[3, 391:400])), 6))
(k37w1c29_truescore <- round((2 * k37w1c29_tpr * k37w1c29_tnr) / (k37w1c29_tpr + k37w1c29_tnr), 6))

(k37w2c29_tpr <- round(sum(cm_c29_tables[1, 401:410]) / (sum(cm_c29_tables[1, 401:410]) + sum(cm_c29_tables[2, 401:410])), 6))
(k37w2c29_tnr <- round(sum(cm_c29_tables[4, 401:410]) / (sum(cm_c29_tables[4, 401:410]) + sum(cm_c29_tables[3, 401:410])), 6))
(k37w2c29_truescore <- round((2 * k37w2c29_tpr * k37w2c29_tnr) / (k37w2c29_tpr + k37w2c29_tnr), 6))

(k37w3c29_tpr <- round(sum(cm_c29_tables[1, 411:420]) / (sum(cm_c29_tables[1, 411:420]) + sum(cm_c29_tables[2, 411:420])), 6))
(k37w3c29_tnr <- round(sum(cm_c29_tables[4, 411:420]) / (sum(cm_c29_tables[4, 411:420]) + sum(cm_c29_tables[3, 411:420])), 6))
(k37w3c29_truescore <- round((2 * k37w3c29_tpr * k37w3c29_tnr) / (k37w3c29_tpr + k37w3c29_tnr), 6))


(k39w1c29_tpr <- round(sum(cm_c29_tables[1, 421:430]) / (sum(cm_c29_tables[1, 421:430]) + sum(cm_c29_tables[2, 421:430])), 6))
(k39w1c29_tnr <- round(sum(cm_c29_tables[4, 421:430]) / (sum(cm_c29_tables[4, 421:430]) + sum(cm_c29_tables[3, 421:430])), 6))
(k39w1c29_truescore <- round((2 * k39w1c29_tpr * k39w1c29_tnr) / (k39w1c29_tpr + k39w1c29_tnr), 6))

(k39w2c29_tpr <- round(sum(cm_c29_tables[1, 431:440]) / (sum(cm_c29_tables[1, 431:440]) + sum(cm_c29_tables[2, 431:440])), 6))
(k39w2c29_tnr <- round(sum(cm_c29_tables[4, 431:440]) / (sum(cm_c29_tables[4, 431:440]) + sum(cm_c29_tables[3, 431:440])), 6))
(k39w2c29_truescore <- round((2 * k39w2c29_tpr * k39w2c29_tnr) / (k39w2c29_tpr + k39w2c29_tnr), 6))

(k39w3c29_tpr <- round(sum(cm_c29_tables[1, 441:450]) / (sum(cm_c29_tables[1, 441:450]) + sum(cm_c29_tables[2, 441:450])), 6))
(k39w3c29_tnr <- round(sum(cm_c29_tables[4, 441:450]) / (sum(cm_c29_tables[4, 441:450]) + sum(cm_c29_tables[3, 441:450])), 6))
(k39w3c29_truescore <- round((2 * k39w3c29_tpr * k39w3c29_tnr) / (k39w3c29_tpr + k39w3c29_tnr), 6))


# Compile the 0.29 cutoff results in a table, and identify the combination of 
# of k and kernel that maximizes the Truescore.

c29_results <- tibble(k = c(11, 11, 11, 13, 13, 13, 15, 15, 15, 
                            17, 17, 17, 19, 19, 19, 21, 21, 21, 
                            23, 23, 23, 25, 25, 25, 27, 27, 27, 
                            29, 29, 29, 31, 31, 31, 33, 33, 33, 
                            35, 35, 35, 37, 37, 37, 39, 39, 39), 
                      Kernel = rep(c("triangular", "gaussian", "optimal"), 15), 
                      Cut = 0.29, 
                      TPR = c(k11w1c29_tpr, k11w2c29_tpr, k11w3c29_tpr, k13w1c29_tpr, 
                              k13w2c29_tpr, k13w3c29_tpr, k15w1c29_tpr, k15w2c29_tpr, 
                              k15w3c29_tpr, k17w1c29_tpr, k17w2c29_tpr, k17w3c29_tpr, 
                              k19w1c29_tpr, k19w2c29_tpr, k19w3c29_tpr, k21w1c29_tpr, 
                              k21w2c29_tpr, k21w3c29_tpr, k23w1c29_tpr, k23w2c29_tpr, 
                              k23w3c29_tpr, k25w1c29_tpr, k25w2c29_tpr, k25w3c29_tpr, 
                              k27w1c29_tpr, k27w2c29_tpr, k27w3c29_tpr, k29w1c29_tpr, 
                              k29w2c29_tpr, k29w3c29_tpr, k31w1c29_tpr, k31w2c29_tpr, 
                              k31w3c29_tpr, k33w1c29_tpr, k33w2c29_tpr, k33w3c29_tpr, 
                              k35w1c29_tpr, k35w2c29_tpr, k35w3c29_tpr, k37w1c29_tpr, 
                              k37w2c29_tpr, k37w3c29_tpr, k39w1c29_tpr, k39w2c29_tpr, 
                              k39w3c29_tpr), 
                      TNR = c(k11w1c29_tnr, k11w2c29_tnr, k11w3c29_tnr, k13w1c29_tnr, 
                              k13w2c29_tnr, k13w3c29_tnr, k15w1c29_tnr, k15w2c29_tnr, 
                              k15w3c29_tnr, k17w1c29_tnr, k17w2c29_tnr, k17w3c29_tnr, 
                              k19w1c29_tnr, k19w2c29_tnr, k19w3c29_tnr, k21w1c29_tnr, 
                              k21w2c29_tnr, k21w3c29_tnr, k23w1c29_tnr, k23w2c29_tnr, 
                              k23w3c29_tnr, k25w1c29_tnr, k25w2c29_tnr, k25w3c29_tnr, 
                              k27w1c29_tnr, k27w2c29_tnr, k27w3c29_tnr, k29w1c29_tnr, 
                              k29w2c29_tnr, k29w3c29_tnr, k31w1c29_tnr, k31w2c29_tnr, 
                              k31w3c29_tnr, k33w1c29_tnr, k33w2c29_tnr, k33w3c29_tnr, 
                              k35w1c29_tnr, k35w2c29_tnr, k35w3c29_tnr, k37w1c29_tnr, 
                              k37w2c29_tnr, k37w3c29_tnr, k39w1c29_tnr, k39w2c29_tnr, 
                              k39w3c29_tnr), 
                      Truescore = c(k11w1c29_truescore, k11w2c29_truescore, 
                                    k11w3c29_truescore, k13w1c29_truescore, 
                                    k13w2c29_truescore, k13w3c29_truescore, 
                                    k15w1c29_truescore, k15w2c29_truescore, 
                                    k15w3c29_truescore, k17w1c29_truescore, 
                                    k17w2c29_truescore, k17w3c29_truescore, 
                                    k19w1c29_truescore, k19w2c29_truescore, 
                                    k19w3c29_truescore, k21w1c29_truescore, 
                                    k21w2c29_truescore, k21w3c29_truescore, 
                                    k23w1c29_truescore, k23w2c29_truescore, 
                                    k23w3c29_truescore, k25w1c29_truescore, 
                                    k25w2c29_truescore, k25w3c29_truescore, 
                                    k27w1c29_truescore, k27w2c29_truescore, 
                                    k27w3c29_truescore, k29w1c29_truescore, 
                                    k29w2c29_truescore, k29w3c29_truescore, 
                                    k31w1c29_truescore, k31w2c29_truescore, 
                                    k31w3c29_truescore, k33w1c29_truescore, 
                                    k33w2c29_truescore, k33w3c29_truescore, 
                                    k35w1c29_truescore, k35w2c29_truescore, 
                                    k35w3c29_truescore, k37w1c29_truescore, 
                                    k37w2c29_truescore, k37w3c29_truescore, 
                                    k39w1c29_truescore, k39w2c29_truescore, 
                                    k39w3c29_truescore))

knitr::kable(c29_results[1:45, ], caption = "c29_results")

ggplot(c29_results, aes(k, Truescore, shape = Kernel, color = Kernel)) + 
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.29 (Truescore)")

# For the Cutoff of 0.29, compute the Minimum Distance to (0, 1) for each 
# combination of k and Kernel. 

c29_results <- c29_results %>% mutate(Distance = sqrt((1 - TPR)^2 + (1 - TNR)^2))

ggplot(c29_results, aes(k, Distance, shape = Kernel, color = Kernel)) +
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.29 (Distance)")

# For the Cutoff of 0.29, identify the optimal combination of values for k and Kernel 
# based on each of our assessment methods, Truescore and Mimimum Distance to (0, 1).

max(c29_results$Truescore)
(c29_opt_k_ts <- c29_results$k[which.max(c29_results$Truescore)])
(c29_opt_kernel_ts <- c29_results$Kernel[which.max(c29_results$Truescore)])
(c29_opt_cut_ts <- c29_results$Cut[which.max(c29_results$Truescore)])
(c29_opt_tpr_ts <- c29_results$TPR[which.max(c29_results$Truescore)])
(c29_opt_tnr_ts <- c29_results$TNR[which.max(c29_results$Truescore)])
(c29_opt_d_ts <- c29_results$Distance[which.max(c29_results$Truescore)])

min(c29_results$Distance)
(c29_opt_k_dist <- c29_results$k[which.min(c29_results$Distance)])
(c29_opt_kernel_dist <- c29_results$Kernel[which.min(c29_results$Distance)])
(c29_opt_cut_dist <- c29_results$Cut[which.min(c29_results$Distance)])  
(c29_opt_tpr_dist <- c29_results$TPR[which.min(c29_results$Distance)])
(c29_opt_tnr_dist <- c29_results$TNR[which.min(c29_results$Distance)])
(c29_opt_t_dist <- c29_results$Truescore[which.min(c29_results$Distance)])

############################
# 0.30 Cutoff
############################

# For the decision cutoff of 0.30, generate a confusion matrix for
# every combination of k (11:39 by two), kernel (1:3) and fold (1:10).

cm_c30 <- sapply(kwf_dfs_l, function(x) {
  ss <- subset(x, select = c(pred30, obs))
  confusionMatrix(ss$pred30, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c30) <- kwf_dfs_v

cm_c30_tables <- sapply(cm_c30, "[[", 2)
cm_c30_tables <- as_tibble(cm_c30_tables)

# For each combination of k and kernel, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each combination of k and kernel when the decision cutoff is 0.30.

(k11w1c30_tpr <- round(sum(cm_c30_tables[1, 1:10]) / (sum(cm_c30_tables[1, 1:10]) + sum(cm_c30_tables[2, 1:10])), 6))
(k11w1c30_tnr <- round(sum(cm_c30_tables[4, 1:10]) / (sum(cm_c30_tables[4, 1:10]) + sum(cm_c30_tables[3, 1:10])), 6))
(k11w1c30_truescore <- round((2 * k11w1c30_tpr * k11w1c30_tnr) / (k11w1c30_tpr + k11w1c30_tnr), 6))

(k11w2c30_tpr <- round(sum(cm_c30_tables[1, 11:20]) / (sum(cm_c30_tables[1, 11:20]) + sum(cm_c30_tables[2, 11:20])), 6))
(k11w2c30_tnr <- round(sum(cm_c30_tables[4, 11:20]) / (sum(cm_c30_tables[4, 11:20]) + sum(cm_c30_tables[3, 11:20])), 6))
(k11w2c30_truescore <- round((2 * k11w2c30_tpr * k11w2c30_tnr) / (k11w2c30_tpr + k11w2c30_tnr), 6))

(k11w3c30_tpr <- round(sum(cm_c30_tables[1, 21:30]) / (sum(cm_c30_tables[1, 21:30]) + sum(cm_c30_tables[2, 21:30])), 6))
(k11w3c30_tnr <- round(sum(cm_c30_tables[4, 21:30]) / (sum(cm_c30_tables[4, 21:30]) + sum(cm_c30_tables[3, 21:30])), 6))
(k11w3c30_truescore <- round((2 * k11w3c30_tpr * k11w3c30_tnr) / (k11w3c30_tpr + k11w3c30_tnr), 6))


(k13w1c30_tpr <- round(sum(cm_c30_tables[1, 31:40]) / (sum(cm_c30_tables[1, 31:40]) + sum(cm_c30_tables[2, 31:40])), 6))
(k13w1c30_tnr <- round(sum(cm_c30_tables[4, 31:40]) / (sum(cm_c30_tables[4, 31:40]) + sum(cm_c30_tables[3, 31:40])), 6))
(k13w1c30_truescore <- round((2 * k13w1c30_tpr * k13w1c30_tnr) / (k13w1c30_tpr + k13w1c30_tnr), 6))

(k13w2c30_tpr <- round(sum(cm_c30_tables[1, 41:50]) / (sum(cm_c30_tables[1, 41:50]) + sum(cm_c30_tables[2, 41:50])), 6))
(k13w2c30_tnr <- round(sum(cm_c30_tables[4, 41:50]) / (sum(cm_c30_tables[4, 41:50]) + sum(cm_c30_tables[3, 41:50])), 6))
(k13w2c30_truescore <- round((2 * k13w2c30_tpr * k13w2c30_tnr) / (k13w2c30_tpr + k13w2c30_tnr), 6))

(k13w3c30_tpr <- round(sum(cm_c30_tables[1, 51:60]) / (sum(cm_c30_tables[1, 51:60]) + sum(cm_c30_tables[2, 51:60])), 6))
(k13w3c30_tnr <- round(sum(cm_c30_tables[4, 51:60]) / (sum(cm_c30_tables[4, 51:60]) + sum(cm_c30_tables[3, 51:60])), 6))
(k13w3c30_truescore <- round((2 * k13w3c30_tpr * k13w3c30_tnr) / (k13w3c30_tpr + k13w3c30_tnr), 6))


(k15w1c30_tpr <- round(sum(cm_c30_tables[1, 61:70]) / (sum(cm_c30_tables[1, 61:70]) + sum(cm_c30_tables[2, 61:70])), 6))
(k15w1c30_tnr <- round(sum(cm_c30_tables[4, 61:70]) / (sum(cm_c30_tables[4, 61:70]) + sum(cm_c30_tables[3, 61:70])), 6))
(k15w1c30_truescore <- round((2 * k15w1c30_tpr * k15w1c30_tnr) / (k15w1c30_tpr + k15w1c30_tnr), 6))

(k15w2c30_tpr <- round(sum(cm_c30_tables[1, 71:80]) / (sum(cm_c30_tables[1, 71:80]) + sum(cm_c30_tables[2, 71:80])), 6))
(k15w2c30_tnr <- round(sum(cm_c30_tables[4, 71:80]) / (sum(cm_c30_tables[4, 71:80]) + sum(cm_c30_tables[3, 71:80])), 6))
(k15w2c30_truescore <- round((2 * k15w2c30_tpr * k15w2c30_tnr) / (k15w2c30_tpr + k15w2c30_tnr), 6))

(k15w3c30_tpr <- round(sum(cm_c30_tables[1, 81:90]) / (sum(cm_c30_tables[1, 81:90]) + sum(cm_c30_tables[2, 81:90])), 6))
(k15w3c30_tnr <- round(sum(cm_c30_tables[4, 81:90]) / (sum(cm_c30_tables[4, 81:90]) + sum(cm_c30_tables[3, 81:90])), 6))
(k15w3c30_truescore <- round((2 * k15w3c30_tpr * k15w3c30_tnr) / (k15w3c30_tpr + k15w3c30_tnr), 6))


(k17w1c30_tpr <- round(sum(cm_c30_tables[1, 91:100]) / (sum(cm_c30_tables[1, 91:100]) + sum(cm_c30_tables[2, 91:100])), 6))
(k17w1c30_tnr <- round(sum(cm_c30_tables[4, 91:100]) / (sum(cm_c30_tables[4, 91:100]) + sum(cm_c30_tables[3, 91:100])), 6))
(k17w1c30_truescore <- round((2 * k17w1c30_tpr * k17w1c30_tnr) / (k17w1c30_tpr + k17w1c30_tnr), 6))

(k17w2c30_tpr <- round(sum(cm_c30_tables[1, 101:110]) / (sum(cm_c30_tables[1, 101:110]) + sum(cm_c30_tables[2, 101:110])), 6))
(k17w2c30_tnr <- round(sum(cm_c30_tables[4, 101:110]) / (sum(cm_c30_tables[4, 101:110]) + sum(cm_c30_tables[3, 101:110])), 6))
(k17w2c30_truescore <- round((2 * k17w2c30_tpr * k17w2c30_tnr) / (k17w2c30_tpr + k17w2c30_tnr), 6))

(k17w3c30_tpr <- round(sum(cm_c30_tables[1, 111:120]) / (sum(cm_c30_tables[1, 111:120]) + sum(cm_c30_tables[2, 111:120])), 6))
(k17w3c30_tnr <- round(sum(cm_c30_tables[4, 111:120]) / (sum(cm_c30_tables[4, 111:120]) + sum(cm_c30_tables[3, 111:120])), 6))
(k17w3c30_truescore <- round((2 * k17w3c30_tpr * k17w3c30_tnr) / (k17w3c30_tpr + k17w3c30_tnr), 6))


(k19w1c30_tpr <- round(sum(cm_c30_tables[1, 121:130]) / (sum(cm_c30_tables[1, 121:130]) + sum(cm_c30_tables[2, 121:130])), 6))
(k19w1c30_tnr <- round(sum(cm_c30_tables[4, 121:130]) / (sum(cm_c30_tables[4, 121:130]) + sum(cm_c30_tables[3, 121:130])), 6))
(k19w1c30_truescore <- round((2 * k19w1c30_tpr * k19w1c30_tnr) / (k19w1c30_tpr + k19w1c30_tnr), 6))

(k19w2c30_tpr <- round(sum(cm_c30_tables[1, 131:140]) / (sum(cm_c30_tables[1, 131:140]) + sum(cm_c30_tables[2, 131:140])), 6))
(k19w2c30_tnr <- round(sum(cm_c30_tables[4, 131:140]) / (sum(cm_c30_tables[4, 131:140]) + sum(cm_c30_tables[3, 131:140])), 6))
(k19w2c30_truescore <- round((2 * k19w2c30_tpr * k19w2c30_tnr) / (k19w2c30_tpr + k19w2c30_tnr), 6))

(k19w3c30_tpr <- round(sum(cm_c30_tables[1, 141:150]) / (sum(cm_c30_tables[1, 141:150]) + sum(cm_c30_tables[2, 141:150])), 6))
(k19w3c30_tnr <- round(sum(cm_c30_tables[4, 141:150]) / (sum(cm_c30_tables[4, 141:150]) + sum(cm_c30_tables[3, 141:150])), 6))
(k19w3c30_truescore <- round((2 * k19w3c30_tpr * k19w3c30_tnr) / (k19w3c30_tpr + k19w3c30_tnr), 6))


(k21w1c30_tpr <- round(sum(cm_c30_tables[1, 151:160]) / (sum(cm_c30_tables[1, 151:160]) + sum(cm_c30_tables[2, 151:160])), 6))
(k21w1c30_tnr <- round(sum(cm_c30_tables[4, 151:160]) / (sum(cm_c30_tables[4, 151:160]) + sum(cm_c30_tables[3, 151:160])), 6))
(k21w1c30_truescore <- round((2 * k21w1c30_tpr * k21w1c30_tnr) / (k21w1c30_tpr + k21w1c30_tnr), 6))

(k21w2c30_tpr <- round(sum(cm_c30_tables[1, 161:170]) / (sum(cm_c30_tables[1, 161:170]) + sum(cm_c30_tables[2, 161:170])), 6))
(k21w2c30_tnr <- round(sum(cm_c30_tables[4, 161:170]) / (sum(cm_c30_tables[4, 161:170]) + sum(cm_c30_tables[3, 161:170])), 6))
(k21w2c30_truescore <- round((2 * k21w2c30_tpr * k21w2c30_tnr) / (k21w2c30_tpr + k21w2c30_tnr), 6))

(k21w3c30_tpr <- round(sum(cm_c30_tables[1, 171:180]) / (sum(cm_c30_tables[1, 171:180]) + sum(cm_c30_tables[2, 171:180])), 6))
(k21w3c30_tnr <- round(sum(cm_c30_tables[4, 171:180]) / (sum(cm_c30_tables[4, 171:180]) + sum(cm_c30_tables[3, 171:180])), 6))
(k21w3c30_truescore <- round((2 * k21w3c30_tpr * k21w3c30_tnr) / (k21w3c30_tpr + k21w3c30_tnr), 6))


(k23w1c30_tpr <- round(sum(cm_c30_tables[1, 181:190]) / (sum(cm_c30_tables[1, 181:190]) + sum(cm_c30_tables[2, 181:190])), 6))
(k23w1c30_tnr <- round(sum(cm_c30_tables[4, 181:190]) / (sum(cm_c30_tables[4, 181:190]) + sum(cm_c30_tables[3, 181:190])), 6))
(k23w1c30_truescore <- round((2 * k23w1c30_tpr * k23w1c30_tnr) / (k23w1c30_tpr + k23w1c30_tnr), 6))

(k23w2c30_tpr <- round(sum(cm_c30_tables[1, 191:200]) / (sum(cm_c30_tables[1, 191:200]) + sum(cm_c30_tables[2, 191:200])), 6))
(k23w2c30_tnr <- round(sum(cm_c30_tables[4, 191:200]) / (sum(cm_c30_tables[4, 191:200]) + sum(cm_c30_tables[3, 191:200])), 6))
(k23w2c30_truescore <- round((2 * k23w2c30_tpr * k23w2c30_tnr) / (k23w2c30_tpr + k23w2c30_tnr), 6))

(k23w3c30_tpr <- round(sum(cm_c30_tables[1, 201:210]) / (sum(cm_c30_tables[1, 201:210]) + sum(cm_c30_tables[2, 201:210])), 6))
(k23w3c30_tnr <- round(sum(cm_c30_tables[4, 201:210]) / (sum(cm_c30_tables[4, 201:210]) + sum(cm_c30_tables[3, 201:210])), 6))
(k23w3c30_truescore <- round((2 * k23w3c30_tpr * k23w3c30_tnr) / (k23w3c30_tpr + k23w3c30_tnr), 6))


(k25w1c30_tpr <- round(sum(cm_c30_tables[1, 211:220]) / (sum(cm_c30_tables[1, 211:220]) + sum(cm_c30_tables[2, 211:220])), 6))
(k25w1c30_tnr <- round(sum(cm_c30_tables[4, 211:220]) / (sum(cm_c30_tables[4, 211:220]) + sum(cm_c30_tables[3, 211:220])), 6))
(k25w1c30_truescore <- round((2 * k25w1c30_tpr * k25w1c30_tnr) / (k25w1c30_tpr + k25w1c30_tnr), 6))

(k25w2c30_tpr <- round(sum(cm_c30_tables[1, 221:230]) / (sum(cm_c30_tables[1, 221:230]) + sum(cm_c30_tables[2, 221:230])), 6))
(k25w2c30_tnr <- round(sum(cm_c30_tables[4, 221:230]) / (sum(cm_c30_tables[4, 221:230]) + sum(cm_c30_tables[3, 221:230])), 6))
(k25w2c30_truescore <- round((2 * k25w2c30_tpr * k25w2c30_tnr) / (k25w2c30_tpr + k25w2c30_tnr), 6))

(k25w3c30_tpr <- round(sum(cm_c30_tables[1, 231:240]) / (sum(cm_c30_tables[1, 231:240]) + sum(cm_c30_tables[2, 231:240])), 6))
(k25w3c30_tnr <- round(sum(cm_c30_tables[4, 231:240]) / (sum(cm_c30_tables[4, 231:240]) + sum(cm_c30_tables[3, 231:240])), 6))
(k25w3c30_truescore <- round((2 * k25w3c30_tpr * k25w3c30_tnr) / (k25w3c30_tpr + k25w3c30_tnr), 6))


(k27w1c30_tpr <- round(sum(cm_c30_tables[1, 241:250]) / (sum(cm_c30_tables[1, 241:250]) + sum(cm_c30_tables[2, 241:250])), 6))
(k27w1c30_tnr <- round(sum(cm_c30_tables[4, 241:250]) / (sum(cm_c30_tables[4, 241:250]) + sum(cm_c30_tables[3, 241:250])), 6))
(k27w1c30_truescore <- round((2 * k27w1c30_tpr * k27w1c30_tnr) / (k27w1c30_tpr + k27w1c30_tnr), 6))

(k27w2c30_tpr <- round(sum(cm_c30_tables[1, 251:260]) / (sum(cm_c30_tables[1, 251:260]) + sum(cm_c30_tables[2, 251:260])), 6))
(k27w2c30_tnr <- round(sum(cm_c30_tables[4, 251:260]) / (sum(cm_c30_tables[4, 251:260]) + sum(cm_c30_tables[3, 251:260])), 6))
(k27w2c30_truescore <- round((2 * k27w2c30_tpr * k27w2c30_tnr) / (k27w2c30_tpr + k27w2c30_tnr), 6))

(k27w3c30_tpr <- round(sum(cm_c30_tables[1, 261:270]) / (sum(cm_c30_tables[1, 261:270]) + sum(cm_c30_tables[2, 261:270])), 6))
(k27w3c30_tnr <- round(sum(cm_c30_tables[4, 261:270]) / (sum(cm_c30_tables[4, 261:270]) + sum(cm_c30_tables[3, 261:270])), 6))
(k27w3c30_truescore <- round((2 * k27w3c30_tpr * k27w3c30_tnr) / (k27w3c30_tpr + k27w3c30_tnr), 6))


(k29w1c30_tpr <- round(sum(cm_c30_tables[1, 271:280]) / (sum(cm_c30_tables[1, 271:280]) + sum(cm_c30_tables[2, 271:280])), 6))
(k29w1c30_tnr <- round(sum(cm_c30_tables[4, 271:280]) / (sum(cm_c30_tables[4, 271:280]) + sum(cm_c30_tables[3, 271:280])), 6))
(k29w1c30_truescore <- round((2 * k29w1c30_tpr * k29w1c30_tnr) / (k29w1c30_tpr + k29w1c30_tnr), 6))

(k29w2c30_tpr <- round(sum(cm_c30_tables[1, 281:290]) / (sum(cm_c30_tables[1, 281:290]) + sum(cm_c30_tables[2, 281:290])), 6))
(k29w2c30_tnr <- round(sum(cm_c30_tables[4, 281:290]) / (sum(cm_c30_tables[4, 281:290]) + sum(cm_c30_tables[3, 281:290])), 6))
(k29w2c30_truescore <- round((2 * k29w2c30_tpr * k29w2c30_tnr) / (k29w2c30_tpr + k29w2c30_tnr), 6))

(k29w3c30_tpr <- round(sum(cm_c30_tables[1, 291:300]) / (sum(cm_c30_tables[1, 291:300]) + sum(cm_c30_tables[2, 291:300])), 6))
(k29w3c30_tnr <- round(sum(cm_c30_tables[4, 291:300]) / (sum(cm_c30_tables[4, 291:300]) + sum(cm_c30_tables[3, 291:300])), 6))
(k29w3c30_truescore <- round((2 * k29w3c30_tpr * k29w3c30_tnr) / (k29w3c30_tpr + k29w3c30_tnr), 6))


(k31w1c30_tpr <- round(sum(cm_c30_tables[1, 301:310]) / (sum(cm_c30_tables[1, 301:310]) + sum(cm_c30_tables[2, 301:310])), 6))
(k31w1c30_tnr <- round(sum(cm_c30_tables[4, 301:310]) / (sum(cm_c30_tables[4, 301:310]) + sum(cm_c30_tables[3, 301:310])), 6))
(k31w1c30_truescore <- round((2 * k31w1c30_tpr * k31w1c30_tnr) / (k31w1c30_tpr + k31w1c30_tnr), 6))

(k31w2c30_tpr <- round(sum(cm_c30_tables[1, 311:320]) / (sum(cm_c30_tables[1, 311:320]) + sum(cm_c30_tables[2, 311:320])), 6))
(k31w2c30_tnr <- round(sum(cm_c30_tables[4, 311:320]) / (sum(cm_c30_tables[4, 311:320]) + sum(cm_c30_tables[3, 311:320])), 6))
(k31w2c30_truescore <- round((2 * k31w2c30_tpr * k31w2c30_tnr) / (k31w2c30_tpr + k31w2c30_tnr), 6))

(k31w3c30_tpr <- round(sum(cm_c30_tables[1, 321:330]) / (sum(cm_c30_tables[1, 321:330]) + sum(cm_c30_tables[2, 321:330])), 6))
(k31w3c30_tnr <- round(sum(cm_c30_tables[4, 321:330]) / (sum(cm_c30_tables[4, 321:330]) + sum(cm_c30_tables[3, 321:330])), 6))
(k31w3c30_truescore <- round((2 * k31w3c30_tpr * k31w3c30_tnr) / (k31w3c30_tpr + k31w3c30_tnr), 6))


(k33w1c30_tpr <- round(sum(cm_c30_tables[1, 331:340]) / (sum(cm_c30_tables[1, 331:340]) + sum(cm_c30_tables[2, 331:340])), 6))
(k33w1c30_tnr <- round(sum(cm_c30_tables[4, 331:340]) / (sum(cm_c30_tables[4, 331:340]) + sum(cm_c30_tables[3, 331:340])), 6))
(k33w1c30_truescore <- round((2 * k33w1c30_tpr * k33w1c30_tnr) / (k33w1c30_tpr + k33w1c30_tnr), 6))

(k33w2c30_tpr <- round(sum(cm_c30_tables[1, 341:350]) / (sum(cm_c30_tables[1, 341:350]) + sum(cm_c30_tables[2, 341:350])), 6))
(k33w2c30_tnr <- round(sum(cm_c30_tables[4, 341:350]) / (sum(cm_c30_tables[4, 341:350]) + sum(cm_c30_tables[3, 341:350])), 6))
(k33w2c30_truescore <- round((2 * k33w2c30_tpr * k33w2c30_tnr) / (k33w2c30_tpr + k33w2c30_tnr), 6))

(k33w3c30_tpr <- round(sum(cm_c30_tables[1, 351:360]) / (sum(cm_c30_tables[1, 351:360]) + sum(cm_c30_tables[2, 351:360])), 6))
(k33w3c30_tnr <- round(sum(cm_c30_tables[4, 351:360]) / (sum(cm_c30_tables[4, 351:360]) + sum(cm_c30_tables[3, 351:360])), 6))
(k33w3c30_truescore <- round((2 * k33w3c30_tpr * k33w3c30_tnr) / (k33w3c30_tpr + k33w3c30_tnr), 6))


(k35w1c30_tpr <- round(sum(cm_c30_tables[1, 361:370]) / (sum(cm_c30_tables[1, 361:370]) + sum(cm_c30_tables[2, 361:370])), 6))
(k35w1c30_tnr <- round(sum(cm_c30_tables[4, 361:370]) / (sum(cm_c30_tables[4, 361:370]) + sum(cm_c30_tables[3, 361:370])), 6))
(k35w1c30_truescore <- round((2 * k35w1c30_tpr * k35w1c30_tnr) / (k35w1c30_tpr + k35w1c30_tnr), 6))

(k35w2c30_tpr <- round(sum(cm_c30_tables[1, 371:380]) / (sum(cm_c30_tables[1, 371:380]) + sum(cm_c30_tables[2, 371:380])), 6))
(k35w2c30_tnr <- round(sum(cm_c30_tables[4, 371:380]) / (sum(cm_c30_tables[4, 371:380]) + sum(cm_c30_tables[3, 371:380])), 6))
(k35w2c30_truescore <- round((2 * k35w2c30_tpr * k35w2c30_tnr) / (k35w2c30_tpr + k35w2c30_tnr), 6))

(k35w3c30_tpr <- round(sum(cm_c30_tables[1, 381:390]) / (sum(cm_c30_tables[1, 381:390]) + sum(cm_c30_tables[2, 381:390])), 6))
(k35w3c30_tnr <- round(sum(cm_c30_tables[4, 381:390]) / (sum(cm_c30_tables[4, 381:390]) + sum(cm_c30_tables[3, 381:390])), 6))
(k35w3c30_truescore <- round((2 * k35w3c30_tpr * k35w3c30_tnr) / (k35w3c30_tpr + k35w3c30_tnr), 6))


(k37w1c30_tpr <- round(sum(cm_c30_tables[1, 391:400]) / (sum(cm_c30_tables[1, 391:400]) + sum(cm_c30_tables[2, 391:400])), 6))
(k37w1c30_tnr <- round(sum(cm_c30_tables[4, 391:400]) / (sum(cm_c30_tables[4, 391:400]) + sum(cm_c30_tables[3, 391:400])), 6))
(k37w1c30_truescore <- round((2 * k37w1c30_tpr * k37w1c30_tnr) / (k37w1c30_tpr + k37w1c30_tnr), 6))

(k37w2c30_tpr <- round(sum(cm_c30_tables[1, 401:410]) / (sum(cm_c30_tables[1, 401:410]) + sum(cm_c30_tables[2, 401:410])), 6))
(k37w2c30_tnr <- round(sum(cm_c30_tables[4, 401:410]) / (sum(cm_c30_tables[4, 401:410]) + sum(cm_c30_tables[3, 401:410])), 6))
(k37w2c30_truescore <- round((2 * k37w2c30_tpr * k37w2c30_tnr) / (k37w2c30_tpr + k37w2c30_tnr), 6))

(k37w3c30_tpr <- round(sum(cm_c30_tables[1, 411:420]) / (sum(cm_c30_tables[1, 411:420]) + sum(cm_c30_tables[2, 411:420])), 6))
(k37w3c30_tnr <- round(sum(cm_c30_tables[4, 411:420]) / (sum(cm_c30_tables[4, 411:420]) + sum(cm_c30_tables[3, 411:420])), 6))
(k37w3c30_truescore <- round((2 * k37w3c30_tpr * k37w3c30_tnr) / (k37w3c30_tpr + k37w3c30_tnr), 6))


(k39w1c30_tpr <- round(sum(cm_c30_tables[1, 421:430]) / (sum(cm_c30_tables[1, 421:430]) + sum(cm_c30_tables[2, 421:430])), 6))
(k39w1c30_tnr <- round(sum(cm_c30_tables[4, 421:430]) / (sum(cm_c30_tables[4, 421:430]) + sum(cm_c30_tables[3, 421:430])), 6))
(k39w1c30_truescore <- round((2 * k39w1c30_tpr * k39w1c30_tnr) / (k39w1c30_tpr + k39w1c30_tnr), 6))

(k39w2c30_tpr <- round(sum(cm_c30_tables[1, 431:440]) / (sum(cm_c30_tables[1, 431:440]) + sum(cm_c30_tables[2, 431:440])), 6))
(k39w2c30_tnr <- round(sum(cm_c30_tables[4, 431:440]) / (sum(cm_c30_tables[4, 431:440]) + sum(cm_c30_tables[3, 431:440])), 6))
(k39w2c30_truescore <- round((2 * k39w2c30_tpr * k39w2c30_tnr) / (k39w2c30_tpr + k39w2c30_tnr), 6))

(k39w3c30_tpr <- round(sum(cm_c30_tables[1, 441:450]) / (sum(cm_c30_tables[1, 441:450]) + sum(cm_c30_tables[2, 441:450])), 6))
(k39w3c30_tnr <- round(sum(cm_c30_tables[4, 441:450]) / (sum(cm_c30_tables[4, 441:450]) + sum(cm_c30_tables[3, 441:450])), 6))
(k39w3c30_truescore <- round((2 * k39w3c30_tpr * k39w3c30_tnr) / (k39w3c30_tpr + k39w3c30_tnr), 6))


# Compile the 0.30 cutoff results in a table, and identify the combination of 
# of k and kernel that maximizes the Truescore.

c30_results <- tibble(k = c(11, 11, 11, 13, 13, 13, 15, 15, 15, 
                            17, 17, 17, 19, 19, 19, 21, 21, 21, 
                            23, 23, 23, 25, 25, 25, 27, 27, 27, 
                            29, 29, 29, 31, 31, 31, 33, 33, 33, 
                            35, 35, 35, 37, 37, 37, 39, 39, 39), 
                      Kernel = rep(c("triangular", "gaussian", "optimal"), 15), 
                      Cut = 0.30, 
                      TPR = c(k11w1c30_tpr, k11w2c30_tpr, k11w3c30_tpr, k13w1c30_tpr, 
                              k13w2c30_tpr, k13w3c30_tpr, k15w1c30_tpr, k15w2c30_tpr, 
                              k15w3c30_tpr, k17w1c30_tpr, k17w2c30_tpr, k17w3c30_tpr, 
                              k19w1c30_tpr, k19w2c30_tpr, k19w3c30_tpr, k21w1c30_tpr, 
                              k21w2c30_tpr, k21w3c30_tpr, k23w1c30_tpr, k23w2c30_tpr, 
                              k23w3c30_tpr, k25w1c30_tpr, k25w2c30_tpr, k25w3c30_tpr, 
                              k27w1c30_tpr, k27w2c30_tpr, k27w3c30_tpr, k29w1c30_tpr, 
                              k29w2c30_tpr, k29w3c30_tpr, k31w1c30_tpr, k31w2c30_tpr, 
                              k31w3c30_tpr, k33w1c30_tpr, k33w2c30_tpr, k33w3c30_tpr, 
                              k35w1c30_tpr, k35w2c30_tpr, k35w3c30_tpr, k37w1c30_tpr, 
                              k37w2c30_tpr, k37w3c30_tpr, k39w1c30_tpr, k39w2c30_tpr, 
                              k39w3c30_tpr), 
                      TNR = c(k11w1c30_tnr, k11w2c30_tnr, k11w3c30_tnr, k13w1c30_tnr, 
                              k13w2c30_tnr, k13w3c30_tnr, k15w1c30_tnr, k15w2c30_tnr, 
                              k15w3c30_tnr, k17w1c30_tnr, k17w2c30_tnr, k17w3c30_tnr, 
                              k19w1c30_tnr, k19w2c30_tnr, k19w3c30_tnr, k21w1c30_tnr, 
                              k21w2c30_tnr, k21w3c30_tnr, k23w1c30_tnr, k23w2c30_tnr, 
                              k23w3c30_tnr, k25w1c30_tnr, k25w2c30_tnr, k25w3c30_tnr, 
                              k27w1c30_tnr, k27w2c30_tnr, k27w3c30_tnr, k29w1c30_tnr, 
                              k29w2c30_tnr, k29w3c30_tnr, k31w1c30_tnr, k31w2c30_tnr, 
                              k31w3c30_tnr, k33w1c30_tnr, k33w2c30_tnr, k33w3c30_tnr, 
                              k35w1c30_tnr, k35w2c30_tnr, k35w3c30_tnr, k37w1c30_tnr, 
                              k37w2c30_tnr, k37w3c30_tnr, k39w1c30_tnr, k39w2c30_tnr, 
                              k39w3c30_tnr), 
                      Truescore = c(k11w1c30_truescore, k11w2c30_truescore, 
                                    k11w3c30_truescore, k13w1c30_truescore, 
                                    k13w2c30_truescore, k13w3c30_truescore, 
                                    k15w1c30_truescore, k15w2c30_truescore, 
                                    k15w3c30_truescore, k17w1c30_truescore, 
                                    k17w2c30_truescore, k17w3c30_truescore, 
                                    k19w1c30_truescore, k19w2c30_truescore, 
                                    k19w3c30_truescore, k21w1c30_truescore, 
                                    k21w2c30_truescore, k21w3c30_truescore, 
                                    k23w1c30_truescore, k23w2c30_truescore, 
                                    k23w3c30_truescore, k25w1c30_truescore, 
                                    k25w2c30_truescore, k25w3c30_truescore, 
                                    k27w1c30_truescore, k27w2c30_truescore, 
                                    k27w3c30_truescore, k29w1c30_truescore, 
                                    k29w2c30_truescore, k29w3c30_truescore, 
                                    k31w1c30_truescore, k31w2c30_truescore, 
                                    k31w3c30_truescore, k33w1c30_truescore, 
                                    k33w2c30_truescore, k33w3c30_truescore, 
                                    k35w1c30_truescore, k35w2c30_truescore, 
                                    k35w3c30_truescore, k37w1c30_truescore, 
                                    k37w2c30_truescore, k37w3c30_truescore, 
                                    k39w1c30_truescore, k39w2c30_truescore, 
                                    k39w3c30_truescore))

knitr::kable(c30_results[1:45, ], caption = "c30_results")

ggplot(c30_results, aes(k, Truescore, shape = Kernel, color = Kernel)) + 
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.30 (Truescore)")

# For the Cutoff of 0.30, compute the Minimum Distance to (0, 1) for each 
# combination of k and Kernel. 

c30_results <- c30_results %>% mutate(Distance = sqrt((1 - TPR)^2 + (1 - TNR)^2))

ggplot(c30_results, aes(k, Distance, shape = Kernel, color = Kernel)) +
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.30 (Distance)")

# For the Cutoff of 0.30, identify the optimal combination of values for k and Kernel 
# based on each of our assessment methods, Truescore and Mimimum Distance to (0, 1).

max(c30_results$Truescore)
(c30_opt_k_ts <- c30_results$k[which.max(c30_results$Truescore)])
(c30_opt_kernel_ts <- c30_results$Kernel[which.max(c30_results$Truescore)])
(c30_opt_cut_ts <- c30_results$Cut[which.max(c30_results$Truescore)])
(c30_opt_tpr_ts <- c30_results$TPR[which.max(c30_results$Truescore)])
(c30_opt_tnr_ts <- c30_results$TNR[which.max(c30_results$Truescore)])
(c30_opt_d_ts <- c30_results$Distance[which.max(c30_results$Truescore)])

min(c30_results$Distance)
(c30_opt_k_dist <- c30_results$k[which.min(c30_results$Distance)])
(c30_opt_kernel_dist <- c30_results$Kernel[which.min(c30_results$Distance)])
(c30_opt_cut_dist <- c30_results$Cut[which.min(c30_results$Distance)])  
(c30_opt_tpr_dist <- c30_results$TPR[which.min(c30_results$Distance)])
(c30_opt_tnr_dist <- c30_results$TNR[which.min(c30_results$Distance)])
(c30_opt_t_dist <- c30_results$Truescore[which.min(c30_results$Distance)])

############################
# 0.31 Cutoff
############################

# For the decision cutoff of 0.31, generate a confusion matrix for
# every combination of k (11:39 by two), kernel (1:3) and fold (1:10).

cm_c31 <- sapply(kwf_dfs_l, function(x) {
  ss <- subset(x, select = c(pred31, obs))
  confusionMatrix(ss$pred31, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c31) <- kwf_dfs_v

cm_c31_tables <- sapply(cm_c31, "[[", 2)
cm_c31_tables <- as_tibble(cm_c31_tables)

# For each combination of k and kernel, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each combination of k and kernel when the decision cutoff is 0.31.

(k11w1c31_tpr <- round(sum(cm_c31_tables[1, 1:10]) / (sum(cm_c31_tables[1, 1:10]) + sum(cm_c31_tables[2, 1:10])), 6))
(k11w1c31_tnr <- round(sum(cm_c31_tables[4, 1:10]) / (sum(cm_c31_tables[4, 1:10]) + sum(cm_c31_tables[3, 1:10])), 6))
(k11w1c31_truescore <- round((2 * k11w1c31_tpr * k11w1c31_tnr) / (k11w1c31_tpr + k11w1c31_tnr), 6))

(k11w2c31_tpr <- round(sum(cm_c31_tables[1, 11:20]) / (sum(cm_c31_tables[1, 11:20]) + sum(cm_c31_tables[2, 11:20])), 6))
(k11w2c31_tnr <- round(sum(cm_c31_tables[4, 11:20]) / (sum(cm_c31_tables[4, 11:20]) + sum(cm_c31_tables[3, 11:20])), 6))
(k11w2c31_truescore <- round((2 * k11w2c31_tpr * k11w2c31_tnr) / (k11w2c31_tpr + k11w2c31_tnr), 6))

(k11w3c31_tpr <- round(sum(cm_c31_tables[1, 21:30]) / (sum(cm_c31_tables[1, 21:30]) + sum(cm_c31_tables[2, 21:30])), 6))
(k11w3c31_tnr <- round(sum(cm_c31_tables[4, 21:30]) / (sum(cm_c31_tables[4, 21:30]) + sum(cm_c31_tables[3, 21:30])), 6))
(k11w3c31_truescore <- round((2 * k11w3c31_tpr * k11w3c31_tnr) / (k11w3c31_tpr + k11w3c31_tnr), 6))


(k13w1c31_tpr <- round(sum(cm_c31_tables[1, 31:40]) / (sum(cm_c31_tables[1, 31:40]) + sum(cm_c31_tables[2, 31:40])), 6))
(k13w1c31_tnr <- round(sum(cm_c31_tables[4, 31:40]) / (sum(cm_c31_tables[4, 31:40]) + sum(cm_c31_tables[3, 31:40])), 6))
(k13w1c31_truescore <- round((2 * k13w1c31_tpr * k13w1c31_tnr) / (k13w1c31_tpr + k13w1c31_tnr), 6))

(k13w2c31_tpr <- round(sum(cm_c31_tables[1, 41:50]) / (sum(cm_c31_tables[1, 41:50]) + sum(cm_c31_tables[2, 41:50])), 6))
(k13w2c31_tnr <- round(sum(cm_c31_tables[4, 41:50]) / (sum(cm_c31_tables[4, 41:50]) + sum(cm_c31_tables[3, 41:50])), 6))
(k13w2c31_truescore <- round((2 * k13w2c31_tpr * k13w2c31_tnr) / (k13w2c31_tpr + k13w2c31_tnr), 6))

(k13w3c31_tpr <- round(sum(cm_c31_tables[1, 51:60]) / (sum(cm_c31_tables[1, 51:60]) + sum(cm_c31_tables[2, 51:60])), 6))
(k13w3c31_tnr <- round(sum(cm_c31_tables[4, 51:60]) / (sum(cm_c31_tables[4, 51:60]) + sum(cm_c31_tables[3, 51:60])), 6))
(k13w3c31_truescore <- round((2 * k13w3c31_tpr * k13w3c31_tnr) / (k13w3c31_tpr + k13w3c31_tnr), 6))


(k15w1c31_tpr <- round(sum(cm_c31_tables[1, 61:70]) / (sum(cm_c31_tables[1, 61:70]) + sum(cm_c31_tables[2, 61:70])), 6))
(k15w1c31_tnr <- round(sum(cm_c31_tables[4, 61:70]) / (sum(cm_c31_tables[4, 61:70]) + sum(cm_c31_tables[3, 61:70])), 6))
(k15w1c31_truescore <- round((2 * k15w1c31_tpr * k15w1c31_tnr) / (k15w1c31_tpr + k15w1c31_tnr), 6))

(k15w2c31_tpr <- round(sum(cm_c31_tables[1, 71:80]) / (sum(cm_c31_tables[1, 71:80]) + sum(cm_c31_tables[2, 71:80])), 6))
(k15w2c31_tnr <- round(sum(cm_c31_tables[4, 71:80]) / (sum(cm_c31_tables[4, 71:80]) + sum(cm_c31_tables[3, 71:80])), 6))
(k15w2c31_truescore <- round((2 * k15w2c31_tpr * k15w2c31_tnr) / (k15w2c31_tpr + k15w2c31_tnr), 6))

(k15w3c31_tpr <- round(sum(cm_c31_tables[1, 81:90]) / (sum(cm_c31_tables[1, 81:90]) + sum(cm_c31_tables[2, 81:90])), 6))
(k15w3c31_tnr <- round(sum(cm_c31_tables[4, 81:90]) / (sum(cm_c31_tables[4, 81:90]) + sum(cm_c31_tables[3, 81:90])), 6))
(k15w3c31_truescore <- round((2 * k15w3c31_tpr * k15w3c31_tnr) / (k15w3c31_tpr + k15w3c31_tnr), 6))


(k17w1c31_tpr <- round(sum(cm_c31_tables[1, 91:100]) / (sum(cm_c31_tables[1, 91:100]) + sum(cm_c31_tables[2, 91:100])), 6))
(k17w1c31_tnr <- round(sum(cm_c31_tables[4, 91:100]) / (sum(cm_c31_tables[4, 91:100]) + sum(cm_c31_tables[3, 91:100])), 6))
(k17w1c31_truescore <- round((2 * k17w1c31_tpr * k17w1c31_tnr) / (k17w1c31_tpr + k17w1c31_tnr), 6))

(k17w2c31_tpr <- round(sum(cm_c31_tables[1, 101:110]) / (sum(cm_c31_tables[1, 101:110]) + sum(cm_c31_tables[2, 101:110])), 6))
(k17w2c31_tnr <- round(sum(cm_c31_tables[4, 101:110]) / (sum(cm_c31_tables[4, 101:110]) + sum(cm_c31_tables[3, 101:110])), 6))
(k17w2c31_truescore <- round((2 * k17w2c31_tpr * k17w2c31_tnr) / (k17w2c31_tpr + k17w2c31_tnr), 6))

(k17w3c31_tpr <- round(sum(cm_c31_tables[1, 111:120]) / (sum(cm_c31_tables[1, 111:120]) + sum(cm_c31_tables[2, 111:120])), 6))
(k17w3c31_tnr <- round(sum(cm_c31_tables[4, 111:120]) / (sum(cm_c31_tables[4, 111:120]) + sum(cm_c31_tables[3, 111:120])), 6))
(k17w3c31_truescore <- round((2 * k17w3c31_tpr * k17w3c31_tnr) / (k17w3c31_tpr + k17w3c31_tnr), 6))


(k19w1c31_tpr <- round(sum(cm_c31_tables[1, 121:130]) / (sum(cm_c31_tables[1, 121:130]) + sum(cm_c31_tables[2, 121:130])), 6))
(k19w1c31_tnr <- round(sum(cm_c31_tables[4, 121:130]) / (sum(cm_c31_tables[4, 121:130]) + sum(cm_c31_tables[3, 121:130])), 6))
(k19w1c31_truescore <- round((2 * k19w1c31_tpr * k19w1c31_tnr) / (k19w1c31_tpr + k19w1c31_tnr), 6))

(k19w2c31_tpr <- round(sum(cm_c31_tables[1, 131:140]) / (sum(cm_c31_tables[1, 131:140]) + sum(cm_c31_tables[2, 131:140])), 6))
(k19w2c31_tnr <- round(sum(cm_c31_tables[4, 131:140]) / (sum(cm_c31_tables[4, 131:140]) + sum(cm_c31_tables[3, 131:140])), 6))
(k19w2c31_truescore <- round((2 * k19w2c31_tpr * k19w2c31_tnr) / (k19w2c31_tpr + k19w2c31_tnr), 6))

(k19w3c31_tpr <- round(sum(cm_c31_tables[1, 141:150]) / (sum(cm_c31_tables[1, 141:150]) + sum(cm_c31_tables[2, 141:150])), 6))
(k19w3c31_tnr <- round(sum(cm_c31_tables[4, 141:150]) / (sum(cm_c31_tables[4, 141:150]) + sum(cm_c31_tables[3, 141:150])), 6))
(k19w3c31_truescore <- round((2 * k19w3c31_tpr * k19w3c31_tnr) / (k19w3c31_tpr + k19w3c31_tnr), 6))


(k21w1c31_tpr <- round(sum(cm_c31_tables[1, 151:160]) / (sum(cm_c31_tables[1, 151:160]) + sum(cm_c31_tables[2, 151:160])), 6))
(k21w1c31_tnr <- round(sum(cm_c31_tables[4, 151:160]) / (sum(cm_c31_tables[4, 151:160]) + sum(cm_c31_tables[3, 151:160])), 6))
(k21w1c31_truescore <- round((2 * k21w1c31_tpr * k21w1c31_tnr) / (k21w1c31_tpr + k21w1c31_tnr), 6))

(k21w2c31_tpr <- round(sum(cm_c31_tables[1, 161:170]) / (sum(cm_c31_tables[1, 161:170]) + sum(cm_c31_tables[2, 161:170])), 6))
(k21w2c31_tnr <- round(sum(cm_c31_tables[4, 161:170]) / (sum(cm_c31_tables[4, 161:170]) + sum(cm_c31_tables[3, 161:170])), 6))
(k21w2c31_truescore <- round((2 * k21w2c31_tpr * k21w2c31_tnr) / (k21w2c31_tpr + k21w2c31_tnr), 6))

(k21w3c31_tpr <- round(sum(cm_c31_tables[1, 171:180]) / (sum(cm_c31_tables[1, 171:180]) + sum(cm_c31_tables[2, 171:180])), 6))
(k21w3c31_tnr <- round(sum(cm_c31_tables[4, 171:180]) / (sum(cm_c31_tables[4, 171:180]) + sum(cm_c31_tables[3, 171:180])), 6))
(k21w3c31_truescore <- round((2 * k21w3c31_tpr * k21w3c31_tnr) / (k21w3c31_tpr + k21w3c31_tnr), 6))


(k23w1c31_tpr <- round(sum(cm_c31_tables[1, 181:190]) / (sum(cm_c31_tables[1, 181:190]) + sum(cm_c31_tables[2, 181:190])), 6))
(k23w1c31_tnr <- round(sum(cm_c31_tables[4, 181:190]) / (sum(cm_c31_tables[4, 181:190]) + sum(cm_c31_tables[3, 181:190])), 6))
(k23w1c31_truescore <- round((2 * k23w1c31_tpr * k23w1c31_tnr) / (k23w1c31_tpr + k23w1c31_tnr), 6))

(k23w2c31_tpr <- round(sum(cm_c31_tables[1, 191:200]) / (sum(cm_c31_tables[1, 191:200]) + sum(cm_c31_tables[2, 191:200])), 6))
(k23w2c31_tnr <- round(sum(cm_c31_tables[4, 191:200]) / (sum(cm_c31_tables[4, 191:200]) + sum(cm_c31_tables[3, 191:200])), 6))
(k23w2c31_truescore <- round((2 * k23w2c31_tpr * k23w2c31_tnr) / (k23w2c31_tpr + k23w2c31_tnr), 6))

(k23w3c31_tpr <- round(sum(cm_c31_tables[1, 201:210]) / (sum(cm_c31_tables[1, 201:210]) + sum(cm_c31_tables[2, 201:210])), 6))
(k23w3c31_tnr <- round(sum(cm_c31_tables[4, 201:210]) / (sum(cm_c31_tables[4, 201:210]) + sum(cm_c31_tables[3, 201:210])), 6))
(k23w3c31_truescore <- round((2 * k23w3c31_tpr * k23w3c31_tnr) / (k23w3c31_tpr + k23w3c31_tnr), 6))


(k25w1c31_tpr <- round(sum(cm_c31_tables[1, 211:220]) / (sum(cm_c31_tables[1, 211:220]) + sum(cm_c31_tables[2, 211:220])), 6))
(k25w1c31_tnr <- round(sum(cm_c31_tables[4, 211:220]) / (sum(cm_c31_tables[4, 211:220]) + sum(cm_c31_tables[3, 211:220])), 6))
(k25w1c31_truescore <- round((2 * k25w1c31_tpr * k25w1c31_tnr) / (k25w1c31_tpr + k25w1c31_tnr), 6))

(k25w2c31_tpr <- round(sum(cm_c31_tables[1, 221:230]) / (sum(cm_c31_tables[1, 221:230]) + sum(cm_c31_tables[2, 221:230])), 6))
(k25w2c31_tnr <- round(sum(cm_c31_tables[4, 221:230]) / (sum(cm_c31_tables[4, 221:230]) + sum(cm_c31_tables[3, 221:230])), 6))
(k25w2c31_truescore <- round((2 * k25w2c31_tpr * k25w2c31_tnr) / (k25w2c31_tpr + k25w2c31_tnr), 6))

(k25w3c31_tpr <- round(sum(cm_c31_tables[1, 231:240]) / (sum(cm_c31_tables[1, 231:240]) + sum(cm_c31_tables[2, 231:240])), 6))
(k25w3c31_tnr <- round(sum(cm_c31_tables[4, 231:240]) / (sum(cm_c31_tables[4, 231:240]) + sum(cm_c31_tables[3, 231:240])), 6))
(k25w3c31_truescore <- round((2 * k25w3c31_tpr * k25w3c31_tnr) / (k25w3c31_tpr + k25w3c31_tnr), 6))


(k27w1c31_tpr <- round(sum(cm_c31_tables[1, 241:250]) / (sum(cm_c31_tables[1, 241:250]) + sum(cm_c31_tables[2, 241:250])), 6))
(k27w1c31_tnr <- round(sum(cm_c31_tables[4, 241:250]) / (sum(cm_c31_tables[4, 241:250]) + sum(cm_c31_tables[3, 241:250])), 6))
(k27w1c31_truescore <- round((2 * k27w1c31_tpr * k27w1c31_tnr) / (k27w1c31_tpr + k27w1c31_tnr), 6))

(k27w2c31_tpr <- round(sum(cm_c31_tables[1, 251:260]) / (sum(cm_c31_tables[1, 251:260]) + sum(cm_c31_tables[2, 251:260])), 6))
(k27w2c31_tnr <- round(sum(cm_c31_tables[4, 251:260]) / (sum(cm_c31_tables[4, 251:260]) + sum(cm_c31_tables[3, 251:260])), 6))
(k27w2c31_truescore <- round((2 * k27w2c31_tpr * k27w2c31_tnr) / (k27w2c31_tpr + k27w2c31_tnr), 6))

(k27w3c31_tpr <- round(sum(cm_c31_tables[1, 261:270]) / (sum(cm_c31_tables[1, 261:270]) + sum(cm_c31_tables[2, 261:270])), 6))
(k27w3c31_tnr <- round(sum(cm_c31_tables[4, 261:270]) / (sum(cm_c31_tables[4, 261:270]) + sum(cm_c31_tables[3, 261:270])), 6))
(k27w3c31_truescore <- round((2 * k27w3c31_tpr * k27w3c31_tnr) / (k27w3c31_tpr + k27w3c31_tnr), 6))


(k29w1c31_tpr <- round(sum(cm_c31_tables[1, 271:280]) / (sum(cm_c31_tables[1, 271:280]) + sum(cm_c31_tables[2, 271:280])), 6))
(k29w1c31_tnr <- round(sum(cm_c31_tables[4, 271:280]) / (sum(cm_c31_tables[4, 271:280]) + sum(cm_c31_tables[3, 271:280])), 6))
(k29w1c31_truescore <- round((2 * k29w1c31_tpr * k29w1c31_tnr) / (k29w1c31_tpr + k29w1c31_tnr), 6))

(k29w2c31_tpr <- round(sum(cm_c31_tables[1, 281:290]) / (sum(cm_c31_tables[1, 281:290]) + sum(cm_c31_tables[2, 281:290])), 6))
(k29w2c31_tnr <- round(sum(cm_c31_tables[4, 281:290]) / (sum(cm_c31_tables[4, 281:290]) + sum(cm_c31_tables[3, 281:290])), 6))
(k29w2c31_truescore <- round((2 * k29w2c31_tpr * k29w2c31_tnr) / (k29w2c31_tpr + k29w2c31_tnr), 6))

(k29w3c31_tpr <- round(sum(cm_c31_tables[1, 291:300]) / (sum(cm_c31_tables[1, 291:300]) + sum(cm_c31_tables[2, 291:300])), 6))
(k29w3c31_tnr <- round(sum(cm_c31_tables[4, 291:300]) / (sum(cm_c31_tables[4, 291:300]) + sum(cm_c31_tables[3, 291:300])), 6))
(k29w3c31_truescore <- round((2 * k29w3c31_tpr * k29w3c31_tnr) / (k29w3c31_tpr + k29w3c31_tnr), 6))


(k31w1c31_tpr <- round(sum(cm_c31_tables[1, 301:310]) / (sum(cm_c31_tables[1, 301:310]) + sum(cm_c31_tables[2, 301:310])), 6))
(k31w1c31_tnr <- round(sum(cm_c31_tables[4, 301:310]) / (sum(cm_c31_tables[4, 301:310]) + sum(cm_c31_tables[3, 301:310])), 6))
(k31w1c31_truescore <- round((2 * k31w1c31_tpr * k31w1c31_tnr) / (k31w1c31_tpr + k31w1c31_tnr), 6))

(k31w2c31_tpr <- round(sum(cm_c31_tables[1, 311:320]) / (sum(cm_c31_tables[1, 311:320]) + sum(cm_c31_tables[2, 311:320])), 6))
(k31w2c31_tnr <- round(sum(cm_c31_tables[4, 311:320]) / (sum(cm_c31_tables[4, 311:320]) + sum(cm_c31_tables[3, 311:320])), 6))
(k31w2c31_truescore <- round((2 * k31w2c31_tpr * k31w2c31_tnr) / (k31w2c31_tpr + k31w2c31_tnr), 6))

(k31w3c31_tpr <- round(sum(cm_c31_tables[1, 321:330]) / (sum(cm_c31_tables[1, 321:330]) + sum(cm_c31_tables[2, 321:330])), 6))
(k31w3c31_tnr <- round(sum(cm_c31_tables[4, 321:330]) / (sum(cm_c31_tables[4, 321:330]) + sum(cm_c31_tables[3, 321:330])), 6))
(k31w3c31_truescore <- round((2 * k31w3c31_tpr * k31w3c31_tnr) / (k31w3c31_tpr + k31w3c31_tnr), 6))


(k33w1c31_tpr <- round(sum(cm_c31_tables[1, 331:340]) / (sum(cm_c31_tables[1, 331:340]) + sum(cm_c31_tables[2, 331:340])), 6))
(k33w1c31_tnr <- round(sum(cm_c31_tables[4, 331:340]) / (sum(cm_c31_tables[4, 331:340]) + sum(cm_c31_tables[3, 331:340])), 6))
(k33w1c31_truescore <- round((2 * k33w1c31_tpr * k33w1c31_tnr) / (k33w1c31_tpr + k33w1c31_tnr), 6))

(k33w2c31_tpr <- round(sum(cm_c31_tables[1, 341:350]) / (sum(cm_c31_tables[1, 341:350]) + sum(cm_c31_tables[2, 341:350])), 6))
(k33w2c31_tnr <- round(sum(cm_c31_tables[4, 341:350]) / (sum(cm_c31_tables[4, 341:350]) + sum(cm_c31_tables[3, 341:350])), 6))
(k33w2c31_truescore <- round((2 * k33w2c31_tpr * k33w2c31_tnr) / (k33w2c31_tpr + k33w2c31_tnr), 6))

(k33w3c31_tpr <- round(sum(cm_c31_tables[1, 351:360]) / (sum(cm_c31_tables[1, 351:360]) + sum(cm_c31_tables[2, 351:360])), 6))
(k33w3c31_tnr <- round(sum(cm_c31_tables[4, 351:360]) / (sum(cm_c31_tables[4, 351:360]) + sum(cm_c31_tables[3, 351:360])), 6))
(k33w3c31_truescore <- round((2 * k33w3c31_tpr * k33w3c31_tnr) / (k33w3c31_tpr + k33w3c31_tnr), 6))


(k35w1c31_tpr <- round(sum(cm_c31_tables[1, 361:370]) / (sum(cm_c31_tables[1, 361:370]) + sum(cm_c31_tables[2, 361:370])), 6))
(k35w1c31_tnr <- round(sum(cm_c31_tables[4, 361:370]) / (sum(cm_c31_tables[4, 361:370]) + sum(cm_c31_tables[3, 361:370])), 6))
(k35w1c31_truescore <- round((2 * k35w1c31_tpr * k35w1c31_tnr) / (k35w1c31_tpr + k35w1c31_tnr), 6))

(k35w2c31_tpr <- round(sum(cm_c31_tables[1, 371:380]) / (sum(cm_c31_tables[1, 371:380]) + sum(cm_c31_tables[2, 371:380])), 6))
(k35w2c31_tnr <- round(sum(cm_c31_tables[4, 371:380]) / (sum(cm_c31_tables[4, 371:380]) + sum(cm_c31_tables[3, 371:380])), 6))
(k35w2c31_truescore <- round((2 * k35w2c31_tpr * k35w2c31_tnr) / (k35w2c31_tpr + k35w2c31_tnr), 6))

(k35w3c31_tpr <- round(sum(cm_c31_tables[1, 381:390]) / (sum(cm_c31_tables[1, 381:390]) + sum(cm_c31_tables[2, 381:390])), 6))
(k35w3c31_tnr <- round(sum(cm_c31_tables[4, 381:390]) / (sum(cm_c31_tables[4, 381:390]) + sum(cm_c31_tables[3, 381:390])), 6))
(k35w3c31_truescore <- round((2 * k35w3c31_tpr * k35w3c31_tnr) / (k35w3c31_tpr + k35w3c31_tnr), 6))


(k37w1c31_tpr <- round(sum(cm_c31_tables[1, 391:400]) / (sum(cm_c31_tables[1, 391:400]) + sum(cm_c31_tables[2, 391:400])), 6))
(k37w1c31_tnr <- round(sum(cm_c31_tables[4, 391:400]) / (sum(cm_c31_tables[4, 391:400]) + sum(cm_c31_tables[3, 391:400])), 6))
(k37w1c31_truescore <- round((2 * k37w1c31_tpr * k37w1c31_tnr) / (k37w1c31_tpr + k37w1c31_tnr), 6))

(k37w2c31_tpr <- round(sum(cm_c31_tables[1, 401:410]) / (sum(cm_c31_tables[1, 401:410]) + sum(cm_c31_tables[2, 401:410])), 6))
(k37w2c31_tnr <- round(sum(cm_c31_tables[4, 401:410]) / (sum(cm_c31_tables[4, 401:410]) + sum(cm_c31_tables[3, 401:410])), 6))
(k37w2c31_truescore <- round((2 * k37w2c31_tpr * k37w2c31_tnr) / (k37w2c31_tpr + k37w2c31_tnr), 6))

(k37w3c31_tpr <- round(sum(cm_c31_tables[1, 411:420]) / (sum(cm_c31_tables[1, 411:420]) + sum(cm_c31_tables[2, 411:420])), 6))
(k37w3c31_tnr <- round(sum(cm_c31_tables[4, 411:420]) / (sum(cm_c31_tables[4, 411:420]) + sum(cm_c31_tables[3, 411:420])), 6))
(k37w3c31_truescore <- round((2 * k37w3c31_tpr * k37w3c31_tnr) / (k37w3c31_tpr + k37w3c31_tnr), 6))


(k39w1c31_tpr <- round(sum(cm_c31_tables[1, 421:430]) / (sum(cm_c31_tables[1, 421:430]) + sum(cm_c31_tables[2, 421:430])), 6))
(k39w1c31_tnr <- round(sum(cm_c31_tables[4, 421:430]) / (sum(cm_c31_tables[4, 421:430]) + sum(cm_c31_tables[3, 421:430])), 6))
(k39w1c31_truescore <- round((2 * k39w1c31_tpr * k39w1c31_tnr) / (k39w1c31_tpr + k39w1c31_tnr), 6))

(k39w2c31_tpr <- round(sum(cm_c31_tables[1, 431:440]) / (sum(cm_c31_tables[1, 431:440]) + sum(cm_c31_tables[2, 431:440])), 6))
(k39w2c31_tnr <- round(sum(cm_c31_tables[4, 431:440]) / (sum(cm_c31_tables[4, 431:440]) + sum(cm_c31_tables[3, 431:440])), 6))
(k39w2c31_truescore <- round((2 * k39w2c31_tpr * k39w2c31_tnr) / (k39w2c31_tpr + k39w2c31_tnr), 6))

(k39w3c31_tpr <- round(sum(cm_c31_tables[1, 441:450]) / (sum(cm_c31_tables[1, 441:450]) + sum(cm_c31_tables[2, 441:450])), 6))
(k39w3c31_tnr <- round(sum(cm_c31_tables[4, 441:450]) / (sum(cm_c31_tables[4, 441:450]) + sum(cm_c31_tables[3, 441:450])), 6))
(k39w3c31_truescore <- round((2 * k39w3c31_tpr * k39w3c31_tnr) / (k39w3c31_tpr + k39w3c31_tnr), 6))


# Compile the 0.31 cutoff results in a table, and identify the combination of 
# of k and kernel that maximizes the Truescore.

c31_results <- tibble(k = c(11, 11, 11, 13, 13, 13, 15, 15, 15, 
                            17, 17, 17, 19, 19, 19, 21, 21, 21, 
                            23, 23, 23, 25, 25, 25, 27, 27, 27, 
                            29, 29, 29, 31, 31, 31, 33, 33, 33, 
                            35, 35, 35, 37, 37, 37, 39, 39, 39), 
                      Kernel = rep(c("triangular", "gaussian", "optimal"), 15), 
                      Cut = 0.31, 
                      TPR = c(k11w1c31_tpr, k11w2c31_tpr, k11w3c31_tpr, k13w1c31_tpr, 
                              k13w2c31_tpr, k13w3c31_tpr, k15w1c31_tpr, k15w2c31_tpr, 
                              k15w3c31_tpr, k17w1c31_tpr, k17w2c31_tpr, k17w3c31_tpr, 
                              k19w1c31_tpr, k19w2c31_tpr, k19w3c31_tpr, k21w1c31_tpr, 
                              k21w2c31_tpr, k21w3c31_tpr, k23w1c31_tpr, k23w2c31_tpr, 
                              k23w3c31_tpr, k25w1c31_tpr, k25w2c31_tpr, k25w3c31_tpr, 
                              k27w1c31_tpr, k27w2c31_tpr, k27w3c31_tpr, k29w1c31_tpr, 
                              k29w2c31_tpr, k29w3c31_tpr, k31w1c31_tpr, k31w2c31_tpr, 
                              k31w3c31_tpr, k33w1c31_tpr, k33w2c31_tpr, k33w3c31_tpr, 
                              k35w1c31_tpr, k35w2c31_tpr, k35w3c31_tpr, k37w1c31_tpr, 
                              k37w2c31_tpr, k37w3c31_tpr, k39w1c31_tpr, k39w2c31_tpr, 
                              k39w3c31_tpr), 
                      TNR = c(k11w1c31_tnr, k11w2c31_tnr, k11w3c31_tnr, k13w1c31_tnr, 
                              k13w2c31_tnr, k13w3c31_tnr, k15w1c31_tnr, k15w2c31_tnr, 
                              k15w3c31_tnr, k17w1c31_tnr, k17w2c31_tnr, k17w3c31_tnr, 
                              k19w1c31_tnr, k19w2c31_tnr, k19w3c31_tnr, k21w1c31_tnr, 
                              k21w2c31_tnr, k21w3c31_tnr, k23w1c31_tnr, k23w2c31_tnr, 
                              k23w3c31_tnr, k25w1c31_tnr, k25w2c31_tnr, k25w3c31_tnr, 
                              k27w1c31_tnr, k27w2c31_tnr, k27w3c31_tnr, k29w1c31_tnr, 
                              k29w2c31_tnr, k29w3c31_tnr, k31w1c31_tnr, k31w2c31_tnr, 
                              k31w3c31_tnr, k33w1c31_tnr, k33w2c31_tnr, k33w3c31_tnr, 
                              k35w1c31_tnr, k35w2c31_tnr, k35w3c31_tnr, k37w1c31_tnr, 
                              k37w2c31_tnr, k37w3c31_tnr, k39w1c31_tnr, k39w2c31_tnr, 
                              k39w3c31_tnr), 
                      Truescore = c(k11w1c31_truescore, k11w2c31_truescore, 
                                    k11w3c31_truescore, k13w1c31_truescore, 
                                    k13w2c31_truescore, k13w3c31_truescore, 
                                    k15w1c31_truescore, k15w2c31_truescore, 
                                    k15w3c31_truescore, k17w1c31_truescore, 
                                    k17w2c31_truescore, k17w3c31_truescore, 
                                    k19w1c31_truescore, k19w2c31_truescore, 
                                    k19w3c31_truescore, k21w1c31_truescore, 
                                    k21w2c31_truescore, k21w3c31_truescore, 
                                    k23w1c31_truescore, k23w2c31_truescore, 
                                    k23w3c31_truescore, k25w1c31_truescore, 
                                    k25w2c31_truescore, k25w3c31_truescore, 
                                    k27w1c31_truescore, k27w2c31_truescore, 
                                    k27w3c31_truescore, k29w1c31_truescore, 
                                    k29w2c31_truescore, k29w3c31_truescore, 
                                    k31w1c31_truescore, k31w2c31_truescore, 
                                    k31w3c31_truescore, k33w1c31_truescore, 
                                    k33w2c31_truescore, k33w3c31_truescore, 
                                    k35w1c31_truescore, k35w2c31_truescore, 
                                    k35w3c31_truescore, k37w1c31_truescore, 
                                    k37w2c31_truescore, k37w3c31_truescore, 
                                    k39w1c31_truescore, k39w2c31_truescore, 
                                    k39w3c31_truescore))

knitr::kable(c31_results[1:45, ], caption = "c31_results")

ggplot(c31_results, aes(k, Truescore, shape = Kernel, color = Kernel)) + 
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.31 (Truescore)")

# For the Cutoff of 0.31, compute the Minimum Distance to (0, 1) for each 
# combination of k and Kernel. 

c31_results <- c31_results %>% mutate(Distance = sqrt((1 - TPR)^2 + (1 - TNR)^2))

ggplot(c31_results, aes(k, Distance, shape = Kernel, color = Kernel)) +
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.31 (Distance)")

# For the Cutoff of 0.31, identify the optimal combination of values for k and Kernel 
# based on each of our assessment methods, Truescore and Mimimum Distance to (0, 1).

max(c31_results$Truescore)
(c31_opt_k_ts <- c31_results$k[which.max(c31_results$Truescore)])
(c31_opt_kernel_ts <- c31_results$Kernel[which.max(c31_results$Truescore)])
(c31_opt_cut_ts <- c31_results$Cut[which.max(c31_results$Truescore)])
(c31_opt_tpr_ts <- c31_results$TPR[which.max(c31_results$Truescore)])
(c31_opt_tnr_ts <- c31_results$TNR[which.max(c31_results$Truescore)])
(c31_opt_d_ts <- c31_results$Distance[which.max(c31_results$Truescore)])

min(c31_results$Distance)
(c31_opt_k_dist <- c31_results$k[which.min(c31_results$Distance)])
(c31_opt_kernel_dist <- c31_results$Kernel[which.min(c31_results$Distance)])
(c31_opt_cut_dist <- c31_results$Cut[which.min(c31_results$Distance)])  
(c31_opt_tpr_dist <- c31_results$TPR[which.min(c31_results$Distance)])
(c31_opt_tnr_dist <- c31_results$TNR[which.min(c31_results$Distance)])
(c31_opt_t_dist <- c31_results$Truescore[which.min(c31_results$Distance)])

############################
# 0.32 Cutoff
############################

# For the decision cutoff of 0.32, generate a confusion matrix for
# every combination of k (11:39 by two), kernel (1:3) and fold (1:10).

cm_c32 <- sapply(kwf_dfs_l, function(x) {
  ss <- subset(x, select = c(pred32, obs))
  confusionMatrix(ss$pred32, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c32) <- kwf_dfs_v

cm_c32_tables <- sapply(cm_c32, "[[", 2)
cm_c32_tables <- as_tibble(cm_c32_tables)

# For each combination of k and kernel, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each combination of k and kernel when the decision cutoff is 0.32.

(k11w1c32_tpr <- round(sum(cm_c32_tables[1, 1:10]) / (sum(cm_c32_tables[1, 1:10]) + sum(cm_c32_tables[2, 1:10])), 6))
(k11w1c32_tnr <- round(sum(cm_c32_tables[4, 1:10]) / (sum(cm_c32_tables[4, 1:10]) + sum(cm_c32_tables[3, 1:10])), 6))
(k11w1c32_truescore <- round((2 * k11w1c32_tpr * k11w1c32_tnr) / (k11w1c32_tpr + k11w1c32_tnr), 6))

(k11w2c32_tpr <- round(sum(cm_c32_tables[1, 11:20]) / (sum(cm_c32_tables[1, 11:20]) + sum(cm_c32_tables[2, 11:20])), 6))
(k11w2c32_tnr <- round(sum(cm_c32_tables[4, 11:20]) / (sum(cm_c32_tables[4, 11:20]) + sum(cm_c32_tables[3, 11:20])), 6))
(k11w2c32_truescore <- round((2 * k11w2c32_tpr * k11w2c32_tnr) / (k11w2c32_tpr + k11w2c32_tnr), 6))

(k11w3c32_tpr <- round(sum(cm_c32_tables[1, 21:30]) / (sum(cm_c32_tables[1, 21:30]) + sum(cm_c32_tables[2, 21:30])), 6))
(k11w3c32_tnr <- round(sum(cm_c32_tables[4, 21:30]) / (sum(cm_c32_tables[4, 21:30]) + sum(cm_c32_tables[3, 21:30])), 6))
(k11w3c32_truescore <- round((2 * k11w3c32_tpr * k11w3c32_tnr) / (k11w3c32_tpr + k11w3c32_tnr), 6))


(k13w1c32_tpr <- round(sum(cm_c32_tables[1, 31:40]) / (sum(cm_c32_tables[1, 31:40]) + sum(cm_c32_tables[2, 31:40])), 6))
(k13w1c32_tnr <- round(sum(cm_c32_tables[4, 31:40]) / (sum(cm_c32_tables[4, 31:40]) + sum(cm_c32_tables[3, 31:40])), 6))
(k13w1c32_truescore <- round((2 * k13w1c32_tpr * k13w1c32_tnr) / (k13w1c32_tpr + k13w1c32_tnr), 6))

(k13w2c32_tpr <- round(sum(cm_c32_tables[1, 41:50]) / (sum(cm_c32_tables[1, 41:50]) + sum(cm_c32_tables[2, 41:50])), 6))
(k13w2c32_tnr <- round(sum(cm_c32_tables[4, 41:50]) / (sum(cm_c32_tables[4, 41:50]) + sum(cm_c32_tables[3, 41:50])), 6))
(k13w2c32_truescore <- round((2 * k13w2c32_tpr * k13w2c32_tnr) / (k13w2c32_tpr + k13w2c32_tnr), 6))

(k13w3c32_tpr <- round(sum(cm_c32_tables[1, 51:60]) / (sum(cm_c32_tables[1, 51:60]) + sum(cm_c32_tables[2, 51:60])), 6))
(k13w3c32_tnr <- round(sum(cm_c32_tables[4, 51:60]) / (sum(cm_c32_tables[4, 51:60]) + sum(cm_c32_tables[3, 51:60])), 6))
(k13w3c32_truescore <- round((2 * k13w3c32_tpr * k13w3c32_tnr) / (k13w3c32_tpr + k13w3c32_tnr), 6))


(k15w1c32_tpr <- round(sum(cm_c32_tables[1, 61:70]) / (sum(cm_c32_tables[1, 61:70]) + sum(cm_c32_tables[2, 61:70])), 6))
(k15w1c32_tnr <- round(sum(cm_c32_tables[4, 61:70]) / (sum(cm_c32_tables[4, 61:70]) + sum(cm_c32_tables[3, 61:70])), 6))
(k15w1c32_truescore <- round((2 * k15w1c32_tpr * k15w1c32_tnr) / (k15w1c32_tpr + k15w1c32_tnr), 6))

(k15w2c32_tpr <- round(sum(cm_c32_tables[1, 71:80]) / (sum(cm_c32_tables[1, 71:80]) + sum(cm_c32_tables[2, 71:80])), 6))
(k15w2c32_tnr <- round(sum(cm_c32_tables[4, 71:80]) / (sum(cm_c32_tables[4, 71:80]) + sum(cm_c32_tables[3, 71:80])), 6))
(k15w2c32_truescore <- round((2 * k15w2c32_tpr * k15w2c32_tnr) / (k15w2c32_tpr + k15w2c32_tnr), 6))

(k15w3c32_tpr <- round(sum(cm_c32_tables[1, 81:90]) / (sum(cm_c32_tables[1, 81:90]) + sum(cm_c32_tables[2, 81:90])), 6))
(k15w3c32_tnr <- round(sum(cm_c32_tables[4, 81:90]) / (sum(cm_c32_tables[4, 81:90]) + sum(cm_c32_tables[3, 81:90])), 6))
(k15w3c32_truescore <- round((2 * k15w3c32_tpr * k15w3c32_tnr) / (k15w3c32_tpr + k15w3c32_tnr), 6))


(k17w1c32_tpr <- round(sum(cm_c32_tables[1, 91:100]) / (sum(cm_c32_tables[1, 91:100]) + sum(cm_c32_tables[2, 91:100])), 6))
(k17w1c32_tnr <- round(sum(cm_c32_tables[4, 91:100]) / (sum(cm_c32_tables[4, 91:100]) + sum(cm_c32_tables[3, 91:100])), 6))
(k17w1c32_truescore <- round((2 * k17w1c32_tpr * k17w1c32_tnr) / (k17w1c32_tpr + k17w1c32_tnr), 6))

(k17w2c32_tpr <- round(sum(cm_c32_tables[1, 101:110]) / (sum(cm_c32_tables[1, 101:110]) + sum(cm_c32_tables[2, 101:110])), 6))
(k17w2c32_tnr <- round(sum(cm_c32_tables[4, 101:110]) / (sum(cm_c32_tables[4, 101:110]) + sum(cm_c32_tables[3, 101:110])), 6))
(k17w2c32_truescore <- round((2 * k17w2c32_tpr * k17w2c32_tnr) / (k17w2c32_tpr + k17w2c32_tnr), 6))

(k17w3c32_tpr <- round(sum(cm_c32_tables[1, 111:120]) / (sum(cm_c32_tables[1, 111:120]) + sum(cm_c32_tables[2, 111:120])), 6))
(k17w3c32_tnr <- round(sum(cm_c32_tables[4, 111:120]) / (sum(cm_c32_tables[4, 111:120]) + sum(cm_c32_tables[3, 111:120])), 6))
(k17w3c32_truescore <- round((2 * k17w3c32_tpr * k17w3c32_tnr) / (k17w3c32_tpr + k17w3c32_tnr), 6))


(k19w1c32_tpr <- round(sum(cm_c32_tables[1, 121:130]) / (sum(cm_c32_tables[1, 121:130]) + sum(cm_c32_tables[2, 121:130])), 6))
(k19w1c32_tnr <- round(sum(cm_c32_tables[4, 121:130]) / (sum(cm_c32_tables[4, 121:130]) + sum(cm_c32_tables[3, 121:130])), 6))
(k19w1c32_truescore <- round((2 * k19w1c32_tpr * k19w1c32_tnr) / (k19w1c32_tpr + k19w1c32_tnr), 6))

(k19w2c32_tpr <- round(sum(cm_c32_tables[1, 131:140]) / (sum(cm_c32_tables[1, 131:140]) + sum(cm_c32_tables[2, 131:140])), 6))
(k19w2c32_tnr <- round(sum(cm_c32_tables[4, 131:140]) / (sum(cm_c32_tables[4, 131:140]) + sum(cm_c32_tables[3, 131:140])), 6))
(k19w2c32_truescore <- round((2 * k19w2c32_tpr * k19w2c32_tnr) / (k19w2c32_tpr + k19w2c32_tnr), 6))

(k19w3c32_tpr <- round(sum(cm_c32_tables[1, 141:150]) / (sum(cm_c32_tables[1, 141:150]) + sum(cm_c32_tables[2, 141:150])), 6))
(k19w3c32_tnr <- round(sum(cm_c32_tables[4, 141:150]) / (sum(cm_c32_tables[4, 141:150]) + sum(cm_c32_tables[3, 141:150])), 6))
(k19w3c32_truescore <- round((2 * k19w3c32_tpr * k19w3c32_tnr) / (k19w3c32_tpr + k19w3c32_tnr), 6))


(k21w1c32_tpr <- round(sum(cm_c32_tables[1, 151:160]) / (sum(cm_c32_tables[1, 151:160]) + sum(cm_c32_tables[2, 151:160])), 6))
(k21w1c32_tnr <- round(sum(cm_c32_tables[4, 151:160]) / (sum(cm_c32_tables[4, 151:160]) + sum(cm_c32_tables[3, 151:160])), 6))
(k21w1c32_truescore <- round((2 * k21w1c32_tpr * k21w1c32_tnr) / (k21w1c32_tpr + k21w1c32_tnr), 6))

(k21w2c32_tpr <- round(sum(cm_c32_tables[1, 161:170]) / (sum(cm_c32_tables[1, 161:170]) + sum(cm_c32_tables[2, 161:170])), 6))
(k21w2c32_tnr <- round(sum(cm_c32_tables[4, 161:170]) / (sum(cm_c32_tables[4, 161:170]) + sum(cm_c32_tables[3, 161:170])), 6))
(k21w2c32_truescore <- round((2 * k21w2c32_tpr * k21w2c32_tnr) / (k21w2c32_tpr + k21w2c32_tnr), 6))

(k21w3c32_tpr <- round(sum(cm_c32_tables[1, 171:180]) / (sum(cm_c32_tables[1, 171:180]) + sum(cm_c32_tables[2, 171:180])), 6))
(k21w3c32_tnr <- round(sum(cm_c32_tables[4, 171:180]) / (sum(cm_c32_tables[4, 171:180]) + sum(cm_c32_tables[3, 171:180])), 6))
(k21w3c32_truescore <- round((2 * k21w3c32_tpr * k21w3c32_tnr) / (k21w3c32_tpr + k21w3c32_tnr), 6))


(k23w1c32_tpr <- round(sum(cm_c32_tables[1, 181:190]) / (sum(cm_c32_tables[1, 181:190]) + sum(cm_c32_tables[2, 181:190])), 6))
(k23w1c32_tnr <- round(sum(cm_c32_tables[4, 181:190]) / (sum(cm_c32_tables[4, 181:190]) + sum(cm_c32_tables[3, 181:190])), 6))
(k23w1c32_truescore <- round((2 * k23w1c32_tpr * k23w1c32_tnr) / (k23w1c32_tpr + k23w1c32_tnr), 6))

(k23w2c32_tpr <- round(sum(cm_c32_tables[1, 191:200]) / (sum(cm_c32_tables[1, 191:200]) + sum(cm_c32_tables[2, 191:200])), 6))
(k23w2c32_tnr <- round(sum(cm_c32_tables[4, 191:200]) / (sum(cm_c32_tables[4, 191:200]) + sum(cm_c32_tables[3, 191:200])), 6))
(k23w2c32_truescore <- round((2 * k23w2c32_tpr * k23w2c32_tnr) / (k23w2c32_tpr + k23w2c32_tnr), 6))

(k23w3c32_tpr <- round(sum(cm_c32_tables[1, 201:210]) / (sum(cm_c32_tables[1, 201:210]) + sum(cm_c32_tables[2, 201:210])), 6))
(k23w3c32_tnr <- round(sum(cm_c32_tables[4, 201:210]) / (sum(cm_c32_tables[4, 201:210]) + sum(cm_c32_tables[3, 201:210])), 6))
(k23w3c32_truescore <- round((2 * k23w3c32_tpr * k23w3c32_tnr) / (k23w3c32_tpr + k23w3c32_tnr), 6))


(k25w1c32_tpr <- round(sum(cm_c32_tables[1, 211:220]) / (sum(cm_c32_tables[1, 211:220]) + sum(cm_c32_tables[2, 211:220])), 6))
(k25w1c32_tnr <- round(sum(cm_c32_tables[4, 211:220]) / (sum(cm_c32_tables[4, 211:220]) + sum(cm_c32_tables[3, 211:220])), 6))
(k25w1c32_truescore <- round((2 * k25w1c32_tpr * k25w1c32_tnr) / (k25w1c32_tpr + k25w1c32_tnr), 6))

(k25w2c32_tpr <- round(sum(cm_c32_tables[1, 221:230]) / (sum(cm_c32_tables[1, 221:230]) + sum(cm_c32_tables[2, 221:230])), 6))
(k25w2c32_tnr <- round(sum(cm_c32_tables[4, 221:230]) / (sum(cm_c32_tables[4, 221:230]) + sum(cm_c32_tables[3, 221:230])), 6))
(k25w2c32_truescore <- round((2 * k25w2c32_tpr * k25w2c32_tnr) / (k25w2c32_tpr + k25w2c32_tnr), 6))

(k25w3c32_tpr <- round(sum(cm_c32_tables[1, 231:240]) / (sum(cm_c32_tables[1, 231:240]) + sum(cm_c32_tables[2, 231:240])), 6))
(k25w3c32_tnr <- round(sum(cm_c32_tables[4, 231:240]) / (sum(cm_c32_tables[4, 231:240]) + sum(cm_c32_tables[3, 231:240])), 6))
(k25w3c32_truescore <- round((2 * k25w3c32_tpr * k25w3c32_tnr) / (k25w3c32_tpr + k25w3c32_tnr), 6))


(k27w1c32_tpr <- round(sum(cm_c32_tables[1, 241:250]) / (sum(cm_c32_tables[1, 241:250]) + sum(cm_c32_tables[2, 241:250])), 6))
(k27w1c32_tnr <- round(sum(cm_c32_tables[4, 241:250]) / (sum(cm_c32_tables[4, 241:250]) + sum(cm_c32_tables[3, 241:250])), 6))
(k27w1c32_truescore <- round((2 * k27w1c32_tpr * k27w1c32_tnr) / (k27w1c32_tpr + k27w1c32_tnr), 6))

(k27w2c32_tpr <- round(sum(cm_c32_tables[1, 251:260]) / (sum(cm_c32_tables[1, 251:260]) + sum(cm_c32_tables[2, 251:260])), 6))
(k27w2c32_tnr <- round(sum(cm_c32_tables[4, 251:260]) / (sum(cm_c32_tables[4, 251:260]) + sum(cm_c32_tables[3, 251:260])), 6))
(k27w2c32_truescore <- round((2 * k27w2c32_tpr * k27w2c32_tnr) / (k27w2c32_tpr + k27w2c32_tnr), 6))

(k27w3c32_tpr <- round(sum(cm_c32_tables[1, 261:270]) / (sum(cm_c32_tables[1, 261:270]) + sum(cm_c32_tables[2, 261:270])), 6))
(k27w3c32_tnr <- round(sum(cm_c32_tables[4, 261:270]) / (sum(cm_c32_tables[4, 261:270]) + sum(cm_c32_tables[3, 261:270])), 6))
(k27w3c32_truescore <- round((2 * k27w3c32_tpr * k27w3c32_tnr) / (k27w3c32_tpr + k27w3c32_tnr), 6))


(k29w1c32_tpr <- round(sum(cm_c32_tables[1, 271:280]) / (sum(cm_c32_tables[1, 271:280]) + sum(cm_c32_tables[2, 271:280])), 6))
(k29w1c32_tnr <- round(sum(cm_c32_tables[4, 271:280]) / (sum(cm_c32_tables[4, 271:280]) + sum(cm_c32_tables[3, 271:280])), 6))
(k29w1c32_truescore <- round((2 * k29w1c32_tpr * k29w1c32_tnr) / (k29w1c32_tpr + k29w1c32_tnr), 6))

(k29w2c32_tpr <- round(sum(cm_c32_tables[1, 281:290]) / (sum(cm_c32_tables[1, 281:290]) + sum(cm_c32_tables[2, 281:290])), 6))
(k29w2c32_tnr <- round(sum(cm_c32_tables[4, 281:290]) / (sum(cm_c32_tables[4, 281:290]) + sum(cm_c32_tables[3, 281:290])), 6))
(k29w2c32_truescore <- round((2 * k29w2c32_tpr * k29w2c32_tnr) / (k29w2c32_tpr + k29w2c32_tnr), 6))

(k29w3c32_tpr <- round(sum(cm_c32_tables[1, 291:300]) / (sum(cm_c32_tables[1, 291:300]) + sum(cm_c32_tables[2, 291:300])), 6))
(k29w3c32_tnr <- round(sum(cm_c32_tables[4, 291:300]) / (sum(cm_c32_tables[4, 291:300]) + sum(cm_c32_tables[3, 291:300])), 6))
(k29w3c32_truescore <- round((2 * k29w3c32_tpr * k29w3c32_tnr) / (k29w3c32_tpr + k29w3c32_tnr), 6))


(k31w1c32_tpr <- round(sum(cm_c32_tables[1, 301:310]) / (sum(cm_c32_tables[1, 301:310]) + sum(cm_c32_tables[2, 301:310])), 6))
(k31w1c32_tnr <- round(sum(cm_c32_tables[4, 301:310]) / (sum(cm_c32_tables[4, 301:310]) + sum(cm_c32_tables[3, 301:310])), 6))
(k31w1c32_truescore <- round((2 * k31w1c32_tpr * k31w1c32_tnr) / (k31w1c32_tpr + k31w1c32_tnr), 6))

(k31w2c32_tpr <- round(sum(cm_c32_tables[1, 311:320]) / (sum(cm_c32_tables[1, 311:320]) + sum(cm_c32_tables[2, 311:320])), 6))
(k31w2c32_tnr <- round(sum(cm_c32_tables[4, 311:320]) / (sum(cm_c32_tables[4, 311:320]) + sum(cm_c32_tables[3, 311:320])), 6))
(k31w2c32_truescore <- round((2 * k31w2c32_tpr * k31w2c32_tnr) / (k31w2c32_tpr + k31w2c32_tnr), 6))

(k31w3c32_tpr <- round(sum(cm_c32_tables[1, 321:330]) / (sum(cm_c32_tables[1, 321:330]) + sum(cm_c32_tables[2, 321:330])), 6))
(k31w3c32_tnr <- round(sum(cm_c32_tables[4, 321:330]) / (sum(cm_c32_tables[4, 321:330]) + sum(cm_c32_tables[3, 321:330])), 6))
(k31w3c32_truescore <- round((2 * k31w3c32_tpr * k31w3c32_tnr) / (k31w3c32_tpr + k31w3c32_tnr), 6))


(k33w1c32_tpr <- round(sum(cm_c32_tables[1, 331:340]) / (sum(cm_c32_tables[1, 331:340]) + sum(cm_c32_tables[2, 331:340])), 6))
(k33w1c32_tnr <- round(sum(cm_c32_tables[4, 331:340]) / (sum(cm_c32_tables[4, 331:340]) + sum(cm_c32_tables[3, 331:340])), 6))
(k33w1c32_truescore <- round((2 * k33w1c32_tpr * k33w1c32_tnr) / (k33w1c32_tpr + k33w1c32_tnr), 6))

(k33w2c32_tpr <- round(sum(cm_c32_tables[1, 341:350]) / (sum(cm_c32_tables[1, 341:350]) + sum(cm_c32_tables[2, 341:350])), 6))
(k33w2c32_tnr <- round(sum(cm_c32_tables[4, 341:350]) / (sum(cm_c32_tables[4, 341:350]) + sum(cm_c32_tables[3, 341:350])), 6))
(k33w2c32_truescore <- round((2 * k33w2c32_tpr * k33w2c32_tnr) / (k33w2c32_tpr + k33w2c32_tnr), 6))

(k33w3c32_tpr <- round(sum(cm_c32_tables[1, 351:360]) / (sum(cm_c32_tables[1, 351:360]) + sum(cm_c32_tables[2, 351:360])), 6))
(k33w3c32_tnr <- round(sum(cm_c32_tables[4, 351:360]) / (sum(cm_c32_tables[4, 351:360]) + sum(cm_c32_tables[3, 351:360])), 6))
(k33w3c32_truescore <- round((2 * k33w3c32_tpr * k33w3c32_tnr) / (k33w3c32_tpr + k33w3c32_tnr), 6))


(k35w1c32_tpr <- round(sum(cm_c32_tables[1, 361:370]) / (sum(cm_c32_tables[1, 361:370]) + sum(cm_c32_tables[2, 361:370])), 6))
(k35w1c32_tnr <- round(sum(cm_c32_tables[4, 361:370]) / (sum(cm_c32_tables[4, 361:370]) + sum(cm_c32_tables[3, 361:370])), 6))
(k35w1c32_truescore <- round((2 * k35w1c32_tpr * k35w1c32_tnr) / (k35w1c32_tpr + k35w1c32_tnr), 6))

(k35w2c32_tpr <- round(sum(cm_c32_tables[1, 371:380]) / (sum(cm_c32_tables[1, 371:380]) + sum(cm_c32_tables[2, 371:380])), 6))
(k35w2c32_tnr <- round(sum(cm_c32_tables[4, 371:380]) / (sum(cm_c32_tables[4, 371:380]) + sum(cm_c32_tables[3, 371:380])), 6))
(k35w2c32_truescore <- round((2 * k35w2c32_tpr * k35w2c32_tnr) / (k35w2c32_tpr + k35w2c32_tnr), 6))

(k35w3c32_tpr <- round(sum(cm_c32_tables[1, 381:390]) / (sum(cm_c32_tables[1, 381:390]) + sum(cm_c32_tables[2, 381:390])), 6))
(k35w3c32_tnr <- round(sum(cm_c32_tables[4, 381:390]) / (sum(cm_c32_tables[4, 381:390]) + sum(cm_c32_tables[3, 381:390])), 6))
(k35w3c32_truescore <- round((2 * k35w3c32_tpr * k35w3c32_tnr) / (k35w3c32_tpr + k35w3c32_tnr), 6))


(k37w1c32_tpr <- round(sum(cm_c32_tables[1, 391:400]) / (sum(cm_c32_tables[1, 391:400]) + sum(cm_c32_tables[2, 391:400])), 6))
(k37w1c32_tnr <- round(sum(cm_c32_tables[4, 391:400]) / (sum(cm_c32_tables[4, 391:400]) + sum(cm_c32_tables[3, 391:400])), 6))
(k37w1c32_truescore <- round((2 * k37w1c32_tpr * k37w1c32_tnr) / (k37w1c32_tpr + k37w1c32_tnr), 6))

(k37w2c32_tpr <- round(sum(cm_c32_tables[1, 401:410]) / (sum(cm_c32_tables[1, 401:410]) + sum(cm_c32_tables[2, 401:410])), 6))
(k37w2c32_tnr <- round(sum(cm_c32_tables[4, 401:410]) / (sum(cm_c32_tables[4, 401:410]) + sum(cm_c32_tables[3, 401:410])), 6))
(k37w2c32_truescore <- round((2 * k37w2c32_tpr * k37w2c32_tnr) / (k37w2c32_tpr + k37w2c32_tnr), 6))

(k37w3c32_tpr <- round(sum(cm_c32_tables[1, 411:420]) / (sum(cm_c32_tables[1, 411:420]) + sum(cm_c32_tables[2, 411:420])), 6))
(k37w3c32_tnr <- round(sum(cm_c32_tables[4, 411:420]) / (sum(cm_c32_tables[4, 411:420]) + sum(cm_c32_tables[3, 411:420])), 6))
(k37w3c32_truescore <- round((2 * k37w3c32_tpr * k37w3c32_tnr) / (k37w3c32_tpr + k37w3c32_tnr), 6))


(k39w1c32_tpr <- round(sum(cm_c32_tables[1, 421:430]) / (sum(cm_c32_tables[1, 421:430]) + sum(cm_c32_tables[2, 421:430])), 6))
(k39w1c32_tnr <- round(sum(cm_c32_tables[4, 421:430]) / (sum(cm_c32_tables[4, 421:430]) + sum(cm_c32_tables[3, 421:430])), 6))
(k39w1c32_truescore <- round((2 * k39w1c32_tpr * k39w1c32_tnr) / (k39w1c32_tpr + k39w1c32_tnr), 6))

(k39w2c32_tpr <- round(sum(cm_c32_tables[1, 431:440]) / (sum(cm_c32_tables[1, 431:440]) + sum(cm_c32_tables[2, 431:440])), 6))
(k39w2c32_tnr <- round(sum(cm_c32_tables[4, 431:440]) / (sum(cm_c32_tables[4, 431:440]) + sum(cm_c32_tables[3, 431:440])), 6))
(k39w2c32_truescore <- round((2 * k39w2c32_tpr * k39w2c32_tnr) / (k39w2c32_tpr + k39w2c32_tnr), 6))

(k39w3c32_tpr <- round(sum(cm_c32_tables[1, 441:450]) / (sum(cm_c32_tables[1, 441:450]) + sum(cm_c32_tables[2, 441:450])), 6))
(k39w3c32_tnr <- round(sum(cm_c32_tables[4, 441:450]) / (sum(cm_c32_tables[4, 441:450]) + sum(cm_c32_tables[3, 441:450])), 6))
(k39w3c32_truescore <- round((2 * k39w3c32_tpr * k39w3c32_tnr) / (k39w3c32_tpr + k39w3c32_tnr), 6))


# Compile the 0.32 cutoff results in a table, and identify the combination of 
# of k and kernel that maximizes the Truescore.

c32_results <- tibble(k = c(11, 11, 11, 13, 13, 13, 15, 15, 15, 
                            17, 17, 17, 19, 19, 19, 21, 21, 21, 
                            23, 23, 23, 25, 25, 25, 27, 27, 27, 
                            29, 29, 29, 31, 31, 31, 33, 33, 33, 
                            35, 35, 35, 37, 37, 37, 39, 39, 39), 
                      Kernel = rep(c("triangular", "gaussian", "optimal"), 15), 
                      Cut = 0.32, 
                      TPR = c(k11w1c32_tpr, k11w2c32_tpr, k11w3c32_tpr, k13w1c32_tpr, 
                              k13w2c32_tpr, k13w3c32_tpr, k15w1c32_tpr, k15w2c32_tpr, 
                              k15w3c32_tpr, k17w1c32_tpr, k17w2c32_tpr, k17w3c32_tpr, 
                              k19w1c32_tpr, k19w2c32_tpr, k19w3c32_tpr, k21w1c32_tpr, 
                              k21w2c32_tpr, k21w3c32_tpr, k23w1c32_tpr, k23w2c32_tpr, 
                              k23w3c32_tpr, k25w1c32_tpr, k25w2c32_tpr, k25w3c32_tpr, 
                              k27w1c32_tpr, k27w2c32_tpr, k27w3c32_tpr, k29w1c32_tpr, 
                              k29w2c32_tpr, k29w3c32_tpr, k31w1c32_tpr, k31w2c32_tpr, 
                              k31w3c32_tpr, k33w1c32_tpr, k33w2c32_tpr, k33w3c32_tpr, 
                              k35w1c32_tpr, k35w2c32_tpr, k35w3c32_tpr, k37w1c32_tpr, 
                              k37w2c32_tpr, k37w3c32_tpr, k39w1c32_tpr, k39w2c32_tpr, 
                              k39w3c32_tpr), 
                      TNR = c(k11w1c32_tnr, k11w2c32_tnr, k11w3c32_tnr, k13w1c32_tnr, 
                              k13w2c32_tnr, k13w3c32_tnr, k15w1c32_tnr, k15w2c32_tnr, 
                              k15w3c32_tnr, k17w1c32_tnr, k17w2c32_tnr, k17w3c32_tnr, 
                              k19w1c32_tnr, k19w2c32_tnr, k19w3c32_tnr, k21w1c32_tnr, 
                              k21w2c32_tnr, k21w3c32_tnr, k23w1c32_tnr, k23w2c32_tnr, 
                              k23w3c32_tnr, k25w1c32_tnr, k25w2c32_tnr, k25w3c32_tnr, 
                              k27w1c32_tnr, k27w2c32_tnr, k27w3c32_tnr, k29w1c32_tnr, 
                              k29w2c32_tnr, k29w3c32_tnr, k31w1c32_tnr, k31w2c32_tnr, 
                              k31w3c32_tnr, k33w1c32_tnr, k33w2c32_tnr, k33w3c32_tnr, 
                              k35w1c32_tnr, k35w2c32_tnr, k35w3c32_tnr, k37w1c32_tnr, 
                              k37w2c32_tnr, k37w3c32_tnr, k39w1c32_tnr, k39w2c32_tnr, 
                              k39w3c32_tnr), 
                      Truescore = c(k11w1c32_truescore, k11w2c32_truescore, 
                                    k11w3c32_truescore, k13w1c32_truescore, 
                                    k13w2c32_truescore, k13w3c32_truescore, 
                                    k15w1c32_truescore, k15w2c32_truescore, 
                                    k15w3c32_truescore, k17w1c32_truescore, 
                                    k17w2c32_truescore, k17w3c32_truescore, 
                                    k19w1c32_truescore, k19w2c32_truescore, 
                                    k19w3c32_truescore, k21w1c32_truescore, 
                                    k21w2c32_truescore, k21w3c32_truescore, 
                                    k23w1c32_truescore, k23w2c32_truescore, 
                                    k23w3c32_truescore, k25w1c32_truescore, 
                                    k25w2c32_truescore, k25w3c32_truescore, 
                                    k27w1c32_truescore, k27w2c32_truescore, 
                                    k27w3c32_truescore, k29w1c32_truescore, 
                                    k29w2c32_truescore, k29w3c32_truescore, 
                                    k31w1c32_truescore, k31w2c32_truescore, 
                                    k31w3c32_truescore, k33w1c32_truescore, 
                                    k33w2c32_truescore, k33w3c32_truescore, 
                                    k35w1c32_truescore, k35w2c32_truescore, 
                                    k35w3c32_truescore, k37w1c32_truescore, 
                                    k37w2c32_truescore, k37w3c32_truescore, 
                                    k39w1c32_truescore, k39w2c32_truescore, 
                                    k39w3c32_truescore))

knitr::kable(c32_results[1:45, ], caption = "c32_results")

ggplot(c32_results, aes(k, Truescore, shape = Kernel, color = Kernel)) + 
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.32 (Truescore)")

# For the Cutoff of 0.32, compute the Minimum Distance to (0, 1) for each 
# combination of k and Kernel. 

c32_results <- c32_results %>% mutate(Distance = sqrt((1 - TPR)^2 + (1 - TNR)^2))

ggplot(c32_results, aes(k, Distance, shape = Kernel, color = Kernel)) +
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.32 (Distance)")

# For the Cutoff of 0.32, identify the optimal combination of values for k and Kernel 
# based on each of our assessment methods, Truescore and Mimimum Distance to (0, 1).

max(c32_results$Truescore)
(c32_opt_k_ts <- c32_results$k[which.max(c32_results$Truescore)])
(c32_opt_kernel_ts <- c32_results$Kernel[which.max(c32_results$Truescore)])
(c32_opt_cut_ts <- c32_results$Cut[which.max(c32_results$Truescore)])
(c32_opt_tpr_ts <- c32_results$TPR[which.max(c32_results$Truescore)])
(c32_opt_tnr_ts <- c32_results$TNR[which.max(c32_results$Truescore)])
(c32_opt_d_ts <- c32_results$Distance[which.max(c32_results$Truescore)])

min(c32_results$Distance)
(c32_opt_k_dist <- c32_results$k[which.min(c32_results$Distance)])
(c32_opt_kernel_dist <- c32_results$Kernel[which.min(c32_results$Distance)])
(c32_opt_cut_dist <- c32_results$Cut[which.min(c32_results$Distance)])  
(c32_opt_tpr_dist <- c32_results$TPR[which.min(c32_results$Distance)])
(c32_opt_tnr_dist <- c32_results$TNR[which.min(c32_results$Distance)])
(c32_opt_t_dist <- c32_results$Truescore[which.min(c32_results$Distance)])

############################
# 0.33 Cutoff
############################

# For the decision cutoff of 0.33, generate a confusion matrix for
# every combination of k (11:39 by two), kernel (1:3) and fold (1:10).

cm_c33 <- sapply(kwf_dfs_l, function(x) {
  ss <- subset(x, select = c(pred33, obs))
  confusionMatrix(ss$pred33, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c33) <- kwf_dfs_v

cm_c33_tables <- sapply(cm_c33, "[[", 2)
cm_c33_tables <- as_tibble(cm_c33_tables)

# For each combination of k and kernel, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each combination of k and kernel when the decision cutoff is 0.33.

(k11w1c33_tpr <- round(sum(cm_c33_tables[1, 1:10]) / (sum(cm_c33_tables[1, 1:10]) + sum(cm_c33_tables[2, 1:10])), 6))
(k11w1c33_tnr <- round(sum(cm_c33_tables[4, 1:10]) / (sum(cm_c33_tables[4, 1:10]) + sum(cm_c33_tables[3, 1:10])), 6))
(k11w1c33_truescore <- round((2 * k11w1c33_tpr * k11w1c33_tnr) / (k11w1c33_tpr + k11w1c33_tnr), 6))

(k11w2c33_tpr <- round(sum(cm_c33_tables[1, 11:20]) / (sum(cm_c33_tables[1, 11:20]) + sum(cm_c33_tables[2, 11:20])), 6))
(k11w2c33_tnr <- round(sum(cm_c33_tables[4, 11:20]) / (sum(cm_c33_tables[4, 11:20]) + sum(cm_c33_tables[3, 11:20])), 6))
(k11w2c33_truescore <- round((2 * k11w2c33_tpr * k11w2c33_tnr) / (k11w2c33_tpr + k11w2c33_tnr), 6))

(k11w3c33_tpr <- round(sum(cm_c33_tables[1, 21:30]) / (sum(cm_c33_tables[1, 21:30]) + sum(cm_c33_tables[2, 21:30])), 6))
(k11w3c33_tnr <- round(sum(cm_c33_tables[4, 21:30]) / (sum(cm_c33_tables[4, 21:30]) + sum(cm_c33_tables[3, 21:30])), 6))
(k11w3c33_truescore <- round((2 * k11w3c33_tpr * k11w3c33_tnr) / (k11w3c33_tpr + k11w3c33_tnr), 6))


(k13w1c33_tpr <- round(sum(cm_c33_tables[1, 31:40]) / (sum(cm_c33_tables[1, 31:40]) + sum(cm_c33_tables[2, 31:40])), 6))
(k13w1c33_tnr <- round(sum(cm_c33_tables[4, 31:40]) / (sum(cm_c33_tables[4, 31:40]) + sum(cm_c33_tables[3, 31:40])), 6))
(k13w1c33_truescore <- round((2 * k13w1c33_tpr * k13w1c33_tnr) / (k13w1c33_tpr + k13w1c33_tnr), 6))

(k13w2c33_tpr <- round(sum(cm_c33_tables[1, 41:50]) / (sum(cm_c33_tables[1, 41:50]) + sum(cm_c33_tables[2, 41:50])), 6))
(k13w2c33_tnr <- round(sum(cm_c33_tables[4, 41:50]) / (sum(cm_c33_tables[4, 41:50]) + sum(cm_c33_tables[3, 41:50])), 6))
(k13w2c33_truescore <- round((2 * k13w2c33_tpr * k13w2c33_tnr) / (k13w2c33_tpr + k13w2c33_tnr), 6))

(k13w3c33_tpr <- round(sum(cm_c33_tables[1, 51:60]) / (sum(cm_c33_tables[1, 51:60]) + sum(cm_c33_tables[2, 51:60])), 6))
(k13w3c33_tnr <- round(sum(cm_c33_tables[4, 51:60]) / (sum(cm_c33_tables[4, 51:60]) + sum(cm_c33_tables[3, 51:60])), 6))
(k13w3c33_truescore <- round((2 * k13w3c33_tpr * k13w3c33_tnr) / (k13w3c33_tpr + k13w3c33_tnr), 6))


(k15w1c33_tpr <- round(sum(cm_c33_tables[1, 61:70]) / (sum(cm_c33_tables[1, 61:70]) + sum(cm_c33_tables[2, 61:70])), 6))
(k15w1c33_tnr <- round(sum(cm_c33_tables[4, 61:70]) / (sum(cm_c33_tables[4, 61:70]) + sum(cm_c33_tables[3, 61:70])), 6))
(k15w1c33_truescore <- round((2 * k15w1c33_tpr * k15w1c33_tnr) / (k15w1c33_tpr + k15w1c33_tnr), 6))

(k15w2c33_tpr <- round(sum(cm_c33_tables[1, 71:80]) / (sum(cm_c33_tables[1, 71:80]) + sum(cm_c33_tables[2, 71:80])), 6))
(k15w2c33_tnr <- round(sum(cm_c33_tables[4, 71:80]) / (sum(cm_c33_tables[4, 71:80]) + sum(cm_c33_tables[3, 71:80])), 6))
(k15w2c33_truescore <- round((2 * k15w2c33_tpr * k15w2c33_tnr) / (k15w2c33_tpr + k15w2c33_tnr), 6))

(k15w3c33_tpr <- round(sum(cm_c33_tables[1, 81:90]) / (sum(cm_c33_tables[1, 81:90]) + sum(cm_c33_tables[2, 81:90])), 6))
(k15w3c33_tnr <- round(sum(cm_c33_tables[4, 81:90]) / (sum(cm_c33_tables[4, 81:90]) + sum(cm_c33_tables[3, 81:90])), 6))
(k15w3c33_truescore <- round((2 * k15w3c33_tpr * k15w3c33_tnr) / (k15w3c33_tpr + k15w3c33_tnr), 6))


(k17w1c33_tpr <- round(sum(cm_c33_tables[1, 91:100]) / (sum(cm_c33_tables[1, 91:100]) + sum(cm_c33_tables[2, 91:100])), 6))
(k17w1c33_tnr <- round(sum(cm_c33_tables[4, 91:100]) / (sum(cm_c33_tables[4, 91:100]) + sum(cm_c33_tables[3, 91:100])), 6))
(k17w1c33_truescore <- round((2 * k17w1c33_tpr * k17w1c33_tnr) / (k17w1c33_tpr + k17w1c33_tnr), 6))

(k17w2c33_tpr <- round(sum(cm_c33_tables[1, 101:110]) / (sum(cm_c33_tables[1, 101:110]) + sum(cm_c33_tables[2, 101:110])), 6))
(k17w2c33_tnr <- round(sum(cm_c33_tables[4, 101:110]) / (sum(cm_c33_tables[4, 101:110]) + sum(cm_c33_tables[3, 101:110])), 6))
(k17w2c33_truescore <- round((2 * k17w2c33_tpr * k17w2c33_tnr) / (k17w2c33_tpr + k17w2c33_tnr), 6))

(k17w3c33_tpr <- round(sum(cm_c33_tables[1, 111:120]) / (sum(cm_c33_tables[1, 111:120]) + sum(cm_c33_tables[2, 111:120])), 6))
(k17w3c33_tnr <- round(sum(cm_c33_tables[4, 111:120]) / (sum(cm_c33_tables[4, 111:120]) + sum(cm_c33_tables[3, 111:120])), 6))
(k17w3c33_truescore <- round((2 * k17w3c33_tpr * k17w3c33_tnr) / (k17w3c33_tpr + k17w3c33_tnr), 6))


(k19w1c33_tpr <- round(sum(cm_c33_tables[1, 121:130]) / (sum(cm_c33_tables[1, 121:130]) + sum(cm_c33_tables[2, 121:130])), 6))
(k19w1c33_tnr <- round(sum(cm_c33_tables[4, 121:130]) / (sum(cm_c33_tables[4, 121:130]) + sum(cm_c33_tables[3, 121:130])), 6))
(k19w1c33_truescore <- round((2 * k19w1c33_tpr * k19w1c33_tnr) / (k19w1c33_tpr + k19w1c33_tnr), 6))

(k19w2c33_tpr <- round(sum(cm_c33_tables[1, 131:140]) / (sum(cm_c33_tables[1, 131:140]) + sum(cm_c33_tables[2, 131:140])), 6))
(k19w2c33_tnr <- round(sum(cm_c33_tables[4, 131:140]) / (sum(cm_c33_tables[4, 131:140]) + sum(cm_c33_tables[3, 131:140])), 6))
(k19w2c33_truescore <- round((2 * k19w2c33_tpr * k19w2c33_tnr) / (k19w2c33_tpr + k19w2c33_tnr), 6))

(k19w3c33_tpr <- round(sum(cm_c33_tables[1, 141:150]) / (sum(cm_c33_tables[1, 141:150]) + sum(cm_c33_tables[2, 141:150])), 6))
(k19w3c33_tnr <- round(sum(cm_c33_tables[4, 141:150]) / (sum(cm_c33_tables[4, 141:150]) + sum(cm_c33_tables[3, 141:150])), 6))
(k19w3c33_truescore <- round((2 * k19w3c33_tpr * k19w3c33_tnr) / (k19w3c33_tpr + k19w3c33_tnr), 6))


(k21w1c33_tpr <- round(sum(cm_c33_tables[1, 151:160]) / (sum(cm_c33_tables[1, 151:160]) + sum(cm_c33_tables[2, 151:160])), 6))
(k21w1c33_tnr <- round(sum(cm_c33_tables[4, 151:160]) / (sum(cm_c33_tables[4, 151:160]) + sum(cm_c33_tables[3, 151:160])), 6))
(k21w1c33_truescore <- round((2 * k21w1c33_tpr * k21w1c33_tnr) / (k21w1c33_tpr + k21w1c33_tnr), 6))

(k21w2c33_tpr <- round(sum(cm_c33_tables[1, 161:170]) / (sum(cm_c33_tables[1, 161:170]) + sum(cm_c33_tables[2, 161:170])), 6))
(k21w2c33_tnr <- round(sum(cm_c33_tables[4, 161:170]) / (sum(cm_c33_tables[4, 161:170]) + sum(cm_c33_tables[3, 161:170])), 6))
(k21w2c33_truescore <- round((2 * k21w2c33_tpr * k21w2c33_tnr) / (k21w2c33_tpr + k21w2c33_tnr), 6))

(k21w3c33_tpr <- round(sum(cm_c33_tables[1, 171:180]) / (sum(cm_c33_tables[1, 171:180]) + sum(cm_c33_tables[2, 171:180])), 6))
(k21w3c33_tnr <- round(sum(cm_c33_tables[4, 171:180]) / (sum(cm_c33_tables[4, 171:180]) + sum(cm_c33_tables[3, 171:180])), 6))
(k21w3c33_truescore <- round((2 * k21w3c33_tpr * k21w3c33_tnr) / (k21w3c33_tpr + k21w3c33_tnr), 6))


(k23w1c33_tpr <- round(sum(cm_c33_tables[1, 181:190]) / (sum(cm_c33_tables[1, 181:190]) + sum(cm_c33_tables[2, 181:190])), 6))
(k23w1c33_tnr <- round(sum(cm_c33_tables[4, 181:190]) / (sum(cm_c33_tables[4, 181:190]) + sum(cm_c33_tables[3, 181:190])), 6))
(k23w1c33_truescore <- round((2 * k23w1c33_tpr * k23w1c33_tnr) / (k23w1c33_tpr + k23w1c33_tnr), 6))

(k23w2c33_tpr <- round(sum(cm_c33_tables[1, 191:200]) / (sum(cm_c33_tables[1, 191:200]) + sum(cm_c33_tables[2, 191:200])), 6))
(k23w2c33_tnr <- round(sum(cm_c33_tables[4, 191:200]) / (sum(cm_c33_tables[4, 191:200]) + sum(cm_c33_tables[3, 191:200])), 6))
(k23w2c33_truescore <- round((2 * k23w2c33_tpr * k23w2c33_tnr) / (k23w2c33_tpr + k23w2c33_tnr), 6))

(k23w3c33_tpr <- round(sum(cm_c33_tables[1, 201:210]) / (sum(cm_c33_tables[1, 201:210]) + sum(cm_c33_tables[2, 201:210])), 6))
(k23w3c33_tnr <- round(sum(cm_c33_tables[4, 201:210]) / (sum(cm_c33_tables[4, 201:210]) + sum(cm_c33_tables[3, 201:210])), 6))
(k23w3c33_truescore <- round((2 * k23w3c33_tpr * k23w3c33_tnr) / (k23w3c33_tpr + k23w3c33_tnr), 6))


(k25w1c33_tpr <- round(sum(cm_c33_tables[1, 211:220]) / (sum(cm_c33_tables[1, 211:220]) + sum(cm_c33_tables[2, 211:220])), 6))
(k25w1c33_tnr <- round(sum(cm_c33_tables[4, 211:220]) / (sum(cm_c33_tables[4, 211:220]) + sum(cm_c33_tables[3, 211:220])), 6))
(k25w1c33_truescore <- round((2 * k25w1c33_tpr * k25w1c33_tnr) / (k25w1c33_tpr + k25w1c33_tnr), 6))

(k25w2c33_tpr <- round(sum(cm_c33_tables[1, 221:230]) / (sum(cm_c33_tables[1, 221:230]) + sum(cm_c33_tables[2, 221:230])), 6))
(k25w2c33_tnr <- round(sum(cm_c33_tables[4, 221:230]) / (sum(cm_c33_tables[4, 221:230]) + sum(cm_c33_tables[3, 221:230])), 6))
(k25w2c33_truescore <- round((2 * k25w2c33_tpr * k25w2c33_tnr) / (k25w2c33_tpr + k25w2c33_tnr), 6))

(k25w3c33_tpr <- round(sum(cm_c33_tables[1, 231:240]) / (sum(cm_c33_tables[1, 231:240]) + sum(cm_c33_tables[2, 231:240])), 6))
(k25w3c33_tnr <- round(sum(cm_c33_tables[4, 231:240]) / (sum(cm_c33_tables[4, 231:240]) + sum(cm_c33_tables[3, 231:240])), 6))
(k25w3c33_truescore <- round((2 * k25w3c33_tpr * k25w3c33_tnr) / (k25w3c33_tpr + k25w3c33_tnr), 6))


(k27w1c33_tpr <- round(sum(cm_c33_tables[1, 241:250]) / (sum(cm_c33_tables[1, 241:250]) + sum(cm_c33_tables[2, 241:250])), 6))
(k27w1c33_tnr <- round(sum(cm_c33_tables[4, 241:250]) / (sum(cm_c33_tables[4, 241:250]) + sum(cm_c33_tables[3, 241:250])), 6))
(k27w1c33_truescore <- round((2 * k27w1c33_tpr * k27w1c33_tnr) / (k27w1c33_tpr + k27w1c33_tnr), 6))

(k27w2c33_tpr <- round(sum(cm_c33_tables[1, 251:260]) / (sum(cm_c33_tables[1, 251:260]) + sum(cm_c33_tables[2, 251:260])), 6))
(k27w2c33_tnr <- round(sum(cm_c33_tables[4, 251:260]) / (sum(cm_c33_tables[4, 251:260]) + sum(cm_c33_tables[3, 251:260])), 6))
(k27w2c33_truescore <- round((2 * k27w2c33_tpr * k27w2c33_tnr) / (k27w2c33_tpr + k27w2c33_tnr), 6))

(k27w3c33_tpr <- round(sum(cm_c33_tables[1, 261:270]) / (sum(cm_c33_tables[1, 261:270]) + sum(cm_c33_tables[2, 261:270])), 6))
(k27w3c33_tnr <- round(sum(cm_c33_tables[4, 261:270]) / (sum(cm_c33_tables[4, 261:270]) + sum(cm_c33_tables[3, 261:270])), 6))
(k27w3c33_truescore <- round((2 * k27w3c33_tpr * k27w3c33_tnr) / (k27w3c33_tpr + k27w3c33_tnr), 6))


(k29w1c33_tpr <- round(sum(cm_c33_tables[1, 271:280]) / (sum(cm_c33_tables[1, 271:280]) + sum(cm_c33_tables[2, 271:280])), 6))
(k29w1c33_tnr <- round(sum(cm_c33_tables[4, 271:280]) / (sum(cm_c33_tables[4, 271:280]) + sum(cm_c33_tables[3, 271:280])), 6))
(k29w1c33_truescore <- round((2 * k29w1c33_tpr * k29w1c33_tnr) / (k29w1c33_tpr + k29w1c33_tnr), 6))

(k29w2c33_tpr <- round(sum(cm_c33_tables[1, 281:290]) / (sum(cm_c33_tables[1, 281:290]) + sum(cm_c33_tables[2, 281:290])), 6))
(k29w2c33_tnr <- round(sum(cm_c33_tables[4, 281:290]) / (sum(cm_c33_tables[4, 281:290]) + sum(cm_c33_tables[3, 281:290])), 6))
(k29w2c33_truescore <- round((2 * k29w2c33_tpr * k29w2c33_tnr) / (k29w2c33_tpr + k29w2c33_tnr), 6))

(k29w3c33_tpr <- round(sum(cm_c33_tables[1, 291:300]) / (sum(cm_c33_tables[1, 291:300]) + sum(cm_c33_tables[2, 291:300])), 6))
(k29w3c33_tnr <- round(sum(cm_c33_tables[4, 291:300]) / (sum(cm_c33_tables[4, 291:300]) + sum(cm_c33_tables[3, 291:300])), 6))
(k29w3c33_truescore <- round((2 * k29w3c33_tpr * k29w3c33_tnr) / (k29w3c33_tpr + k29w3c33_tnr), 6))


(k31w1c33_tpr <- round(sum(cm_c33_tables[1, 301:310]) / (sum(cm_c33_tables[1, 301:310]) + sum(cm_c33_tables[2, 301:310])), 6))
(k31w1c33_tnr <- round(sum(cm_c33_tables[4, 301:310]) / (sum(cm_c33_tables[4, 301:310]) + sum(cm_c33_tables[3, 301:310])), 6))
(k31w1c33_truescore <- round((2 * k31w1c33_tpr * k31w1c33_tnr) / (k31w1c33_tpr + k31w1c33_tnr), 6))

(k31w2c33_tpr <- round(sum(cm_c33_tables[1, 311:320]) / (sum(cm_c33_tables[1, 311:320]) + sum(cm_c33_tables[2, 311:320])), 6))
(k31w2c33_tnr <- round(sum(cm_c33_tables[4, 311:320]) / (sum(cm_c33_tables[4, 311:320]) + sum(cm_c33_tables[3, 311:320])), 6))
(k31w2c33_truescore <- round((2 * k31w2c33_tpr * k31w2c33_tnr) / (k31w2c33_tpr + k31w2c33_tnr), 6))

(k31w3c33_tpr <- round(sum(cm_c33_tables[1, 321:330]) / (sum(cm_c33_tables[1, 321:330]) + sum(cm_c33_tables[2, 321:330])), 6))
(k31w3c33_tnr <- round(sum(cm_c33_tables[4, 321:330]) / (sum(cm_c33_tables[4, 321:330]) + sum(cm_c33_tables[3, 321:330])), 6))
(k31w3c33_truescore <- round((2 * k31w3c33_tpr * k31w3c33_tnr) / (k31w3c33_tpr + k31w3c33_tnr), 6))


(k33w1c33_tpr <- round(sum(cm_c33_tables[1, 331:340]) / (sum(cm_c33_tables[1, 331:340]) + sum(cm_c33_tables[2, 331:340])), 6))
(k33w1c33_tnr <- round(sum(cm_c33_tables[4, 331:340]) / (sum(cm_c33_tables[4, 331:340]) + sum(cm_c33_tables[3, 331:340])), 6))
(k33w1c33_truescore <- round((2 * k33w1c33_tpr * k33w1c33_tnr) / (k33w1c33_tpr + k33w1c33_tnr), 6))

(k33w2c33_tpr <- round(sum(cm_c33_tables[1, 341:350]) / (sum(cm_c33_tables[1, 341:350]) + sum(cm_c33_tables[2, 341:350])), 6))
(k33w2c33_tnr <- round(sum(cm_c33_tables[4, 341:350]) / (sum(cm_c33_tables[4, 341:350]) + sum(cm_c33_tables[3, 341:350])), 6))
(k33w2c33_truescore <- round((2 * k33w2c33_tpr * k33w2c33_tnr) / (k33w2c33_tpr + k33w2c33_tnr), 6))

(k33w3c33_tpr <- round(sum(cm_c33_tables[1, 351:360]) / (sum(cm_c33_tables[1, 351:360]) + sum(cm_c33_tables[2, 351:360])), 6))
(k33w3c33_tnr <- round(sum(cm_c33_tables[4, 351:360]) / (sum(cm_c33_tables[4, 351:360]) + sum(cm_c33_tables[3, 351:360])), 6))
(k33w3c33_truescore <- round((2 * k33w3c33_tpr * k33w3c33_tnr) / (k33w3c33_tpr + k33w3c33_tnr), 6))


(k35w1c33_tpr <- round(sum(cm_c33_tables[1, 361:370]) / (sum(cm_c33_tables[1, 361:370]) + sum(cm_c33_tables[2, 361:370])), 6))
(k35w1c33_tnr <- round(sum(cm_c33_tables[4, 361:370]) / (sum(cm_c33_tables[4, 361:370]) + sum(cm_c33_tables[3, 361:370])), 6))
(k35w1c33_truescore <- round((2 * k35w1c33_tpr * k35w1c33_tnr) / (k35w1c33_tpr + k35w1c33_tnr), 6))

(k35w2c33_tpr <- round(sum(cm_c33_tables[1, 371:380]) / (sum(cm_c33_tables[1, 371:380]) + sum(cm_c33_tables[2, 371:380])), 6))
(k35w2c33_tnr <- round(sum(cm_c33_tables[4, 371:380]) / (sum(cm_c33_tables[4, 371:380]) + sum(cm_c33_tables[3, 371:380])), 6))
(k35w2c33_truescore <- round((2 * k35w2c33_tpr * k35w2c33_tnr) / (k35w2c33_tpr + k35w2c33_tnr), 6))

(k35w3c33_tpr <- round(sum(cm_c33_tables[1, 381:390]) / (sum(cm_c33_tables[1, 381:390]) + sum(cm_c33_tables[2, 381:390])), 6))
(k35w3c33_tnr <- round(sum(cm_c33_tables[4, 381:390]) / (sum(cm_c33_tables[4, 381:390]) + sum(cm_c33_tables[3, 381:390])), 6))
(k35w3c33_truescore <- round((2 * k35w3c33_tpr * k35w3c33_tnr) / (k35w3c33_tpr + k35w3c33_tnr), 6))


(k37w1c33_tpr <- round(sum(cm_c33_tables[1, 391:400]) / (sum(cm_c33_tables[1, 391:400]) + sum(cm_c33_tables[2, 391:400])), 6))
(k37w1c33_tnr <- round(sum(cm_c33_tables[4, 391:400]) / (sum(cm_c33_tables[4, 391:400]) + sum(cm_c33_tables[3, 391:400])), 6))
(k37w1c33_truescore <- round((2 * k37w1c33_tpr * k37w1c33_tnr) / (k37w1c33_tpr + k37w1c33_tnr), 6))

(k37w2c33_tpr <- round(sum(cm_c33_tables[1, 401:410]) / (sum(cm_c33_tables[1, 401:410]) + sum(cm_c33_tables[2, 401:410])), 6))
(k37w2c33_tnr <- round(sum(cm_c33_tables[4, 401:410]) / (sum(cm_c33_tables[4, 401:410]) + sum(cm_c33_tables[3, 401:410])), 6))
(k37w2c33_truescore <- round((2 * k37w2c33_tpr * k37w2c33_tnr) / (k37w2c33_tpr + k37w2c33_tnr), 6))

(k37w3c33_tpr <- round(sum(cm_c33_tables[1, 411:420]) / (sum(cm_c33_tables[1, 411:420]) + sum(cm_c33_tables[2, 411:420])), 6))
(k37w3c33_tnr <- round(sum(cm_c33_tables[4, 411:420]) / (sum(cm_c33_tables[4, 411:420]) + sum(cm_c33_tables[3, 411:420])), 6))
(k37w3c33_truescore <- round((2 * k37w3c33_tpr * k37w3c33_tnr) / (k37w3c33_tpr + k37w3c33_tnr), 6))


(k39w1c33_tpr <- round(sum(cm_c33_tables[1, 421:430]) / (sum(cm_c33_tables[1, 421:430]) + sum(cm_c33_tables[2, 421:430])), 6))
(k39w1c33_tnr <- round(sum(cm_c33_tables[4, 421:430]) / (sum(cm_c33_tables[4, 421:430]) + sum(cm_c33_tables[3, 421:430])), 6))
(k39w1c33_truescore <- round((2 * k39w1c33_tpr * k39w1c33_tnr) / (k39w1c33_tpr + k39w1c33_tnr), 6))

(k39w2c33_tpr <- round(sum(cm_c33_tables[1, 431:440]) / (sum(cm_c33_tables[1, 431:440]) + sum(cm_c33_tables[2, 431:440])), 6))
(k39w2c33_tnr <- round(sum(cm_c33_tables[4, 431:440]) / (sum(cm_c33_tables[4, 431:440]) + sum(cm_c33_tables[3, 431:440])), 6))
(k39w2c33_truescore <- round((2 * k39w2c33_tpr * k39w2c33_tnr) / (k39w2c33_tpr + k39w2c33_tnr), 6))

(k39w3c33_tpr <- round(sum(cm_c33_tables[1, 441:450]) / (sum(cm_c33_tables[1, 441:450]) + sum(cm_c33_tables[2, 441:450])), 6))
(k39w3c33_tnr <- round(sum(cm_c33_tables[4, 441:450]) / (sum(cm_c33_tables[4, 441:450]) + sum(cm_c33_tables[3, 441:450])), 6))
(k39w3c33_truescore <- round((2 * k39w3c33_tpr * k39w3c33_tnr) / (k39w3c33_tpr + k39w3c33_tnr), 6))


# Compile the 0.33 cutoff results in a table, and identify the combination of 
# of k and kernel that maximizes the Truescore.

c33_results <- tibble(k = c(11, 11, 11, 13, 13, 13, 15, 15, 15, 
                            17, 17, 17, 19, 19, 19, 21, 21, 21, 
                            23, 23, 23, 25, 25, 25, 27, 27, 27, 
                            29, 29, 29, 31, 31, 31, 33, 33, 33, 
                            35, 35, 35, 37, 37, 37, 39, 39, 39), 
                      Kernel = rep(c("triangular", "gaussian", "optimal"), 15), 
                      Cut = 0.33, 
                      TPR = c(k11w1c33_tpr, k11w2c33_tpr, k11w3c33_tpr, k13w1c33_tpr, 
                              k13w2c33_tpr, k13w3c33_tpr, k15w1c33_tpr, k15w2c33_tpr, 
                              k15w3c33_tpr, k17w1c33_tpr, k17w2c33_tpr, k17w3c33_tpr, 
                              k19w1c33_tpr, k19w2c33_tpr, k19w3c33_tpr, k21w1c33_tpr, 
                              k21w2c33_tpr, k21w3c33_tpr, k23w1c33_tpr, k23w2c33_tpr, 
                              k23w3c33_tpr, k25w1c33_tpr, k25w2c33_tpr, k25w3c33_tpr, 
                              k27w1c33_tpr, k27w2c33_tpr, k27w3c33_tpr, k29w1c33_tpr, 
                              k29w2c33_tpr, k29w3c33_tpr, k31w1c33_tpr, k31w2c33_tpr, 
                              k31w3c33_tpr, k33w1c33_tpr, k33w2c33_tpr, k33w3c33_tpr, 
                              k35w1c33_tpr, k35w2c33_tpr, k35w3c33_tpr, k37w1c33_tpr, 
                              k37w2c33_tpr, k37w3c33_tpr, k39w1c33_tpr, k39w2c33_tpr, 
                              k39w3c33_tpr), 
                      TNR = c(k11w1c33_tnr, k11w2c33_tnr, k11w3c33_tnr, k13w1c33_tnr, 
                              k13w2c33_tnr, k13w3c33_tnr, k15w1c33_tnr, k15w2c33_tnr, 
                              k15w3c33_tnr, k17w1c33_tnr, k17w2c33_tnr, k17w3c33_tnr, 
                              k19w1c33_tnr, k19w2c33_tnr, k19w3c33_tnr, k21w1c33_tnr, 
                              k21w2c33_tnr, k21w3c33_tnr, k23w1c33_tnr, k23w2c33_tnr, 
                              k23w3c33_tnr, k25w1c33_tnr, k25w2c33_tnr, k25w3c33_tnr, 
                              k27w1c33_tnr, k27w2c33_tnr, k27w3c33_tnr, k29w1c33_tnr, 
                              k29w2c33_tnr, k29w3c33_tnr, k31w1c33_tnr, k31w2c33_tnr, 
                              k31w3c33_tnr, k33w1c33_tnr, k33w2c33_tnr, k33w3c33_tnr, 
                              k35w1c33_tnr, k35w2c33_tnr, k35w3c33_tnr, k37w1c33_tnr, 
                              k37w2c33_tnr, k37w3c33_tnr, k39w1c33_tnr, k39w2c33_tnr, 
                              k39w3c33_tnr), 
                      Truescore = c(k11w1c33_truescore, k11w2c33_truescore, 
                                    k11w3c33_truescore, k13w1c33_truescore, 
                                    k13w2c33_truescore, k13w3c33_truescore, 
                                    k15w1c33_truescore, k15w2c33_truescore, 
                                    k15w3c33_truescore, k17w1c33_truescore, 
                                    k17w2c33_truescore, k17w3c33_truescore, 
                                    k19w1c33_truescore, k19w2c33_truescore, 
                                    k19w3c33_truescore, k21w1c33_truescore, 
                                    k21w2c33_truescore, k21w3c33_truescore, 
                                    k23w1c33_truescore, k23w2c33_truescore, 
                                    k23w3c33_truescore, k25w1c33_truescore, 
                                    k25w2c33_truescore, k25w3c33_truescore, 
                                    k27w1c33_truescore, k27w2c33_truescore, 
                                    k27w3c33_truescore, k29w1c33_truescore, 
                                    k29w2c33_truescore, k29w3c33_truescore, 
                                    k31w1c33_truescore, k31w2c33_truescore, 
                                    k31w3c33_truescore, k33w1c33_truescore, 
                                    k33w2c33_truescore, k33w3c33_truescore, 
                                    k35w1c33_truescore, k35w2c33_truescore, 
                                    k35w3c33_truescore, k37w1c33_truescore, 
                                    k37w2c33_truescore, k37w3c33_truescore, 
                                    k39w1c33_truescore, k39w2c33_truescore, 
                                    k39w3c33_truescore))

knitr::kable(c33_results[1:45, ], caption = "c33_results")

ggplot(c33_results, aes(k, Truescore, shape = Kernel, color = Kernel)) + 
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.33 (Truescore)")

# For the Cutoff of 0.33, compute the Minimum Distance to (0, 1) for each 
# combination of k and Kernel. 

c33_results <- c33_results %>% mutate(Distance = sqrt((1 - TPR)^2 + (1 - TNR)^2))

ggplot(c33_results, aes(k, Distance, shape = Kernel, color = Kernel)) +
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.33 (Distance)")

# For the Cutoff of 0.33, identify the optimal combination of values for k and Kernel 
# based on each of our assessment methods, Truescore and Mimimum Distance to (0, 1).

max(c33_results$Truescore)
(c33_opt_k_ts <- c33_results$k[which.max(c33_results$Truescore)])
(c33_opt_kernel_ts <- c33_results$Kernel[which.max(c33_results$Truescore)])
(c33_opt_cut_ts <- c33_results$Cut[which.max(c33_results$Truescore)])
(c33_opt_tpr_ts <- c33_results$TPR[which.max(c33_results$Truescore)])
(c33_opt_tnr_ts <- c33_results$TNR[which.max(c33_results$Truescore)])
(c33_opt_d_ts <- c33_results$Distance[which.max(c33_results$Truescore)])

min(c33_results$Distance)
(c33_opt_k_dist <- c33_results$k[which.min(c33_results$Distance)])
(c33_opt_kernel_dist <- c33_results$Kernel[which.min(c33_results$Distance)])
(c33_opt_cut_dist <- c33_results$Cut[which.min(c33_results$Distance)])  
(c33_opt_tpr_dist <- c33_results$TPR[which.min(c33_results$Distance)])
(c33_opt_tnr_dist <- c33_results$TNR[which.min(c33_results$Distance)])
(c33_opt_t_dist <- c33_results$Truescore[which.min(c33_results$Distance)])

############################
# 0.34 Cutoff
############################

# For the decision cutoff of 0.34, generate a confusion matrix for
# every combination of k (11:39 by two), kernel (1:3) and fold (1:10).

cm_c34 <- sapply(kwf_dfs_l, function(x) {
  ss <- subset(x, select = c(pred34, obs))
  confusionMatrix(ss$pred34, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c34) <- kwf_dfs_v

cm_c34_tables <- sapply(cm_c34, "[[", 2)
cm_c34_tables <- as_tibble(cm_c34_tables)

# For each combination of k and kernel, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each combination of k and kernel when the decision cutoff is 0.34.

(k11w1c34_tpr <- round(sum(cm_c34_tables[1, 1:10]) / (sum(cm_c34_tables[1, 1:10]) + sum(cm_c34_tables[2, 1:10])), 6))
(k11w1c34_tnr <- round(sum(cm_c34_tables[4, 1:10]) / (sum(cm_c34_tables[4, 1:10]) + sum(cm_c34_tables[3, 1:10])), 6))
(k11w1c34_truescore <- round((2 * k11w1c34_tpr * k11w1c34_tnr) / (k11w1c34_tpr + k11w1c34_tnr), 6))

(k11w2c34_tpr <- round(sum(cm_c34_tables[1, 11:20]) / (sum(cm_c34_tables[1, 11:20]) + sum(cm_c34_tables[2, 11:20])), 6))
(k11w2c34_tnr <- round(sum(cm_c34_tables[4, 11:20]) / (sum(cm_c34_tables[4, 11:20]) + sum(cm_c34_tables[3, 11:20])), 6))
(k11w2c34_truescore <- round((2 * k11w2c34_tpr * k11w2c34_tnr) / (k11w2c34_tpr + k11w2c34_tnr), 6))

(k11w3c34_tpr <- round(sum(cm_c34_tables[1, 21:30]) / (sum(cm_c34_tables[1, 21:30]) + sum(cm_c34_tables[2, 21:30])), 6))
(k11w3c34_tnr <- round(sum(cm_c34_tables[4, 21:30]) / (sum(cm_c34_tables[4, 21:30]) + sum(cm_c34_tables[3, 21:30])), 6))
(k11w3c34_truescore <- round((2 * k11w3c34_tpr * k11w3c34_tnr) / (k11w3c34_tpr + k11w3c34_tnr), 6))


(k13w1c34_tpr <- round(sum(cm_c34_tables[1, 31:40]) / (sum(cm_c34_tables[1, 31:40]) + sum(cm_c34_tables[2, 31:40])), 6))
(k13w1c34_tnr <- round(sum(cm_c34_tables[4, 31:40]) / (sum(cm_c34_tables[4, 31:40]) + sum(cm_c34_tables[3, 31:40])), 6))
(k13w1c34_truescore <- round((2 * k13w1c34_tpr * k13w1c34_tnr) / (k13w1c34_tpr + k13w1c34_tnr), 6))

(k13w2c34_tpr <- round(sum(cm_c34_tables[1, 41:50]) / (sum(cm_c34_tables[1, 41:50]) + sum(cm_c34_tables[2, 41:50])), 6))
(k13w2c34_tnr <- round(sum(cm_c34_tables[4, 41:50]) / (sum(cm_c34_tables[4, 41:50]) + sum(cm_c34_tables[3, 41:50])), 6))
(k13w2c34_truescore <- round((2 * k13w2c34_tpr * k13w2c34_tnr) / (k13w2c34_tpr + k13w2c34_tnr), 6))

(k13w3c34_tpr <- round(sum(cm_c34_tables[1, 51:60]) / (sum(cm_c34_tables[1, 51:60]) + sum(cm_c34_tables[2, 51:60])), 6))
(k13w3c34_tnr <- round(sum(cm_c34_tables[4, 51:60]) / (sum(cm_c34_tables[4, 51:60]) + sum(cm_c34_tables[3, 51:60])), 6))
(k13w3c34_truescore <- round((2 * k13w3c34_tpr * k13w3c34_tnr) / (k13w3c34_tpr + k13w3c34_tnr), 6))


(k15w1c34_tpr <- round(sum(cm_c34_tables[1, 61:70]) / (sum(cm_c34_tables[1, 61:70]) + sum(cm_c34_tables[2, 61:70])), 6))
(k15w1c34_tnr <- round(sum(cm_c34_tables[4, 61:70]) / (sum(cm_c34_tables[4, 61:70]) + sum(cm_c34_tables[3, 61:70])), 6))
(k15w1c34_truescore <- round((2 * k15w1c34_tpr * k15w1c34_tnr) / (k15w1c34_tpr + k15w1c34_tnr), 6))

(k15w2c34_tpr <- round(sum(cm_c34_tables[1, 71:80]) / (sum(cm_c34_tables[1, 71:80]) + sum(cm_c34_tables[2, 71:80])), 6))
(k15w2c34_tnr <- round(sum(cm_c34_tables[4, 71:80]) / (sum(cm_c34_tables[4, 71:80]) + sum(cm_c34_tables[3, 71:80])), 6))
(k15w2c34_truescore <- round((2 * k15w2c34_tpr * k15w2c34_tnr) / (k15w2c34_tpr + k15w2c34_tnr), 6))

(k15w3c34_tpr <- round(sum(cm_c34_tables[1, 81:90]) / (sum(cm_c34_tables[1, 81:90]) + sum(cm_c34_tables[2, 81:90])), 6))
(k15w3c34_tnr <- round(sum(cm_c34_tables[4, 81:90]) / (sum(cm_c34_tables[4, 81:90]) + sum(cm_c34_tables[3, 81:90])), 6))
(k15w3c34_truescore <- round((2 * k15w3c34_tpr * k15w3c34_tnr) / (k15w3c34_tpr + k15w3c34_tnr), 6))


(k17w1c34_tpr <- round(sum(cm_c34_tables[1, 91:100]) / (sum(cm_c34_tables[1, 91:100]) + sum(cm_c34_tables[2, 91:100])), 6))
(k17w1c34_tnr <- round(sum(cm_c34_tables[4, 91:100]) / (sum(cm_c34_tables[4, 91:100]) + sum(cm_c34_tables[3, 91:100])), 6))
(k17w1c34_truescore <- round((2 * k17w1c34_tpr * k17w1c34_tnr) / (k17w1c34_tpr + k17w1c34_tnr), 6))

(k17w2c34_tpr <- round(sum(cm_c34_tables[1, 101:110]) / (sum(cm_c34_tables[1, 101:110]) + sum(cm_c34_tables[2, 101:110])), 6))
(k17w2c34_tnr <- round(sum(cm_c34_tables[4, 101:110]) / (sum(cm_c34_tables[4, 101:110]) + sum(cm_c34_tables[3, 101:110])), 6))
(k17w2c34_truescore <- round((2 * k17w2c34_tpr * k17w2c34_tnr) / (k17w2c34_tpr + k17w2c34_tnr), 6))

(k17w3c34_tpr <- round(sum(cm_c34_tables[1, 111:120]) / (sum(cm_c34_tables[1, 111:120]) + sum(cm_c34_tables[2, 111:120])), 6))
(k17w3c34_tnr <- round(sum(cm_c34_tables[4, 111:120]) / (sum(cm_c34_tables[4, 111:120]) + sum(cm_c34_tables[3, 111:120])), 6))
(k17w3c34_truescore <- round((2 * k17w3c34_tpr * k17w3c34_tnr) / (k17w3c34_tpr + k17w3c34_tnr), 6))


(k19w1c34_tpr <- round(sum(cm_c34_tables[1, 121:130]) / (sum(cm_c34_tables[1, 121:130]) + sum(cm_c34_tables[2, 121:130])), 6))
(k19w1c34_tnr <- round(sum(cm_c34_tables[4, 121:130]) / (sum(cm_c34_tables[4, 121:130]) + sum(cm_c34_tables[3, 121:130])), 6))
(k19w1c34_truescore <- round((2 * k19w1c34_tpr * k19w1c34_tnr) / (k19w1c34_tpr + k19w1c34_tnr), 6))

(k19w2c34_tpr <- round(sum(cm_c34_tables[1, 131:140]) / (sum(cm_c34_tables[1, 131:140]) + sum(cm_c34_tables[2, 131:140])), 6))
(k19w2c34_tnr <- round(sum(cm_c34_tables[4, 131:140]) / (sum(cm_c34_tables[4, 131:140]) + sum(cm_c34_tables[3, 131:140])), 6))
(k19w2c34_truescore <- round((2 * k19w2c34_tpr * k19w2c34_tnr) / (k19w2c34_tpr + k19w2c34_tnr), 6))

(k19w3c34_tpr <- round(sum(cm_c34_tables[1, 141:150]) / (sum(cm_c34_tables[1, 141:150]) + sum(cm_c34_tables[2, 141:150])), 6))
(k19w3c34_tnr <- round(sum(cm_c34_tables[4, 141:150]) / (sum(cm_c34_tables[4, 141:150]) + sum(cm_c34_tables[3, 141:150])), 6))
(k19w3c34_truescore <- round((2 * k19w3c34_tpr * k19w3c34_tnr) / (k19w3c34_tpr + k19w3c34_tnr), 6))


(k21w1c34_tpr <- round(sum(cm_c34_tables[1, 151:160]) / (sum(cm_c34_tables[1, 151:160]) + sum(cm_c34_tables[2, 151:160])), 6))
(k21w1c34_tnr <- round(sum(cm_c34_tables[4, 151:160]) / (sum(cm_c34_tables[4, 151:160]) + sum(cm_c34_tables[3, 151:160])), 6))
(k21w1c34_truescore <- round((2 * k21w1c34_tpr * k21w1c34_tnr) / (k21w1c34_tpr + k21w1c34_tnr), 6))

(k21w2c34_tpr <- round(sum(cm_c34_tables[1, 161:170]) / (sum(cm_c34_tables[1, 161:170]) + sum(cm_c34_tables[2, 161:170])), 6))
(k21w2c34_tnr <- round(sum(cm_c34_tables[4, 161:170]) / (sum(cm_c34_tables[4, 161:170]) + sum(cm_c34_tables[3, 161:170])), 6))
(k21w2c34_truescore <- round((2 * k21w2c34_tpr * k21w2c34_tnr) / (k21w2c34_tpr + k21w2c34_tnr), 6))

(k21w3c34_tpr <- round(sum(cm_c34_tables[1, 171:180]) / (sum(cm_c34_tables[1, 171:180]) + sum(cm_c34_tables[2, 171:180])), 6))
(k21w3c34_tnr <- round(sum(cm_c34_tables[4, 171:180]) / (sum(cm_c34_tables[4, 171:180]) + sum(cm_c34_tables[3, 171:180])), 6))
(k21w3c34_truescore <- round((2 * k21w3c34_tpr * k21w3c34_tnr) / (k21w3c34_tpr + k21w3c34_tnr), 6))


(k23w1c34_tpr <- round(sum(cm_c34_tables[1, 181:190]) / (sum(cm_c34_tables[1, 181:190]) + sum(cm_c34_tables[2, 181:190])), 6))
(k23w1c34_tnr <- round(sum(cm_c34_tables[4, 181:190]) / (sum(cm_c34_tables[4, 181:190]) + sum(cm_c34_tables[3, 181:190])), 6))
(k23w1c34_truescore <- round((2 * k23w1c34_tpr * k23w1c34_tnr) / (k23w1c34_tpr + k23w1c34_tnr), 6))

(k23w2c34_tpr <- round(sum(cm_c34_tables[1, 191:200]) / (sum(cm_c34_tables[1, 191:200]) + sum(cm_c34_tables[2, 191:200])), 6))
(k23w2c34_tnr <- round(sum(cm_c34_tables[4, 191:200]) / (sum(cm_c34_tables[4, 191:200]) + sum(cm_c34_tables[3, 191:200])), 6))
(k23w2c34_truescore <- round((2 * k23w2c34_tpr * k23w2c34_tnr) / (k23w2c34_tpr + k23w2c34_tnr), 6))

(k23w3c34_tpr <- round(sum(cm_c34_tables[1, 201:210]) / (sum(cm_c34_tables[1, 201:210]) + sum(cm_c34_tables[2, 201:210])), 6))
(k23w3c34_tnr <- round(sum(cm_c34_tables[4, 201:210]) / (sum(cm_c34_tables[4, 201:210]) + sum(cm_c34_tables[3, 201:210])), 6))
(k23w3c34_truescore <- round((2 * k23w3c34_tpr * k23w3c34_tnr) / (k23w3c34_tpr + k23w3c34_tnr), 6))


(k25w1c34_tpr <- round(sum(cm_c34_tables[1, 211:220]) / (sum(cm_c34_tables[1, 211:220]) + sum(cm_c34_tables[2, 211:220])), 6))
(k25w1c34_tnr <- round(sum(cm_c34_tables[4, 211:220]) / (sum(cm_c34_tables[4, 211:220]) + sum(cm_c34_tables[3, 211:220])), 6))
(k25w1c34_truescore <- round((2 * k25w1c34_tpr * k25w1c34_tnr) / (k25w1c34_tpr + k25w1c34_tnr), 6))

(k25w2c34_tpr <- round(sum(cm_c34_tables[1, 221:230]) / (sum(cm_c34_tables[1, 221:230]) + sum(cm_c34_tables[2, 221:230])), 6))
(k25w2c34_tnr <- round(sum(cm_c34_tables[4, 221:230]) / (sum(cm_c34_tables[4, 221:230]) + sum(cm_c34_tables[3, 221:230])), 6))
(k25w2c34_truescore <- round((2 * k25w2c34_tpr * k25w2c34_tnr) / (k25w2c34_tpr + k25w2c34_tnr), 6))

(k25w3c34_tpr <- round(sum(cm_c34_tables[1, 231:240]) / (sum(cm_c34_tables[1, 231:240]) + sum(cm_c34_tables[2, 231:240])), 6))
(k25w3c34_tnr <- round(sum(cm_c34_tables[4, 231:240]) / (sum(cm_c34_tables[4, 231:240]) + sum(cm_c34_tables[3, 231:240])), 6))
(k25w3c34_truescore <- round((2 * k25w3c34_tpr * k25w3c34_tnr) / (k25w3c34_tpr + k25w3c34_tnr), 6))


(k27w1c34_tpr <- round(sum(cm_c34_tables[1, 241:250]) / (sum(cm_c34_tables[1, 241:250]) + sum(cm_c34_tables[2, 241:250])), 6))
(k27w1c34_tnr <- round(sum(cm_c34_tables[4, 241:250]) / (sum(cm_c34_tables[4, 241:250]) + sum(cm_c34_tables[3, 241:250])), 6))
(k27w1c34_truescore <- round((2 * k27w1c34_tpr * k27w1c34_tnr) / (k27w1c34_tpr + k27w1c34_tnr), 6))

(k27w2c34_tpr <- round(sum(cm_c34_tables[1, 251:260]) / (sum(cm_c34_tables[1, 251:260]) + sum(cm_c34_tables[2, 251:260])), 6))
(k27w2c34_tnr <- round(sum(cm_c34_tables[4, 251:260]) / (sum(cm_c34_tables[4, 251:260]) + sum(cm_c34_tables[3, 251:260])), 6))
(k27w2c34_truescore <- round((2 * k27w2c34_tpr * k27w2c34_tnr) / (k27w2c34_tpr + k27w2c34_tnr), 6))

(k27w3c34_tpr <- round(sum(cm_c34_tables[1, 261:270]) / (sum(cm_c34_tables[1, 261:270]) + sum(cm_c34_tables[2, 261:270])), 6))
(k27w3c34_tnr <- round(sum(cm_c34_tables[4, 261:270]) / (sum(cm_c34_tables[4, 261:270]) + sum(cm_c34_tables[3, 261:270])), 6))
(k27w3c34_truescore <- round((2 * k27w3c34_tpr * k27w3c34_tnr) / (k27w3c34_tpr + k27w3c34_tnr), 6))


(k29w1c34_tpr <- round(sum(cm_c34_tables[1, 271:280]) / (sum(cm_c34_tables[1, 271:280]) + sum(cm_c34_tables[2, 271:280])), 6))
(k29w1c34_tnr <- round(sum(cm_c34_tables[4, 271:280]) / (sum(cm_c34_tables[4, 271:280]) + sum(cm_c34_tables[3, 271:280])), 6))
(k29w1c34_truescore <- round((2 * k29w1c34_tpr * k29w1c34_tnr) / (k29w1c34_tpr + k29w1c34_tnr), 6))

(k29w2c34_tpr <- round(sum(cm_c34_tables[1, 281:290]) / (sum(cm_c34_tables[1, 281:290]) + sum(cm_c34_tables[2, 281:290])), 6))
(k29w2c34_tnr <- round(sum(cm_c34_tables[4, 281:290]) / (sum(cm_c34_tables[4, 281:290]) + sum(cm_c34_tables[3, 281:290])), 6))
(k29w2c34_truescore <- round((2 * k29w2c34_tpr * k29w2c34_tnr) / (k29w2c34_tpr + k29w2c34_tnr), 6))

(k29w3c34_tpr <- round(sum(cm_c34_tables[1, 291:300]) / (sum(cm_c34_tables[1, 291:300]) + sum(cm_c34_tables[2, 291:300])), 6))
(k29w3c34_tnr <- round(sum(cm_c34_tables[4, 291:300]) / (sum(cm_c34_tables[4, 291:300]) + sum(cm_c34_tables[3, 291:300])), 6))
(k29w3c34_truescore <- round((2 * k29w3c34_tpr * k29w3c34_tnr) / (k29w3c34_tpr + k29w3c34_tnr), 6))


(k31w1c34_tpr <- round(sum(cm_c34_tables[1, 301:310]) / (sum(cm_c34_tables[1, 301:310]) + sum(cm_c34_tables[2, 301:310])), 6))
(k31w1c34_tnr <- round(sum(cm_c34_tables[4, 301:310]) / (sum(cm_c34_tables[4, 301:310]) + sum(cm_c34_tables[3, 301:310])), 6))
(k31w1c34_truescore <- round((2 * k31w1c34_tpr * k31w1c34_tnr) / (k31w1c34_tpr + k31w1c34_tnr), 6))

(k31w2c34_tpr <- round(sum(cm_c34_tables[1, 311:320]) / (sum(cm_c34_tables[1, 311:320]) + sum(cm_c34_tables[2, 311:320])), 6))
(k31w2c34_tnr <- round(sum(cm_c34_tables[4, 311:320]) / (sum(cm_c34_tables[4, 311:320]) + sum(cm_c34_tables[3, 311:320])), 6))
(k31w2c34_truescore <- round((2 * k31w2c34_tpr * k31w2c34_tnr) / (k31w2c34_tpr + k31w2c34_tnr), 6))

(k31w3c34_tpr <- round(sum(cm_c34_tables[1, 321:330]) / (sum(cm_c34_tables[1, 321:330]) + sum(cm_c34_tables[2, 321:330])), 6))
(k31w3c34_tnr <- round(sum(cm_c34_tables[4, 321:330]) / (sum(cm_c34_tables[4, 321:330]) + sum(cm_c34_tables[3, 321:330])), 6))
(k31w3c34_truescore <- round((2 * k31w3c34_tpr * k31w3c34_tnr) / (k31w3c34_tpr + k31w3c34_tnr), 6))


(k33w1c34_tpr <- round(sum(cm_c34_tables[1, 331:340]) / (sum(cm_c34_tables[1, 331:340]) + sum(cm_c34_tables[2, 331:340])), 6))
(k33w1c34_tnr <- round(sum(cm_c34_tables[4, 331:340]) / (sum(cm_c34_tables[4, 331:340]) + sum(cm_c34_tables[3, 331:340])), 6))
(k33w1c34_truescore <- round((2 * k33w1c34_tpr * k33w1c34_tnr) / (k33w1c34_tpr + k33w1c34_tnr), 6))

(k33w2c34_tpr <- round(sum(cm_c34_tables[1, 341:350]) / (sum(cm_c34_tables[1, 341:350]) + sum(cm_c34_tables[2, 341:350])), 6))
(k33w2c34_tnr <- round(sum(cm_c34_tables[4, 341:350]) / (sum(cm_c34_tables[4, 341:350]) + sum(cm_c34_tables[3, 341:350])), 6))
(k33w2c34_truescore <- round((2 * k33w2c34_tpr * k33w2c34_tnr) / (k33w2c34_tpr + k33w2c34_tnr), 6))

(k33w3c34_tpr <- round(sum(cm_c34_tables[1, 351:360]) / (sum(cm_c34_tables[1, 351:360]) + sum(cm_c34_tables[2, 351:360])), 6))
(k33w3c34_tnr <- round(sum(cm_c34_tables[4, 351:360]) / (sum(cm_c34_tables[4, 351:360]) + sum(cm_c34_tables[3, 351:360])), 6))
(k33w3c34_truescore <- round((2 * k33w3c34_tpr * k33w3c34_tnr) / (k33w3c34_tpr + k33w3c34_tnr), 6))


(k35w1c34_tpr <- round(sum(cm_c34_tables[1, 361:370]) / (sum(cm_c34_tables[1, 361:370]) + sum(cm_c34_tables[2, 361:370])), 6))
(k35w1c34_tnr <- round(sum(cm_c34_tables[4, 361:370]) / (sum(cm_c34_tables[4, 361:370]) + sum(cm_c34_tables[3, 361:370])), 6))
(k35w1c34_truescore <- round((2 * k35w1c34_tpr * k35w1c34_tnr) / (k35w1c34_tpr + k35w1c34_tnr), 6))

(k35w2c34_tpr <- round(sum(cm_c34_tables[1, 371:380]) / (sum(cm_c34_tables[1, 371:380]) + sum(cm_c34_tables[2, 371:380])), 6))
(k35w2c34_tnr <- round(sum(cm_c34_tables[4, 371:380]) / (sum(cm_c34_tables[4, 371:380]) + sum(cm_c34_tables[3, 371:380])), 6))
(k35w2c34_truescore <- round((2 * k35w2c34_tpr * k35w2c34_tnr) / (k35w2c34_tpr + k35w2c34_tnr), 6))

(k35w3c34_tpr <- round(sum(cm_c34_tables[1, 381:390]) / (sum(cm_c34_tables[1, 381:390]) + sum(cm_c34_tables[2, 381:390])), 6))
(k35w3c34_tnr <- round(sum(cm_c34_tables[4, 381:390]) / (sum(cm_c34_tables[4, 381:390]) + sum(cm_c34_tables[3, 381:390])), 6))
(k35w3c34_truescore <- round((2 * k35w3c34_tpr * k35w3c34_tnr) / (k35w3c34_tpr + k35w3c34_tnr), 6))


(k37w1c34_tpr <- round(sum(cm_c34_tables[1, 391:400]) / (sum(cm_c34_tables[1, 391:400]) + sum(cm_c34_tables[2, 391:400])), 6))
(k37w1c34_tnr <- round(sum(cm_c34_tables[4, 391:400]) / (sum(cm_c34_tables[4, 391:400]) + sum(cm_c34_tables[3, 391:400])), 6))
(k37w1c34_truescore <- round((2 * k37w1c34_tpr * k37w1c34_tnr) / (k37w1c34_tpr + k37w1c34_tnr), 6))

(k37w2c34_tpr <- round(sum(cm_c34_tables[1, 401:410]) / (sum(cm_c34_tables[1, 401:410]) + sum(cm_c34_tables[2, 401:410])), 6))
(k37w2c34_tnr <- round(sum(cm_c34_tables[4, 401:410]) / (sum(cm_c34_tables[4, 401:410]) + sum(cm_c34_tables[3, 401:410])), 6))
(k37w2c34_truescore <- round((2 * k37w2c34_tpr * k37w2c34_tnr) / (k37w2c34_tpr + k37w2c34_tnr), 6))

(k37w3c34_tpr <- round(sum(cm_c34_tables[1, 411:420]) / (sum(cm_c34_tables[1, 411:420]) + sum(cm_c34_tables[2, 411:420])), 6))
(k37w3c34_tnr <- round(sum(cm_c34_tables[4, 411:420]) / (sum(cm_c34_tables[4, 411:420]) + sum(cm_c34_tables[3, 411:420])), 6))
(k37w3c34_truescore <- round((2 * k37w3c34_tpr * k37w3c34_tnr) / (k37w3c34_tpr + k37w3c34_tnr), 6))


(k39w1c34_tpr <- round(sum(cm_c34_tables[1, 421:430]) / (sum(cm_c34_tables[1, 421:430]) + sum(cm_c34_tables[2, 421:430])), 6))
(k39w1c34_tnr <- round(sum(cm_c34_tables[4, 421:430]) / (sum(cm_c34_tables[4, 421:430]) + sum(cm_c34_tables[3, 421:430])), 6))
(k39w1c34_truescore <- round((2 * k39w1c34_tpr * k39w1c34_tnr) / (k39w1c34_tpr + k39w1c34_tnr), 6))

(k39w2c34_tpr <- round(sum(cm_c34_tables[1, 431:440]) / (sum(cm_c34_tables[1, 431:440]) + sum(cm_c34_tables[2, 431:440])), 6))
(k39w2c34_tnr <- round(sum(cm_c34_tables[4, 431:440]) / (sum(cm_c34_tables[4, 431:440]) + sum(cm_c34_tables[3, 431:440])), 6))
(k39w2c34_truescore <- round((2 * k39w2c34_tpr * k39w2c34_tnr) / (k39w2c34_tpr + k39w2c34_tnr), 6))

(k39w3c34_tpr <- round(sum(cm_c34_tables[1, 441:450]) / (sum(cm_c34_tables[1, 441:450]) + sum(cm_c34_tables[2, 441:450])), 6))
(k39w3c34_tnr <- round(sum(cm_c34_tables[4, 441:450]) / (sum(cm_c34_tables[4, 441:450]) + sum(cm_c34_tables[3, 441:450])), 6))
(k39w3c34_truescore <- round((2 * k39w3c34_tpr * k39w3c34_tnr) / (k39w3c34_tpr + k39w3c34_tnr), 6))


# Compile the 0.34 cutoff results in a table, and identify the combination of 
# of k and kernel that maximizes the Truescore.

c34_results <- tibble(k = c(11, 11, 11, 13, 13, 13, 15, 15, 15, 
                            17, 17, 17, 19, 19, 19, 21, 21, 21, 
                            23, 23, 23, 25, 25, 25, 27, 27, 27, 
                            29, 29, 29, 31, 31, 31, 33, 33, 33, 
                            35, 35, 35, 37, 37, 37, 39, 39, 39), 
                      Kernel = rep(c("triangular", "gaussian", "optimal"), 15), 
                      Cut = 0.34, 
                      TPR = c(k11w1c34_tpr, k11w2c34_tpr, k11w3c34_tpr, k13w1c34_tpr, 
                              k13w2c34_tpr, k13w3c34_tpr, k15w1c34_tpr, k15w2c34_tpr, 
                              k15w3c34_tpr, k17w1c34_tpr, k17w2c34_tpr, k17w3c34_tpr, 
                              k19w1c34_tpr, k19w2c34_tpr, k19w3c34_tpr, k21w1c34_tpr, 
                              k21w2c34_tpr, k21w3c34_tpr, k23w1c34_tpr, k23w2c34_tpr, 
                              k23w3c34_tpr, k25w1c34_tpr, k25w2c34_tpr, k25w3c34_tpr, 
                              k27w1c34_tpr, k27w2c34_tpr, k27w3c34_tpr, k29w1c34_tpr, 
                              k29w2c34_tpr, k29w3c34_tpr, k31w1c34_tpr, k31w2c34_tpr, 
                              k31w3c34_tpr, k33w1c34_tpr, k33w2c34_tpr, k33w3c34_tpr, 
                              k35w1c34_tpr, k35w2c34_tpr, k35w3c34_tpr, k37w1c34_tpr, 
                              k37w2c34_tpr, k37w3c34_tpr, k39w1c34_tpr, k39w2c34_tpr, 
                              k39w3c34_tpr), 
                      TNR = c(k11w1c34_tnr, k11w2c34_tnr, k11w3c34_tnr, k13w1c34_tnr, 
                              k13w2c34_tnr, k13w3c34_tnr, k15w1c34_tnr, k15w2c34_tnr, 
                              k15w3c34_tnr, k17w1c34_tnr, k17w2c34_tnr, k17w3c34_tnr, 
                              k19w1c34_tnr, k19w2c34_tnr, k19w3c34_tnr, k21w1c34_tnr, 
                              k21w2c34_tnr, k21w3c34_tnr, k23w1c34_tnr, k23w2c34_tnr, 
                              k23w3c34_tnr, k25w1c34_tnr, k25w2c34_tnr, k25w3c34_tnr, 
                              k27w1c34_tnr, k27w2c34_tnr, k27w3c34_tnr, k29w1c34_tnr, 
                              k29w2c34_tnr, k29w3c34_tnr, k31w1c34_tnr, k31w2c34_tnr, 
                              k31w3c34_tnr, k33w1c34_tnr, k33w2c34_tnr, k33w3c34_tnr, 
                              k35w1c34_tnr, k35w2c34_tnr, k35w3c34_tnr, k37w1c34_tnr, 
                              k37w2c34_tnr, k37w3c34_tnr, k39w1c34_tnr, k39w2c34_tnr, 
                              k39w3c34_tnr), 
                      Truescore = c(k11w1c34_truescore, k11w2c34_truescore, 
                                    k11w3c34_truescore, k13w1c34_truescore, 
                                    k13w2c34_truescore, k13w3c34_truescore, 
                                    k15w1c34_truescore, k15w2c34_truescore, 
                                    k15w3c34_truescore, k17w1c34_truescore, 
                                    k17w2c34_truescore, k17w3c34_truescore, 
                                    k19w1c34_truescore, k19w2c34_truescore, 
                                    k19w3c34_truescore, k21w1c34_truescore, 
                                    k21w2c34_truescore, k21w3c34_truescore, 
                                    k23w1c34_truescore, k23w2c34_truescore, 
                                    k23w3c34_truescore, k25w1c34_truescore, 
                                    k25w2c34_truescore, k25w3c34_truescore, 
                                    k27w1c34_truescore, k27w2c34_truescore, 
                                    k27w3c34_truescore, k29w1c34_truescore, 
                                    k29w2c34_truescore, k29w3c34_truescore, 
                                    k31w1c34_truescore, k31w2c34_truescore, 
                                    k31w3c34_truescore, k33w1c34_truescore, 
                                    k33w2c34_truescore, k33w3c34_truescore, 
                                    k35w1c34_truescore, k35w2c34_truescore, 
                                    k35w3c34_truescore, k37w1c34_truescore, 
                                    k37w2c34_truescore, k37w3c34_truescore, 
                                    k39w1c34_truescore, k39w2c34_truescore, 
                                    k39w3c34_truescore))

knitr::kable(c34_results[1:45, ], caption = "c34_results")

ggplot(c34_results, aes(k, Truescore, shape = Kernel, color = Kernel)) + 
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.34 (Truescore)")

# For the Cutoff of 0.34, compute the Minimum Distance to (0, 1) for each 
# combination of k and Kernel. 

c34_results <- c34_results %>% mutate(Distance = sqrt((1 - TPR)^2 + (1 - TNR)^2))

ggplot(c34_results, aes(k, Distance, shape = Kernel, color = Kernel)) +
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.34 (Distance)")

# For the Cutoff of 0.34, identify the optimal combination of values for k and Kernel 
# based on each of our assessment methods, Truescore and Mimimum Distance to (0, 1).

max(c34_results$Truescore)
(c34_opt_k_ts <- c34_results$k[which.max(c34_results$Truescore)])
(c34_opt_kernel_ts <- c34_results$Kernel[which.max(c34_results$Truescore)])
(c34_opt_cut_ts <- c34_results$Cut[which.max(c34_results$Truescore)])
(c34_opt_tpr_ts <- c34_results$TPR[which.max(c34_results$Truescore)])
(c34_opt_tnr_ts <- c34_results$TNR[which.max(c34_results$Truescore)])
(c34_opt_d_ts <- c34_results$Distance[which.max(c34_results$Truescore)])

min(c34_results$Distance)
(c34_opt_k_dist <- c34_results$k[which.min(c34_results$Distance)])
(c34_opt_kernel_dist <- c34_results$Kernel[which.min(c34_results$Distance)])
(c34_opt_cut_dist <- c34_results$Cut[which.min(c34_results$Distance)])  
(c34_opt_tpr_dist <- c34_results$TPR[which.min(c34_results$Distance)])
(c34_opt_tnr_dist <- c34_results$TNR[which.min(c34_results$Distance)])
(c34_opt_t_dist <- c34_results$Truescore[which.min(c34_results$Distance)])

############################
# 0.35 Cutoff
############################

# For the decision cutoff of 0.35, generate a confusion matrix for
# every combination of k (11:39 by two), kernel (1:3) and fold (1:10).

cm_c35 <- sapply(kwf_dfs_l, function(x) {
  ss <- subset(x, select = c(pred35, obs))
  confusionMatrix(ss$pred35, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c35) <- kwf_dfs_v

cm_c35_tables <- sapply(cm_c35, "[[", 2)
cm_c35_tables <- as_tibble(cm_c35_tables)

# For each combination of k and kernel, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each combination of k and kernel when the decision cutoff is 0.35.

(k11w1c35_tpr <- round(sum(cm_c35_tables[1, 1:10]) / (sum(cm_c35_tables[1, 1:10]) + sum(cm_c35_tables[2, 1:10])), 6))
(k11w1c35_tnr <- round(sum(cm_c35_tables[4, 1:10]) / (sum(cm_c35_tables[4, 1:10]) + sum(cm_c35_tables[3, 1:10])), 6))
(k11w1c35_truescore <- round((2 * k11w1c35_tpr * k11w1c35_tnr) / (k11w1c35_tpr + k11w1c35_tnr), 6))

(k11w2c35_tpr <- round(sum(cm_c35_tables[1, 11:20]) / (sum(cm_c35_tables[1, 11:20]) + sum(cm_c35_tables[2, 11:20])), 6))
(k11w2c35_tnr <- round(sum(cm_c35_tables[4, 11:20]) / (sum(cm_c35_tables[4, 11:20]) + sum(cm_c35_tables[3, 11:20])), 6))
(k11w2c35_truescore <- round((2 * k11w2c35_tpr * k11w2c35_tnr) / (k11w2c35_tpr + k11w2c35_tnr), 6))

(k11w3c35_tpr <- round(sum(cm_c35_tables[1, 21:30]) / (sum(cm_c35_tables[1, 21:30]) + sum(cm_c35_tables[2, 21:30])), 6))
(k11w3c35_tnr <- round(sum(cm_c35_tables[4, 21:30]) / (sum(cm_c35_tables[4, 21:30]) + sum(cm_c35_tables[3, 21:30])), 6))
(k11w3c35_truescore <- round((2 * k11w3c35_tpr * k11w3c35_tnr) / (k11w3c35_tpr + k11w3c35_tnr), 6))


(k13w1c35_tpr <- round(sum(cm_c35_tables[1, 31:40]) / (sum(cm_c35_tables[1, 31:40]) + sum(cm_c35_tables[2, 31:40])), 6))
(k13w1c35_tnr <- round(sum(cm_c35_tables[4, 31:40]) / (sum(cm_c35_tables[4, 31:40]) + sum(cm_c35_tables[3, 31:40])), 6))
(k13w1c35_truescore <- round((2 * k13w1c35_tpr * k13w1c35_tnr) / (k13w1c35_tpr + k13w1c35_tnr), 6))

(k13w2c35_tpr <- round(sum(cm_c35_tables[1, 41:50]) / (sum(cm_c35_tables[1, 41:50]) + sum(cm_c35_tables[2, 41:50])), 6))
(k13w2c35_tnr <- round(sum(cm_c35_tables[4, 41:50]) / (sum(cm_c35_tables[4, 41:50]) + sum(cm_c35_tables[3, 41:50])), 6))
(k13w2c35_truescore <- round((2 * k13w2c35_tpr * k13w2c35_tnr) / (k13w2c35_tpr + k13w2c35_tnr), 6))

(k13w3c35_tpr <- round(sum(cm_c35_tables[1, 51:60]) / (sum(cm_c35_tables[1, 51:60]) + sum(cm_c35_tables[2, 51:60])), 6))
(k13w3c35_tnr <- round(sum(cm_c35_tables[4, 51:60]) / (sum(cm_c35_tables[4, 51:60]) + sum(cm_c35_tables[3, 51:60])), 6))
(k13w3c35_truescore <- round((2 * k13w3c35_tpr * k13w3c35_tnr) / (k13w3c35_tpr + k13w3c35_tnr), 6))


(k15w1c35_tpr <- round(sum(cm_c35_tables[1, 61:70]) / (sum(cm_c35_tables[1, 61:70]) + sum(cm_c35_tables[2, 61:70])), 6))
(k15w1c35_tnr <- round(sum(cm_c35_tables[4, 61:70]) / (sum(cm_c35_tables[4, 61:70]) + sum(cm_c35_tables[3, 61:70])), 6))
(k15w1c35_truescore <- round((2 * k15w1c35_tpr * k15w1c35_tnr) / (k15w1c35_tpr + k15w1c35_tnr), 6))

(k15w2c35_tpr <- round(sum(cm_c35_tables[1, 71:80]) / (sum(cm_c35_tables[1, 71:80]) + sum(cm_c35_tables[2, 71:80])), 6))
(k15w2c35_tnr <- round(sum(cm_c35_tables[4, 71:80]) / (sum(cm_c35_tables[4, 71:80]) + sum(cm_c35_tables[3, 71:80])), 6))
(k15w2c35_truescore <- round((2 * k15w2c35_tpr * k15w2c35_tnr) / (k15w2c35_tpr + k15w2c35_tnr), 6))

(k15w3c35_tpr <- round(sum(cm_c35_tables[1, 81:90]) / (sum(cm_c35_tables[1, 81:90]) + sum(cm_c35_tables[2, 81:90])), 6))
(k15w3c35_tnr <- round(sum(cm_c35_tables[4, 81:90]) / (sum(cm_c35_tables[4, 81:90]) + sum(cm_c35_tables[3, 81:90])), 6))
(k15w3c35_truescore <- round((2 * k15w3c35_tpr * k15w3c35_tnr) / (k15w3c35_tpr + k15w3c35_tnr), 6))


(k17w1c35_tpr <- round(sum(cm_c35_tables[1, 91:100]) / (sum(cm_c35_tables[1, 91:100]) + sum(cm_c35_tables[2, 91:100])), 6))
(k17w1c35_tnr <- round(sum(cm_c35_tables[4, 91:100]) / (sum(cm_c35_tables[4, 91:100]) + sum(cm_c35_tables[3, 91:100])), 6))
(k17w1c35_truescore <- round((2 * k17w1c35_tpr * k17w1c35_tnr) / (k17w1c35_tpr + k17w1c35_tnr), 6))

(k17w2c35_tpr <- round(sum(cm_c35_tables[1, 101:110]) / (sum(cm_c35_tables[1, 101:110]) + sum(cm_c35_tables[2, 101:110])), 6))
(k17w2c35_tnr <- round(sum(cm_c35_tables[4, 101:110]) / (sum(cm_c35_tables[4, 101:110]) + sum(cm_c35_tables[3, 101:110])), 6))
(k17w2c35_truescore <- round((2 * k17w2c35_tpr * k17w2c35_tnr) / (k17w2c35_tpr + k17w2c35_tnr), 6))

(k17w3c35_tpr <- round(sum(cm_c35_tables[1, 111:120]) / (sum(cm_c35_tables[1, 111:120]) + sum(cm_c35_tables[2, 111:120])), 6))
(k17w3c35_tnr <- round(sum(cm_c35_tables[4, 111:120]) / (sum(cm_c35_tables[4, 111:120]) + sum(cm_c35_tables[3, 111:120])), 6))
(k17w3c35_truescore <- round((2 * k17w3c35_tpr * k17w3c35_tnr) / (k17w3c35_tpr + k17w3c35_tnr), 6))


(k19w1c35_tpr <- round(sum(cm_c35_tables[1, 121:130]) / (sum(cm_c35_tables[1, 121:130]) + sum(cm_c35_tables[2, 121:130])), 6))
(k19w1c35_tnr <- round(sum(cm_c35_tables[4, 121:130]) / (sum(cm_c35_tables[4, 121:130]) + sum(cm_c35_tables[3, 121:130])), 6))
(k19w1c35_truescore <- round((2 * k19w1c35_tpr * k19w1c35_tnr) / (k19w1c35_tpr + k19w1c35_tnr), 6))

(k19w2c35_tpr <- round(sum(cm_c35_tables[1, 131:140]) / (sum(cm_c35_tables[1, 131:140]) + sum(cm_c35_tables[2, 131:140])), 6))
(k19w2c35_tnr <- round(sum(cm_c35_tables[4, 131:140]) / (sum(cm_c35_tables[4, 131:140]) + sum(cm_c35_tables[3, 131:140])), 6))
(k19w2c35_truescore <- round((2 * k19w2c35_tpr * k19w2c35_tnr) / (k19w2c35_tpr + k19w2c35_tnr), 6))

(k19w3c35_tpr <- round(sum(cm_c35_tables[1, 141:150]) / (sum(cm_c35_tables[1, 141:150]) + sum(cm_c35_tables[2, 141:150])), 6))
(k19w3c35_tnr <- round(sum(cm_c35_tables[4, 141:150]) / (sum(cm_c35_tables[4, 141:150]) + sum(cm_c35_tables[3, 141:150])), 6))
(k19w3c35_truescore <- round((2 * k19w3c35_tpr * k19w3c35_tnr) / (k19w3c35_tpr + k19w3c35_tnr), 6))


(k21w1c35_tpr <- round(sum(cm_c35_tables[1, 151:160]) / (sum(cm_c35_tables[1, 151:160]) + sum(cm_c35_tables[2, 151:160])), 6))
(k21w1c35_tnr <- round(sum(cm_c35_tables[4, 151:160]) / (sum(cm_c35_tables[4, 151:160]) + sum(cm_c35_tables[3, 151:160])), 6))
(k21w1c35_truescore <- round((2 * k21w1c35_tpr * k21w1c35_tnr) / (k21w1c35_tpr + k21w1c35_tnr), 6))

(k21w2c35_tpr <- round(sum(cm_c35_tables[1, 161:170]) / (sum(cm_c35_tables[1, 161:170]) + sum(cm_c35_tables[2, 161:170])), 6))
(k21w2c35_tnr <- round(sum(cm_c35_tables[4, 161:170]) / (sum(cm_c35_tables[4, 161:170]) + sum(cm_c35_tables[3, 161:170])), 6))
(k21w2c35_truescore <- round((2 * k21w2c35_tpr * k21w2c35_tnr) / (k21w2c35_tpr + k21w2c35_tnr), 6))

(k21w3c35_tpr <- round(sum(cm_c35_tables[1, 171:180]) / (sum(cm_c35_tables[1, 171:180]) + sum(cm_c35_tables[2, 171:180])), 6))
(k21w3c35_tnr <- round(sum(cm_c35_tables[4, 171:180]) / (sum(cm_c35_tables[4, 171:180]) + sum(cm_c35_tables[3, 171:180])), 6))
(k21w3c35_truescore <- round((2 * k21w3c35_tpr * k21w3c35_tnr) / (k21w3c35_tpr + k21w3c35_tnr), 6))


(k23w1c35_tpr <- round(sum(cm_c35_tables[1, 181:190]) / (sum(cm_c35_tables[1, 181:190]) + sum(cm_c35_tables[2, 181:190])), 6))
(k23w1c35_tnr <- round(sum(cm_c35_tables[4, 181:190]) / (sum(cm_c35_tables[4, 181:190]) + sum(cm_c35_tables[3, 181:190])), 6))
(k23w1c35_truescore <- round((2 * k23w1c35_tpr * k23w1c35_tnr) / (k23w1c35_tpr + k23w1c35_tnr), 6))

(k23w2c35_tpr <- round(sum(cm_c35_tables[1, 191:200]) / (sum(cm_c35_tables[1, 191:200]) + sum(cm_c35_tables[2, 191:200])), 6))
(k23w2c35_tnr <- round(sum(cm_c35_tables[4, 191:200]) / (sum(cm_c35_tables[4, 191:200]) + sum(cm_c35_tables[3, 191:200])), 6))
(k23w2c35_truescore <- round((2 * k23w2c35_tpr * k23w2c35_tnr) / (k23w2c35_tpr + k23w2c35_tnr), 6))

(k23w3c35_tpr <- round(sum(cm_c35_tables[1, 201:210]) / (sum(cm_c35_tables[1, 201:210]) + sum(cm_c35_tables[2, 201:210])), 6))
(k23w3c35_tnr <- round(sum(cm_c35_tables[4, 201:210]) / (sum(cm_c35_tables[4, 201:210]) + sum(cm_c35_tables[3, 201:210])), 6))
(k23w3c35_truescore <- round((2 * k23w3c35_tpr * k23w3c35_tnr) / (k23w3c35_tpr + k23w3c35_tnr), 6))


(k25w1c35_tpr <- round(sum(cm_c35_tables[1, 211:220]) / (sum(cm_c35_tables[1, 211:220]) + sum(cm_c35_tables[2, 211:220])), 6))
(k25w1c35_tnr <- round(sum(cm_c35_tables[4, 211:220]) / (sum(cm_c35_tables[4, 211:220]) + sum(cm_c35_tables[3, 211:220])), 6))
(k25w1c35_truescore <- round((2 * k25w1c35_tpr * k25w1c35_tnr) / (k25w1c35_tpr + k25w1c35_tnr), 6))

(k25w2c35_tpr <- round(sum(cm_c35_tables[1, 221:230]) / (sum(cm_c35_tables[1, 221:230]) + sum(cm_c35_tables[2, 221:230])), 6))
(k25w2c35_tnr <- round(sum(cm_c35_tables[4, 221:230]) / (sum(cm_c35_tables[4, 221:230]) + sum(cm_c35_tables[3, 221:230])), 6))
(k25w2c35_truescore <- round((2 * k25w2c35_tpr * k25w2c35_tnr) / (k25w2c35_tpr + k25w2c35_tnr), 6))

(k25w3c35_tpr <- round(sum(cm_c35_tables[1, 231:240]) / (sum(cm_c35_tables[1, 231:240]) + sum(cm_c35_tables[2, 231:240])), 6))
(k25w3c35_tnr <- round(sum(cm_c35_tables[4, 231:240]) / (sum(cm_c35_tables[4, 231:240]) + sum(cm_c35_tables[3, 231:240])), 6))
(k25w3c35_truescore <- round((2 * k25w3c35_tpr * k25w3c35_tnr) / (k25w3c35_tpr + k25w3c35_tnr), 6))


(k27w1c35_tpr <- round(sum(cm_c35_tables[1, 241:250]) / (sum(cm_c35_tables[1, 241:250]) + sum(cm_c35_tables[2, 241:250])), 6))
(k27w1c35_tnr <- round(sum(cm_c35_tables[4, 241:250]) / (sum(cm_c35_tables[4, 241:250]) + sum(cm_c35_tables[3, 241:250])), 6))
(k27w1c35_truescore <- round((2 * k27w1c35_tpr * k27w1c35_tnr) / (k27w1c35_tpr + k27w1c35_tnr), 6))

(k27w2c35_tpr <- round(sum(cm_c35_tables[1, 251:260]) / (sum(cm_c35_tables[1, 251:260]) + sum(cm_c35_tables[2, 251:260])), 6))
(k27w2c35_tnr <- round(sum(cm_c35_tables[4, 251:260]) / (sum(cm_c35_tables[4, 251:260]) + sum(cm_c35_tables[3, 251:260])), 6))
(k27w2c35_truescore <- round((2 * k27w2c35_tpr * k27w2c35_tnr) / (k27w2c35_tpr + k27w2c35_tnr), 6))

(k27w3c35_tpr <- round(sum(cm_c35_tables[1, 261:270]) / (sum(cm_c35_tables[1, 261:270]) + sum(cm_c35_tables[2, 261:270])), 6))
(k27w3c35_tnr <- round(sum(cm_c35_tables[4, 261:270]) / (sum(cm_c35_tables[4, 261:270]) + sum(cm_c35_tables[3, 261:270])), 6))
(k27w3c35_truescore <- round((2 * k27w3c35_tpr * k27w3c35_tnr) / (k27w3c35_tpr + k27w3c35_tnr), 6))


(k29w1c35_tpr <- round(sum(cm_c35_tables[1, 271:280]) / (sum(cm_c35_tables[1, 271:280]) + sum(cm_c35_tables[2, 271:280])), 6))
(k29w1c35_tnr <- round(sum(cm_c35_tables[4, 271:280]) / (sum(cm_c35_tables[4, 271:280]) + sum(cm_c35_tables[3, 271:280])), 6))
(k29w1c35_truescore <- round((2 * k29w1c35_tpr * k29w1c35_tnr) / (k29w1c35_tpr + k29w1c35_tnr), 6))

(k29w2c35_tpr <- round(sum(cm_c35_tables[1, 281:290]) / (sum(cm_c35_tables[1, 281:290]) + sum(cm_c35_tables[2, 281:290])), 6))
(k29w2c35_tnr <- round(sum(cm_c35_tables[4, 281:290]) / (sum(cm_c35_tables[4, 281:290]) + sum(cm_c35_tables[3, 281:290])), 6))
(k29w2c35_truescore <- round((2 * k29w2c35_tpr * k29w2c35_tnr) / (k29w2c35_tpr + k29w2c35_tnr), 6))

(k29w3c35_tpr <- round(sum(cm_c35_tables[1, 291:300]) / (sum(cm_c35_tables[1, 291:300]) + sum(cm_c35_tables[2, 291:300])), 6))
(k29w3c35_tnr <- round(sum(cm_c35_tables[4, 291:300]) / (sum(cm_c35_tables[4, 291:300]) + sum(cm_c35_tables[3, 291:300])), 6))
(k29w3c35_truescore <- round((2 * k29w3c35_tpr * k29w3c35_tnr) / (k29w3c35_tpr + k29w3c35_tnr), 6))


(k31w1c35_tpr <- round(sum(cm_c35_tables[1, 301:310]) / (sum(cm_c35_tables[1, 301:310]) + sum(cm_c35_tables[2, 301:310])), 6))
(k31w1c35_tnr <- round(sum(cm_c35_tables[4, 301:310]) / (sum(cm_c35_tables[4, 301:310]) + sum(cm_c35_tables[3, 301:310])), 6))
(k31w1c35_truescore <- round((2 * k31w1c35_tpr * k31w1c35_tnr) / (k31w1c35_tpr + k31w1c35_tnr), 6))

(k31w2c35_tpr <- round(sum(cm_c35_tables[1, 311:320]) / (sum(cm_c35_tables[1, 311:320]) + sum(cm_c35_tables[2, 311:320])), 6))
(k31w2c35_tnr <- round(sum(cm_c35_tables[4, 311:320]) / (sum(cm_c35_tables[4, 311:320]) + sum(cm_c35_tables[3, 311:320])), 6))
(k31w2c35_truescore <- round((2 * k31w2c35_tpr * k31w2c35_tnr) / (k31w2c35_tpr + k31w2c35_tnr), 6))

(k31w3c35_tpr <- round(sum(cm_c35_tables[1, 321:330]) / (sum(cm_c35_tables[1, 321:330]) + sum(cm_c35_tables[2, 321:330])), 6))
(k31w3c35_tnr <- round(sum(cm_c35_tables[4, 321:330]) / (sum(cm_c35_tables[4, 321:330]) + sum(cm_c35_tables[3, 321:330])), 6))
(k31w3c35_truescore <- round((2 * k31w3c35_tpr * k31w3c35_tnr) / (k31w3c35_tpr + k31w3c35_tnr), 6))


(k33w1c35_tpr <- round(sum(cm_c35_tables[1, 331:340]) / (sum(cm_c35_tables[1, 331:340]) + sum(cm_c35_tables[2, 331:340])), 6))
(k33w1c35_tnr <- round(sum(cm_c35_tables[4, 331:340]) / (sum(cm_c35_tables[4, 331:340]) + sum(cm_c35_tables[3, 331:340])), 6))
(k33w1c35_truescore <- round((2 * k33w1c35_tpr * k33w1c35_tnr) / (k33w1c35_tpr + k33w1c35_tnr), 6))

(k33w2c35_tpr <- round(sum(cm_c35_tables[1, 341:350]) / (sum(cm_c35_tables[1, 341:350]) + sum(cm_c35_tables[2, 341:350])), 6))
(k33w2c35_tnr <- round(sum(cm_c35_tables[4, 341:350]) / (sum(cm_c35_tables[4, 341:350]) + sum(cm_c35_tables[3, 341:350])), 6))
(k33w2c35_truescore <- round((2 * k33w2c35_tpr * k33w2c35_tnr) / (k33w2c35_tpr + k33w2c35_tnr), 6))

(k33w3c35_tpr <- round(sum(cm_c35_tables[1, 351:360]) / (sum(cm_c35_tables[1, 351:360]) + sum(cm_c35_tables[2, 351:360])), 6))
(k33w3c35_tnr <- round(sum(cm_c35_tables[4, 351:360]) / (sum(cm_c35_tables[4, 351:360]) + sum(cm_c35_tables[3, 351:360])), 6))
(k33w3c35_truescore <- round((2 * k33w3c35_tpr * k33w3c35_tnr) / (k33w3c35_tpr + k33w3c35_tnr), 6))


(k35w1c35_tpr <- round(sum(cm_c35_tables[1, 361:370]) / (sum(cm_c35_tables[1, 361:370]) + sum(cm_c35_tables[2, 361:370])), 6))
(k35w1c35_tnr <- round(sum(cm_c35_tables[4, 361:370]) / (sum(cm_c35_tables[4, 361:370]) + sum(cm_c35_tables[3, 361:370])), 6))
(k35w1c35_truescore <- round((2 * k35w1c35_tpr * k35w1c35_tnr) / (k35w1c35_tpr + k35w1c35_tnr), 6))

(k35w2c35_tpr <- round(sum(cm_c35_tables[1, 371:380]) / (sum(cm_c35_tables[1, 371:380]) + sum(cm_c35_tables[2, 371:380])), 6))
(k35w2c35_tnr <- round(sum(cm_c35_tables[4, 371:380]) / (sum(cm_c35_tables[4, 371:380]) + sum(cm_c35_tables[3, 371:380])), 6))
(k35w2c35_truescore <- round((2 * k35w2c35_tpr * k35w2c35_tnr) / (k35w2c35_tpr + k35w2c35_tnr), 6))

(k35w3c35_tpr <- round(sum(cm_c35_tables[1, 381:390]) / (sum(cm_c35_tables[1, 381:390]) + sum(cm_c35_tables[2, 381:390])), 6))
(k35w3c35_tnr <- round(sum(cm_c35_tables[4, 381:390]) / (sum(cm_c35_tables[4, 381:390]) + sum(cm_c35_tables[3, 381:390])), 6))
(k35w3c35_truescore <- round((2 * k35w3c35_tpr * k35w3c35_tnr) / (k35w3c35_tpr + k35w3c35_tnr), 6))


(k37w1c35_tpr <- round(sum(cm_c35_tables[1, 391:400]) / (sum(cm_c35_tables[1, 391:400]) + sum(cm_c35_tables[2, 391:400])), 6))
(k37w1c35_tnr <- round(sum(cm_c35_tables[4, 391:400]) / (sum(cm_c35_tables[4, 391:400]) + sum(cm_c35_tables[3, 391:400])), 6))
(k37w1c35_truescore <- round((2 * k37w1c35_tpr * k37w1c35_tnr) / (k37w1c35_tpr + k37w1c35_tnr), 6))

(k37w2c35_tpr <- round(sum(cm_c35_tables[1, 401:410]) / (sum(cm_c35_tables[1, 401:410]) + sum(cm_c35_tables[2, 401:410])), 6))
(k37w2c35_tnr <- round(sum(cm_c35_tables[4, 401:410]) / (sum(cm_c35_tables[4, 401:410]) + sum(cm_c35_tables[3, 401:410])), 6))
(k37w2c35_truescore <- round((2 * k37w2c35_tpr * k37w2c35_tnr) / (k37w2c35_tpr + k37w2c35_tnr), 6))

(k37w3c35_tpr <- round(sum(cm_c35_tables[1, 411:420]) / (sum(cm_c35_tables[1, 411:420]) + sum(cm_c35_tables[2, 411:420])), 6))
(k37w3c35_tnr <- round(sum(cm_c35_tables[4, 411:420]) / (sum(cm_c35_tables[4, 411:420]) + sum(cm_c35_tables[3, 411:420])), 6))
(k37w3c35_truescore <- round((2 * k37w3c35_tpr * k37w3c35_tnr) / (k37w3c35_tpr + k37w3c35_tnr), 6))


(k39w1c35_tpr <- round(sum(cm_c35_tables[1, 421:430]) / (sum(cm_c35_tables[1, 421:430]) + sum(cm_c35_tables[2, 421:430])), 6))
(k39w1c35_tnr <- round(sum(cm_c35_tables[4, 421:430]) / (sum(cm_c35_tables[4, 421:430]) + sum(cm_c35_tables[3, 421:430])), 6))
(k39w1c35_truescore <- round((2 * k39w1c35_tpr * k39w1c35_tnr) / (k39w1c35_tpr + k39w1c35_tnr), 6))

(k39w2c35_tpr <- round(sum(cm_c35_tables[1, 431:440]) / (sum(cm_c35_tables[1, 431:440]) + sum(cm_c35_tables[2, 431:440])), 6))
(k39w2c35_tnr <- round(sum(cm_c35_tables[4, 431:440]) / (sum(cm_c35_tables[4, 431:440]) + sum(cm_c35_tables[3, 431:440])), 6))
(k39w2c35_truescore <- round((2 * k39w2c35_tpr * k39w2c35_tnr) / (k39w2c35_tpr + k39w2c35_tnr), 6))

(k39w3c35_tpr <- round(sum(cm_c35_tables[1, 441:450]) / (sum(cm_c35_tables[1, 441:450]) + sum(cm_c35_tables[2, 441:450])), 6))
(k39w3c35_tnr <- round(sum(cm_c35_tables[4, 441:450]) / (sum(cm_c35_tables[4, 441:450]) + sum(cm_c35_tables[3, 441:450])), 6))
(k39w3c35_truescore <- round((2 * k39w3c35_tpr * k39w3c35_tnr) / (k39w3c35_tpr + k39w3c35_tnr), 6))


# Compile the 0.35 cutoff results in a table, and identify the combination of 
# of k and kernel that maximizes the Truescore.

c35_results <- tibble(k = c(11, 11, 11, 13, 13, 13, 15, 15, 15, 
                            17, 17, 17, 19, 19, 19, 21, 21, 21, 
                            23, 23, 23, 25, 25, 25, 27, 27, 27, 
                            29, 29, 29, 31, 31, 31, 33, 33, 33, 
                            35, 35, 35, 37, 37, 37, 39, 39, 39), 
                      Kernel = rep(c("triangular", "gaussian", "optimal"), 15), 
                      Cut = 0.35, 
                      TPR = c(k11w1c35_tpr, k11w2c35_tpr, k11w3c35_tpr, k13w1c35_tpr, 
                              k13w2c35_tpr, k13w3c35_tpr, k15w1c35_tpr, k15w2c35_tpr, 
                              k15w3c35_tpr, k17w1c35_tpr, k17w2c35_tpr, k17w3c35_tpr, 
                              k19w1c35_tpr, k19w2c35_tpr, k19w3c35_tpr, k21w1c35_tpr, 
                              k21w2c35_tpr, k21w3c35_tpr, k23w1c35_tpr, k23w2c35_tpr, 
                              k23w3c35_tpr, k25w1c35_tpr, k25w2c35_tpr, k25w3c35_tpr, 
                              k27w1c35_tpr, k27w2c35_tpr, k27w3c35_tpr, k29w1c35_tpr, 
                              k29w2c35_tpr, k29w3c35_tpr, k31w1c35_tpr, k31w2c35_tpr, 
                              k31w3c35_tpr, k33w1c35_tpr, k33w2c35_tpr, k33w3c35_tpr, 
                              k35w1c35_tpr, k35w2c35_tpr, k35w3c35_tpr, k37w1c35_tpr, 
                              k37w2c35_tpr, k37w3c35_tpr, k39w1c35_tpr, k39w2c35_tpr, 
                              k39w3c35_tpr), 
                      TNR = c(k11w1c35_tnr, k11w2c35_tnr, k11w3c35_tnr, k13w1c35_tnr, 
                              k13w2c35_tnr, k13w3c35_tnr, k15w1c35_tnr, k15w2c35_tnr, 
                              k15w3c35_tnr, k17w1c35_tnr, k17w2c35_tnr, k17w3c35_tnr, 
                              k19w1c35_tnr, k19w2c35_tnr, k19w3c35_tnr, k21w1c35_tnr, 
                              k21w2c35_tnr, k21w3c35_tnr, k23w1c35_tnr, k23w2c35_tnr, 
                              k23w3c35_tnr, k25w1c35_tnr, k25w2c35_tnr, k25w3c35_tnr, 
                              k27w1c35_tnr, k27w2c35_tnr, k27w3c35_tnr, k29w1c35_tnr, 
                              k29w2c35_tnr, k29w3c35_tnr, k31w1c35_tnr, k31w2c35_tnr, 
                              k31w3c35_tnr, k33w1c35_tnr, k33w2c35_tnr, k33w3c35_tnr, 
                              k35w1c35_tnr, k35w2c35_tnr, k35w3c35_tnr, k37w1c35_tnr, 
                              k37w2c35_tnr, k37w3c35_tnr, k39w1c35_tnr, k39w2c35_tnr, 
                              k39w3c35_tnr), 
                      Truescore = c(k11w1c35_truescore, k11w2c35_truescore, 
                                    k11w3c35_truescore, k13w1c35_truescore, 
                                    k13w2c35_truescore, k13w3c35_truescore, 
                                    k15w1c35_truescore, k15w2c35_truescore, 
                                    k15w3c35_truescore, k17w1c35_truescore, 
                                    k17w2c35_truescore, k17w3c35_truescore, 
                                    k19w1c35_truescore, k19w2c35_truescore, 
                                    k19w3c35_truescore, k21w1c35_truescore, 
                                    k21w2c35_truescore, k21w3c35_truescore, 
                                    k23w1c35_truescore, k23w2c35_truescore, 
                                    k23w3c35_truescore, k25w1c35_truescore, 
                                    k25w2c35_truescore, k25w3c35_truescore, 
                                    k27w1c35_truescore, k27w2c35_truescore, 
                                    k27w3c35_truescore, k29w1c35_truescore, 
                                    k29w2c35_truescore, k29w3c35_truescore, 
                                    k31w1c35_truescore, k31w2c35_truescore, 
                                    k31w3c35_truescore, k33w1c35_truescore, 
                                    k33w2c35_truescore, k33w3c35_truescore, 
                                    k35w1c35_truescore, k35w2c35_truescore, 
                                    k35w3c35_truescore, k37w1c35_truescore, 
                                    k37w2c35_truescore, k37w3c35_truescore, 
                                    k39w1c35_truescore, k39w2c35_truescore, 
                                    k39w3c35_truescore))

knitr::kable(c35_results[1:45, ], caption = "c35_results")

ggplot(c35_results, aes(k, Truescore, shape = Kernel, color = Kernel)) + 
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.35 (Truescore)")

# For the Cutoff of 0.35, compute the Minimum Distance to (0, 1) for each 
# combination of k and Kernel. 

c35_results <- c35_results %>% mutate(Distance = sqrt((1 - TPR)^2 + (1 - TNR)^2))

ggplot(c35_results, aes(k, Distance, shape = Kernel, color = Kernel)) +
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.35 (Distance)")

# For the Cutoff of 0.35, identify the optimal combination of values for k and Kernel 
# based on each of our assessment methods, Truescore and Mimimum Distance to (0, 1).

max(c35_results$Truescore)
(c35_opt_k_ts <- c35_results$k[which.max(c35_results$Truescore)])
(c35_opt_kernel_ts <- c35_results$Kernel[which.max(c35_results$Truescore)])
(c35_opt_cut_ts <- c35_results$Cut[which.max(c35_results$Truescore)])
(c35_opt_tpr_ts <- c35_results$TPR[which.max(c35_results$Truescore)])
(c35_opt_tnr_ts <- c35_results$TNR[which.max(c35_results$Truescore)])
(c35_opt_d_ts <- c35_results$Distance[which.max(c35_results$Truescore)])

min(c35_results$Distance)
(c35_opt_k_dist <- c35_results$k[which.min(c35_results$Distance)])
(c35_opt_kernel_dist <- c35_results$Kernel[which.min(c35_results$Distance)])
(c35_opt_cut_dist <- c35_results$Cut[which.min(c35_results$Distance)])  
(c35_opt_tpr_dist <- c35_results$TPR[which.min(c35_results$Distance)])
(c35_opt_tnr_dist <- c35_results$TNR[which.min(c35_results$Distance)])
(c35_opt_t_dist <- c35_results$Truescore[which.min(c35_results$Distance)])

############################
# 0.36 Cutoff
############################

# For the decision cutoff of 0.36, generate a confusion matrix for
# every combination of k (11:39 by two), kernel (1:3) and fold (1:10).

cm_c36 <- sapply(kwf_dfs_l, function(x) {
  ss <- subset(x, select = c(pred36, obs))
  confusionMatrix(ss$pred36, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c36) <- kwf_dfs_v

cm_c36_tables <- sapply(cm_c36, "[[", 2)
cm_c36_tables <- as_tibble(cm_c36_tables)

# For each combination of k and kernel, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each combination of k and kernel when the decision cutoff is 0.36.

(k11w1c36_tpr <- round(sum(cm_c36_tables[1, 1:10]) / (sum(cm_c36_tables[1, 1:10]) + sum(cm_c36_tables[2, 1:10])), 6))
(k11w1c36_tnr <- round(sum(cm_c36_tables[4, 1:10]) / (sum(cm_c36_tables[4, 1:10]) + sum(cm_c36_tables[3, 1:10])), 6))
(k11w1c36_truescore <- round((2 * k11w1c36_tpr * k11w1c36_tnr) / (k11w1c36_tpr + k11w1c36_tnr), 6))

(k11w2c36_tpr <- round(sum(cm_c36_tables[1, 11:20]) / (sum(cm_c36_tables[1, 11:20]) + sum(cm_c36_tables[2, 11:20])), 6))
(k11w2c36_tnr <- round(sum(cm_c36_tables[4, 11:20]) / (sum(cm_c36_tables[4, 11:20]) + sum(cm_c36_tables[3, 11:20])), 6))
(k11w2c36_truescore <- round((2 * k11w2c36_tpr * k11w2c36_tnr) / (k11w2c36_tpr + k11w2c36_tnr), 6))

(k11w3c36_tpr <- round(sum(cm_c36_tables[1, 21:30]) / (sum(cm_c36_tables[1, 21:30]) + sum(cm_c36_tables[2, 21:30])), 6))
(k11w3c36_tnr <- round(sum(cm_c36_tables[4, 21:30]) / (sum(cm_c36_tables[4, 21:30]) + sum(cm_c36_tables[3, 21:30])), 6))
(k11w3c36_truescore <- round((2 * k11w3c36_tpr * k11w3c36_tnr) / (k11w3c36_tpr + k11w3c36_tnr), 6))


(k13w1c36_tpr <- round(sum(cm_c36_tables[1, 31:40]) / (sum(cm_c36_tables[1, 31:40]) + sum(cm_c36_tables[2, 31:40])), 6))
(k13w1c36_tnr <- round(sum(cm_c36_tables[4, 31:40]) / (sum(cm_c36_tables[4, 31:40]) + sum(cm_c36_tables[3, 31:40])), 6))
(k13w1c36_truescore <- round((2 * k13w1c36_tpr * k13w1c36_tnr) / (k13w1c36_tpr + k13w1c36_tnr), 6))

(k13w2c36_tpr <- round(sum(cm_c36_tables[1, 41:50]) / (sum(cm_c36_tables[1, 41:50]) + sum(cm_c36_tables[2, 41:50])), 6))
(k13w2c36_tnr <- round(sum(cm_c36_tables[4, 41:50]) / (sum(cm_c36_tables[4, 41:50]) + sum(cm_c36_tables[3, 41:50])), 6))
(k13w2c36_truescore <- round((2 * k13w2c36_tpr * k13w2c36_tnr) / (k13w2c36_tpr + k13w2c36_tnr), 6))

(k13w3c36_tpr <- round(sum(cm_c36_tables[1, 51:60]) / (sum(cm_c36_tables[1, 51:60]) + sum(cm_c36_tables[2, 51:60])), 6))
(k13w3c36_tnr <- round(sum(cm_c36_tables[4, 51:60]) / (sum(cm_c36_tables[4, 51:60]) + sum(cm_c36_tables[3, 51:60])), 6))
(k13w3c36_truescore <- round((2 * k13w3c36_tpr * k13w3c36_tnr) / (k13w3c36_tpr + k13w3c36_tnr), 6))


(k15w1c36_tpr <- round(sum(cm_c36_tables[1, 61:70]) / (sum(cm_c36_tables[1, 61:70]) + sum(cm_c36_tables[2, 61:70])), 6))
(k15w1c36_tnr <- round(sum(cm_c36_tables[4, 61:70]) / (sum(cm_c36_tables[4, 61:70]) + sum(cm_c36_tables[3, 61:70])), 6))
(k15w1c36_truescore <- round((2 * k15w1c36_tpr * k15w1c36_tnr) / (k15w1c36_tpr + k15w1c36_tnr), 6))

(k15w2c36_tpr <- round(sum(cm_c36_tables[1, 71:80]) / (sum(cm_c36_tables[1, 71:80]) + sum(cm_c36_tables[2, 71:80])), 6))
(k15w2c36_tnr <- round(sum(cm_c36_tables[4, 71:80]) / (sum(cm_c36_tables[4, 71:80]) + sum(cm_c36_tables[3, 71:80])), 6))
(k15w2c36_truescore <- round((2 * k15w2c36_tpr * k15w2c36_tnr) / (k15w2c36_tpr + k15w2c36_tnr), 6))

(k15w3c36_tpr <- round(sum(cm_c36_tables[1, 81:90]) / (sum(cm_c36_tables[1, 81:90]) + sum(cm_c36_tables[2, 81:90])), 6))
(k15w3c36_tnr <- round(sum(cm_c36_tables[4, 81:90]) / (sum(cm_c36_tables[4, 81:90]) + sum(cm_c36_tables[3, 81:90])), 6))
(k15w3c36_truescore <- round((2 * k15w3c36_tpr * k15w3c36_tnr) / (k15w3c36_tpr + k15w3c36_tnr), 6))


(k17w1c36_tpr <- round(sum(cm_c36_tables[1, 91:100]) / (sum(cm_c36_tables[1, 91:100]) + sum(cm_c36_tables[2, 91:100])), 6))
(k17w1c36_tnr <- round(sum(cm_c36_tables[4, 91:100]) / (sum(cm_c36_tables[4, 91:100]) + sum(cm_c36_tables[3, 91:100])), 6))
(k17w1c36_truescore <- round((2 * k17w1c36_tpr * k17w1c36_tnr) / (k17w1c36_tpr + k17w1c36_tnr), 6))

(k17w2c36_tpr <- round(sum(cm_c36_tables[1, 101:110]) / (sum(cm_c36_tables[1, 101:110]) + sum(cm_c36_tables[2, 101:110])), 6))
(k17w2c36_tnr <- round(sum(cm_c36_tables[4, 101:110]) / (sum(cm_c36_tables[4, 101:110]) + sum(cm_c36_tables[3, 101:110])), 6))
(k17w2c36_truescore <- round((2 * k17w2c36_tpr * k17w2c36_tnr) / (k17w2c36_tpr + k17w2c36_tnr), 6))

(k17w3c36_tpr <- round(sum(cm_c36_tables[1, 111:120]) / (sum(cm_c36_tables[1, 111:120]) + sum(cm_c36_tables[2, 111:120])), 6))
(k17w3c36_tnr <- round(sum(cm_c36_tables[4, 111:120]) / (sum(cm_c36_tables[4, 111:120]) + sum(cm_c36_tables[3, 111:120])), 6))
(k17w3c36_truescore <- round((2 * k17w3c36_tpr * k17w3c36_tnr) / (k17w3c36_tpr + k17w3c36_tnr), 6))


(k19w1c36_tpr <- round(sum(cm_c36_tables[1, 121:130]) / (sum(cm_c36_tables[1, 121:130]) + sum(cm_c36_tables[2, 121:130])), 6))
(k19w1c36_tnr <- round(sum(cm_c36_tables[4, 121:130]) / (sum(cm_c36_tables[4, 121:130]) + sum(cm_c36_tables[3, 121:130])), 6))
(k19w1c36_truescore <- round((2 * k19w1c36_tpr * k19w1c36_tnr) / (k19w1c36_tpr + k19w1c36_tnr), 6))

(k19w2c36_tpr <- round(sum(cm_c36_tables[1, 131:140]) / (sum(cm_c36_tables[1, 131:140]) + sum(cm_c36_tables[2, 131:140])), 6))
(k19w2c36_tnr <- round(sum(cm_c36_tables[4, 131:140]) / (sum(cm_c36_tables[4, 131:140]) + sum(cm_c36_tables[3, 131:140])), 6))
(k19w2c36_truescore <- round((2 * k19w2c36_tpr * k19w2c36_tnr) / (k19w2c36_tpr + k19w2c36_tnr), 6))

(k19w3c36_tpr <- round(sum(cm_c36_tables[1, 141:150]) / (sum(cm_c36_tables[1, 141:150]) + sum(cm_c36_tables[2, 141:150])), 6))
(k19w3c36_tnr <- round(sum(cm_c36_tables[4, 141:150]) / (sum(cm_c36_tables[4, 141:150]) + sum(cm_c36_tables[3, 141:150])), 6))
(k19w3c36_truescore <- round((2 * k19w3c36_tpr * k19w3c36_tnr) / (k19w3c36_tpr + k19w3c36_tnr), 6))


(k21w1c36_tpr <- round(sum(cm_c36_tables[1, 151:160]) / (sum(cm_c36_tables[1, 151:160]) + sum(cm_c36_tables[2, 151:160])), 6))
(k21w1c36_tnr <- round(sum(cm_c36_tables[4, 151:160]) / (sum(cm_c36_tables[4, 151:160]) + sum(cm_c36_tables[3, 151:160])), 6))
(k21w1c36_truescore <- round((2 * k21w1c36_tpr * k21w1c36_tnr) / (k21w1c36_tpr + k21w1c36_tnr), 6))

(k21w2c36_tpr <- round(sum(cm_c36_tables[1, 161:170]) / (sum(cm_c36_tables[1, 161:170]) + sum(cm_c36_tables[2, 161:170])), 6))
(k21w2c36_tnr <- round(sum(cm_c36_tables[4, 161:170]) / (sum(cm_c36_tables[4, 161:170]) + sum(cm_c36_tables[3, 161:170])), 6))
(k21w2c36_truescore <- round((2 * k21w2c36_tpr * k21w2c36_tnr) / (k21w2c36_tpr + k21w2c36_tnr), 6))

(k21w3c36_tpr <- round(sum(cm_c36_tables[1, 171:180]) / (sum(cm_c36_tables[1, 171:180]) + sum(cm_c36_tables[2, 171:180])), 6))
(k21w3c36_tnr <- round(sum(cm_c36_tables[4, 171:180]) / (sum(cm_c36_tables[4, 171:180]) + sum(cm_c36_tables[3, 171:180])), 6))
(k21w3c36_truescore <- round((2 * k21w3c36_tpr * k21w3c36_tnr) / (k21w3c36_tpr + k21w3c36_tnr), 6))


(k23w1c36_tpr <- round(sum(cm_c36_tables[1, 181:190]) / (sum(cm_c36_tables[1, 181:190]) + sum(cm_c36_tables[2, 181:190])), 6))
(k23w1c36_tnr <- round(sum(cm_c36_tables[4, 181:190]) / (sum(cm_c36_tables[4, 181:190]) + sum(cm_c36_tables[3, 181:190])), 6))
(k23w1c36_truescore <- round((2 * k23w1c36_tpr * k23w1c36_tnr) / (k23w1c36_tpr + k23w1c36_tnr), 6))

(k23w2c36_tpr <- round(sum(cm_c36_tables[1, 191:200]) / (sum(cm_c36_tables[1, 191:200]) + sum(cm_c36_tables[2, 191:200])), 6))
(k23w2c36_tnr <- round(sum(cm_c36_tables[4, 191:200]) / (sum(cm_c36_tables[4, 191:200]) + sum(cm_c36_tables[3, 191:200])), 6))
(k23w2c36_truescore <- round((2 * k23w2c36_tpr * k23w2c36_tnr) / (k23w2c36_tpr + k23w2c36_tnr), 6))

(k23w3c36_tpr <- round(sum(cm_c36_tables[1, 201:210]) / (sum(cm_c36_tables[1, 201:210]) + sum(cm_c36_tables[2, 201:210])), 6))
(k23w3c36_tnr <- round(sum(cm_c36_tables[4, 201:210]) / (sum(cm_c36_tables[4, 201:210]) + sum(cm_c36_tables[3, 201:210])), 6))
(k23w3c36_truescore <- round((2 * k23w3c36_tpr * k23w3c36_tnr) / (k23w3c36_tpr + k23w3c36_tnr), 6))


(k25w1c36_tpr <- round(sum(cm_c36_tables[1, 211:220]) / (sum(cm_c36_tables[1, 211:220]) + sum(cm_c36_tables[2, 211:220])), 6))
(k25w1c36_tnr <- round(sum(cm_c36_tables[4, 211:220]) / (sum(cm_c36_tables[4, 211:220]) + sum(cm_c36_tables[3, 211:220])), 6))
(k25w1c36_truescore <- round((2 * k25w1c36_tpr * k25w1c36_tnr) / (k25w1c36_tpr + k25w1c36_tnr), 6))

(k25w2c36_tpr <- round(sum(cm_c36_tables[1, 221:230]) / (sum(cm_c36_tables[1, 221:230]) + sum(cm_c36_tables[2, 221:230])), 6))
(k25w2c36_tnr <- round(sum(cm_c36_tables[4, 221:230]) / (sum(cm_c36_tables[4, 221:230]) + sum(cm_c36_tables[3, 221:230])), 6))
(k25w2c36_truescore <- round((2 * k25w2c36_tpr * k25w2c36_tnr) / (k25w2c36_tpr + k25w2c36_tnr), 6))

(k25w3c36_tpr <- round(sum(cm_c36_tables[1, 231:240]) / (sum(cm_c36_tables[1, 231:240]) + sum(cm_c36_tables[2, 231:240])), 6))
(k25w3c36_tnr <- round(sum(cm_c36_tables[4, 231:240]) / (sum(cm_c36_tables[4, 231:240]) + sum(cm_c36_tables[3, 231:240])), 6))
(k25w3c36_truescore <- round((2 * k25w3c36_tpr * k25w3c36_tnr) / (k25w3c36_tpr + k25w3c36_tnr), 6))


(k27w1c36_tpr <- round(sum(cm_c36_tables[1, 241:250]) / (sum(cm_c36_tables[1, 241:250]) + sum(cm_c36_tables[2, 241:250])), 6))
(k27w1c36_tnr <- round(sum(cm_c36_tables[4, 241:250]) / (sum(cm_c36_tables[4, 241:250]) + sum(cm_c36_tables[3, 241:250])), 6))
(k27w1c36_truescore <- round((2 * k27w1c36_tpr * k27w1c36_tnr) / (k27w1c36_tpr + k27w1c36_tnr), 6))

(k27w2c36_tpr <- round(sum(cm_c36_tables[1, 251:260]) / (sum(cm_c36_tables[1, 251:260]) + sum(cm_c36_tables[2, 251:260])), 6))
(k27w2c36_tnr <- round(sum(cm_c36_tables[4, 251:260]) / (sum(cm_c36_tables[4, 251:260]) + sum(cm_c36_tables[3, 251:260])), 6))
(k27w2c36_truescore <- round((2 * k27w2c36_tpr * k27w2c36_tnr) / (k27w2c36_tpr + k27w2c36_tnr), 6))

(k27w3c36_tpr <- round(sum(cm_c36_tables[1, 261:270]) / (sum(cm_c36_tables[1, 261:270]) + sum(cm_c36_tables[2, 261:270])), 6))
(k27w3c36_tnr <- round(sum(cm_c36_tables[4, 261:270]) / (sum(cm_c36_tables[4, 261:270]) + sum(cm_c36_tables[3, 261:270])), 6))
(k27w3c36_truescore <- round((2 * k27w3c36_tpr * k27w3c36_tnr) / (k27w3c36_tpr + k27w3c36_tnr), 6))


(k29w1c36_tpr <- round(sum(cm_c36_tables[1, 271:280]) / (sum(cm_c36_tables[1, 271:280]) + sum(cm_c36_tables[2, 271:280])), 6))
(k29w1c36_tnr <- round(sum(cm_c36_tables[4, 271:280]) / (sum(cm_c36_tables[4, 271:280]) + sum(cm_c36_tables[3, 271:280])), 6))
(k29w1c36_truescore <- round((2 * k29w1c36_tpr * k29w1c36_tnr) / (k29w1c36_tpr + k29w1c36_tnr), 6))

(k29w2c36_tpr <- round(sum(cm_c36_tables[1, 281:290]) / (sum(cm_c36_tables[1, 281:290]) + sum(cm_c36_tables[2, 281:290])), 6))
(k29w2c36_tnr <- round(sum(cm_c36_tables[4, 281:290]) / (sum(cm_c36_tables[4, 281:290]) + sum(cm_c36_tables[3, 281:290])), 6))
(k29w2c36_truescore <- round((2 * k29w2c36_tpr * k29w2c36_tnr) / (k29w2c36_tpr + k29w2c36_tnr), 6))

(k29w3c36_tpr <- round(sum(cm_c36_tables[1, 291:300]) / (sum(cm_c36_tables[1, 291:300]) + sum(cm_c36_tables[2, 291:300])), 6))
(k29w3c36_tnr <- round(sum(cm_c36_tables[4, 291:300]) / (sum(cm_c36_tables[4, 291:300]) + sum(cm_c36_tables[3, 291:300])), 6))
(k29w3c36_truescore <- round((2 * k29w3c36_tpr * k29w3c36_tnr) / (k29w3c36_tpr + k29w3c36_tnr), 6))


(k31w1c36_tpr <- round(sum(cm_c36_tables[1, 301:310]) / (sum(cm_c36_tables[1, 301:310]) + sum(cm_c36_tables[2, 301:310])), 6))
(k31w1c36_tnr <- round(sum(cm_c36_tables[4, 301:310]) / (sum(cm_c36_tables[4, 301:310]) + sum(cm_c36_tables[3, 301:310])), 6))
(k31w1c36_truescore <- round((2 * k31w1c36_tpr * k31w1c36_tnr) / (k31w1c36_tpr + k31w1c36_tnr), 6))

(k31w2c36_tpr <- round(sum(cm_c36_tables[1, 311:320]) / (sum(cm_c36_tables[1, 311:320]) + sum(cm_c36_tables[2, 311:320])), 6))
(k31w2c36_tnr <- round(sum(cm_c36_tables[4, 311:320]) / (sum(cm_c36_tables[4, 311:320]) + sum(cm_c36_tables[3, 311:320])), 6))
(k31w2c36_truescore <- round((2 * k31w2c36_tpr * k31w2c36_tnr) / (k31w2c36_tpr + k31w2c36_tnr), 6))

(k31w3c36_tpr <- round(sum(cm_c36_tables[1, 321:330]) / (sum(cm_c36_tables[1, 321:330]) + sum(cm_c36_tables[2, 321:330])), 6))
(k31w3c36_tnr <- round(sum(cm_c36_tables[4, 321:330]) / (sum(cm_c36_tables[4, 321:330]) + sum(cm_c36_tables[3, 321:330])), 6))
(k31w3c36_truescore <- round((2 * k31w3c36_tpr * k31w3c36_tnr) / (k31w3c36_tpr + k31w3c36_tnr), 6))


(k33w1c36_tpr <- round(sum(cm_c36_tables[1, 331:340]) / (sum(cm_c36_tables[1, 331:340]) + sum(cm_c36_tables[2, 331:340])), 6))
(k33w1c36_tnr <- round(sum(cm_c36_tables[4, 331:340]) / (sum(cm_c36_tables[4, 331:340]) + sum(cm_c36_tables[3, 331:340])), 6))
(k33w1c36_truescore <- round((2 * k33w1c36_tpr * k33w1c36_tnr) / (k33w1c36_tpr + k33w1c36_tnr), 6))

(k33w2c36_tpr <- round(sum(cm_c36_tables[1, 341:350]) / (sum(cm_c36_tables[1, 341:350]) + sum(cm_c36_tables[2, 341:350])), 6))
(k33w2c36_tnr <- round(sum(cm_c36_tables[4, 341:350]) / (sum(cm_c36_tables[4, 341:350]) + sum(cm_c36_tables[3, 341:350])), 6))
(k33w2c36_truescore <- round((2 * k33w2c36_tpr * k33w2c36_tnr) / (k33w2c36_tpr + k33w2c36_tnr), 6))

(k33w3c36_tpr <- round(sum(cm_c36_tables[1, 351:360]) / (sum(cm_c36_tables[1, 351:360]) + sum(cm_c36_tables[2, 351:360])), 6))
(k33w3c36_tnr <- round(sum(cm_c36_tables[4, 351:360]) / (sum(cm_c36_tables[4, 351:360]) + sum(cm_c36_tables[3, 351:360])), 6))
(k33w3c36_truescore <- round((2 * k33w3c36_tpr * k33w3c36_tnr) / (k33w3c36_tpr + k33w3c36_tnr), 6))


(k35w1c36_tpr <- round(sum(cm_c36_tables[1, 361:370]) / (sum(cm_c36_tables[1, 361:370]) + sum(cm_c36_tables[2, 361:370])), 6))
(k35w1c36_tnr <- round(sum(cm_c36_tables[4, 361:370]) / (sum(cm_c36_tables[4, 361:370]) + sum(cm_c36_tables[3, 361:370])), 6))
(k35w1c36_truescore <- round((2 * k35w1c36_tpr * k35w1c36_tnr) / (k35w1c36_tpr + k35w1c36_tnr), 6))

(k35w2c36_tpr <- round(sum(cm_c36_tables[1, 371:380]) / (sum(cm_c36_tables[1, 371:380]) + sum(cm_c36_tables[2, 371:380])), 6))
(k35w2c36_tnr <- round(sum(cm_c36_tables[4, 371:380]) / (sum(cm_c36_tables[4, 371:380]) + sum(cm_c36_tables[3, 371:380])), 6))
(k35w2c36_truescore <- round((2 * k35w2c36_tpr * k35w2c36_tnr) / (k35w2c36_tpr + k35w2c36_tnr), 6))

(k35w3c36_tpr <- round(sum(cm_c36_tables[1, 381:390]) / (sum(cm_c36_tables[1, 381:390]) + sum(cm_c36_tables[2, 381:390])), 6))
(k35w3c36_tnr <- round(sum(cm_c36_tables[4, 381:390]) / (sum(cm_c36_tables[4, 381:390]) + sum(cm_c36_tables[3, 381:390])), 6))
(k35w3c36_truescore <- round((2 * k35w3c36_tpr * k35w3c36_tnr) / (k35w3c36_tpr + k35w3c36_tnr), 6))


(k37w1c36_tpr <- round(sum(cm_c36_tables[1, 391:400]) / (sum(cm_c36_tables[1, 391:400]) + sum(cm_c36_tables[2, 391:400])), 6))
(k37w1c36_tnr <- round(sum(cm_c36_tables[4, 391:400]) / (sum(cm_c36_tables[4, 391:400]) + sum(cm_c36_tables[3, 391:400])), 6))
(k37w1c36_truescore <- round((2 * k37w1c36_tpr * k37w1c36_tnr) / (k37w1c36_tpr + k37w1c36_tnr), 6))

(k37w2c36_tpr <- round(sum(cm_c36_tables[1, 401:410]) / (sum(cm_c36_tables[1, 401:410]) + sum(cm_c36_tables[2, 401:410])), 6))
(k37w2c36_tnr <- round(sum(cm_c36_tables[4, 401:410]) / (sum(cm_c36_tables[4, 401:410]) + sum(cm_c36_tables[3, 401:410])), 6))
(k37w2c36_truescore <- round((2 * k37w2c36_tpr * k37w2c36_tnr) / (k37w2c36_tpr + k37w2c36_tnr), 6))

(k37w3c36_tpr <- round(sum(cm_c36_tables[1, 411:420]) / (sum(cm_c36_tables[1, 411:420]) + sum(cm_c36_tables[2, 411:420])), 6))
(k37w3c36_tnr <- round(sum(cm_c36_tables[4, 411:420]) / (sum(cm_c36_tables[4, 411:420]) + sum(cm_c36_tables[3, 411:420])), 6))
(k37w3c36_truescore <- round((2 * k37w3c36_tpr * k37w3c36_tnr) / (k37w3c36_tpr + k37w3c36_tnr), 6))


(k39w1c36_tpr <- round(sum(cm_c36_tables[1, 421:430]) / (sum(cm_c36_tables[1, 421:430]) + sum(cm_c36_tables[2, 421:430])), 6))
(k39w1c36_tnr <- round(sum(cm_c36_tables[4, 421:430]) / (sum(cm_c36_tables[4, 421:430]) + sum(cm_c36_tables[3, 421:430])), 6))
(k39w1c36_truescore <- round((2 * k39w1c36_tpr * k39w1c36_tnr) / (k39w1c36_tpr + k39w1c36_tnr), 6))

(k39w2c36_tpr <- round(sum(cm_c36_tables[1, 431:440]) / (sum(cm_c36_tables[1, 431:440]) + sum(cm_c36_tables[2, 431:440])), 6))
(k39w2c36_tnr <- round(sum(cm_c36_tables[4, 431:440]) / (sum(cm_c36_tables[4, 431:440]) + sum(cm_c36_tables[3, 431:440])), 6))
(k39w2c36_truescore <- round((2 * k39w2c36_tpr * k39w2c36_tnr) / (k39w2c36_tpr + k39w2c36_tnr), 6))

(k39w3c36_tpr <- round(sum(cm_c36_tables[1, 441:450]) / (sum(cm_c36_tables[1, 441:450]) + sum(cm_c36_tables[2, 441:450])), 6))
(k39w3c36_tnr <- round(sum(cm_c36_tables[4, 441:450]) / (sum(cm_c36_tables[4, 441:450]) + sum(cm_c36_tables[3, 441:450])), 6))
(k39w3c36_truescore <- round((2 * k39w3c36_tpr * k39w3c36_tnr) / (k39w3c36_tpr + k39w3c36_tnr), 6))


# Compile the 0.36 cutoff results in a table, and identify the combination of 
# of k and kernel that maximizes the Truescore.

c36_results <- tibble(k = c(11, 11, 11, 13, 13, 13, 15, 15, 15, 
                            17, 17, 17, 19, 19, 19, 21, 21, 21, 
                            23, 23, 23, 25, 25, 25, 27, 27, 27, 
                            29, 29, 29, 31, 31, 31, 33, 33, 33, 
                            35, 35, 35, 37, 37, 37, 39, 39, 39), 
                      Kernel = rep(c("triangular", "gaussian", "optimal"), 15), 
                      Cut = 0.36, 
                      TPR = c(k11w1c36_tpr, k11w2c36_tpr, k11w3c36_tpr, k13w1c36_tpr, 
                              k13w2c36_tpr, k13w3c36_tpr, k15w1c36_tpr, k15w2c36_tpr, 
                              k15w3c36_tpr, k17w1c36_tpr, k17w2c36_tpr, k17w3c36_tpr, 
                              k19w1c36_tpr, k19w2c36_tpr, k19w3c36_tpr, k21w1c36_tpr, 
                              k21w2c36_tpr, k21w3c36_tpr, k23w1c36_tpr, k23w2c36_tpr, 
                              k23w3c36_tpr, k25w1c36_tpr, k25w2c36_tpr, k25w3c36_tpr, 
                              k27w1c36_tpr, k27w2c36_tpr, k27w3c36_tpr, k29w1c36_tpr, 
                              k29w2c36_tpr, k29w3c36_tpr, k31w1c36_tpr, k31w2c36_tpr, 
                              k31w3c36_tpr, k33w1c36_tpr, k33w2c36_tpr, k33w3c36_tpr, 
                              k35w1c36_tpr, k35w2c36_tpr, k35w3c36_tpr, k37w1c36_tpr, 
                              k37w2c36_tpr, k37w3c36_tpr, k39w1c36_tpr, k39w2c36_tpr, 
                              k39w3c36_tpr), 
                      TNR = c(k11w1c36_tnr, k11w2c36_tnr, k11w3c36_tnr, k13w1c36_tnr, 
                              k13w2c36_tnr, k13w3c36_tnr, k15w1c36_tnr, k15w2c36_tnr, 
                              k15w3c36_tnr, k17w1c36_tnr, k17w2c36_tnr, k17w3c36_tnr, 
                              k19w1c36_tnr, k19w2c36_tnr, k19w3c36_tnr, k21w1c36_tnr, 
                              k21w2c36_tnr, k21w3c36_tnr, k23w1c36_tnr, k23w2c36_tnr, 
                              k23w3c36_tnr, k25w1c36_tnr, k25w2c36_tnr, k25w3c36_tnr, 
                              k27w1c36_tnr, k27w2c36_tnr, k27w3c36_tnr, k29w1c36_tnr, 
                              k29w2c36_tnr, k29w3c36_tnr, k31w1c36_tnr, k31w2c36_tnr, 
                              k31w3c36_tnr, k33w1c36_tnr, k33w2c36_tnr, k33w3c36_tnr, 
                              k35w1c36_tnr, k35w2c36_tnr, k35w3c36_tnr, k37w1c36_tnr, 
                              k37w2c36_tnr, k37w3c36_tnr, k39w1c36_tnr, k39w2c36_tnr, 
                              k39w3c36_tnr), 
                      Truescore = c(k11w1c36_truescore, k11w2c36_truescore, 
                                    k11w3c36_truescore, k13w1c36_truescore, 
                                    k13w2c36_truescore, k13w3c36_truescore, 
                                    k15w1c36_truescore, k15w2c36_truescore, 
                                    k15w3c36_truescore, k17w1c36_truescore, 
                                    k17w2c36_truescore, k17w3c36_truescore, 
                                    k19w1c36_truescore, k19w2c36_truescore, 
                                    k19w3c36_truescore, k21w1c36_truescore, 
                                    k21w2c36_truescore, k21w3c36_truescore, 
                                    k23w1c36_truescore, k23w2c36_truescore, 
                                    k23w3c36_truescore, k25w1c36_truescore, 
                                    k25w2c36_truescore, k25w3c36_truescore, 
                                    k27w1c36_truescore, k27w2c36_truescore, 
                                    k27w3c36_truescore, k29w1c36_truescore, 
                                    k29w2c36_truescore, k29w3c36_truescore, 
                                    k31w1c36_truescore, k31w2c36_truescore, 
                                    k31w3c36_truescore, k33w1c36_truescore, 
                                    k33w2c36_truescore, k33w3c36_truescore, 
                                    k35w1c36_truescore, k35w2c36_truescore, 
                                    k35w3c36_truescore, k37w1c36_truescore, 
                                    k37w2c36_truescore, k37w3c36_truescore, 
                                    k39w1c36_truescore, k39w2c36_truescore, 
                                    k39w3c36_truescore))

knitr::kable(c36_results[1:45, ], caption = "c36_results")

ggplot(c36_results, aes(k, Truescore, shape = Kernel, color = Kernel)) + 
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.36 (Truescore)")

# For the Cutoff of 0.36, compute the Minimum Distance to (0, 1) for each 
# combination of k and Kernel. 

c36_results <- c36_results %>% mutate(Distance = sqrt((1 - TPR)^2 + (1 - TNR)^2))

ggplot(c36_results, aes(k, Distance, shape = Kernel, color = Kernel)) +
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.36 (Distance)")

# For the Cutoff of 0.36, identify the optimal combination of values for k and Kernel 
# based on each of our assessment methods, Truescore and Mimimum Distance to (0, 1).

max(c36_results$Truescore)
(c36_opt_k_ts <- c36_results$k[which.max(c36_results$Truescore)])
(c36_opt_kernel_ts <- c36_results$Kernel[which.max(c36_results$Truescore)])
(c36_opt_cut_ts <- c36_results$Cut[which.max(c36_results$Truescore)])
(c36_opt_tpr_ts <- c36_results$TPR[which.max(c36_results$Truescore)])
(c36_opt_tnr_ts <- c36_results$TNR[which.max(c36_results$Truescore)])
(c36_opt_d_ts <- c36_results$Distance[which.max(c36_results$Truescore)])

min(c36_results$Distance)
(c36_opt_k_dist <- c36_results$k[which.min(c36_results$Distance)])
(c36_opt_kernel_dist <- c36_results$Kernel[which.min(c36_results$Distance)])
(c36_opt_cut_dist <- c36_results$Cut[which.min(c36_results$Distance)])  
(c36_opt_tpr_dist <- c36_results$TPR[which.min(c36_results$Distance)])
(c36_opt_tnr_dist <- c36_results$TNR[which.min(c36_results$Distance)])
(c36_opt_t_dist <- c36_results$Truescore[which.min(c36_results$Distance)])

############################
# 0.37 Cutoff
############################

# For the decision cutoff of 0.37, generate a confusion matrix for
# every combination of k (11:39 by two), kernel (1:3) and fold (1:10).

cm_c37 <- sapply(kwf_dfs_l, function(x) {
  ss <- subset(x, select = c(pred37, obs))
  confusionMatrix(ss$pred37, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c37) <- kwf_dfs_v

cm_c37_tables <- sapply(cm_c37, "[[", 2)
cm_c37_tables <- as_tibble(cm_c37_tables)

# For each combination of k and kernel, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each combination of k and kernel when the decision cutoff is 0.37.

(k11w1c37_tpr <- round(sum(cm_c37_tables[1, 1:10]) / (sum(cm_c37_tables[1, 1:10]) + sum(cm_c37_tables[2, 1:10])), 6))
(k11w1c37_tnr <- round(sum(cm_c37_tables[4, 1:10]) / (sum(cm_c37_tables[4, 1:10]) + sum(cm_c37_tables[3, 1:10])), 6))
(k11w1c37_truescore <- round((2 * k11w1c37_tpr * k11w1c37_tnr) / (k11w1c37_tpr + k11w1c37_tnr), 6))

(k11w2c37_tpr <- round(sum(cm_c37_tables[1, 11:20]) / (sum(cm_c37_tables[1, 11:20]) + sum(cm_c37_tables[2, 11:20])), 6))
(k11w2c37_tnr <- round(sum(cm_c37_tables[4, 11:20]) / (sum(cm_c37_tables[4, 11:20]) + sum(cm_c37_tables[3, 11:20])), 6))
(k11w2c37_truescore <- round((2 * k11w2c37_tpr * k11w2c37_tnr) / (k11w2c37_tpr + k11w2c37_tnr), 6))

(k11w3c37_tpr <- round(sum(cm_c37_tables[1, 21:30]) / (sum(cm_c37_tables[1, 21:30]) + sum(cm_c37_tables[2, 21:30])), 6))
(k11w3c37_tnr <- round(sum(cm_c37_tables[4, 21:30]) / (sum(cm_c37_tables[4, 21:30]) + sum(cm_c37_tables[3, 21:30])), 6))
(k11w3c37_truescore <- round((2 * k11w3c37_tpr * k11w3c37_tnr) / (k11w3c37_tpr + k11w3c37_tnr), 6))


(k13w1c37_tpr <- round(sum(cm_c37_tables[1, 31:40]) / (sum(cm_c37_tables[1, 31:40]) + sum(cm_c37_tables[2, 31:40])), 6))
(k13w1c37_tnr <- round(sum(cm_c37_tables[4, 31:40]) / (sum(cm_c37_tables[4, 31:40]) + sum(cm_c37_tables[3, 31:40])), 6))
(k13w1c37_truescore <- round((2 * k13w1c37_tpr * k13w1c37_tnr) / (k13w1c37_tpr + k13w1c37_tnr), 6))

(k13w2c37_tpr <- round(sum(cm_c37_tables[1, 41:50]) / (sum(cm_c37_tables[1, 41:50]) + sum(cm_c37_tables[2, 41:50])), 6))
(k13w2c37_tnr <- round(sum(cm_c37_tables[4, 41:50]) / (sum(cm_c37_tables[4, 41:50]) + sum(cm_c37_tables[3, 41:50])), 6))
(k13w2c37_truescore <- round((2 * k13w2c37_tpr * k13w2c37_tnr) / (k13w2c37_tpr + k13w2c37_tnr), 6))

(k13w3c37_tpr <- round(sum(cm_c37_tables[1, 51:60]) / (sum(cm_c37_tables[1, 51:60]) + sum(cm_c37_tables[2, 51:60])), 6))
(k13w3c37_tnr <- round(sum(cm_c37_tables[4, 51:60]) / (sum(cm_c37_tables[4, 51:60]) + sum(cm_c37_tables[3, 51:60])), 6))
(k13w3c37_truescore <- round((2 * k13w3c37_tpr * k13w3c37_tnr) / (k13w3c37_tpr + k13w3c37_tnr), 6))


(k15w1c37_tpr <- round(sum(cm_c37_tables[1, 61:70]) / (sum(cm_c37_tables[1, 61:70]) + sum(cm_c37_tables[2, 61:70])), 6))
(k15w1c37_tnr <- round(sum(cm_c37_tables[4, 61:70]) / (sum(cm_c37_tables[4, 61:70]) + sum(cm_c37_tables[3, 61:70])), 6))
(k15w1c37_truescore <- round((2 * k15w1c37_tpr * k15w1c37_tnr) / (k15w1c37_tpr + k15w1c37_tnr), 6))

(k15w2c37_tpr <- round(sum(cm_c37_tables[1, 71:80]) / (sum(cm_c37_tables[1, 71:80]) + sum(cm_c37_tables[2, 71:80])), 6))
(k15w2c37_tnr <- round(sum(cm_c37_tables[4, 71:80]) / (sum(cm_c37_tables[4, 71:80]) + sum(cm_c37_tables[3, 71:80])), 6))
(k15w2c37_truescore <- round((2 * k15w2c37_tpr * k15w2c37_tnr) / (k15w2c37_tpr + k15w2c37_tnr), 6))

(k15w3c37_tpr <- round(sum(cm_c37_tables[1, 81:90]) / (sum(cm_c37_tables[1, 81:90]) + sum(cm_c37_tables[2, 81:90])), 6))
(k15w3c37_tnr <- round(sum(cm_c37_tables[4, 81:90]) / (sum(cm_c37_tables[4, 81:90]) + sum(cm_c37_tables[3, 81:90])), 6))
(k15w3c37_truescore <- round((2 * k15w3c37_tpr * k15w3c37_tnr) / (k15w3c37_tpr + k15w3c37_tnr), 6))


(k17w1c37_tpr <- round(sum(cm_c37_tables[1, 91:100]) / (sum(cm_c37_tables[1, 91:100]) + sum(cm_c37_tables[2, 91:100])), 6))
(k17w1c37_tnr <- round(sum(cm_c37_tables[4, 91:100]) / (sum(cm_c37_tables[4, 91:100]) + sum(cm_c37_tables[3, 91:100])), 6))
(k17w1c37_truescore <- round((2 * k17w1c37_tpr * k17w1c37_tnr) / (k17w1c37_tpr + k17w1c37_tnr), 6))

(k17w2c37_tpr <- round(sum(cm_c37_tables[1, 101:110]) / (sum(cm_c37_tables[1, 101:110]) + sum(cm_c37_tables[2, 101:110])), 6))
(k17w2c37_tnr <- round(sum(cm_c37_tables[4, 101:110]) / (sum(cm_c37_tables[4, 101:110]) + sum(cm_c37_tables[3, 101:110])), 6))
(k17w2c37_truescore <- round((2 * k17w2c37_tpr * k17w2c37_tnr) / (k17w2c37_tpr + k17w2c37_tnr), 6))

(k17w3c37_tpr <- round(sum(cm_c37_tables[1, 111:120]) / (sum(cm_c37_tables[1, 111:120]) + sum(cm_c37_tables[2, 111:120])), 6))
(k17w3c37_tnr <- round(sum(cm_c37_tables[4, 111:120]) / (sum(cm_c37_tables[4, 111:120]) + sum(cm_c37_tables[3, 111:120])), 6))
(k17w3c37_truescore <- round((2 * k17w3c37_tpr * k17w3c37_tnr) / (k17w3c37_tpr + k17w3c37_tnr), 6))


(k19w1c37_tpr <- round(sum(cm_c37_tables[1, 121:130]) / (sum(cm_c37_tables[1, 121:130]) + sum(cm_c37_tables[2, 121:130])), 6))
(k19w1c37_tnr <- round(sum(cm_c37_tables[4, 121:130]) / (sum(cm_c37_tables[4, 121:130]) + sum(cm_c37_tables[3, 121:130])), 6))
(k19w1c37_truescore <- round((2 * k19w1c37_tpr * k19w1c37_tnr) / (k19w1c37_tpr + k19w1c37_tnr), 6))

(k19w2c37_tpr <- round(sum(cm_c37_tables[1, 131:140]) / (sum(cm_c37_tables[1, 131:140]) + sum(cm_c37_tables[2, 131:140])), 6))
(k19w2c37_tnr <- round(sum(cm_c37_tables[4, 131:140]) / (sum(cm_c37_tables[4, 131:140]) + sum(cm_c37_tables[3, 131:140])), 6))
(k19w2c37_truescore <- round((2 * k19w2c37_tpr * k19w2c37_tnr) / (k19w2c37_tpr + k19w2c37_tnr), 6))

(k19w3c37_tpr <- round(sum(cm_c37_tables[1, 141:150]) / (sum(cm_c37_tables[1, 141:150]) + sum(cm_c37_tables[2, 141:150])), 6))
(k19w3c37_tnr <- round(sum(cm_c37_tables[4, 141:150]) / (sum(cm_c37_tables[4, 141:150]) + sum(cm_c37_tables[3, 141:150])), 6))
(k19w3c37_truescore <- round((2 * k19w3c37_tpr * k19w3c37_tnr) / (k19w3c37_tpr + k19w3c37_tnr), 6))


(k21w1c37_tpr <- round(sum(cm_c37_tables[1, 151:160]) / (sum(cm_c37_tables[1, 151:160]) + sum(cm_c37_tables[2, 151:160])), 6))
(k21w1c37_tnr <- round(sum(cm_c37_tables[4, 151:160]) / (sum(cm_c37_tables[4, 151:160]) + sum(cm_c37_tables[3, 151:160])), 6))
(k21w1c37_truescore <- round((2 * k21w1c37_tpr * k21w1c37_tnr) / (k21w1c37_tpr + k21w1c37_tnr), 6))

(k21w2c37_tpr <- round(sum(cm_c37_tables[1, 161:170]) / (sum(cm_c37_tables[1, 161:170]) + sum(cm_c37_tables[2, 161:170])), 6))
(k21w2c37_tnr <- round(sum(cm_c37_tables[4, 161:170]) / (sum(cm_c37_tables[4, 161:170]) + sum(cm_c37_tables[3, 161:170])), 6))
(k21w2c37_truescore <- round((2 * k21w2c37_tpr * k21w2c37_tnr) / (k21w2c37_tpr + k21w2c37_tnr), 6))

(k21w3c37_tpr <- round(sum(cm_c37_tables[1, 171:180]) / (sum(cm_c37_tables[1, 171:180]) + sum(cm_c37_tables[2, 171:180])), 6))
(k21w3c37_tnr <- round(sum(cm_c37_tables[4, 171:180]) / (sum(cm_c37_tables[4, 171:180]) + sum(cm_c37_tables[3, 171:180])), 6))
(k21w3c37_truescore <- round((2 * k21w3c37_tpr * k21w3c37_tnr) / (k21w3c37_tpr + k21w3c37_tnr), 6))


(k23w1c37_tpr <- round(sum(cm_c37_tables[1, 181:190]) / (sum(cm_c37_tables[1, 181:190]) + sum(cm_c37_tables[2, 181:190])), 6))
(k23w1c37_tnr <- round(sum(cm_c37_tables[4, 181:190]) / (sum(cm_c37_tables[4, 181:190]) + sum(cm_c37_tables[3, 181:190])), 6))
(k23w1c37_truescore <- round((2 * k23w1c37_tpr * k23w1c37_tnr) / (k23w1c37_tpr + k23w1c37_tnr), 6))

(k23w2c37_tpr <- round(sum(cm_c37_tables[1, 191:200]) / (sum(cm_c37_tables[1, 191:200]) + sum(cm_c37_tables[2, 191:200])), 6))
(k23w2c37_tnr <- round(sum(cm_c37_tables[4, 191:200]) / (sum(cm_c37_tables[4, 191:200]) + sum(cm_c37_tables[3, 191:200])), 6))
(k23w2c37_truescore <- round((2 * k23w2c37_tpr * k23w2c37_tnr) / (k23w2c37_tpr + k23w2c37_tnr), 6))

(k23w3c37_tpr <- round(sum(cm_c37_tables[1, 201:210]) / (sum(cm_c37_tables[1, 201:210]) + sum(cm_c37_tables[2, 201:210])), 6))
(k23w3c37_tnr <- round(sum(cm_c37_tables[4, 201:210]) / (sum(cm_c37_tables[4, 201:210]) + sum(cm_c37_tables[3, 201:210])), 6))
(k23w3c37_truescore <- round((2 * k23w3c37_tpr * k23w3c37_tnr) / (k23w3c37_tpr + k23w3c37_tnr), 6))


(k25w1c37_tpr <- round(sum(cm_c37_tables[1, 211:220]) / (sum(cm_c37_tables[1, 211:220]) + sum(cm_c37_tables[2, 211:220])), 6))
(k25w1c37_tnr <- round(sum(cm_c37_tables[4, 211:220]) / (sum(cm_c37_tables[4, 211:220]) + sum(cm_c37_tables[3, 211:220])), 6))
(k25w1c37_truescore <- round((2 * k25w1c37_tpr * k25w1c37_tnr) / (k25w1c37_tpr + k25w1c37_tnr), 6))

(k25w2c37_tpr <- round(sum(cm_c37_tables[1, 221:230]) / (sum(cm_c37_tables[1, 221:230]) + sum(cm_c37_tables[2, 221:230])), 6))
(k25w2c37_tnr <- round(sum(cm_c37_tables[4, 221:230]) / (sum(cm_c37_tables[4, 221:230]) + sum(cm_c37_tables[3, 221:230])), 6))
(k25w2c37_truescore <- round((2 * k25w2c37_tpr * k25w2c37_tnr) / (k25w2c37_tpr + k25w2c37_tnr), 6))

(k25w3c37_tpr <- round(sum(cm_c37_tables[1, 231:240]) / (sum(cm_c37_tables[1, 231:240]) + sum(cm_c37_tables[2, 231:240])), 6))
(k25w3c37_tnr <- round(sum(cm_c37_tables[4, 231:240]) / (sum(cm_c37_tables[4, 231:240]) + sum(cm_c37_tables[3, 231:240])), 6))
(k25w3c37_truescore <- round((2 * k25w3c37_tpr * k25w3c37_tnr) / (k25w3c37_tpr + k25w3c37_tnr), 6))


(k27w1c37_tpr <- round(sum(cm_c37_tables[1, 241:250]) / (sum(cm_c37_tables[1, 241:250]) + sum(cm_c37_tables[2, 241:250])), 6))
(k27w1c37_tnr <- round(sum(cm_c37_tables[4, 241:250]) / (sum(cm_c37_tables[4, 241:250]) + sum(cm_c37_tables[3, 241:250])), 6))
(k27w1c37_truescore <- round((2 * k27w1c37_tpr * k27w1c37_tnr) / (k27w1c37_tpr + k27w1c37_tnr), 6))

(k27w2c37_tpr <- round(sum(cm_c37_tables[1, 251:260]) / (sum(cm_c37_tables[1, 251:260]) + sum(cm_c37_tables[2, 251:260])), 6))
(k27w2c37_tnr <- round(sum(cm_c37_tables[4, 251:260]) / (sum(cm_c37_tables[4, 251:260]) + sum(cm_c37_tables[3, 251:260])), 6))
(k27w2c37_truescore <- round((2 * k27w2c37_tpr * k27w2c37_tnr) / (k27w2c37_tpr + k27w2c37_tnr), 6))

(k27w3c37_tpr <- round(sum(cm_c37_tables[1, 261:270]) / (sum(cm_c37_tables[1, 261:270]) + sum(cm_c37_tables[2, 261:270])), 6))
(k27w3c37_tnr <- round(sum(cm_c37_tables[4, 261:270]) / (sum(cm_c37_tables[4, 261:270]) + sum(cm_c37_tables[3, 261:270])), 6))
(k27w3c37_truescore <- round((2 * k27w3c37_tpr * k27w3c37_tnr) / (k27w3c37_tpr + k27w3c37_tnr), 6))


(k29w1c37_tpr <- round(sum(cm_c37_tables[1, 271:280]) / (sum(cm_c37_tables[1, 271:280]) + sum(cm_c37_tables[2, 271:280])), 6))
(k29w1c37_tnr <- round(sum(cm_c37_tables[4, 271:280]) / (sum(cm_c37_tables[4, 271:280]) + sum(cm_c37_tables[3, 271:280])), 6))
(k29w1c37_truescore <- round((2 * k29w1c37_tpr * k29w1c37_tnr) / (k29w1c37_tpr + k29w1c37_tnr), 6))

(k29w2c37_tpr <- round(sum(cm_c37_tables[1, 281:290]) / (sum(cm_c37_tables[1, 281:290]) + sum(cm_c37_tables[2, 281:290])), 6))
(k29w2c37_tnr <- round(sum(cm_c37_tables[4, 281:290]) / (sum(cm_c37_tables[4, 281:290]) + sum(cm_c37_tables[3, 281:290])), 6))
(k29w2c37_truescore <- round((2 * k29w2c37_tpr * k29w2c37_tnr) / (k29w2c37_tpr + k29w2c37_tnr), 6))

(k29w3c37_tpr <- round(sum(cm_c37_tables[1, 291:300]) / (sum(cm_c37_tables[1, 291:300]) + sum(cm_c37_tables[2, 291:300])), 6))
(k29w3c37_tnr <- round(sum(cm_c37_tables[4, 291:300]) / (sum(cm_c37_tables[4, 291:300]) + sum(cm_c37_tables[3, 291:300])), 6))
(k29w3c37_truescore <- round((2 * k29w3c37_tpr * k29w3c37_tnr) / (k29w3c37_tpr + k29w3c37_tnr), 6))


(k31w1c37_tpr <- round(sum(cm_c37_tables[1, 301:310]) / (sum(cm_c37_tables[1, 301:310]) + sum(cm_c37_tables[2, 301:310])), 6))
(k31w1c37_tnr <- round(sum(cm_c37_tables[4, 301:310]) / (sum(cm_c37_tables[4, 301:310]) + sum(cm_c37_tables[3, 301:310])), 6))
(k31w1c37_truescore <- round((2 * k31w1c37_tpr * k31w1c37_tnr) / (k31w1c37_tpr + k31w1c37_tnr), 6))

(k31w2c37_tpr <- round(sum(cm_c37_tables[1, 311:320]) / (sum(cm_c37_tables[1, 311:320]) + sum(cm_c37_tables[2, 311:320])), 6))
(k31w2c37_tnr <- round(sum(cm_c37_tables[4, 311:320]) / (sum(cm_c37_tables[4, 311:320]) + sum(cm_c37_tables[3, 311:320])), 6))
(k31w2c37_truescore <- round((2 * k31w2c37_tpr * k31w2c37_tnr) / (k31w2c37_tpr + k31w2c37_tnr), 6))

(k31w3c37_tpr <- round(sum(cm_c37_tables[1, 321:330]) / (sum(cm_c37_tables[1, 321:330]) + sum(cm_c37_tables[2, 321:330])), 6))
(k31w3c37_tnr <- round(sum(cm_c37_tables[4, 321:330]) / (sum(cm_c37_tables[4, 321:330]) + sum(cm_c37_tables[3, 321:330])), 6))
(k31w3c37_truescore <- round((2 * k31w3c37_tpr * k31w3c37_tnr) / (k31w3c37_tpr + k31w3c37_tnr), 6))


(k33w1c37_tpr <- round(sum(cm_c37_tables[1, 331:340]) / (sum(cm_c37_tables[1, 331:340]) + sum(cm_c37_tables[2, 331:340])), 6))
(k33w1c37_tnr <- round(sum(cm_c37_tables[4, 331:340]) / (sum(cm_c37_tables[4, 331:340]) + sum(cm_c37_tables[3, 331:340])), 6))
(k33w1c37_truescore <- round((2 * k33w1c37_tpr * k33w1c37_tnr) / (k33w1c37_tpr + k33w1c37_tnr), 6))

(k33w2c37_tpr <- round(sum(cm_c37_tables[1, 341:350]) / (sum(cm_c37_tables[1, 341:350]) + sum(cm_c37_tables[2, 341:350])), 6))
(k33w2c37_tnr <- round(sum(cm_c37_tables[4, 341:350]) / (sum(cm_c37_tables[4, 341:350]) + sum(cm_c37_tables[3, 341:350])), 6))
(k33w2c37_truescore <- round((2 * k33w2c37_tpr * k33w2c37_tnr) / (k33w2c37_tpr + k33w2c37_tnr), 6))

(k33w3c37_tpr <- round(sum(cm_c37_tables[1, 351:360]) / (sum(cm_c37_tables[1, 351:360]) + sum(cm_c37_tables[2, 351:360])), 6))
(k33w3c37_tnr <- round(sum(cm_c37_tables[4, 351:360]) / (sum(cm_c37_tables[4, 351:360]) + sum(cm_c37_tables[3, 351:360])), 6))
(k33w3c37_truescore <- round((2 * k33w3c37_tpr * k33w3c37_tnr) / (k33w3c37_tpr + k33w3c37_tnr), 6))


(k35w1c37_tpr <- round(sum(cm_c37_tables[1, 361:370]) / (sum(cm_c37_tables[1, 361:370]) + sum(cm_c37_tables[2, 361:370])), 6))
(k35w1c37_tnr <- round(sum(cm_c37_tables[4, 361:370]) / (sum(cm_c37_tables[4, 361:370]) + sum(cm_c37_tables[3, 361:370])), 6))
(k35w1c37_truescore <- round((2 * k35w1c37_tpr * k35w1c37_tnr) / (k35w1c37_tpr + k35w1c37_tnr), 6))

(k35w2c37_tpr <- round(sum(cm_c37_tables[1, 371:380]) / (sum(cm_c37_tables[1, 371:380]) + sum(cm_c37_tables[2, 371:380])), 6))
(k35w2c37_tnr <- round(sum(cm_c37_tables[4, 371:380]) / (sum(cm_c37_tables[4, 371:380]) + sum(cm_c37_tables[3, 371:380])), 6))
(k35w2c37_truescore <- round((2 * k35w2c37_tpr * k35w2c37_tnr) / (k35w2c37_tpr + k35w2c37_tnr), 6))

(k35w3c37_tpr <- round(sum(cm_c37_tables[1, 381:390]) / (sum(cm_c37_tables[1, 381:390]) + sum(cm_c37_tables[2, 381:390])), 6))
(k35w3c37_tnr <- round(sum(cm_c37_tables[4, 381:390]) / (sum(cm_c37_tables[4, 381:390]) + sum(cm_c37_tables[3, 381:390])), 6))
(k35w3c37_truescore <- round((2 * k35w3c37_tpr * k35w3c37_tnr) / (k35w3c37_tpr + k35w3c37_tnr), 6))


(k37w1c37_tpr <- round(sum(cm_c37_tables[1, 391:400]) / (sum(cm_c37_tables[1, 391:400]) + sum(cm_c37_tables[2, 391:400])), 6))
(k37w1c37_tnr <- round(sum(cm_c37_tables[4, 391:400]) / (sum(cm_c37_tables[4, 391:400]) + sum(cm_c37_tables[3, 391:400])), 6))
(k37w1c37_truescore <- round((2 * k37w1c37_tpr * k37w1c37_tnr) / (k37w1c37_tpr + k37w1c37_tnr), 6))

(k37w2c37_tpr <- round(sum(cm_c37_tables[1, 401:410]) / (sum(cm_c37_tables[1, 401:410]) + sum(cm_c37_tables[2, 401:410])), 6))
(k37w2c37_tnr <- round(sum(cm_c37_tables[4, 401:410]) / (sum(cm_c37_tables[4, 401:410]) + sum(cm_c37_tables[3, 401:410])), 6))
(k37w2c37_truescore <- round((2 * k37w2c37_tpr * k37w2c37_tnr) / (k37w2c37_tpr + k37w2c37_tnr), 6))

(k37w3c37_tpr <- round(sum(cm_c37_tables[1, 411:420]) / (sum(cm_c37_tables[1, 411:420]) + sum(cm_c37_tables[2, 411:420])), 6))
(k37w3c37_tnr <- round(sum(cm_c37_tables[4, 411:420]) / (sum(cm_c37_tables[4, 411:420]) + sum(cm_c37_tables[3, 411:420])), 6))
(k37w3c37_truescore <- round((2 * k37w3c37_tpr * k37w3c37_tnr) / (k37w3c37_tpr + k37w3c37_tnr), 6))


(k39w1c37_tpr <- round(sum(cm_c37_tables[1, 421:430]) / (sum(cm_c37_tables[1, 421:430]) + sum(cm_c37_tables[2, 421:430])), 6))
(k39w1c37_tnr <- round(sum(cm_c37_tables[4, 421:430]) / (sum(cm_c37_tables[4, 421:430]) + sum(cm_c37_tables[3, 421:430])), 6))
(k39w1c37_truescore <- round((2 * k39w1c37_tpr * k39w1c37_tnr) / (k39w1c37_tpr + k39w1c37_tnr), 6))

(k39w2c37_tpr <- round(sum(cm_c37_tables[1, 431:440]) / (sum(cm_c37_tables[1, 431:440]) + sum(cm_c37_tables[2, 431:440])), 6))
(k39w2c37_tnr <- round(sum(cm_c37_tables[4, 431:440]) / (sum(cm_c37_tables[4, 431:440]) + sum(cm_c37_tables[3, 431:440])), 6))
(k39w2c37_truescore <- round((2 * k39w2c37_tpr * k39w2c37_tnr) / (k39w2c37_tpr + k39w2c37_tnr), 6))

(k39w3c37_tpr <- round(sum(cm_c37_tables[1, 441:450]) / (sum(cm_c37_tables[1, 441:450]) + sum(cm_c37_tables[2, 441:450])), 6))
(k39w3c37_tnr <- round(sum(cm_c37_tables[4, 441:450]) / (sum(cm_c37_tables[4, 441:450]) + sum(cm_c37_tables[3, 441:450])), 6))
(k39w3c37_truescore <- round((2 * k39w3c37_tpr * k39w3c37_tnr) / (k39w3c37_tpr + k39w3c37_tnr), 6))


# Compile the 0.37 cutoff results in a table, and identify the combination of 
# of k and kernel that maximizes the Truescore.

c37_results <- tibble(k = c(11, 11, 11, 13, 13, 13, 15, 15, 15, 
                            17, 17, 17, 19, 19, 19, 21, 21, 21, 
                            23, 23, 23, 25, 25, 25, 27, 27, 27, 
                            29, 29, 29, 31, 31, 31, 33, 33, 33, 
                            35, 35, 35, 37, 37, 37, 39, 39, 39), 
                      Kernel = rep(c("triangular", "gaussian", "optimal"), 15), 
                      Cut = 0.37, 
                      TPR = c(k11w1c37_tpr, k11w2c37_tpr, k11w3c37_tpr, k13w1c37_tpr, 
                              k13w2c37_tpr, k13w3c37_tpr, k15w1c37_tpr, k15w2c37_tpr, 
                              k15w3c37_tpr, k17w1c37_tpr, k17w2c37_tpr, k17w3c37_tpr, 
                              k19w1c37_tpr, k19w2c37_tpr, k19w3c37_tpr, k21w1c37_tpr, 
                              k21w2c37_tpr, k21w3c37_tpr, k23w1c37_tpr, k23w2c37_tpr, 
                              k23w3c37_tpr, k25w1c37_tpr, k25w2c37_tpr, k25w3c37_tpr, 
                              k27w1c37_tpr, k27w2c37_tpr, k27w3c37_tpr, k29w1c37_tpr, 
                              k29w2c37_tpr, k29w3c37_tpr, k31w1c37_tpr, k31w2c37_tpr, 
                              k31w3c37_tpr, k33w1c37_tpr, k33w2c37_tpr, k33w3c37_tpr, 
                              k35w1c37_tpr, k35w2c37_tpr, k35w3c37_tpr, k37w1c37_tpr, 
                              k37w2c37_tpr, k37w3c37_tpr, k39w1c37_tpr, k39w2c37_tpr, 
                              k39w3c37_tpr), 
                      TNR = c(k11w1c37_tnr, k11w2c37_tnr, k11w3c37_tnr, k13w1c37_tnr, 
                              k13w2c37_tnr, k13w3c37_tnr, k15w1c37_tnr, k15w2c37_tnr, 
                              k15w3c37_tnr, k17w1c37_tnr, k17w2c37_tnr, k17w3c37_tnr, 
                              k19w1c37_tnr, k19w2c37_tnr, k19w3c37_tnr, k21w1c37_tnr, 
                              k21w2c37_tnr, k21w3c37_tnr, k23w1c37_tnr, k23w2c37_tnr, 
                              k23w3c37_tnr, k25w1c37_tnr, k25w2c37_tnr, k25w3c37_tnr, 
                              k27w1c37_tnr, k27w2c37_tnr, k27w3c37_tnr, k29w1c37_tnr, 
                              k29w2c37_tnr, k29w3c37_tnr, k31w1c37_tnr, k31w2c37_tnr, 
                              k31w3c37_tnr, k33w1c37_tnr, k33w2c37_tnr, k33w3c37_tnr, 
                              k35w1c37_tnr, k35w2c37_tnr, k35w3c37_tnr, k37w1c37_tnr, 
                              k37w2c37_tnr, k37w3c37_tnr, k39w1c37_tnr, k39w2c37_tnr, 
                              k39w3c37_tnr), 
                      Truescore = c(k11w1c37_truescore, k11w2c37_truescore, 
                                    k11w3c37_truescore, k13w1c37_truescore, 
                                    k13w2c37_truescore, k13w3c37_truescore, 
                                    k15w1c37_truescore, k15w2c37_truescore, 
                                    k15w3c37_truescore, k17w1c37_truescore, 
                                    k17w2c37_truescore, k17w3c37_truescore, 
                                    k19w1c37_truescore, k19w2c37_truescore, 
                                    k19w3c37_truescore, k21w1c37_truescore, 
                                    k21w2c37_truescore, k21w3c37_truescore, 
                                    k23w1c37_truescore, k23w2c37_truescore, 
                                    k23w3c37_truescore, k25w1c37_truescore, 
                                    k25w2c37_truescore, k25w3c37_truescore, 
                                    k27w1c37_truescore, k27w2c37_truescore, 
                                    k27w3c37_truescore, k29w1c37_truescore, 
                                    k29w2c37_truescore, k29w3c37_truescore, 
                                    k31w1c37_truescore, k31w2c37_truescore, 
                                    k31w3c37_truescore, k33w1c37_truescore, 
                                    k33w2c37_truescore, k33w3c37_truescore, 
                                    k35w1c37_truescore, k35w2c37_truescore, 
                                    k35w3c37_truescore, k37w1c37_truescore, 
                                    k37w2c37_truescore, k37w3c37_truescore, 
                                    k39w1c37_truescore, k39w2c37_truescore, 
                                    k39w3c37_truescore))

knitr::kable(c37_results[1:45, ], caption = "c37_results")

ggplot(c37_results, aes(k, Truescore, shape = Kernel, color = Kernel)) + 
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.37 (Truescore)")

# For the Cutoff of 0.37, compute the Minimum Distance to (0, 1) for each 
# combination of k and Kernel. 

c37_results <- c37_results %>% mutate(Distance = sqrt((1 - TPR)^2 + (1 - TNR)^2))

ggplot(c37_results, aes(k, Distance, shape = Kernel, color = Kernel)) +
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.37 (Distance)")

# For the Cutoff of 0.37, identify the optimal combination of values for k and Kernel 
# based on each of our assessment methods, Truescore and Mimimum Distance to (0, 1).

max(c37_results$Truescore)
(c37_opt_k_ts <- c37_results$k[which.max(c37_results$Truescore)])
(c37_opt_kernel_ts <- c37_results$Kernel[which.max(c37_results$Truescore)])
(c37_opt_cut_ts <- c37_results$Cut[which.max(c37_results$Truescore)])
(c37_opt_tpr_ts <- c37_results$TPR[which.max(c37_results$Truescore)])
(c37_opt_tnr_ts <- c37_results$TNR[which.max(c37_results$Truescore)])
(c37_opt_d_ts <- c37_results$Distance[which.max(c37_results$Truescore)])

min(c37_results$Distance)
(c37_opt_k_dist <- c37_results$k[which.min(c37_results$Distance)])
(c37_opt_kernel_dist <- c37_results$Kernel[which.min(c37_results$Distance)])
(c37_opt_cut_dist <- c37_results$Cut[which.min(c37_results$Distance)])  
(c37_opt_tpr_dist <- c37_results$TPR[which.min(c37_results$Distance)])
(c37_opt_tnr_dist <- c37_results$TNR[which.min(c37_results$Distance)])
(c37_opt_t_dist <- c37_results$Truescore[which.min(c37_results$Distance)])

############################
# 0.38 Cutoff
############################

# For the decision cutoff of 0.38, generate a confusion matrix for
# every combination of k (11:39 by two), kernel (1:3) and fold (1:10).

cm_c38 <- sapply(kwf_dfs_l, function(x) {
  ss <- subset(x, select = c(pred38, obs))
  confusionMatrix(ss$pred38, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

names(cm_c38) <- kwf_dfs_v

cm_c38_tables <- sapply(cm_c38, "[[", 2)
cm_c38_tables <- as_tibble(cm_c38_tables)

# For each combination of k and kernel, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each combination of k and kernel when the decision cutoff is 0.38.

(k11w1c38_tpr <- round(sum(cm_c38_tables[1, 1:10]) / (sum(cm_c38_tables[1, 1:10]) + sum(cm_c38_tables[2, 1:10])), 6))
(k11w1c38_tnr <- round(sum(cm_c38_tables[4, 1:10]) / (sum(cm_c38_tables[4, 1:10]) + sum(cm_c38_tables[3, 1:10])), 6))
(k11w1c38_truescore <- round((2 * k11w1c38_tpr * k11w1c38_tnr) / (k11w1c38_tpr + k11w1c38_tnr), 6))

(k11w2c38_tpr <- round(sum(cm_c38_tables[1, 11:20]) / (sum(cm_c38_tables[1, 11:20]) + sum(cm_c38_tables[2, 11:20])), 6))
(k11w2c38_tnr <- round(sum(cm_c38_tables[4, 11:20]) / (sum(cm_c38_tables[4, 11:20]) + sum(cm_c38_tables[3, 11:20])), 6))
(k11w2c38_truescore <- round((2 * k11w2c38_tpr * k11w2c38_tnr) / (k11w2c38_tpr + k11w2c38_tnr), 6))

(k11w3c38_tpr <- round(sum(cm_c38_tables[1, 21:30]) / (sum(cm_c38_tables[1, 21:30]) + sum(cm_c38_tables[2, 21:30])), 6))
(k11w3c38_tnr <- round(sum(cm_c38_tables[4, 21:30]) / (sum(cm_c38_tables[4, 21:30]) + sum(cm_c38_tables[3, 21:30])), 6))
(k11w3c38_truescore <- round((2 * k11w3c38_tpr * k11w3c38_tnr) / (k11w3c38_tpr + k11w3c38_tnr), 6))


(k13w1c38_tpr <- round(sum(cm_c38_tables[1, 31:40]) / (sum(cm_c38_tables[1, 31:40]) + sum(cm_c38_tables[2, 31:40])), 6))
(k13w1c38_tnr <- round(sum(cm_c38_tables[4, 31:40]) / (sum(cm_c38_tables[4, 31:40]) + sum(cm_c38_tables[3, 31:40])), 6))
(k13w1c38_truescore <- round((2 * k13w1c38_tpr * k13w1c38_tnr) / (k13w1c38_tpr + k13w1c38_tnr), 6))

(k13w2c38_tpr <- round(sum(cm_c38_tables[1, 41:50]) / (sum(cm_c38_tables[1, 41:50]) + sum(cm_c38_tables[2, 41:50])), 6))
(k13w2c38_tnr <- round(sum(cm_c38_tables[4, 41:50]) / (sum(cm_c38_tables[4, 41:50]) + sum(cm_c38_tables[3, 41:50])), 6))
(k13w2c38_truescore <- round((2 * k13w2c38_tpr * k13w2c38_tnr) / (k13w2c38_tpr + k13w2c38_tnr), 6))

(k13w3c38_tpr <- round(sum(cm_c38_tables[1, 51:60]) / (sum(cm_c38_tables[1, 51:60]) + sum(cm_c38_tables[2, 51:60])), 6))
(k13w3c38_tnr <- round(sum(cm_c38_tables[4, 51:60]) / (sum(cm_c38_tables[4, 51:60]) + sum(cm_c38_tables[3, 51:60])), 6))
(k13w3c38_truescore <- round((2 * k13w3c38_tpr * k13w3c38_tnr) / (k13w3c38_tpr + k13w3c38_tnr), 6))


(k15w1c38_tpr <- round(sum(cm_c38_tables[1, 61:70]) / (sum(cm_c38_tables[1, 61:70]) + sum(cm_c38_tables[2, 61:70])), 6))
(k15w1c38_tnr <- round(sum(cm_c38_tables[4, 61:70]) / (sum(cm_c38_tables[4, 61:70]) + sum(cm_c38_tables[3, 61:70])), 6))
(k15w1c38_truescore <- round((2 * k15w1c38_tpr * k15w1c38_tnr) / (k15w1c38_tpr + k15w1c38_tnr), 6))

(k15w2c38_tpr <- round(sum(cm_c38_tables[1, 71:80]) / (sum(cm_c38_tables[1, 71:80]) + sum(cm_c38_tables[2, 71:80])), 6))
(k15w2c38_tnr <- round(sum(cm_c38_tables[4, 71:80]) / (sum(cm_c38_tables[4, 71:80]) + sum(cm_c38_tables[3, 71:80])), 6))
(k15w2c38_truescore <- round((2 * k15w2c38_tpr * k15w2c38_tnr) / (k15w2c38_tpr + k15w2c38_tnr), 6))

(k15w3c38_tpr <- round(sum(cm_c38_tables[1, 81:90]) / (sum(cm_c38_tables[1, 81:90]) + sum(cm_c38_tables[2, 81:90])), 6))
(k15w3c38_tnr <- round(sum(cm_c38_tables[4, 81:90]) / (sum(cm_c38_tables[4, 81:90]) + sum(cm_c38_tables[3, 81:90])), 6))
(k15w3c38_truescore <- round((2 * k15w3c38_tpr * k15w3c38_tnr) / (k15w3c38_tpr + k15w3c38_tnr), 6))


(k17w1c38_tpr <- round(sum(cm_c38_tables[1, 91:100]) / (sum(cm_c38_tables[1, 91:100]) + sum(cm_c38_tables[2, 91:100])), 6))
(k17w1c38_tnr <- round(sum(cm_c38_tables[4, 91:100]) / (sum(cm_c38_tables[4, 91:100]) + sum(cm_c38_tables[3, 91:100])), 6))
(k17w1c38_truescore <- round((2 * k17w1c38_tpr * k17w1c38_tnr) / (k17w1c38_tpr + k17w1c38_tnr), 6))

(k17w2c38_tpr <- round(sum(cm_c38_tables[1, 101:110]) / (sum(cm_c38_tables[1, 101:110]) + sum(cm_c38_tables[2, 101:110])), 6))
(k17w2c38_tnr <- round(sum(cm_c38_tables[4, 101:110]) / (sum(cm_c38_tables[4, 101:110]) + sum(cm_c38_tables[3, 101:110])), 6))
(k17w2c38_truescore <- round((2 * k17w2c38_tpr * k17w2c38_tnr) / (k17w2c38_tpr + k17w2c38_tnr), 6))

(k17w3c38_tpr <- round(sum(cm_c38_tables[1, 111:120]) / (sum(cm_c38_tables[1, 111:120]) + sum(cm_c38_tables[2, 111:120])), 6))
(k17w3c38_tnr <- round(sum(cm_c38_tables[4, 111:120]) / (sum(cm_c38_tables[4, 111:120]) + sum(cm_c38_tables[3, 111:120])), 6))
(k17w3c38_truescore <- round((2 * k17w3c38_tpr * k17w3c38_tnr) / (k17w3c38_tpr + k17w3c38_tnr), 6))


(k19w1c38_tpr <- round(sum(cm_c38_tables[1, 121:130]) / (sum(cm_c38_tables[1, 121:130]) + sum(cm_c38_tables[2, 121:130])), 6))
(k19w1c38_tnr <- round(sum(cm_c38_tables[4, 121:130]) / (sum(cm_c38_tables[4, 121:130]) + sum(cm_c38_tables[3, 121:130])), 6))
(k19w1c38_truescore <- round((2 * k19w1c38_tpr * k19w1c38_tnr) / (k19w1c38_tpr + k19w1c38_tnr), 6))

(k19w2c38_tpr <- round(sum(cm_c38_tables[1, 131:140]) / (sum(cm_c38_tables[1, 131:140]) + sum(cm_c38_tables[2, 131:140])), 6))
(k19w2c38_tnr <- round(sum(cm_c38_tables[4, 131:140]) / (sum(cm_c38_tables[4, 131:140]) + sum(cm_c38_tables[3, 131:140])), 6))
(k19w2c38_truescore <- round((2 * k19w2c38_tpr * k19w2c38_tnr) / (k19w2c38_tpr + k19w2c38_tnr), 6))

(k19w3c38_tpr <- round(sum(cm_c38_tables[1, 141:150]) / (sum(cm_c38_tables[1, 141:150]) + sum(cm_c38_tables[2, 141:150])), 6))
(k19w3c38_tnr <- round(sum(cm_c38_tables[4, 141:150]) / (sum(cm_c38_tables[4, 141:150]) + sum(cm_c38_tables[3, 141:150])), 6))
(k19w3c38_truescore <- round((2 * k19w3c38_tpr * k19w3c38_tnr) / (k19w3c38_tpr + k19w3c38_tnr), 6))


(k21w1c38_tpr <- round(sum(cm_c38_tables[1, 151:160]) / (sum(cm_c38_tables[1, 151:160]) + sum(cm_c38_tables[2, 151:160])), 6))
(k21w1c38_tnr <- round(sum(cm_c38_tables[4, 151:160]) / (sum(cm_c38_tables[4, 151:160]) + sum(cm_c38_tables[3, 151:160])), 6))
(k21w1c38_truescore <- round((2 * k21w1c38_tpr * k21w1c38_tnr) / (k21w1c38_tpr + k21w1c38_tnr), 6))

(k21w2c38_tpr <- round(sum(cm_c38_tables[1, 161:170]) / (sum(cm_c38_tables[1, 161:170]) + sum(cm_c38_tables[2, 161:170])), 6))
(k21w2c38_tnr <- round(sum(cm_c38_tables[4, 161:170]) / (sum(cm_c38_tables[4, 161:170]) + sum(cm_c38_tables[3, 161:170])), 6))
(k21w2c38_truescore <- round((2 * k21w2c38_tpr * k21w2c38_tnr) / (k21w2c38_tpr + k21w2c38_tnr), 6))

(k21w3c38_tpr <- round(sum(cm_c38_tables[1, 171:180]) / (sum(cm_c38_tables[1, 171:180]) + sum(cm_c38_tables[2, 171:180])), 6))
(k21w3c38_tnr <- round(sum(cm_c38_tables[4, 171:180]) / (sum(cm_c38_tables[4, 171:180]) + sum(cm_c38_tables[3, 171:180])), 6))
(k21w3c38_truescore <- round((2 * k21w3c38_tpr * k21w3c38_tnr) / (k21w3c38_tpr + k21w3c38_tnr), 6))


(k23w1c38_tpr <- round(sum(cm_c38_tables[1, 181:190]) / (sum(cm_c38_tables[1, 181:190]) + sum(cm_c38_tables[2, 181:190])), 6))
(k23w1c38_tnr <- round(sum(cm_c38_tables[4, 181:190]) / (sum(cm_c38_tables[4, 181:190]) + sum(cm_c38_tables[3, 181:190])), 6))
(k23w1c38_truescore <- round((2 * k23w1c38_tpr * k23w1c38_tnr) / (k23w1c38_tpr + k23w1c38_tnr), 6))

(k23w2c38_tpr <- round(sum(cm_c38_tables[1, 191:200]) / (sum(cm_c38_tables[1, 191:200]) + sum(cm_c38_tables[2, 191:200])), 6))
(k23w2c38_tnr <- round(sum(cm_c38_tables[4, 191:200]) / (sum(cm_c38_tables[4, 191:200]) + sum(cm_c38_tables[3, 191:200])), 6))
(k23w2c38_truescore <- round((2 * k23w2c38_tpr * k23w2c38_tnr) / (k23w2c38_tpr + k23w2c38_tnr), 6))

(k23w3c38_tpr <- round(sum(cm_c38_tables[1, 201:210]) / (sum(cm_c38_tables[1, 201:210]) + sum(cm_c38_tables[2, 201:210])), 6))
(k23w3c38_tnr <- round(sum(cm_c38_tables[4, 201:210]) / (sum(cm_c38_tables[4, 201:210]) + sum(cm_c38_tables[3, 201:210])), 6))
(k23w3c38_truescore <- round((2 * k23w3c38_tpr * k23w3c38_tnr) / (k23w3c38_tpr + k23w3c38_tnr), 6))


(k25w1c38_tpr <- round(sum(cm_c38_tables[1, 211:220]) / (sum(cm_c38_tables[1, 211:220]) + sum(cm_c38_tables[2, 211:220])), 6))
(k25w1c38_tnr <- round(sum(cm_c38_tables[4, 211:220]) / (sum(cm_c38_tables[4, 211:220]) + sum(cm_c38_tables[3, 211:220])), 6))
(k25w1c38_truescore <- round((2 * k25w1c38_tpr * k25w1c38_tnr) / (k25w1c38_tpr + k25w1c38_tnr), 6))

(k25w2c38_tpr <- round(sum(cm_c38_tables[1, 221:230]) / (sum(cm_c38_tables[1, 221:230]) + sum(cm_c38_tables[2, 221:230])), 6))
(k25w2c38_tnr <- round(sum(cm_c38_tables[4, 221:230]) / (sum(cm_c38_tables[4, 221:230]) + sum(cm_c38_tables[3, 221:230])), 6))
(k25w2c38_truescore <- round((2 * k25w2c38_tpr * k25w2c38_tnr) / (k25w2c38_tpr + k25w2c38_tnr), 6))

(k25w3c38_tpr <- round(sum(cm_c38_tables[1, 231:240]) / (sum(cm_c38_tables[1, 231:240]) + sum(cm_c38_tables[2, 231:240])), 6))
(k25w3c38_tnr <- round(sum(cm_c38_tables[4, 231:240]) / (sum(cm_c38_tables[4, 231:240]) + sum(cm_c38_tables[3, 231:240])), 6))
(k25w3c38_truescore <- round((2 * k25w3c38_tpr * k25w3c38_tnr) / (k25w3c38_tpr + k25w3c38_tnr), 6))


(k27w1c38_tpr <- round(sum(cm_c38_tables[1, 241:250]) / (sum(cm_c38_tables[1, 241:250]) + sum(cm_c38_tables[2, 241:250])), 6))
(k27w1c38_tnr <- round(sum(cm_c38_tables[4, 241:250]) / (sum(cm_c38_tables[4, 241:250]) + sum(cm_c38_tables[3, 241:250])), 6))
(k27w1c38_truescore <- round((2 * k27w1c38_tpr * k27w1c38_tnr) / (k27w1c38_tpr + k27w1c38_tnr), 6))

(k27w2c38_tpr <- round(sum(cm_c38_tables[1, 251:260]) / (sum(cm_c38_tables[1, 251:260]) + sum(cm_c38_tables[2, 251:260])), 6))
(k27w2c38_tnr <- round(sum(cm_c38_tables[4, 251:260]) / (sum(cm_c38_tables[4, 251:260]) + sum(cm_c38_tables[3, 251:260])), 6))
(k27w2c38_truescore <- round((2 * k27w2c38_tpr * k27w2c38_tnr) / (k27w2c38_tpr + k27w2c38_tnr), 6))

(k27w3c38_tpr <- round(sum(cm_c38_tables[1, 261:270]) / (sum(cm_c38_tables[1, 261:270]) + sum(cm_c38_tables[2, 261:270])), 6))
(k27w3c38_tnr <- round(sum(cm_c38_tables[4, 261:270]) / (sum(cm_c38_tables[4, 261:270]) + sum(cm_c38_tables[3, 261:270])), 6))
(k27w3c38_truescore <- round((2 * k27w3c38_tpr * k27w3c38_tnr) / (k27w3c38_tpr + k27w3c38_tnr), 6))


(k29w1c38_tpr <- round(sum(cm_c38_tables[1, 271:280]) / (sum(cm_c38_tables[1, 271:280]) + sum(cm_c38_tables[2, 271:280])), 6))
(k29w1c38_tnr <- round(sum(cm_c38_tables[4, 271:280]) / (sum(cm_c38_tables[4, 271:280]) + sum(cm_c38_tables[3, 271:280])), 6))
(k29w1c38_truescore <- round((2 * k29w1c38_tpr * k29w1c38_tnr) / (k29w1c38_tpr + k29w1c38_tnr), 6))

(k29w2c38_tpr <- round(sum(cm_c38_tables[1, 281:290]) / (sum(cm_c38_tables[1, 281:290]) + sum(cm_c38_tables[2, 281:290])), 6))
(k29w2c38_tnr <- round(sum(cm_c38_tables[4, 281:290]) / (sum(cm_c38_tables[4, 281:290]) + sum(cm_c38_tables[3, 281:290])), 6))
(k29w2c38_truescore <- round((2 * k29w2c38_tpr * k29w2c38_tnr) / (k29w2c38_tpr + k29w2c38_tnr), 6))

(k29w3c38_tpr <- round(sum(cm_c38_tables[1, 291:300]) / (sum(cm_c38_tables[1, 291:300]) + sum(cm_c38_tables[2, 291:300])), 6))
(k29w3c38_tnr <- round(sum(cm_c38_tables[4, 291:300]) / (sum(cm_c38_tables[4, 291:300]) + sum(cm_c38_tables[3, 291:300])), 6))
(k29w3c38_truescore <- round((2 * k29w3c38_tpr * k29w3c38_tnr) / (k29w3c38_tpr + k29w3c38_tnr), 6))


(k31w1c38_tpr <- round(sum(cm_c38_tables[1, 301:310]) / (sum(cm_c38_tables[1, 301:310]) + sum(cm_c38_tables[2, 301:310])), 6))
(k31w1c38_tnr <- round(sum(cm_c38_tables[4, 301:310]) / (sum(cm_c38_tables[4, 301:310]) + sum(cm_c38_tables[3, 301:310])), 6))
(k31w1c38_truescore <- round((2 * k31w1c38_tpr * k31w1c38_tnr) / (k31w1c38_tpr + k31w1c38_tnr), 6))

(k31w2c38_tpr <- round(sum(cm_c38_tables[1, 311:320]) / (sum(cm_c38_tables[1, 311:320]) + sum(cm_c38_tables[2, 311:320])), 6))
(k31w2c38_tnr <- round(sum(cm_c38_tables[4, 311:320]) / (sum(cm_c38_tables[4, 311:320]) + sum(cm_c38_tables[3, 311:320])), 6))
(k31w2c38_truescore <- round((2 * k31w2c38_tpr * k31w2c38_tnr) / (k31w2c38_tpr + k31w2c38_tnr), 6))

(k31w3c38_tpr <- round(sum(cm_c38_tables[1, 321:330]) / (sum(cm_c38_tables[1, 321:330]) + sum(cm_c38_tables[2, 321:330])), 6))
(k31w3c38_tnr <- round(sum(cm_c38_tables[4, 321:330]) / (sum(cm_c38_tables[4, 321:330]) + sum(cm_c38_tables[3, 321:330])), 6))
(k31w3c38_truescore <- round((2 * k31w3c38_tpr * k31w3c38_tnr) / (k31w3c38_tpr + k31w3c38_tnr), 6))


(k33w1c38_tpr <- round(sum(cm_c38_tables[1, 331:340]) / (sum(cm_c38_tables[1, 331:340]) + sum(cm_c38_tables[2, 331:340])), 6))
(k33w1c38_tnr <- round(sum(cm_c38_tables[4, 331:340]) / (sum(cm_c38_tables[4, 331:340]) + sum(cm_c38_tables[3, 331:340])), 6))
(k33w1c38_truescore <- round((2 * k33w1c38_tpr * k33w1c38_tnr) / (k33w1c38_tpr + k33w1c38_tnr), 6))

(k33w2c38_tpr <- round(sum(cm_c38_tables[1, 341:350]) / (sum(cm_c38_tables[1, 341:350]) + sum(cm_c38_tables[2, 341:350])), 6))
(k33w2c38_tnr <- round(sum(cm_c38_tables[4, 341:350]) / (sum(cm_c38_tables[4, 341:350]) + sum(cm_c38_tables[3, 341:350])), 6))
(k33w2c38_truescore <- round((2 * k33w2c38_tpr * k33w2c38_tnr) / (k33w2c38_tpr + k33w2c38_tnr), 6))

(k33w3c38_tpr <- round(sum(cm_c38_tables[1, 351:360]) / (sum(cm_c38_tables[1, 351:360]) + sum(cm_c38_tables[2, 351:360])), 6))
(k33w3c38_tnr <- round(sum(cm_c38_tables[4, 351:360]) / (sum(cm_c38_tables[4, 351:360]) + sum(cm_c38_tables[3, 351:360])), 6))
(k33w3c38_truescore <- round((2 * k33w3c38_tpr * k33w3c38_tnr) / (k33w3c38_tpr + k33w3c38_tnr), 6))


(k35w1c38_tpr <- round(sum(cm_c38_tables[1, 361:370]) / (sum(cm_c38_tables[1, 361:370]) + sum(cm_c38_tables[2, 361:370])), 6))
(k35w1c38_tnr <- round(sum(cm_c38_tables[4, 361:370]) / (sum(cm_c38_tables[4, 361:370]) + sum(cm_c38_tables[3, 361:370])), 6))
(k35w1c38_truescore <- round((2 * k35w1c38_tpr * k35w1c38_tnr) / (k35w1c38_tpr + k35w1c38_tnr), 6))

(k35w2c38_tpr <- round(sum(cm_c38_tables[1, 371:380]) / (sum(cm_c38_tables[1, 371:380]) + sum(cm_c38_tables[2, 371:380])), 6))
(k35w2c38_tnr <- round(sum(cm_c38_tables[4, 371:380]) / (sum(cm_c38_tables[4, 371:380]) + sum(cm_c38_tables[3, 371:380])), 6))
(k35w2c38_truescore <- round((2 * k35w2c38_tpr * k35w2c38_tnr) / (k35w2c38_tpr + k35w2c38_tnr), 6))

(k35w3c38_tpr <- round(sum(cm_c38_tables[1, 381:390]) / (sum(cm_c38_tables[1, 381:390]) + sum(cm_c38_tables[2, 381:390])), 6))
(k35w3c38_tnr <- round(sum(cm_c38_tables[4, 381:390]) / (sum(cm_c38_tables[4, 381:390]) + sum(cm_c38_tables[3, 381:390])), 6))
(k35w3c38_truescore <- round((2 * k35w3c38_tpr * k35w3c38_tnr) / (k35w3c38_tpr + k35w3c38_tnr), 6))


(k37w1c38_tpr <- round(sum(cm_c38_tables[1, 391:400]) / (sum(cm_c38_tables[1, 391:400]) + sum(cm_c38_tables[2, 391:400])), 6))
(k37w1c38_tnr <- round(sum(cm_c38_tables[4, 391:400]) / (sum(cm_c38_tables[4, 391:400]) + sum(cm_c38_tables[3, 391:400])), 6))
(k37w1c38_truescore <- round((2 * k37w1c38_tpr * k37w1c38_tnr) / (k37w1c38_tpr + k37w1c38_tnr), 6))

(k37w2c38_tpr <- round(sum(cm_c38_tables[1, 401:410]) / (sum(cm_c38_tables[1, 401:410]) + sum(cm_c38_tables[2, 401:410])), 6))
(k37w2c38_tnr <- round(sum(cm_c38_tables[4, 401:410]) / (sum(cm_c38_tables[4, 401:410]) + sum(cm_c38_tables[3, 401:410])), 6))
(k37w2c38_truescore <- round((2 * k37w2c38_tpr * k37w2c38_tnr) / (k37w2c38_tpr + k37w2c38_tnr), 6))

(k37w3c38_tpr <- round(sum(cm_c38_tables[1, 411:420]) / (sum(cm_c38_tables[1, 411:420]) + sum(cm_c38_tables[2, 411:420])), 6))
(k37w3c38_tnr <- round(sum(cm_c38_tables[4, 411:420]) / (sum(cm_c38_tables[4, 411:420]) + sum(cm_c38_tables[3, 411:420])), 6))
(k37w3c38_truescore <- round((2 * k37w3c38_tpr * k37w3c38_tnr) / (k37w3c38_tpr + k37w3c38_tnr), 6))


(k39w1c38_tpr <- round(sum(cm_c38_tables[1, 421:430]) / (sum(cm_c38_tables[1, 421:430]) + sum(cm_c38_tables[2, 421:430])), 6))
(k39w1c38_tnr <- round(sum(cm_c38_tables[4, 421:430]) / (sum(cm_c38_tables[4, 421:430]) + sum(cm_c38_tables[3, 421:430])), 6))
(k39w1c38_truescore <- round((2 * k39w1c38_tpr * k39w1c38_tnr) / (k39w1c38_tpr + k39w1c38_tnr), 6))

(k39w2c38_tpr <- round(sum(cm_c38_tables[1, 431:440]) / (sum(cm_c38_tables[1, 431:440]) + sum(cm_c38_tables[2, 431:440])), 6))
(k39w2c38_tnr <- round(sum(cm_c38_tables[4, 431:440]) / (sum(cm_c38_tables[4, 431:440]) + sum(cm_c38_tables[3, 431:440])), 6))
(k39w2c38_truescore <- round((2 * k39w2c38_tpr * k39w2c38_tnr) / (k39w2c38_tpr + k39w2c38_tnr), 6))

(k39w3c38_tpr <- round(sum(cm_c38_tables[1, 441:450]) / (sum(cm_c38_tables[1, 441:450]) + sum(cm_c38_tables[2, 441:450])), 6))
(k39w3c38_tnr <- round(sum(cm_c38_tables[4, 441:450]) / (sum(cm_c38_tables[4, 441:450]) + sum(cm_c38_tables[3, 441:450])), 6))
(k39w3c38_truescore <- round((2 * k39w3c38_tpr * k39w3c38_tnr) / (k39w3c38_tpr + k39w3c38_tnr), 6))


# Compile the 0.38 cutoff results in a table, and identify the combination of 
# of k and kernel that maximizes the Truescore.

c38_results <- tibble(k = c(11, 11, 11, 13, 13, 13, 15, 15, 15, 
                            17, 17, 17, 19, 19, 19, 21, 21, 21, 
                            23, 23, 23, 25, 25, 25, 27, 27, 27, 
                            29, 29, 29, 31, 31, 31, 33, 33, 33, 
                            35, 35, 35, 37, 37, 37, 39, 39, 39), 
                      Kernel = rep(c("triangular", "gaussian", "optimal"), 15), 
                      Cut = 0.38, 
                      TPR = c(k11w1c38_tpr, k11w2c38_tpr, k11w3c38_tpr, k13w1c38_tpr, 
                              k13w2c38_tpr, k13w3c38_tpr, k15w1c38_tpr, k15w2c38_tpr, 
                              k15w3c38_tpr, k17w1c38_tpr, k17w2c38_tpr, k17w3c38_tpr, 
                              k19w1c38_tpr, k19w2c38_tpr, k19w3c38_tpr, k21w1c38_tpr, 
                              k21w2c38_tpr, k21w3c38_tpr, k23w1c38_tpr, k23w2c38_tpr, 
                              k23w3c38_tpr, k25w1c38_tpr, k25w2c38_tpr, k25w3c38_tpr, 
                              k27w1c38_tpr, k27w2c38_tpr, k27w3c38_tpr, k29w1c38_tpr, 
                              k29w2c38_tpr, k29w3c38_tpr, k31w1c38_tpr, k31w2c38_tpr, 
                              k31w3c38_tpr, k33w1c38_tpr, k33w2c38_tpr, k33w3c38_tpr, 
                              k35w1c38_tpr, k35w2c38_tpr, k35w3c38_tpr, k37w1c38_tpr, 
                              k37w2c38_tpr, k37w3c38_tpr, k39w1c38_tpr, k39w2c38_tpr, 
                              k39w3c38_tpr), 
                      TNR = c(k11w1c38_tnr, k11w2c38_tnr, k11w3c38_tnr, k13w1c38_tnr, 
                              k13w2c38_tnr, k13w3c38_tnr, k15w1c38_tnr, k15w2c38_tnr, 
                              k15w3c38_tnr, k17w1c38_tnr, k17w2c38_tnr, k17w3c38_tnr, 
                              k19w1c38_tnr, k19w2c38_tnr, k19w3c38_tnr, k21w1c38_tnr, 
                              k21w2c38_tnr, k21w3c38_tnr, k23w1c38_tnr, k23w2c38_tnr, 
                              k23w3c38_tnr, k25w1c38_tnr, k25w2c38_tnr, k25w3c38_tnr, 
                              k27w1c38_tnr, k27w2c38_tnr, k27w3c38_tnr, k29w1c38_tnr, 
                              k29w2c38_tnr, k29w3c38_tnr, k31w1c38_tnr, k31w2c38_tnr, 
                              k31w3c38_tnr, k33w1c38_tnr, k33w2c38_tnr, k33w3c38_tnr, 
                              k35w1c38_tnr, k35w2c38_tnr, k35w3c38_tnr, k37w1c38_tnr, 
                              k37w2c38_tnr, k37w3c38_tnr, k39w1c38_tnr, k39w2c38_tnr, 
                              k39w3c38_tnr), 
                      Truescore = c(k11w1c38_truescore, k11w2c38_truescore, 
                                    k11w3c38_truescore, k13w1c38_truescore, 
                                    k13w2c38_truescore, k13w3c38_truescore, 
                                    k15w1c38_truescore, k15w2c38_truescore, 
                                    k15w3c38_truescore, k17w1c38_truescore, 
                                    k17w2c38_truescore, k17w3c38_truescore, 
                                    k19w1c38_truescore, k19w2c38_truescore, 
                                    k19w3c38_truescore, k21w1c38_truescore, 
                                    k21w2c38_truescore, k21w3c38_truescore, 
                                    k23w1c38_truescore, k23w2c38_truescore, 
                                    k23w3c38_truescore, k25w1c38_truescore, 
                                    k25w2c38_truescore, k25w3c38_truescore, 
                                    k27w1c38_truescore, k27w2c38_truescore, 
                                    k27w3c38_truescore, k29w1c38_truescore, 
                                    k29w2c38_truescore, k29w3c38_truescore, 
                                    k31w1c38_truescore, k31w2c38_truescore, 
                                    k31w3c38_truescore, k33w1c38_truescore, 
                                    k33w2c38_truescore, k33w3c38_truescore, 
                                    k35w1c38_truescore, k35w2c38_truescore, 
                                    k35w3c38_truescore, k37w1c38_truescore, 
                                    k37w2c38_truescore, k37w3c38_truescore, 
                                    k39w1c38_truescore, k39w2c38_truescore, 
                                    k39w3c38_truescore))

knitr::kable(c38_results[1:45, ], caption = "c38_results")

ggplot(c38_results, aes(k, Truescore, shape = Kernel, color = Kernel)) + 
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.38 (Truescore)")

# For the Cutoff of 0.38, compute the Minimum Distance to (0, 1) for each 
# combination of k and Kernel. 

c38_results <- c38_results %>% mutate(Distance = sqrt((1 - TPR)^2 + (1 - TNR)^2))

ggplot(c38_results, aes(k, Distance, shape = Kernel, color = Kernel)) +
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.38 (Distance)")

# For the Cutoff of 0.38, identify the optimal combination of values for k and Kernel 
# based on each of our assessment methods, Truescore and Mimimum Distance to (0, 1).

max(c38_results$Truescore)
(c38_opt_k_ts <- c38_results$k[which.max(c38_results$Truescore)])
(c38_opt_kernel_ts <- c38_results$Kernel[which.max(c38_results$Truescore)])
(c38_opt_cut_ts <- c38_results$Cut[which.max(c38_results$Truescore)])
(c38_opt_tpr_ts <- c38_results$TPR[which.max(c38_results$Truescore)])
(c38_opt_tnr_ts <- c38_results$TNR[which.max(c38_results$Truescore)])
(c38_opt_d_ts <- c38_results$Distance[which.max(c38_results$Truescore)])

min(c38_results$Distance)
(c38_opt_k_dist <- c38_results$k[which.min(c38_results$Distance)])
(c38_opt_kernel_dist <- c38_results$Kernel[which.min(c38_results$Distance)])
(c38_opt_cut_dist <- c38_results$Cut[which.min(c38_results$Distance)])  
(c38_opt_tpr_dist <- c38_results$TPR[which.min(c38_results$Distance)])
(c38_opt_tnr_dist <- c38_results$TNR[which.min(c38_results$Distance)])
(c38_opt_t_dist <- c38_results$Truescore[which.min(c38_results$Distance)])

############################
# 0.39 Cutoff
############################

# For the decision cutoff of 0.39, generate a confusion matrix for
# every combination of k (11:39 by two), kernel (1:3) and fold (1:10).

cm_c39 <- sapply(kwf_dfs_l, function(x) {
  ss <- subset(x, select = c(pred39, obs))
  confusionMatrix(ss$pred39, ss$obs)
}, simplify = FALSE, USE.NAMES = TRUE)

rm(kwf_dfs_l)
gc(reset = TRUE)

names(cm_c39) <- kwf_dfs_v

cm_c39_tables <- sapply(cm_c39, "[[", 2)
cm_c39_tables <- as_tibble(cm_c39_tables)

# For each combination of k and kernel, sum the True Positives, False Negatives, 
# True Negatives, and False Positives respectively across all 10 
# folds. Then use these totals to compute the Truescore for 
# each combination of k and kernel when the decision cutoff is 0.39.

(k11w1c39_tpr <- round(sum(cm_c39_tables[1, 1:10]) / (sum(cm_c39_tables[1, 1:10]) + sum(cm_c39_tables[2, 1:10])), 6))
(k11w1c39_tnr <- round(sum(cm_c39_tables[4, 1:10]) / (sum(cm_c39_tables[4, 1:10]) + sum(cm_c39_tables[3, 1:10])), 6))
(k11w1c39_truescore <- round((2 * k11w1c39_tpr * k11w1c39_tnr) / (k11w1c39_tpr + k11w1c39_tnr), 6))

(k11w2c39_tpr <- round(sum(cm_c39_tables[1, 11:20]) / (sum(cm_c39_tables[1, 11:20]) + sum(cm_c39_tables[2, 11:20])), 6))
(k11w2c39_tnr <- round(sum(cm_c39_tables[4, 11:20]) / (sum(cm_c39_tables[4, 11:20]) + sum(cm_c39_tables[3, 11:20])), 6))
(k11w2c39_truescore <- round((2 * k11w2c39_tpr * k11w2c39_tnr) / (k11w2c39_tpr + k11w2c39_tnr), 6))

(k11w3c39_tpr <- round(sum(cm_c39_tables[1, 21:30]) / (sum(cm_c39_tables[1, 21:30]) + sum(cm_c39_tables[2, 21:30])), 6))
(k11w3c39_tnr <- round(sum(cm_c39_tables[4, 21:30]) / (sum(cm_c39_tables[4, 21:30]) + sum(cm_c39_tables[3, 21:30])), 6))
(k11w3c39_truescore <- round((2 * k11w3c39_tpr * k11w3c39_tnr) / (k11w3c39_tpr + k11w3c39_tnr), 6))


(k13w1c39_tpr <- round(sum(cm_c39_tables[1, 31:40]) / (sum(cm_c39_tables[1, 31:40]) + sum(cm_c39_tables[2, 31:40])), 6))
(k13w1c39_tnr <- round(sum(cm_c39_tables[4, 31:40]) / (sum(cm_c39_tables[4, 31:40]) + sum(cm_c39_tables[3, 31:40])), 6))
(k13w1c39_truescore <- round((2 * k13w1c39_tpr * k13w1c39_tnr) / (k13w1c39_tpr + k13w1c39_tnr), 6))

(k13w2c39_tpr <- round(sum(cm_c39_tables[1, 41:50]) / (sum(cm_c39_tables[1, 41:50]) + sum(cm_c39_tables[2, 41:50])), 6))
(k13w2c39_tnr <- round(sum(cm_c39_tables[4, 41:50]) / (sum(cm_c39_tables[4, 41:50]) + sum(cm_c39_tables[3, 41:50])), 6))
(k13w2c39_truescore <- round((2 * k13w2c39_tpr * k13w2c39_tnr) / (k13w2c39_tpr + k13w2c39_tnr), 6))

(k13w3c39_tpr <- round(sum(cm_c39_tables[1, 51:60]) / (sum(cm_c39_tables[1, 51:60]) + sum(cm_c39_tables[2, 51:60])), 6))
(k13w3c39_tnr <- round(sum(cm_c39_tables[4, 51:60]) / (sum(cm_c39_tables[4, 51:60]) + sum(cm_c39_tables[3, 51:60])), 6))
(k13w3c39_truescore <- round((2 * k13w3c39_tpr * k13w3c39_tnr) / (k13w3c39_tpr + k13w3c39_tnr), 6))


(k15w1c39_tpr <- round(sum(cm_c39_tables[1, 61:70]) / (sum(cm_c39_tables[1, 61:70]) + sum(cm_c39_tables[2, 61:70])), 6))
(k15w1c39_tnr <- round(sum(cm_c39_tables[4, 61:70]) / (sum(cm_c39_tables[4, 61:70]) + sum(cm_c39_tables[3, 61:70])), 6))
(k15w1c39_truescore <- round((2 * k15w1c39_tpr * k15w1c39_tnr) / (k15w1c39_tpr + k15w1c39_tnr), 6))

(k15w2c39_tpr <- round(sum(cm_c39_tables[1, 71:80]) / (sum(cm_c39_tables[1, 71:80]) + sum(cm_c39_tables[2, 71:80])), 6))
(k15w2c39_tnr <- round(sum(cm_c39_tables[4, 71:80]) / (sum(cm_c39_tables[4, 71:80]) + sum(cm_c39_tables[3, 71:80])), 6))
(k15w2c39_truescore <- round((2 * k15w2c39_tpr * k15w2c39_tnr) / (k15w2c39_tpr + k15w2c39_tnr), 6))

(k15w3c39_tpr <- round(sum(cm_c39_tables[1, 81:90]) / (sum(cm_c39_tables[1, 81:90]) + sum(cm_c39_tables[2, 81:90])), 6))
(k15w3c39_tnr <- round(sum(cm_c39_tables[4, 81:90]) / (sum(cm_c39_tables[4, 81:90]) + sum(cm_c39_tables[3, 81:90])), 6))
(k15w3c39_truescore <- round((2 * k15w3c39_tpr * k15w3c39_tnr) / (k15w3c39_tpr + k15w3c39_tnr), 6))


(k17w1c39_tpr <- round(sum(cm_c39_tables[1, 91:100]) / (sum(cm_c39_tables[1, 91:100]) + sum(cm_c39_tables[2, 91:100])), 6))
(k17w1c39_tnr <- round(sum(cm_c39_tables[4, 91:100]) / (sum(cm_c39_tables[4, 91:100]) + sum(cm_c39_tables[3, 91:100])), 6))
(k17w1c39_truescore <- round((2 * k17w1c39_tpr * k17w1c39_tnr) / (k17w1c39_tpr + k17w1c39_tnr), 6))

(k17w2c39_tpr <- round(sum(cm_c39_tables[1, 101:110]) / (sum(cm_c39_tables[1, 101:110]) + sum(cm_c39_tables[2, 101:110])), 6))
(k17w2c39_tnr <- round(sum(cm_c39_tables[4, 101:110]) / (sum(cm_c39_tables[4, 101:110]) + sum(cm_c39_tables[3, 101:110])), 6))
(k17w2c39_truescore <- round((2 * k17w2c39_tpr * k17w2c39_tnr) / (k17w2c39_tpr + k17w2c39_tnr), 6))

(k17w3c39_tpr <- round(sum(cm_c39_tables[1, 111:120]) / (sum(cm_c39_tables[1, 111:120]) + sum(cm_c39_tables[2, 111:120])), 6))
(k17w3c39_tnr <- round(sum(cm_c39_tables[4, 111:120]) / (sum(cm_c39_tables[4, 111:120]) + sum(cm_c39_tables[3, 111:120])), 6))
(k17w3c39_truescore <- round((2 * k17w3c39_tpr * k17w3c39_tnr) / (k17w3c39_tpr + k17w3c39_tnr), 6))


(k19w1c39_tpr <- round(sum(cm_c39_tables[1, 121:130]) / (sum(cm_c39_tables[1, 121:130]) + sum(cm_c39_tables[2, 121:130])), 6))
(k19w1c39_tnr <- round(sum(cm_c39_tables[4, 121:130]) / (sum(cm_c39_tables[4, 121:130]) + sum(cm_c39_tables[3, 121:130])), 6))
(k19w1c39_truescore <- round((2 * k19w1c39_tpr * k19w1c39_tnr) / (k19w1c39_tpr + k19w1c39_tnr), 6))

(k19w2c39_tpr <- round(sum(cm_c39_tables[1, 131:140]) / (sum(cm_c39_tables[1, 131:140]) + sum(cm_c39_tables[2, 131:140])), 6))
(k19w2c39_tnr <- round(sum(cm_c39_tables[4, 131:140]) / (sum(cm_c39_tables[4, 131:140]) + sum(cm_c39_tables[3, 131:140])), 6))
(k19w2c39_truescore <- round((2 * k19w2c39_tpr * k19w2c39_tnr) / (k19w2c39_tpr + k19w2c39_tnr), 6))

(k19w3c39_tpr <- round(sum(cm_c39_tables[1, 141:150]) / (sum(cm_c39_tables[1, 141:150]) + sum(cm_c39_tables[2, 141:150])), 6))
(k19w3c39_tnr <- round(sum(cm_c39_tables[4, 141:150]) / (sum(cm_c39_tables[4, 141:150]) + sum(cm_c39_tables[3, 141:150])), 6))
(k19w3c39_truescore <- round((2 * k19w3c39_tpr * k19w3c39_tnr) / (k19w3c39_tpr + k19w3c39_tnr), 6))


(k21w1c39_tpr <- round(sum(cm_c39_tables[1, 151:160]) / (sum(cm_c39_tables[1, 151:160]) + sum(cm_c39_tables[2, 151:160])), 6))
(k21w1c39_tnr <- round(sum(cm_c39_tables[4, 151:160]) / (sum(cm_c39_tables[4, 151:160]) + sum(cm_c39_tables[3, 151:160])), 6))
(k21w1c39_truescore <- round((2 * k21w1c39_tpr * k21w1c39_tnr) / (k21w1c39_tpr + k21w1c39_tnr), 6))

(k21w2c39_tpr <- round(sum(cm_c39_tables[1, 161:170]) / (sum(cm_c39_tables[1, 161:170]) + sum(cm_c39_tables[2, 161:170])), 6))
(k21w2c39_tnr <- round(sum(cm_c39_tables[4, 161:170]) / (sum(cm_c39_tables[4, 161:170]) + sum(cm_c39_tables[3, 161:170])), 6))
(k21w2c39_truescore <- round((2 * k21w2c39_tpr * k21w2c39_tnr) / (k21w2c39_tpr + k21w2c39_tnr), 6))

(k21w3c39_tpr <- round(sum(cm_c39_tables[1, 171:180]) / (sum(cm_c39_tables[1, 171:180]) + sum(cm_c39_tables[2, 171:180])), 6))
(k21w3c39_tnr <- round(sum(cm_c39_tables[4, 171:180]) / (sum(cm_c39_tables[4, 171:180]) + sum(cm_c39_tables[3, 171:180])), 6))
(k21w3c39_truescore <- round((2 * k21w3c39_tpr * k21w3c39_tnr) / (k21w3c39_tpr + k21w3c39_tnr), 6))


(k23w1c39_tpr <- round(sum(cm_c39_tables[1, 181:190]) / (sum(cm_c39_tables[1, 181:190]) + sum(cm_c39_tables[2, 181:190])), 6))
(k23w1c39_tnr <- round(sum(cm_c39_tables[4, 181:190]) / (sum(cm_c39_tables[4, 181:190]) + sum(cm_c39_tables[3, 181:190])), 6))
(k23w1c39_truescore <- round((2 * k23w1c39_tpr * k23w1c39_tnr) / (k23w1c39_tpr + k23w1c39_tnr), 6))

(k23w2c39_tpr <- round(sum(cm_c39_tables[1, 191:200]) / (sum(cm_c39_tables[1, 191:200]) + sum(cm_c39_tables[2, 191:200])), 6))
(k23w2c39_tnr <- round(sum(cm_c39_tables[4, 191:200]) / (sum(cm_c39_tables[4, 191:200]) + sum(cm_c39_tables[3, 191:200])), 6))
(k23w2c39_truescore <- round((2 * k23w2c39_tpr * k23w2c39_tnr) / (k23w2c39_tpr + k23w2c39_tnr), 6))

(k23w3c39_tpr <- round(sum(cm_c39_tables[1, 201:210]) / (sum(cm_c39_tables[1, 201:210]) + sum(cm_c39_tables[2, 201:210])), 6))
(k23w3c39_tnr <- round(sum(cm_c39_tables[4, 201:210]) / (sum(cm_c39_tables[4, 201:210]) + sum(cm_c39_tables[3, 201:210])), 6))
(k23w3c39_truescore <- round((2 * k23w3c39_tpr * k23w3c39_tnr) / (k23w3c39_tpr + k23w3c39_tnr), 6))


(k25w1c39_tpr <- round(sum(cm_c39_tables[1, 211:220]) / (sum(cm_c39_tables[1, 211:220]) + sum(cm_c39_tables[2, 211:220])), 6))
(k25w1c39_tnr <- round(sum(cm_c39_tables[4, 211:220]) / (sum(cm_c39_tables[4, 211:220]) + sum(cm_c39_tables[3, 211:220])), 6))
(k25w1c39_truescore <- round((2 * k25w1c39_tpr * k25w1c39_tnr) / (k25w1c39_tpr + k25w1c39_tnr), 6))

(k25w2c39_tpr <- round(sum(cm_c39_tables[1, 221:230]) / (sum(cm_c39_tables[1, 221:230]) + sum(cm_c39_tables[2, 221:230])), 6))
(k25w2c39_tnr <- round(sum(cm_c39_tables[4, 221:230]) / (sum(cm_c39_tables[4, 221:230]) + sum(cm_c39_tables[3, 221:230])), 6))
(k25w2c39_truescore <- round((2 * k25w2c39_tpr * k25w2c39_tnr) / (k25w2c39_tpr + k25w2c39_tnr), 6))

(k25w3c39_tpr <- round(sum(cm_c39_tables[1, 231:240]) / (sum(cm_c39_tables[1, 231:240]) + sum(cm_c39_tables[2, 231:240])), 6))
(k25w3c39_tnr <- round(sum(cm_c39_tables[4, 231:240]) / (sum(cm_c39_tables[4, 231:240]) + sum(cm_c39_tables[3, 231:240])), 6))
(k25w3c39_truescore <- round((2 * k25w3c39_tpr * k25w3c39_tnr) / (k25w3c39_tpr + k25w3c39_tnr), 6))


(k27w1c39_tpr <- round(sum(cm_c39_tables[1, 241:250]) / (sum(cm_c39_tables[1, 241:250]) + sum(cm_c39_tables[2, 241:250])), 6))
(k27w1c39_tnr <- round(sum(cm_c39_tables[4, 241:250]) / (sum(cm_c39_tables[4, 241:250]) + sum(cm_c39_tables[3, 241:250])), 6))
(k27w1c39_truescore <- round((2 * k27w1c39_tpr * k27w1c39_tnr) / (k27w1c39_tpr + k27w1c39_tnr), 6))

(k27w2c39_tpr <- round(sum(cm_c39_tables[1, 251:260]) / (sum(cm_c39_tables[1, 251:260]) + sum(cm_c39_tables[2, 251:260])), 6))
(k27w2c39_tnr <- round(sum(cm_c39_tables[4, 251:260]) / (sum(cm_c39_tables[4, 251:260]) + sum(cm_c39_tables[3, 251:260])), 6))
(k27w2c39_truescore <- round((2 * k27w2c39_tpr * k27w2c39_tnr) / (k27w2c39_tpr + k27w2c39_tnr), 6))

(k27w3c39_tpr <- round(sum(cm_c39_tables[1, 261:270]) / (sum(cm_c39_tables[1, 261:270]) + sum(cm_c39_tables[2, 261:270])), 6))
(k27w3c39_tnr <- round(sum(cm_c39_tables[4, 261:270]) / (sum(cm_c39_tables[4, 261:270]) + sum(cm_c39_tables[3, 261:270])), 6))
(k27w3c39_truescore <- round((2 * k27w3c39_tpr * k27w3c39_tnr) / (k27w3c39_tpr + k27w3c39_tnr), 6))


(k29w1c39_tpr <- round(sum(cm_c39_tables[1, 271:280]) / (sum(cm_c39_tables[1, 271:280]) + sum(cm_c39_tables[2, 271:280])), 6))
(k29w1c39_tnr <- round(sum(cm_c39_tables[4, 271:280]) / (sum(cm_c39_tables[4, 271:280]) + sum(cm_c39_tables[3, 271:280])), 6))
(k29w1c39_truescore <- round((2 * k29w1c39_tpr * k29w1c39_tnr) / (k29w1c39_tpr + k29w1c39_tnr), 6))

(k29w2c39_tpr <- round(sum(cm_c39_tables[1, 281:290]) / (sum(cm_c39_tables[1, 281:290]) + sum(cm_c39_tables[2, 281:290])), 6))
(k29w2c39_tnr <- round(sum(cm_c39_tables[4, 281:290]) / (sum(cm_c39_tables[4, 281:290]) + sum(cm_c39_tables[3, 281:290])), 6))
(k29w2c39_truescore <- round((2 * k29w2c39_tpr * k29w2c39_tnr) / (k29w2c39_tpr + k29w2c39_tnr), 6))

(k29w3c39_tpr <- round(sum(cm_c39_tables[1, 291:300]) / (sum(cm_c39_tables[1, 291:300]) + sum(cm_c39_tables[2, 291:300])), 6))
(k29w3c39_tnr <- round(sum(cm_c39_tables[4, 291:300]) / (sum(cm_c39_tables[4, 291:300]) + sum(cm_c39_tables[3, 291:300])), 6))
(k29w3c39_truescore <- round((2 * k29w3c39_tpr * k29w3c39_tnr) / (k29w3c39_tpr + k29w3c39_tnr), 6))


(k31w1c39_tpr <- round(sum(cm_c39_tables[1, 301:310]) / (sum(cm_c39_tables[1, 301:310]) + sum(cm_c39_tables[2, 301:310])), 6))
(k31w1c39_tnr <- round(sum(cm_c39_tables[4, 301:310]) / (sum(cm_c39_tables[4, 301:310]) + sum(cm_c39_tables[3, 301:310])), 6))
(k31w1c39_truescore <- round((2 * k31w1c39_tpr * k31w1c39_tnr) / (k31w1c39_tpr + k31w1c39_tnr), 6))

(k31w2c39_tpr <- round(sum(cm_c39_tables[1, 311:320]) / (sum(cm_c39_tables[1, 311:320]) + sum(cm_c39_tables[2, 311:320])), 6))
(k31w2c39_tnr <- round(sum(cm_c39_tables[4, 311:320]) / (sum(cm_c39_tables[4, 311:320]) + sum(cm_c39_tables[3, 311:320])), 6))
(k31w2c39_truescore <- round((2 * k31w2c39_tpr * k31w2c39_tnr) / (k31w2c39_tpr + k31w2c39_tnr), 6))

(k31w3c39_tpr <- round(sum(cm_c39_tables[1, 321:330]) / (sum(cm_c39_tables[1, 321:330]) + sum(cm_c39_tables[2, 321:330])), 6))
(k31w3c39_tnr <- round(sum(cm_c39_tables[4, 321:330]) / (sum(cm_c39_tables[4, 321:330]) + sum(cm_c39_tables[3, 321:330])), 6))
(k31w3c39_truescore <- round((2 * k31w3c39_tpr * k31w3c39_tnr) / (k31w3c39_tpr + k31w3c39_tnr), 6))


(k33w1c39_tpr <- round(sum(cm_c39_tables[1, 331:340]) / (sum(cm_c39_tables[1, 331:340]) + sum(cm_c39_tables[2, 331:340])), 6))
(k33w1c39_tnr <- round(sum(cm_c39_tables[4, 331:340]) / (sum(cm_c39_tables[4, 331:340]) + sum(cm_c39_tables[3, 331:340])), 6))
(k33w1c39_truescore <- round((2 * k33w1c39_tpr * k33w1c39_tnr) / (k33w1c39_tpr + k33w1c39_tnr), 6))

(k33w2c39_tpr <- round(sum(cm_c39_tables[1, 341:350]) / (sum(cm_c39_tables[1, 341:350]) + sum(cm_c39_tables[2, 341:350])), 6))
(k33w2c39_tnr <- round(sum(cm_c39_tables[4, 341:350]) / (sum(cm_c39_tables[4, 341:350]) + sum(cm_c39_tables[3, 341:350])), 6))
(k33w2c39_truescore <- round((2 * k33w2c39_tpr * k33w2c39_tnr) / (k33w2c39_tpr + k33w2c39_tnr), 6))

(k33w3c39_tpr <- round(sum(cm_c39_tables[1, 351:360]) / (sum(cm_c39_tables[1, 351:360]) + sum(cm_c39_tables[2, 351:360])), 6))
(k33w3c39_tnr <- round(sum(cm_c39_tables[4, 351:360]) / (sum(cm_c39_tables[4, 351:360]) + sum(cm_c39_tables[3, 351:360])), 6))
(k33w3c39_truescore <- round((2 * k33w3c39_tpr * k33w3c39_tnr) / (k33w3c39_tpr + k33w3c39_tnr), 6))


(k35w1c39_tpr <- round(sum(cm_c39_tables[1, 361:370]) / (sum(cm_c39_tables[1, 361:370]) + sum(cm_c39_tables[2, 361:370])), 6))
(k35w1c39_tnr <- round(sum(cm_c39_tables[4, 361:370]) / (sum(cm_c39_tables[4, 361:370]) + sum(cm_c39_tables[3, 361:370])), 6))
(k35w1c39_truescore <- round((2 * k35w1c39_tpr * k35w1c39_tnr) / (k35w1c39_tpr + k35w1c39_tnr), 6))

(k35w2c39_tpr <- round(sum(cm_c39_tables[1, 371:380]) / (sum(cm_c39_tables[1, 371:380]) + sum(cm_c39_tables[2, 371:380])), 6))
(k35w2c39_tnr <- round(sum(cm_c39_tables[4, 371:380]) / (sum(cm_c39_tables[4, 371:380]) + sum(cm_c39_tables[3, 371:380])), 6))
(k35w2c39_truescore <- round((2 * k35w2c39_tpr * k35w2c39_tnr) / (k35w2c39_tpr + k35w2c39_tnr), 6))

(k35w3c39_tpr <- round(sum(cm_c39_tables[1, 381:390]) / (sum(cm_c39_tables[1, 381:390]) + sum(cm_c39_tables[2, 381:390])), 6))
(k35w3c39_tnr <- round(sum(cm_c39_tables[4, 381:390]) / (sum(cm_c39_tables[4, 381:390]) + sum(cm_c39_tables[3, 381:390])), 6))
(k35w3c39_truescore <- round((2 * k35w3c39_tpr * k35w3c39_tnr) / (k35w3c39_tpr + k35w3c39_tnr), 6))


(k37w1c39_tpr <- round(sum(cm_c39_tables[1, 391:400]) / (sum(cm_c39_tables[1, 391:400]) + sum(cm_c39_tables[2, 391:400])), 6))
(k37w1c39_tnr <- round(sum(cm_c39_tables[4, 391:400]) / (sum(cm_c39_tables[4, 391:400]) + sum(cm_c39_tables[3, 391:400])), 6))
(k37w1c39_truescore <- round((2 * k37w1c39_tpr * k37w1c39_tnr) / (k37w1c39_tpr + k37w1c39_tnr), 6))

(k37w2c39_tpr <- round(sum(cm_c39_tables[1, 401:410]) / (sum(cm_c39_tables[1, 401:410]) + sum(cm_c39_tables[2, 401:410])), 6))
(k37w2c39_tnr <- round(sum(cm_c39_tables[4, 401:410]) / (sum(cm_c39_tables[4, 401:410]) + sum(cm_c39_tables[3, 401:410])), 6))
(k37w2c39_truescore <- round((2 * k37w2c39_tpr * k37w2c39_tnr) / (k37w2c39_tpr + k37w2c39_tnr), 6))

(k37w3c39_tpr <- round(sum(cm_c39_tables[1, 411:420]) / (sum(cm_c39_tables[1, 411:420]) + sum(cm_c39_tables[2, 411:420])), 6))
(k37w3c39_tnr <- round(sum(cm_c39_tables[4, 411:420]) / (sum(cm_c39_tables[4, 411:420]) + sum(cm_c39_tables[3, 411:420])), 6))
(k37w3c39_truescore <- round((2 * k37w3c39_tpr * k37w3c39_tnr) / (k37w3c39_tpr + k37w3c39_tnr), 6))


(k39w1c39_tpr <- round(sum(cm_c39_tables[1, 421:430]) / (sum(cm_c39_tables[1, 421:430]) + sum(cm_c39_tables[2, 421:430])), 6))
(k39w1c39_tnr <- round(sum(cm_c39_tables[4, 421:430]) / (sum(cm_c39_tables[4, 421:430]) + sum(cm_c39_tables[3, 421:430])), 6))
(k39w1c39_truescore <- round((2 * k39w1c39_tpr * k39w1c39_tnr) / (k39w1c39_tpr + k39w1c39_tnr), 6))

(k39w2c39_tpr <- round(sum(cm_c39_tables[1, 431:440]) / (sum(cm_c39_tables[1, 431:440]) + sum(cm_c39_tables[2, 431:440])), 6))
(k39w2c39_tnr <- round(sum(cm_c39_tables[4, 431:440]) / (sum(cm_c39_tables[4, 431:440]) + sum(cm_c39_tables[3, 431:440])), 6))
(k39w2c39_truescore <- round((2 * k39w2c39_tpr * k39w2c39_tnr) / (k39w2c39_tpr + k39w2c39_tnr), 6))

(k39w3c39_tpr <- round(sum(cm_c39_tables[1, 441:450]) / (sum(cm_c39_tables[1, 441:450]) + sum(cm_c39_tables[2, 441:450])), 6))
(k39w3c39_tnr <- round(sum(cm_c39_tables[4, 441:450]) / (sum(cm_c39_tables[4, 441:450]) + sum(cm_c39_tables[3, 441:450])), 6))
(k39w3c39_truescore <- round((2 * k39w3c39_tpr * k39w3c39_tnr) / (k39w3c39_tpr + k39w3c39_tnr), 6))

# Compile the 0.39 cutoff results in a table, and identify the combination of 
# of k and kernel that maximizes the Truescore.

c39_results <- tibble(k = c(11, 11, 11, 13, 13, 13, 15, 15, 15, 
                            17, 17, 17, 19, 19, 19, 21, 21, 21, 
                            23, 23, 23, 25, 25, 25, 27, 27, 27, 
                            29, 29, 29, 31, 31, 31, 33, 33, 33, 
                            35, 35, 35, 37, 37, 37, 39, 39, 39), 
                      Kernel = rep(c("triangular", "gaussian", "optimal"), 15), 
                      Cut = 0.39, 
                      TPR = c(k11w1c39_tpr, k11w2c39_tpr, k11w3c39_tpr, k13w1c39_tpr, 
                              k13w2c39_tpr, k13w3c39_tpr, k15w1c39_tpr, k15w2c39_tpr, 
                              k15w3c39_tpr, k17w1c39_tpr, k17w2c39_tpr, k17w3c39_tpr, 
                              k19w1c39_tpr, k19w2c39_tpr, k19w3c39_tpr, k21w1c39_tpr, 
                              k21w2c39_tpr, k21w3c39_tpr, k23w1c39_tpr, k23w2c39_tpr, 
                              k23w3c39_tpr, k25w1c39_tpr, k25w2c39_tpr, k25w3c39_tpr, 
                              k27w1c39_tpr, k27w2c39_tpr, k27w3c39_tpr, k29w1c39_tpr, 
                              k29w2c39_tpr, k29w3c39_tpr, k31w1c39_tpr, k31w2c39_tpr, 
                              k31w3c39_tpr, k33w1c39_tpr, k33w2c39_tpr, k33w3c39_tpr, 
                              k35w1c39_tpr, k35w2c39_tpr, k35w3c39_tpr, k37w1c39_tpr, 
                              k37w2c39_tpr, k37w3c39_tpr, k39w1c39_tpr, k39w2c39_tpr, 
                              k39w3c39_tpr), 
                      TNR = c(k11w1c39_tnr, k11w2c39_tnr, k11w3c39_tnr, k13w1c39_tnr, 
                              k13w2c39_tnr, k13w3c39_tnr, k15w1c39_tnr, k15w2c39_tnr, 
                              k15w3c39_tnr, k17w1c39_tnr, k17w2c39_tnr, k17w3c39_tnr, 
                              k19w1c39_tnr, k19w2c39_tnr, k19w3c39_tnr, k21w1c39_tnr, 
                              k21w2c39_tnr, k21w3c39_tnr, k23w1c39_tnr, k23w2c39_tnr, 
                              k23w3c39_tnr, k25w1c39_tnr, k25w2c39_tnr, k25w3c39_tnr, 
                              k27w1c39_tnr, k27w2c39_tnr, k27w3c39_tnr, k29w1c39_tnr, 
                              k29w2c39_tnr, k29w3c39_tnr, k31w1c39_tnr, k31w2c39_tnr, 
                              k31w3c39_tnr, k33w1c39_tnr, k33w2c39_tnr, k33w3c39_tnr, 
                              k35w1c39_tnr, k35w2c39_tnr, k35w3c39_tnr, k37w1c39_tnr, 
                              k37w2c39_tnr, k37w3c39_tnr, k39w1c39_tnr, k39w2c39_tnr, 
                              k39w3c39_tnr), 
                      Truescore = c(k11w1c39_truescore, k11w2c39_truescore, 
                                    k11w3c39_truescore, k13w1c39_truescore, 
                                    k13w2c39_truescore, k13w3c39_truescore, 
                                    k15w1c39_truescore, k15w2c39_truescore, 
                                    k15w3c39_truescore, k17w1c39_truescore, 
                                    k17w2c39_truescore, k17w3c39_truescore, 
                                    k19w1c39_truescore, k19w2c39_truescore, 
                                    k19w3c39_truescore, k21w1c39_truescore, 
                                    k21w2c39_truescore, k21w3c39_truescore, 
                                    k23w1c39_truescore, k23w2c39_truescore, 
                                    k23w3c39_truescore, k25w1c39_truescore, 
                                    k25w2c39_truescore, k25w3c39_truescore, 
                                    k27w1c39_truescore, k27w2c39_truescore, 
                                    k27w3c39_truescore, k29w1c39_truescore, 
                                    k29w2c39_truescore, k29w3c39_truescore, 
                                    k31w1c39_truescore, k31w2c39_truescore, 
                                    k31w3c39_truescore, k33w1c39_truescore, 
                                    k33w2c39_truescore, k33w3c39_truescore, 
                                    k35w1c39_truescore, k35w2c39_truescore, 
                                    k35w3c39_truescore, k37w1c39_truescore, 
                                    k37w2c39_truescore, k37w3c39_truescore, 
                                    k39w1c39_truescore, k39w2c39_truescore, 
                                    k39w3c39_truescore))

knitr::kable(c39_results[1:45, ], caption = "c39_results")

ggplot(c39_results, aes(k, Truescore, shape = Kernel, color = Kernel)) + 
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.39 (Truescore)")

# For the Cutoff of 0.39, compute the Minimum Distance to (0, 1) for each 
# combination of k and Kernel. 

c39_results <- c39_results %>% mutate(Distance = sqrt((1 - TPR)^2 + (1 - TNR)^2))

ggplot(c39_results, aes(k, Distance, shape = Kernel, color = Kernel)) +
  geom_point() + geom_line() + 
  labs(title = "Optimal k and Kernel for Decision Cutoff 0.39 (Distance)")

# For the Cutoff of 0.39, identify the optimal combination of values for k and Kernel 
# based on each of our assessment methods, Truescore and Mimimum Distance to (0, 1).

max(c39_results$Truescore)
(c39_opt_k_ts <- c39_results$k[which.max(c39_results$Truescore)])
(c39_opt_kernel_ts <- c39_results$Kernel[which.max(c39_results$Truescore)])
(c39_opt_cut_ts <- c39_results$Cut[which.max(c39_results$Truescore)])
(c39_opt_tpr_ts <- c39_results$TPR[which.max(c39_results$Truescore)])
(c39_opt_tnr_ts <- c39_results$TNR[which.max(c39_results$Truescore)])
(c39_opt_d_ts <- c39_results$Distance[which.max(c39_results$Truescore)])

min(c39_results$Distance)
(c39_opt_k_dist <- c39_results$k[which.min(c39_results$Distance)])
(c39_opt_kernel_dist <- c39_results$Kernel[which.min(c39_results$Distance)])
(c39_opt_cut_dist <- c39_results$Cut[which.min(c39_results$Distance)])  
(c39_opt_tpr_dist <- c39_results$TPR[which.min(c39_results$Distance)])
(c39_opt_tnr_dist <- c39_results$TNR[which.min(c39_results$Distance)])
(c39_opt_t_dist <- c39_results$Truescore[which.min(c39_results$Distance)])

#################

# Compile the best performing models (based on Maximum Truescore and 
# Shortest Distance) for every Cutoff that was tested.

results_optkKernelcut <- tibble(
  Model = c("c25k27tri_ts", "c25k21tri_d", "c26k33tri_ts", "c26k25tri_d", 
            "c27k29gau_ts", "c27k29gau_d", "c28k23gau_ts", "c28k23gau_d", 
            "c29k31gau_ts", "c29k23gau_d", "c30k35tri_ts", "c30k31tri_d", 
            "c31k31gau_ts", "c31k31gau_d", "c32k31gau_ts", "c32k31gau_d", 
            "c33k35tri_ts", "c33k35tri_d", "c34k31gau_ts", "c34k31gau_d", 
            "c35k31gau_ts", "c35k31gau_d", "c36k31gau_ts", "c36k31gau_d", 
            "c37k31gau_ts", "c37k31gau_d", "c38k35tri_ts", "c38k35tri_d", 
            "c39k35opt_ts", "c39k35opt_d"), 
  Criterion = rep(c("Truescore", "Distance"), 15), 
  Cut = c(0.25, 0.25, 0.26, 0.26, 0.27, 0.27, 0.28, 0.28, 
          0.29, 0.29, 0.30, 0.30, 0.31, 0.31, 0.32, 0.32, 
          0.33, 0.33, 0.34, 0.34, 0.35, 0.35, 0.36, 0.36, 
          0.37, 0.37, 0.38, 0.38, 0.39, 0.39), 
  OptKs = c(c25_opt_k_ts, c25_opt_k_dist, c26_opt_k_ts, c26_opt_k_dist, 
            c27_opt_k_ts, c27_opt_k_dist, c28_opt_k_ts, c28_opt_k_dist, 
            c29_opt_k_ts, c29_opt_k_dist, c30_opt_k_ts, c30_opt_k_dist, 
            c31_opt_k_ts, c31_opt_k_dist, c32_opt_k_ts, c32_opt_k_dist, 
            c33_opt_k_ts, c33_opt_k_dist, c34_opt_k_ts, c34_opt_k_dist, 
            c35_opt_k_ts, c35_opt_k_dist, c36_opt_k_ts, c36_opt_k_dist, 
            c37_opt_k_ts, c37_opt_k_dist, c38_opt_k_ts, c38_opt_k_dist, 
            c39_opt_k_ts, c39_opt_k_dist), 
  OptKernels = c(c25_opt_kernel_ts, c25_opt_kernel_dist, c26_opt_kernel_ts, c26_opt_kernel_dist, 
                 c27_opt_kernel_ts, c27_opt_kernel_dist, c28_opt_kernel_ts, c28_opt_kernel_dist, 
                 c29_opt_kernel_ts, c29_opt_kernel_dist, c30_opt_kernel_ts, c30_opt_kernel_dist, 
                 c31_opt_kernel_ts, c31_opt_kernel_dist, c32_opt_kernel_ts, c32_opt_kernel_dist, 
                 c33_opt_kernel_ts, c33_opt_kernel_dist, c34_opt_kernel_ts, c34_opt_kernel_dist, 
                 c35_opt_kernel_ts, c35_opt_kernel_dist, c36_opt_kernel_ts, c36_opt_kernel_dist, 
                 c37_opt_kernel_ts, c37_opt_kernel_dist, c38_opt_kernel_ts, c38_opt_kernel_dist, 
                 c39_opt_kernel_ts, c39_opt_kernel_dist),  
  MaxTruescores = c(max(c25_results$Truescore), c25_opt_t_dist, 
                    max(c26_results$Truescore), c26_opt_t_dist, 
                    max(c27_results$Truescore), c27_opt_t_dist, 
                    max(c28_results$Truescore), c28_opt_t_dist, 
                    max(c29_results$Truescore), c29_opt_t_dist, 
                    max(c30_results$Truescore), c30_opt_t_dist, 
                    max(c31_results$Truescore), c31_opt_t_dist, 
                    max(c32_results$Truescore), c32_opt_t_dist, 
                    max(c33_results$Truescore), c33_opt_t_dist, 
                    max(c34_results$Truescore), c34_opt_t_dist, 
                    max(c35_results$Truescore), c35_opt_t_dist, 
                    max(c36_results$Truescore), c36_opt_t_dist, 
                    max(c37_results$Truescore), c37_opt_t_dist, 
                    max(c38_results$Truescore), c38_opt_t_dist, 
                    max(c39_results$Truescore), c39_opt_t_dist), 
  MinDistances = c(c25_opt_d_ts, min(c25_results$Distance), 
                   c26_opt_d_ts, min(c26_results$Distance), 
                   c27_opt_d_ts, min(c27_results$Distance), 
                   c28_opt_d_ts, min(c28_results$Distance), 
                   c29_opt_d_ts, min(c29_results$Distance), 
                   c30_opt_d_ts, min(c30_results$Distance), 
                   c31_opt_d_ts, min(c31_results$Distance), 
                   c32_opt_d_ts, min(c32_results$Distance), 
                   c33_opt_d_ts, min(c33_results$Distance), 
                   c34_opt_d_ts, min(c34_results$Distance), 
                   c35_opt_d_ts, min(c35_results$Distance), 
                   c36_opt_d_ts, min(c36_results$Distance), 
                   c37_opt_d_ts, min(c37_results$Distance), 
                   c38_opt_d_ts, min(c38_results$Distance), 
                   c39_opt_d_ts, min(c39_results$Distance)), 
  TPR = c(c25_opt_tpr_ts, c25_opt_tpr_dist, c26_opt_tpr_ts, c26_opt_tpr_dist, 
          c27_opt_tpr_ts, c27_opt_tpr_dist, c28_opt_tpr_ts, c28_opt_tpr_dist, 
          c29_opt_tpr_ts, c29_opt_tpr_dist, c30_opt_tpr_ts, c30_opt_tpr_dist, 
          c31_opt_tpr_ts, c31_opt_tpr_dist, c32_opt_tpr_ts, c32_opt_tpr_dist, 
          c33_opt_tpr_ts, c33_opt_tpr_dist, c34_opt_tpr_ts, c34_opt_tpr_dist, 
          c35_opt_tpr_ts, c35_opt_tpr_dist, c36_opt_tpr_ts, c36_opt_tpr_dist, 
          c37_opt_tpr_ts, c37_opt_tpr_dist, c38_opt_tpr_ts, c38_opt_tpr_dist, 
          c39_opt_tpr_ts, c39_opt_tpr_dist), 
  TNR = c(c25_opt_tnr_ts, c25_opt_tnr_dist, c26_opt_tnr_ts, c26_opt_tnr_dist, 
          c27_opt_tnr_ts, c27_opt_tnr_dist, c28_opt_tnr_ts, c28_opt_tnr_dist, 
          c29_opt_tnr_ts, c29_opt_tnr_dist, c30_opt_tnr_ts, c30_opt_tnr_dist, 
          c31_opt_tnr_ts, c31_opt_tnr_dist, c32_opt_tnr_ts, c32_opt_tnr_dist, 
          c33_opt_tnr_ts, c33_opt_tnr_dist, c34_opt_tnr_ts, c34_opt_tnr_dist, 
          c35_opt_tnr_ts, c35_opt_tnr_dist, c36_opt_tnr_ts, c36_opt_tnr_dist, 
          c37_opt_tnr_ts, c37_opt_tnr_dist, c38_opt_tnr_ts, c38_opt_tnr_dist, 
          c39_opt_tnr_ts, c39_opt_tnr_dist))

# Identify values of k, Kernel and Cutoff that result in the highest Truescore and shortest 
# Distance to (0, 1).

ggplot(results_optkKernelcut, aes(MaxTruescores, Model, color = OptKernels)) +
  geom_point() + 
  labs(title = "Best Performing Model (by Truescore) for Each Decision Cutoff")

ggplot(results_optkKernelcut, aes(MinDistances, Model, color = OptKernels)) +
  geom_point() + 
  labs(title = "Best Performing Model (by Distance) for Each Decision Cutoff")

max(results_optkKernelcut$MaxTruescores)
(kknn_opt_k <- results_optkKernelcut$OptKs[which.max(results_optkKernelcut$MaxTruescores)])
(kknn_opt_kernel <- results_optkKernelcut$OptKernels[which.max(results_optkKernelcut$MaxTruescores)])
(kknn_opt_cut <- results_optkKernelcut$Cut[which.max(results_optkKernelcut$MaxTruescores)])
(kknn_opt_tpr <- results_optkKernelcut$TPR[which.max(results_optkKernelcut$MaxTruescores)])
(kknn_opt_tnr <- results_optkKernelcut$TNR[which.max(results_optkKernelcut$MaxTruescores)])
(kknn_opt_d <- results_optkKernelcut$MinDistances[which.max(results_optkKernelcut$MaxTruescores)])

###############################

# Fit an optimized kknn model (k = 31, Kernel = "gaussian"(w2)) to the 
# entire predictor matrix and outcome vector (train_set2_orig). Then, 
# apply the kknn model (kknn_fitval_k31w2) and optimal cutoff of 0.31 
# to predict class probabilities based on the predictors in test_set2_orig.

train_set2_orig <- train_set[, c("launch_angle", "launch_speed", "spray_angle_Kolp", "spray_angle_adj", 
                                 "hp_to_1b", "num_if_alignment", "events")]
test_set2_orig <- test_set[, c("launch_angle", "launch_speed", "spray_angle_Kolp", "spray_angle_adj", 
                               "hp_to_1b", "num_if_alignment", "events")]

kknn_fitval_k31w2 <- kknn(formula = events ~ ., train = train_set2_orig, 
                          test = test_set2_orig, k = 31, kernel = "gaussian")

kknn_y_hat_prob_tbl <- as_tibble(kknn_fitval_k31w2$prob)
kknn_y_hat_prob_tbl <- kknn_y_hat_prob_tbl %>% 
  mutate(preds = ifelse(kknn_y_hat_prob_tbl$single > 0.31, "single", "field_out"))
kknn_y_hat_prob_tbl$preds <- factor(kknn_y_hat_prob_tbl$preds, levels = c("single", "field_out"))

# Assess the predictive ability of our weighted knn (kknn) model (kknn_fitval_k31w2c31).

confusionMatrix(data = kknn_y_hat_prob_tbl$preds, reference = test_set2_orig$events)
(kknn_k31w2c31_tpr <- round(sensitivity(kknn_y_hat_prob_tbl$preds, reference = factor(test_set2_orig$events)), 6))
(kknn_k31w2c31_tnr <- round(specificity(kknn_y_hat_prob_tbl$preds, reference = factor(test_set2_orig$events)), 6))
(kknn_k31w2c31_fpr <- round(1 - kknn_k31w2c31_tnr, 6))
(kknn_k31w2c31_truescore <- round((2 * kknn_k31w2c31_tpr * kknn_k31w2c31_tnr) / (kknn_k31w2c31_tpr + kknn_k31w2c31_tnr) , 6))
(kknn_k31w2c31_distance <- round(sqrt((1 - kknn_k31w2c31_tpr)^2 + (kknn_k31w2c31_fpr)^2), 6))

assessment_results <- tibble(Model = c("Baseline", "Log Regression", "Log Regression", 
                                       "knn_k5", "knn_k7", "knn_k7c40", "knn_k23c31", 
                                       "kknn_k31w2c31"), 
                             `Cutoff Method` = c(NA, "Truescore", "ROC Distance to (0,1)", 
                                                 "default (0.50)", "default (0.50)", 
                                                 "both", "both", "both"),
                             `Truescore` = c(bl_truescore, max_truescore, min_distance_truescore, 
                                             knn_k5_truescore, knn_k7_truescore, 
                                             knn_k7c40_truescore, knn_k23c31_truescore, 
                                             kknn_k31w2c31_truescore), 
                             TPR = c(bl_tpr, max_truescore_tpr, min_distance_tpr, 
                                     knn_k5_tpr, knn_k7_tpr, knn_k7c40_tpr, knn_k23c31_tpr, 
                                     kknn_k31w2c31_tpr),
                             TNR = c(bl_tnr, max_truescore_tnr, min_distance_tnr, 
                                     knn_k5_tnr, knn_k7_tnr, knn_k7c40_tnr, knn_k23c31_tnr, 
                                     kknn_k31w2c31_tnr), 
                             ROC_Distance = c(NA, max_truescore_distance, min_distance, 
                                              knn_k5_distance, knn_k7_distance, 
                                              knn_k7c40_distance, knn_k23c31_distance, 
                                              kknn_k31w2c31_distance), 
                             FPR = c((1 - bl_tnr), max_truescore_fpr, min_distance_fpr, 
                                     knn_k5_fpr, knn_k7_fpr, knn_k7c40_fpr, knn_k23c31_fpr, 
                                     kknn_k31w2c31_fpr), 
                             `Best Cutoff` = c(NA, max_truescore_cutoff, min_distance_cutoff, 
                                               "default", "default", knn_k7c40_cutoff, 
                                               knn_opt_cut, kknn_opt_cut))
knitr::kable(assessment_results[1:8, ], caption = "Assessment Results")

# ###############################################################
# # Manage Memory in R Before Proceeding
# ###############################################################
# 
# # Published in Jeromy Anglim's Blog: Psychology and Statistics, 
# # http://jeromyanglim.blogspot.com/2009/11/memory-management-in-r-few-tips-and.html
# 
# .ls.objects <- function (pos = 1, pattern, order.by = "Size", decreasing=TRUE, head = TRUE, n = 20) {
#   # based on postings by Petr Pikal and David Hinds to the r-help list in 2004
#   # modified by: Dirk Eddelbuettel (http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session) 
#   # I then gave it a few tweaks (show size as megabytes and use defaults that I like)
#   # a data frame of the objects and their associated storage needs.
#   napply <- function(names, fn) sapply(names, function(x)
#     fn(get(x, pos = pos)))
#   names <- ls(pos = pos, pattern = pattern)
#   obj.class <- napply(names, function(x) as.character(class(x))[1])
#   obj.mode <- napply(names, mode)
#   obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
#   obj.size <- napply(names, object.size) / 10^6 # megabytes
#   obj.dim <- t(napply(names, function(x)
#     as.numeric(dim(x))[1:2]))
#   vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
#   obj.dim[vec, 1] <- napply(names, length)[vec]
#   out <- data.frame(obj.type, obj.size, obj.dim)
#   names(out) <- c("Type", "Size", "Rows", "Columns")
#   out <- out[order(out[[order.by]], decreasing=decreasing), ]
#   if (head)
#     out <- head(out, n)
#   out
# }
# 
# .ls.objects()
# 
# rm(kknn_train_pred_tib)
# rm(kknn_train_pred_tib_f)
# rm(kwf_dfs_l)
# 
# gc(reset = TRUE)




#################################################
# Build and Assess a Classification Tree Model
#################################################

# Develop a classification tree with an optimal value for the 
# complexity parameter (cp). 

  # Build a complete tree with cp set to 0.

train_set3 <- dplyr::select(train_set2, events, launch_angle, launch_speed, 
                            spray_angle_Kolp, spray_angle_adj, if_fielding_alignment, hp_to_1b)

set.seed(1)
ctree_train <- rpart(events ~ ., method = "class", data = train_set3, 
                     parms = list(loss = matrix(c(0, 2, 1, 0), byrow = TRUE, nrow = 2)),  
                     control = rpart.control(xval = 10, minbucket = 2, cp = 0))

  # Determine the optimal value of cp.

plotcp(ctree_train)
ctree_train$cptable

    # Find the lowest estimated cross-validated error (xerror), along with
    # its standard error (xstd).

(cp_min_xerror_index <- which.min(ctree_train$cptable[,"xerror"]))
(cp_min_xerror <- ctree_train$cptable[cp_min_xerror_index, "xerror"])
(cp_min_xerror_xstd <- ctree_train$cptable[cp_min_xerror_index, "xstd"])

    # Identify the highest CP (the smallest tree) whose xerror is still within one
    # standard error of the lowest xerror (Breiman's "one standard error rule").

(cp_max_range_xerror <- cp_min_xerror + cp_min_xerror_xstd)
(cp_min_range_xerror <- cp_min_xerror - cp_min_xerror_xstd)
#cptable <- as.data.frame(ctree_train$cptable)
#cptable1 <- cptable %>% filter(xerror <= cp_max_range_xerror & xerror >= cp_min_range_xerror)
#(opt_cp <- max(cptable1[, 1]))
(opt_cp <- max(ctree_train$cptable[,"CP"][ctree_train$cptable[,"xerror"] < cp_max_range_xerror]))

  # Use opt_cp to prune our classification tree.

ctree_pruned_train <- prune(ctree_train, opt_cp)

  # Review and visualize our pruned classification tree object.

print(ctree_pruned_train)

prp(ctree_pruned_train)  # Once this plot is generated in Rstudio, the text can be made readable
                         # by exporting it and saving it as a pdf file that can be enlarged.

# Identify the optimal decision cutoff to be used for predicting the outcomes in 
# the test set.

  # Use our pruned classification tree and the train set to generate
  # probabilities and predict outcomes across a range of cutoffs (0.15 to 0.39).

ctree_pruned_probs <- predict(ctree_pruned_train, method = "prob") # Omitting the newdata argument is equivalent
                                                                   # to predicting against the train set.
ctree_pruned_probs <- as_tibble(ctree_pruned_probs)

ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred15 = ifelse(ctree_pruned_probs$single > 0.15, "single", "field_out")) 
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred16 = ifelse(ctree_pruned_probs$single > 0.16, "single", "field_out"))
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred17 = ifelse(ctree_pruned_probs$single > 0.17, "single", "field_out"))
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred18 = ifelse(ctree_pruned_probs$single > 0.18, "single", "field_out"))
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred19 = ifelse(ctree_pruned_probs$single > 0.19, "single", "field_out"))
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred20 = ifelse(ctree_pruned_probs$single > 0.20, "single", "field_out"))
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred21 = ifelse(ctree_pruned_probs$single > 0.21, "single", "field_out"))
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred22 = ifelse(ctree_pruned_probs$single > 0.22, "single", "field_out"))
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred23 = ifelse(ctree_pruned_probs$single > 0.23, "single", "field_out"))
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred24 = ifelse(ctree_pruned_probs$single > 0.24, "single", "field_out"))
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred25 = ifelse(ctree_pruned_probs$single > 0.25, "single", "field_out")) 
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred26 = ifelse(ctree_pruned_probs$single > 0.26, "single", "field_out"))
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred27 = ifelse(ctree_pruned_probs$single > 0.27, "single", "field_out"))
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred28 = ifelse(ctree_pruned_probs$single > 0.28, "single", "field_out"))
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred29 = ifelse(ctree_pruned_probs$single > 0.29, "single", "field_out"))
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred30 = ifelse(ctree_pruned_probs$single > 0.30, "single", "field_out"))
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred31 = ifelse(ctree_pruned_probs$single > 0.31, "single", "field_out"))
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred32 = ifelse(ctree_pruned_probs$single > 0.32, "single", "field_out"))
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred33 = ifelse(ctree_pruned_probs$single > 0.33, "single", "field_out"))
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred34 = ifelse(ctree_pruned_probs$single > 0.34, "single", "field_out"))
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred35 = ifelse(ctree_pruned_probs$single > 0.35, "single", "field_out"))
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred36 = ifelse(ctree_pruned_probs$single > 0.36, "single", "field_out"))
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred37 = ifelse(ctree_pruned_probs$single > 0.37, "single", "field_out"))
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred38 = ifelse(ctree_pruned_probs$single > 0.38, "single", "field_out"))
ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate(pred39 = ifelse(ctree_pruned_probs$single > 0.39, "single", "field_out"))

  # Convert all character columns to factors using dplyr package 
  # (https://gist.github.com/ramhiser/character2factor.r).

ctree_pruned_probs <- ctree_pruned_probs %>% 
  mutate_if(sapply(ctree_pruned_probs, is.character), as.factor)

sapply(ctree_pruned_probs, class)

  # Make sure all of the factor outcomes have the same levels in the same order.

ctree_pruned_probs$pred15 <- factor(ctree_pruned_probs$pred15, levels = c("single", "field_out"))
ctree_pruned_probs$pred16 <- factor(ctree_pruned_probs$pred16, levels = c("single", "field_out"))
ctree_pruned_probs$pred17 <- factor(ctree_pruned_probs$pred17, levels = c("single", "field_out"))
ctree_pruned_probs$pred18 <- factor(ctree_pruned_probs$pred18, levels = c("single", "field_out"))                                      
ctree_pruned_probs$pred19 <- factor(ctree_pruned_probs$pred19, levels = c("single", "field_out"))
ctree_pruned_probs$pred20 <- factor(ctree_pruned_probs$pred20, levels = c("single", "field_out"))   
ctree_pruned_probs$pred21 <- factor(ctree_pruned_probs$pred21, levels = c("single", "field_out"))                                      
ctree_pruned_probs$pred22 <- factor(ctree_pruned_probs$pred22, levels = c("single", "field_out"))
ctree_pruned_probs$pred23 <- factor(ctree_pruned_probs$pred23, levels = c("single", "field_out"))                                      
ctree_pruned_probs$pred24 <- factor(ctree_pruned_probs$pred24, levels = c("single", "field_out"))
ctree_pruned_probs$pred25 <- factor(ctree_pruned_probs$pred25, levels = c("single", "field_out"))
ctree_pruned_probs$pred26 <- factor(ctree_pruned_probs$pred26, levels = c("single", "field_out"))
ctree_pruned_probs$pred27 <- factor(ctree_pruned_probs$pred27, levels = c("single", "field_out"))
ctree_pruned_probs$pred28 <- factor(ctree_pruned_probs$pred28, levels = c("single", "field_out"))                                      
ctree_pruned_probs$pred29 <- factor(ctree_pruned_probs$pred29, levels = c("single", "field_out"))
ctree_pruned_probs$pred30 <- factor(ctree_pruned_probs$pred30, levels = c("single", "field_out"))   
ctree_pruned_probs$pred31 <- factor(ctree_pruned_probs$pred31, levels = c("single", "field_out"))                                      
ctree_pruned_probs$pred32 <- factor(ctree_pruned_probs$pred32, levels = c("single", "field_out"))
ctree_pruned_probs$pred33 <- factor(ctree_pruned_probs$pred33, levels = c("single", "field_out"))                                      
ctree_pruned_probs$pred34 <- factor(ctree_pruned_probs$pred34, levels = c("single", "field_out"))                                      
ctree_pruned_probs$pred35 <- factor(ctree_pruned_probs$pred35, levels = c("single", "field_out"))                                      
ctree_pruned_probs$pred36 <- factor(ctree_pruned_probs$pred36, levels = c("single", "field_out"))                                      
ctree_pruned_probs$pred37 <- factor(ctree_pruned_probs$pred37, levels = c("single", "field_out"))                                      
ctree_pruned_probs$pred38 <- factor(ctree_pruned_probs$pred38, levels = c("single", "field_out"))                                      
ctree_pruned_probs$pred39 <- factor(ctree_pruned_probs$pred39, levels = c("single", "field_out"))                                      
sapply(ctree_pruned_probs, levels)

  # Add a column to ctree_pruned_probs containing the actual outcomes

ctree_pruned_probs <- add_column(ctree_pruned_probs, obs = train_set3$events, .before = "single")

  # Compute the Truescore and Distance for each cutoff (0.15 to 0.39).

(c15_tpr <- sensitivity(ctree_pruned_probs$pred15, ctree_pruned_probs$obs))
(c15_tnr <- specificity(ctree_pruned_probs$pred15, ctree_pruned_probs$obs))
(c15_truescore <- round((2 * c15_tpr * c15_tnr) / (c15_tpr + c15_tnr), 6))
(c15_distance <- round(sqrt((1 - c15_tpr)^2 + (1 - c15_tnr)^2), 6))

(c16_tpr <- sensitivity(ctree_pruned_probs$pred16, ctree_pruned_probs$obs))
(c16_tnr <- specificity(ctree_pruned_probs$pred16, ctree_pruned_probs$obs))
(c16_truescore <- round((2 * c16_tpr * c16_tnr) / (c16_tpr + c16_tnr), 6))
(c16_distance <- round(sqrt((1 - c16_tpr)^2 + (1 - c16_tnr)^2), 6))

(c17_tpr <- sensitivity(ctree_pruned_probs$pred17, ctree_pruned_probs$obs))
(c17_tnr <- specificity(ctree_pruned_probs$pred17, ctree_pruned_probs$obs))
(c17_truescore <- round((2 * c17_tpr * c17_tnr) / (c17_tpr + c17_tnr), 6))
(c17_distance <- round(sqrt((1 - c17_tpr)^2 + (1 - c17_tnr)^2), 6))

(c18_tpr <- sensitivity(ctree_pruned_probs$pred18, ctree_pruned_probs$obs))
(c18_tnr <- specificity(ctree_pruned_probs$pred18, ctree_pruned_probs$obs))
(c18_truescore <- round((2 * c18_tpr * c18_tnr) / (c18_tpr + c18_tnr), 6))
(c18_distance <- round(sqrt((1 - c18_tpr)^2 + (1 - c18_tnr)^2), 6))

(c19_tpr <- sensitivity(ctree_pruned_probs$pred19, ctree_pruned_probs$obs))
(c19_tnr <- specificity(ctree_pruned_probs$pred19, ctree_pruned_probs$obs))
(c19_truescore <- round((2 * c19_tpr * c19_tnr) / (c19_tpr + c19_tnr), 6))
(c19_distance <- round(sqrt((1 - c19_tpr)^2 + (1 - c19_tnr)^2), 6))

(c20_tpr <- sensitivity(ctree_pruned_probs$pred20, ctree_pruned_probs$obs))
(c20_tnr <- specificity(ctree_pruned_probs$pred20, ctree_pruned_probs$obs))
(c20_truescore <- round((2 * c20_tpr * c20_tnr) / (c20_tpr + c20_tnr), 6))
(c20_distance <- round(sqrt((1 - c20_tpr)^2 + (1 - c20_tnr)^2), 6))

(c21_tpr <- sensitivity(ctree_pruned_probs$pred21, ctree_pruned_probs$obs))
(c21_tnr <- specificity(ctree_pruned_probs$pred21, ctree_pruned_probs$obs))
(c21_truescore <- round((2 * c21_tpr * c21_tnr) / (c21_tpr + c21_tnr), 6))
(c21_distance <- round(sqrt((1 - c21_tpr)^2 + (1 - c21_tnr)^2), 6))

(c22_tpr <- sensitivity(ctree_pruned_probs$pred22, ctree_pruned_probs$obs))
(c22_tnr <- specificity(ctree_pruned_probs$pred22, ctree_pruned_probs$obs))
(c22_truescore <- round((2 * c22_tpr * c22_tnr) / (c22_tpr + c22_tnr), 6))
(c22_distance <- round(sqrt((1 - c22_tpr)^2 + (1 - c22_tnr)^2), 6))

(c23_tpr <- sensitivity(ctree_pruned_probs$pred23, ctree_pruned_probs$obs))
(c23_tnr <- specificity(ctree_pruned_probs$pred23, ctree_pruned_probs$obs))
(c23_truescore <- round((2 * c23_tpr * c23_tnr) / (c23_tpr + c23_tnr), 6))
(c23_distance <- round(sqrt((1 - c23_tpr)^2 + (1 - c23_tnr)^2), 6))

(c24_tpr <- sensitivity(ctree_pruned_probs$pred24, ctree_pruned_probs$obs))
(c24_tnr <- specificity(ctree_pruned_probs$pred24, ctree_pruned_probs$obs))
(c24_truescore <- round((2 * c24_tpr * c24_tnr) / (c24_tpr + c24_tnr), 6))
(c24_distance <- round(sqrt((1 - c24_tpr)^2 + (1 - c24_tnr)^2), 6))

(c25_tpr <- sensitivity(ctree_pruned_probs$pred25, ctree_pruned_probs$obs))
(c25_tnr <- specificity(ctree_pruned_probs$pred25, ctree_pruned_probs$obs))
(c25_truescore <- round((2 * c25_tpr * c25_tnr) / (c25_tpr + c25_tnr), 6))
(c25_distance <- round(sqrt((1 - c25_tpr)^2 + (1 - c25_tnr)^2), 6))

(c26_tpr <- sensitivity(ctree_pruned_probs$pred26, ctree_pruned_probs$obs))
(c26_tnr <- specificity(ctree_pruned_probs$pred26, ctree_pruned_probs$obs))
(c26_truescore <- round((2 * c26_tpr * c26_tnr) / (c26_tpr + c26_tnr), 6))
(c26_distance <- round(sqrt((1 - c26_tpr)^2 + (1 - c26_tnr)^2), 6))

(c27_tpr <- sensitivity(ctree_pruned_probs$pred27, ctree_pruned_probs$obs))
(c27_tnr <- specificity(ctree_pruned_probs$pred27, ctree_pruned_probs$obs))
(c27_truescore <- round((2 * c27_tpr * c27_tnr) / (c27_tpr + c27_tnr), 6))
(c27_distance <- round(sqrt((1 - c27_tpr)^2 + (1 - c27_tnr)^2), 6))

(c28_tpr <- sensitivity(ctree_pruned_probs$pred28, ctree_pruned_probs$obs))
(c28_tnr <- specificity(ctree_pruned_probs$pred28, ctree_pruned_probs$obs))
(c28_truescore <- round((2 * c28_tpr * c28_tnr) / (c28_tpr + c28_tnr), 6))
(c28_distance <- round(sqrt((1 - c28_tpr)^2 + (1 - c28_tnr)^2), 6))

(c29_tpr <- sensitivity(ctree_pruned_probs$pred29, ctree_pruned_probs$obs))
(c29_tnr <- specificity(ctree_pruned_probs$pred29, ctree_pruned_probs$obs))
(c29_truescore <- round((2 * c29_tpr * c29_tnr) / (c29_tpr + c29_tnr), 6))
(c29_distance <- round(sqrt((1 - c29_tpr)^2 + (1 - c29_tnr)^2), 6))

(c30_tpr <- sensitivity(ctree_pruned_probs$pred30, ctree_pruned_probs$obs))
(c30_tnr <- specificity(ctree_pruned_probs$pred30, ctree_pruned_probs$obs))
(c30_truescore <- round((2 * c30_tpr * c30_tnr) / (c30_tpr + c30_tnr), 6))
(c30_distance <- round(sqrt((1 - c30_tpr)^2 + (1 - c30_tnr)^2), 6))

(c31_tpr <- sensitivity(ctree_pruned_probs$pred31, ctree_pruned_probs$obs))
(c31_tnr <- specificity(ctree_pruned_probs$pred31, ctree_pruned_probs$obs))
(c31_truescore <- round((2 * c31_tpr * c31_tnr) / (c31_tpr + c31_tnr), 6))
(c31_distance <- round(sqrt((1 - c31_tpr)^2 + (1 - c31_tnr)^2), 6))

(c32_tpr <- sensitivity(ctree_pruned_probs$pred32, ctree_pruned_probs$obs))
(c32_tnr <- specificity(ctree_pruned_probs$pred32, ctree_pruned_probs$obs))
(c32_truescore <- round((2 * c32_tpr * c32_tnr) / (c32_tpr + c32_tnr), 6))
(c32_distance <- round(sqrt((1 - c32_tpr)^2 + (1 - c32_tnr)^2), 6))

(c33_tpr <- sensitivity(ctree_pruned_probs$pred33, ctree_pruned_probs$obs))
(c33_tnr <- specificity(ctree_pruned_probs$pred33, ctree_pruned_probs$obs))
(c33_truescore <- round((2 * c33_tpr * c33_tnr) / (c33_tpr + c33_tnr), 6))
(c33_distance <- round(sqrt((1 - c33_tpr)^2 + (1 - c33_tnr)^2), 6))

(c34_tpr <- sensitivity(ctree_pruned_probs$pred34, ctree_pruned_probs$obs))
(c34_tnr <- specificity(ctree_pruned_probs$pred34, ctree_pruned_probs$obs))
(c34_truescore <- round((2 * c34_tpr * c34_tnr) / (c34_tpr + c34_tnr), 6))
(c34_distance <- round(sqrt((1 - c34_tpr)^2 + (1 - c34_tnr)^2), 6))

(c35_tpr <- sensitivity(ctree_pruned_probs$pred35, ctree_pruned_probs$obs))
(c35_tnr <- specificity(ctree_pruned_probs$pred35, ctree_pruned_probs$obs))
(c35_truescore <- round((2 * c35_tpr * c35_tnr) / (c35_tpr + c35_tnr), 6))
(c35_distance <- round(sqrt((1 - c35_tpr)^2 + (1 - c35_tnr)^2), 6))

(c36_tpr <- sensitivity(ctree_pruned_probs$pred36, ctree_pruned_probs$obs))
(c36_tnr <- specificity(ctree_pruned_probs$pred36, ctree_pruned_probs$obs))
(c36_truescore <- round((2 * c36_tpr * c36_tnr) / (c36_tpr + c36_tnr), 6))
(c36_distance <- round(sqrt((1 - c36_tpr)^2 + (1 - c36_tnr)^2), 6))

(c37_tpr <- sensitivity(ctree_pruned_probs$pred37, ctree_pruned_probs$obs))
(c37_tnr <- specificity(ctree_pruned_probs$pred37, ctree_pruned_probs$obs))
(c37_truescore <- round((2 * c37_tpr * c37_tnr) / (c37_tpr + c37_tnr), 6))
(c37_distance <- round(sqrt((1 - c37_tpr)^2 + (1 - c37_tnr)^2), 6))

(c38_tpr <- sensitivity(ctree_pruned_probs$pred38, ctree_pruned_probs$obs))
(c38_tnr <- specificity(ctree_pruned_probs$pred38, ctree_pruned_probs$obs))
(c38_truescore <- round((2 * c38_tpr * c38_tnr) / (c38_tpr + c38_tnr), 6))
(c38_distance <- round(sqrt((1 - c38_tpr)^2 + (1 - c38_tnr)^2), 6))

(c39_tpr <- sensitivity(ctree_pruned_probs$pred39, ctree_pruned_probs$obs))
(c39_tnr <- specificity(ctree_pruned_probs$pred39, ctree_pruned_probs$obs))
(c39_truescore <- round((2 * c39_tpr * c39_tnr) / (c39_tpr + c39_tnr), 6))
(c39_distance <- round(sqrt((1 - c39_tpr)^2 + (1 - c39_tnr)^2), 6))

  # Compile the cutoff results in a table.

cutoff_results <- tibble(cp = opt_cp, 
                         cut = seq(0.15, 0.39, 0.01), 
                         tpr = c(c15_tpr, c16_tpr, c17_tpr, c18_tpr, c19_tpr, 
                                 c20_tpr, c21_tpr, c22_tpr, c23_tpr, c24_tpr, 
                                 c25_tpr, c26_tpr, c27_tpr, c28_tpr, c29_tpr, 
                                 c30_tpr, c31_tpr, c32_tpr, c33_tpr, c34_tpr, 
                                 c35_tpr, c36_tpr, c37_tpr, c38_tpr, c39_tpr), 
                         tnr = c(c15_tnr, c16_tnr, c17_tnr, c18_tnr, c19_tnr, 
                                 c20_tnr, c21_tnr, c22_tnr, c23_tnr, c24_tnr, 
                                 c25_tnr, c26_tnr, c27_tnr, c28_tnr, c29_tnr, 
                                 c30_tnr, c31_tnr, c32_tnr, c33_tnr, c34_tnr, 
                                 c35_tnr, c36_tnr, c37_tnr, c38_tnr, c39_tnr), 
                         truescore = c(c15_truescore, c16_truescore, c17_truescore, 
                                       c18_truescore, c19_truescore, c20_truescore, 
                                       c21_truescore, c22_truescore, c23_truescore, 
                                       c24_truescore, c25_truescore, c26_truescore, 
                                       c27_truescore, c28_truescore, c29_truescore, 
                                       c30_truescore, c31_truescore, c32_truescore, 
                                       c33_truescore, c34_truescore, c35_truescore, 
                                       c36_truescore, c37_truescore, c38_truescore, 
                                       c39_truescore), 
                         distance = c(c15_distance, c16_distance, c17_distance, 
                                      c18_distance, c19_distance, c20_distance, 
                                      c21_distance, c22_distance, c23_distance, 
                                      c24_distance, c25_distance, c26_distance, 
                                      c27_distance, c28_distance, c29_distance, 
                                      c30_distance, c31_distance, c32_distance, 
                                      c33_distance, c34_distance, c35_distance, 
                                      c36_distance, c37_distance, c38_distance, 
                                      c39_distance))

knitr::kable(cutoff_results[1:25, ], caption = "Cutoff Results")

  # Identify the optimal cutoff based on each of our assessment methods, 
  # Truescore and Mimimum Distance to (0, 1).

ggplot(cutoff_results, aes(cut, truescore)) + 
  geom_point() + geom_line() + 
  labs(title = "Optimal cp and cutoff by Truescore")

ggplot(cutoff_results, aes(cut, distance)) + 
  geom_point() + geom_line() + 
  labs(title = "Optimal cp and cutoff by Distance")

max(cutoff_results$truescore)
(opt_cut_ts <- cutoff_results$cut[which.max(cutoff_results$truescore)])
(opt_tpr_ts <- cutoff_results$tpr[which.max(cutoff_results$truescore)])
(opt_tnr_ts <- cutoff_results$tnr[which.max(cutoff_results$truescore)])

min(cutoff_results$distance)
(opt_cut_dist <- cutoff_results$cut[which.min(cutoff_results$distance)])
(opt_tpr_dist <- cutoff_results$tpr[which.min(cutoff_results$distance)])
(opt_tnr_dist <- cutoff_results$tnr[which.min(cutoff_results$distance)])

ctree_opt_cut_ts <- opt_cut_ts
ctree_opt_cut_dist <- opt_cut_dist

# Use our pruned classification tree (constructed using a cp of 0.0001338688) 
# to predict probabilities based on our test set predictors.

test_set3 <- dplyr::select(test_set2, events, launch_angle, launch_speed, 
                           spray_angle_Kolp, spray_angle_adj, if_fielding_alignment, hp_to_1b)

ctree_pruned_test_preds <- predict(ctree_pruned_train, newdata = test_set3[, -1], method = "prob")

ctree_pruned_test_preds <- as_tibble(ctree_pruned_test_preds)

# Use our predicted probabilities for the test set to predict outcomes using 
# the optimal cutoff identified by our Maximum Truescore Method (0.22).

ctree_pruned_test_preds <- ctree_pruned_test_preds %>% 
  mutate(pred22 = ifelse(ctree_pruned_test_preds$single > 0.22, "single", "field_out"))

# Use our predicted probabilities for the test set to predict outcomes using 
# the optimal cutoff identified by our Minimum Distance Method (0.21).

ctree_pruned_test_preds <- ctree_pruned_test_preds %>% 
  mutate(pred21 = ifelse(ctree_pruned_test_preds$single > 0.21, "single", "field_out"))

# Convert all character columns to factors using dplyr package 
# (https://gist.github.com/ramhiser/character2factor.r). Make sure 
# the correct levels are ordered appropriately.

ctree_pruned_test_preds <- ctree_pruned_test_preds %>% 
  mutate_if(sapply(ctree_pruned_test_preds, is.character), as.factor)

sapply(ctree_pruned_test_preds, class)

ctree_pruned_test_preds$pred22 <- factor(ctree_pruned_test_preds$pred22, levels = c("single", "field_out"))
ctree_pruned_test_preds$pred21 <- factor(ctree_pruned_test_preds$pred21, levels = c("single", "field_out"))

(levels(ctree_pruned_test_preds$pred22))
(levels(ctree_pruned_test_preds$pred21))

# Assess the performance of our final pruned classification tree.

ctree_pruned_test_preds <- add_column(ctree_pruned_test_preds, obs = test_set3$events, .before = "single")

(ctree_pruned_c22_tpr <- sensitivity(ctree_pruned_test_preds$pred22, ctree_pruned_test_preds$obs))
(ctree_pruned_c22_tnr <- specificity(ctree_pruned_test_preds$pred22, ctree_pruned_test_preds$obs))
(ctree_pruned_c22_truescore <- round((2 * ctree_pruned_c22_tpr * ctree_pruned_c22_tnr) / 
                                       (ctree_pruned_c22_tpr + ctree_pruned_c22_tnr), 6))
(ctree_pruned_c22_distance <- round(sqrt((1 - ctree_pruned_c22_tpr)^2 + (1 - ctree_pruned_c22_tnr)^2), 6))

(ctree_pruned_c21_tpr <- sensitivity(ctree_pruned_test_preds$pred21, ctree_pruned_test_preds$obs))
(ctree_pruned_c21_tnr <- specificity(ctree_pruned_test_preds$pred21, ctree_pruned_test_preds$obs))
(ctree_pruned_c21_truescore <- round((2 * ctree_pruned_c21_tpr * ctree_pruned_c21_tnr) / 
                                       (ctree_pruned_c21_tpr + ctree_pruned_c21_tnr), 6))
(ctree_pruned_c21_distance <- round(sqrt((1 - ctree_pruned_c21_tpr)^2 + (1 - ctree_pruned_c21_tnr)^2), 6))

assessment_results <- tibble(Model = c("Baseline", "Log Regression", "Log Regression", 
                                       "kNN (k23c31)", "Wtd kNN (k31w2c31)", 
                                       "Classification Tree", "Classification Tree"),  
                             `Cutoff Method` = c(NA, "Truescore", "ROC Distance", "both", 
                                                 "both", "Truescore", "ROC Distance"),
                             `Truescore` = c(bl_truescore, max_truescore, 
                                             min_distance_truescore, 
                                             knn_k23c31_truescore, kknn_k31w2c31_truescore, 
                                             ctree_pruned_c22_truescore, ctree_pruned_c21_truescore), 
                             TPR = c(bl_tpr, max_truescore_tpr, min_distance_tpr, knn_k7c40_tpr, 
                                     knn_k23c31_tpr, ctree_pruned_c22_tpr, ctree_pruned_c21_tpr),
                             TNR = c(bl_tnr, max_truescore_tnr, min_distance_tnr, knn_k23c31_tnr, 
                                     kknn_k31w2c31_tnr, ctree_pruned_c22_tnr, ctree_pruned_c21_tnr), 
                             ROC_Distance = c(NA, max_truescore_distance, min_distance, 
                                              knn_k23c31_distance, kknn_k31w2c31_distance, 
                                              ctree_pruned_c22_distance, ctree_pruned_c21_distance), 
                             `Best Cutoff` = c(NA, max_truescore_cutoff, min_distance_cutoff, 
                                               knn_opt_cut, kknn_opt_cut, ctree_opt_cut_ts, 
                                               ctree_opt_cut_dist))
knitr::kable(assessment_results[1:7, ], caption = "Assessment Results")

# Review the Variable Importance computations from the summary() function, scaled to sum to 100.

summary(ctree_pruned_train, file = "ctree_pruned_train_summary.csv")

ctree_pruned_train_var_imp <- tibble(Predictor = c("launch_angle", "launch_speed", "spray_angle_Kolp", 
                                                   "spray_angle_adj", "if_fielding_alignment", "hp_to_1b"), 
                                     `ctree Importance` = c(48, 32, 4, 13, 2, "< 1"))
knitr::kable(ctree_pruned_train_var_imp[1:6, ], caption = "Variable Importance: Pruned Classification Tree")


#################################################
# Build and Assess a Random Forest Model
#################################################

# Use the randomForest function (randomForest package) to fit a 
# random forest model to our training data. Use the cutoff parameter to 
# test decision cutoffs ranging from 0.24 to 0.33. Determine the optimal 
# cutoff by using both our Maximum Truescore Method and our Minimum 
# Distance Method.

library(randomForest)

set.seed(1)
(rforest_train_c24 <- randomForest(events ~ ., data = train_set3, 
                                   ntree = 1000, mtry = 2, 
                                   cutoff = c("single" = 0.24, "field_out" = 0.76), 
                                   importance = TRUE))
plot(rforest_train_c24)
cm_c24 <- confusionMatrix(rforest_train_c24$predicted, train_set3$events)
(tpr_c24 <- cm_c24$byClass[[1]])
(tnr_c24 <- cm_c24$byClass[[2]])
(rf_c24_truescore <- round((2 * tpr_c24 * tnr_c24) / 
                             (tpr_c24 + tnr_c24), 6))
(rf_c24_distance <- round(sqrt((1 - tpr_c24)^2 + (1 - tnr_c24)^2), 6))


set.seed(1)
(rforest_train_c25 <- randomForest(events ~ ., data = train_set3, 
                                   ntree = 1000, mtry = 2, 
                                   cutoff = c("single" = 0.25, "field_out" = 0.75), 
                                   importance = TRUE))
plot(rforest_train_c25)
cm_c25 <- confusionMatrix(rforest_train_c25$predicted, train_set3$events)
(tpr_c25 <- cm_c25$byClass[[1]])
(tnr_c25 <- cm_c25$byClass[[2]])
(rf_c25_truescore <- round((2 * tpr_c25 * tnr_c25) / 
                             (tpr_c25 + tnr_c25), 6))
(rf_c25_distance <- round(sqrt((1 - tpr_c25)^2 + (1 - tnr_c25)^2), 6))


set.seed(1)
(rforest_train_c26 <- randomForest(events ~ ., data = train_set3, 
                                   ntree = 1000, mtry = 2, 
                                   cutoff = c("single" = 0.26, "field_out" = 0.74), 
                                   importance = TRUE))
plot(rforest_train_c26)
cm_c26 <- confusionMatrix(rforest_train_c26$predicted, train_set3$events)
(tpr_c26 <- cm_c26$byClass[[1]])
(tnr_c26 <- cm_c26$byClass[[2]])
(rf_c26_truescore <- round((2 * tpr_c26 * tnr_c26) / 
                             (tpr_c26 + tnr_c26), 6))
(rf_c26_distance <- round(sqrt((1 - tpr_c26)^2 + (1 - tnr_c26)^2), 6))


set.seed(1)
(rforest_train_c27 <- randomForest(events ~ ., data = train_set3, 
                               ntree = 1000, mtry = 2, 
                               cutoff = c("single" = 0.27, "field_out" = 0.73), 
                               importance = TRUE))
plot(rforest_train_c27)
cm_c27 <- confusionMatrix(rforest_train_c27$predicted, train_set3$events)
(tpr_c27 <- cm_c27$byClass[[1]])
(tnr_c27 <- cm_c27$byClass[[2]])
(rf_c27_truescore <- round((2 * tpr_c27 * tnr_c27) / 
                                       (tpr_c27 + tnr_c27), 6))
(rf_c27_distance <- round(sqrt((1 - tpr_c27)^2 + (1 - tnr_c27)^2), 6))


set.seed(1)
(rforest_train_c28 <- randomForest(events ~ ., data = train_set3, 
                               ntree = 1000, mtry = 2, 
                               cutoff = c("single" = 0.28, "field_out" = 0.72), 
                               importance = TRUE))
plot(rforest_train_c28)
cm_c28 <- confusionMatrix(rforest_train_c28$predicted, train_set3$events)
(tpr_c28 <- cm_c28$byClass[[1]])
(tnr_c28 <- cm_c28$byClass[[2]])
(rf_c28_truescore <- round((2 * tpr_c28 * tnr_c28) / 
                             (tpr_c28 + tnr_c28), 6))
(rf_c28_distance <- round(sqrt((1 - tpr_c28)^2 + (1 - tnr_c28)^2), 6))


set.seed(1)
(rforest_train_c29 <- randomForest(events ~ ., data = train_set3, 
                               ntree = 1000, mtry = 2, 
                               cutoff = c("single" = 0.29, "field_out" = 0.71), 
                               importance = TRUE))
plot(rforest_train_c29)
cm_c29 <- confusionMatrix(rforest_train_c29$predicted, train_set3$events)
(tpr_c29 <- cm_c29$byClass[[1]])
(tnr_c29 <- cm_c29$byClass[[2]])
(rf_c29_truescore <- round((2 * tpr_c29 * tnr_c29) / 
                             (tpr_c29 + tnr_c29), 6))
(rf_c29_distance <- round(sqrt((1 - tpr_c29)^2 + (1 - tnr_c29)^2), 6))


set.seed(1)
(rforest_train_c30 <- randomForest(events ~ ., data = train_set3, 
                               ntree = 1000, mtry = 2, 
                               cutoff = c("single" = 0.30, "field_out" = 0.70), 
                               importance = TRUE))
plot(rforest_train_c30)
cm_c30 <- confusionMatrix(rforest_train_c30$predicted, train_set3$events)
(tpr_c30 <- cm_c30$byClass[[1]])
(tnr_c30 <- cm_c30$byClass[[2]])
(rf_c30_truescore <- round((2 * tpr_c30 * tnr_c30) / 
                             (tpr_c30 + tnr_c30), 6))
(rf_c30_distance <- round(sqrt((1 - tpr_c30)^2 + (1 - tnr_c30)^2), 6))


set.seed(1)
(rforest_train_c31 <- randomForest(events ~ ., data = train_set3, 
                               ntree = 1000, mtry = 2, 
                               cutoff = c("single" = 0.31, "field_out" = 0.69), 
                               importance = TRUE))
plot(rforest_train_c31)
cm_c31 <- confusionMatrix(rforest_train_c31$predicted, train_set3$events)
(tpr_c31 <- cm_c31$byClass[[1]])
(tnr_c31 <- cm_c31$byClass[[2]])
(rf_c31_truescore <- round((2 * tpr_c31 * tnr_c31) / 
                             (tpr_c31 + tnr_c31), 6))
(rf_c31_distance <- round(sqrt((1 - tpr_c31)^2 + (1 - tnr_c31)^2), 6))


set.seed(1)
(rforest_train_c32 <- randomForest(events ~ ., data = train_set3, 
                               ntree = 1000, mtry = 2, 
                               cutoff = c("single" = 0.32, "field_out" = 0.68), 
                               importance = TRUE))
plot(rforest_train_c32)
cm_c32 <- confusionMatrix(rforest_train_c32$predicted, train_set3$events)
(tpr_c32 <- cm_c32$byClass[[1]])
(tnr_c32 <- cm_c32$byClass[[2]])
(rf_c32_truescore <- round((2 * tpr_c32 * tnr_c32) / 
                             (tpr_c32 + tnr_c32), 6))
(rf_c32_distance <- round(sqrt((1 - tpr_c32)^2 + (1 - tnr_c32)^2), 6))


set.seed(1)
(rforest_train_c33 <- randomForest(events ~ ., data = train_set3, 
                               ntree = 1000, mtry = 2, 
                               cutoff = c("single" = 0.33, "field_out" = 0.67), 
                               importance = TRUE))
plot(rforest_train_c33)
cm_c33 <- confusionMatrix(rforest_train_c33$predicted, train_set3$events)
(tpr_c33 <- cm_c33$byClass[[1]])
(tnr_c33 <- cm_c33$byClass[[2]])
(rf_c33_truescore <- round((2 * tpr_c33 * tnr_c33) / 
                             (tpr_c33 + tnr_c33), 6))
(rf_c33_distance <- round(sqrt((1 - tpr_c33)^2 + (1 - tnr_c33)^2), 6))

# Compile the cutoff results in a table.

rf_cutoff_results <- tibble(
                         cut = seq(0.24, 0.33, 0.01), 
                         tpr = c(tpr_c24, tpr_c25, tpr_c26, tpr_c27, tpr_c28, 
                                 tpr_c29, tpr_c30, tpr_c31, tpr_c32, tpr_c33), 
                         tnr = c(tnr_c24, tnr_c25, tnr_c26, tnr_c27, tnr_c28, 
                                 tnr_c29, tnr_c30, tnr_c31, tnr_c32, tnr_c33), 
                         truescore = c(rf_c24_truescore, rf_c25_truescore, 
                                       rf_c26_truescore, rf_c27_truescore, 
                                       rf_c28_truescore, rf_c29_truescore, 
                                       rf_c30_truescore, rf_c31_truescore, 
                                       rf_c32_truescore, rf_c33_truescore), 
                         distance = c(rf_c24_distance, rf_c25_distance, 
                                      rf_c26_distance, rf_c27_distance, 
                                      rf_c28_distance, rf_c29_distance, 
                                      rf_c30_distance, rf_c31_distance, 
                                      rf_c32_distance, rf_c33_distance))

knitr::kable(rf_cutoff_results[1:10, ], caption = "Random Forest Cutoff Results")

# Identify the optimal cutoff based on each of our assessment methods, 
# Truescore and Mimimum Distance to (0, 1).

ggplot(rf_cutoff_results, aes(cut, truescore)) + 
  geom_point() + geom_line() + 
  labs(title = "RF Optimal Cutoff by Truescore")

ggplot(rf_cutoff_results, aes(cut, distance)) + 
  geom_point() + geom_line() + 
  labs(title = "RF Optimal Cutoff by Distance")

(rf_opt_cut_ts <- rf_cutoff_results$cut[which.max(rf_cutoff_results$truescore)])
(rf_opt_cut_dist <- rf_cutoff_results$cut[which.min(rf_cutoff_results$distance)])

# Use our optimized randomForest algorithm (rforest_train_c30) to predict 
# "single" or "field_out" based on our test data.

rforest_predict <- predict(rforest_train_c30, newdata = test_set3, type = "response")
rforest_predict_prob <- predict(rforest_train_c30, newdata = test_set3, type = "prob")

# Assess the predictive ability of the randomForest algorithm.

confusionMatrix(rforest_predict, test_set3$events)
(rforest_tpr <- round(sensitivity(rforest_predict, reference = factor(test_set3$events)), 6))
(rforest_tnr <- round(specificity(rforest_predict, reference = factor(test_set3$events)), 6))
(rforest_truescore <- round((2 * rforest_tpr * rforest_tnr) / (rforest_tpr + rforest_tnr) , 6))
(rforest_distance <- round(sqrt((1 - rforest_tpr)^2 + (1 - rforest_tnr)^2), 6))

assessment_results <- tibble(Model = c("Baseline", "Log Regression", "Log Regression", 
                                       "kNN (k23c31)", "Wtd kNN (k31w2c31)", 
                                       "Classification Tree", "Classification Tree", "Random Forest"),  
                             `Cutoff Method` = c(NA, "Truescore", "ROC Distance", "both", 
                                                 "both", "Truescore", "ROC Distance", "both"),
                             `Truescore` = c(bl_truescore, max_truescore, 
                                             min_distance_truescore, 
                                             knn_k23c31_truescore, kknn_k31w2c31_truescore, 
                                             ctree_pruned_c22_truescore, ctree_pruned_c21_truescore, rforest_truescore), 
                             TPR = c(bl_tpr, max_truescore_tpr, min_distance_tpr, knn_k7c40_tpr, 
                                     knn_k23c31_tpr, ctree_pruned_c22_tpr, ctree_pruned_c21_tpr, rforest_tpr),
                             TNR = c(bl_tnr, max_truescore_tnr, min_distance_tnr, knn_k23c31_tnr, 
                                     kknn_k31w2c31_tnr, ctree_pruned_c22_tnr, ctree_pruned_c21_tnr, rforest_tnr), 
                             ROC_Distance = c(NA, max_truescore_distance, min_distance, 
                                              knn_k23c31_distance, kknn_k31w2c31_distance, 
                                              ctree_pruned_c22_distance, ctree_pruned_c21_distance, rforest_distance), 
                             `Best Cutoff` = c(NA, max_truescore_cutoff, min_distance_cutoff, 
                                               knn_opt_cut, kknn_opt_cut, ctree_opt_cut_ts, 
                                               ctree_opt_cut_dist, rf_opt_cut_ts))
knitr::kable(assessment_results[1:8, ], caption = "Assessment Results")

# Which variables did the randomForest algorithm consider to be the most important?

varImpPlot(rforest_train_c30, sort = TRUE, type = 2, main = "Importance of Variables")
importance(rforest_train_c30, type = 2)

# Compare variable importance as estimated by our classification tree model and our random forest model.

rforest_train_var_imp <- as_tibble(importance(rforest_train_c30, type = 2))
rforest_train_var_imp <- mutate(rforest_train_var_imp, rforest_imp = 
                                  round((MeanDecreaseGini / sum(rforest_train_var_imp$MeanDecreaseGini)) * 100))

variable_imp <- tibble(Predictor = c("launch_angle", "launch_speed", "spray_angle_Kolp", 
                                     "spray_angle_adj", "if_fielding_alignment", "hp_to_1b"), 
                       `ctree` = c(48, 32, 4, 13, 2, "< 1"), 
                       `rforest` = rforest_train_var_imp$rforest_imp)

knitr::kable(variable_imp[1:6, ], caption = "Variable Importance by Model")

##################################################
# Mistaken Predictions by rforest_train_c30 Model
##################################################

# Create a data frame showing all erroneous predictions by our 
# rforest_train_c30 algorithm, along with their potentially 
# relevant characteristics.

# Combine all predictions with actual outcomes.

rf_predict_prob_tbl <- as_tibble(rforest_predict_prob)
rf_predict_tbl <- as_tibble(enframe(rforest_predict))
rf_predict_results <- bind_cols(rf_predict_prob_tbl, rf_predict_tbl)
rf_predict_results <- rf_predict_results %>% 
  dplyr::mutate(obs_nmbr = name, prob_single = single, prob_out = field_out, pred = value) %>% 
  dplyr::select(obs_nmbr, prob_single, prob_out, pred)
rf_predict_results <- bind_cols(rf_predict_results, as_tibble(test_set3$events))
rf_predict_results <- dplyr::rename(rf_predict_results, obs = value)
rf_predict_results$prob_single <- as.numeric(rf_predict_results$prob_single)
rf_predict_results$prob_out <- as.numeric(rf_predict_results$prob_out)

# Identify the mistaken predictions.
rf_predict_results <- rf_predict_results %>% 
  mutate(eval = ifelse(rf_predict_results$pred !=  rf_predict_results$obs, "mistake", "-"))

# Quantify the magnitude of each prediction mistake.

rf_predict_results <- rf_predict_results %>% 
  mutate(prob_diff = abs(prob_single - prob_out))

# Add additional information about each batted ball event.

rf_predict_results <- bind_cols(rf_predict_results, test_set[, 1:38])
rf_predict_results <- dplyr::select(rf_predict_results, -(hc_x), -(hc_y))

# Add new categorical variables based on *spray_angle_Kolp* and *spray_angle_adj*. Include 
# the revised spray angle boundaries for the categories.

rf_predict_results <- rf_predict_results %>% 
  mutate(rev_spray_angle_Kolp_cat = cut(rf_predict_results$spray_angle_Kolp, 
                                    breaks = c(-90, -45.1, -40, -30, -23, -8, 8, 27, 34, 
                                               43, 45, 90), 
                                    labels = c("-90:-45.1", "LF Line (-45:-40)", 
                                               "3rd Baseman (-39.9:-30)", 
                                               "Hole 5-6 (-29.9:-23)", 
                                               "Shortstop (-22.9:-8)", 
                                               "Hole Middle (-7.9:8)", 
                                               "2nd Baseman (8.1:27)", 
                                               "Hole 3-4 (27.1:34)", 
                                               "1st Baseman (34.1:43)", 
                                               "RF Line (43.1:45)", "45.1:90")))

rf_predict_results <- rf_predict_results %>% 
  mutate(rev_spray_angle_adj_cat = cut(rf_predict_results$spray_angle_adj, 
                                   breaks = c(-90, -45.1, -43.1, -34.1, -27.1, -8.1, 8, 25, 35, 
                                              43, 45, 90), 
                                   labels = c("-90:-45.1", "Pulled Down Line (-45:-43.1)", 
                                              "Pulled Corner Inf (-43:-34.1)", 
                                              "Pulled Side Hole (-34:-27.1)", 
                                              "Pulled Mid Inf (-27:-8.1)", 
                                              "Middle Hole (-8:7.9)", 
                                              "Oppo Mid Inf (8:22.9)", 
                                              "Oppo Side Hole (23:29.9)", 
                                              "Oppo Corner Inf (30:39.9)", 
                                              "Oppo Down Line (40:45)", "45.1:90")))

# Reorder the columns in rf_predict_results.

rf_predict_results <- rf_predict_results %>%
  dplyr::select(obs_nmbr, prob_single, prob_out, pred, obs,	eval, prob_diff,
                launch_angle, launch_speed, spray_angle_Kolp, spray_angle_adj,
                if_fielding_alignment, hp_to_1b,
                game_date, batter, player_name, age, stand, position, team, events,
                des,  bb_type, adv_bb_type, launch_speed_cat,
                hc_x_Kolp, hc_y_Kolp, rev_spray_angle_Kolp_cat,
                rev_spray_angle_adj_cat, num_if_alignment,
                hp_to_1b_cat, of_fielding_alignment, hit_location, hit_distance_sc,
                launch_speed_angle, game_pk, game_year, game_type, home_team, away_team,
                pitcher, p_throws, description)

# Create a new data frame with just the mistaken predictions.

rf_predict_mistakes <- rf_predict_results %>% dplyr::filter(eval == "mistake")

# Create a new data frame with just the correct predictions.

rf_predict_correct <- rf_predict_results %>% dplyr::filter(eval != "mistake")

# Create a sample of correct predictions equal in size to the number of 
# mistaken predictions.

nrow(rf_predict_mistakes)
set.seed(1)
rf_predict_correct_sample_3087 <- sample_n(rf_predict_correct, 3087, replace = FALSE)

# Was it more common to mistakenly predict singles (false positive) 
# or outs (false negative)?

prop_false_pos <- mean(rf_predict_mistakes$pred == "single")
prop_false_neg <- mean(rf_predict_mistakes$pred == "field_out")

# Add a column to our data frame of prediction mistakes that describes the 
# type of mistake that was made.

rf_predict_mistakes <- rf_predict_mistakes %>% 
  mutate(eval_type = ifelse(pred == "single", "FalsePos", "FalseNeg"))
rf_predict_mistakes <- rf_predict_mistakes %>% 
  dplyr::select(1:6, eval_type, everything())

# Add a column to our data frame of correct predictions that describes the 
# type of correct prediction that was made.

rf_predict_correct_sample_3087 <- rf_predict_correct_sample_3087 %>% 
  mutate(eval_type = ifelse(pred == "single", "TruePos", "TrueNeg"))
rf_predict_correct_sample_3087 <- rf_predict_correct_sample_3087 %>% 
  dplyr::select(1:6, eval_type, everything())

# Combine our data frames of prediction mistakes and correct predictions.

predictions_6174 <- bind_rows(rf_predict_mistakes, rf_predict_correct_sample_3087)
# predictions_6174$eval_type <- factor(predictions_6174$eval_type, levels = c("TruePos", "FalsePos", "TrueNeg", "FalseNeg"))

# Visualize false positive and true positive predictions by rforest_train_c30 model.

Pos_predictions <- ggplot(dplyr::filter(predictions_6174, 
                     eval_type %in% c("FalsePos", "TruePos")), 
                     aes(hc_x_Kolp, hc_y_Kolp, color = eval_type)) + 
  scale_color_manual(values = c("royalblue3", "red")) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 5, height = 5), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 5, height = 5), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 5, height = 5), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = .9) + theme_linedraw() + 
  labs(title = "False Positive Predictions by rforest_train_c30 Model") + 
  theme(legend.position = c(.85, .2))

# Visualize false negative and true negative predictions by rforest_train_c30 model.

Neg_predictions <- ggplot(dplyr::filter(predictions_6174, 
                     eval_type %in% c("FalseNeg", "TrueNeg")), 
       aes(hc_x_Kolp, hc_y_Kolp, color = eval_type)) + 
  scale_color_manual(values = c("red", "royalblue3")) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 5, height = 5), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 5, height = 5), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 5, height = 5), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = .9) + theme_linedraw() + 
  labs(title = "False Negative Predictions by rforest_train_c30 Model") + 
  theme(legend.position = c(.85, .2))

grid.arrange(Pos_predictions, Neg_predictions, ncol = 2)

############### launch_angles of false positives

# Visualize false positive and true positive predictions by adv_bb_type.

pos_labels <- c("false positives", "true positives")
names(pos_labels) <- c("FalsePos", "TruePos")

ggplot(dplyr::filter(predictions_6174, eval_type %in% c("FalsePos", "TruePos")), 
       aes(hc_x_Kolp, hc_y_Kolp, color = eval_type)) + 
  scale_color_manual(values = c(FalsePos = "royalblue4", TruePos = "red3")) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = 0.7) + theme_linedraw() + 
  labs(title = "False Positives by adv_bb_type") + 
  facet_grid(adv_bb_type ~ eval_type, labeller = labeller(eval_type = pos_labels))

# Using boxplots, compare false positive and true positive predictions by launch_angle.

pos_predictions_by_launch_angle <- ggplot(dplyr::filter(predictions_6174, 
                                                       eval_type %in% c("FalsePos", "TruePos")), 
                                         aes(x = eval_type, y = launch_angle, color = events)) + 
  geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
  geom_point(position = "jitter", alpha = .7, size = 0.8) + 
  scale_y_continuous(breaks = seq(-80, 80, by = 10), 
                     labels = c("-80", "-70", "-60", "-50", "-40", "30",  "20", "10", "0", 
                                "10", "20", "30", "40", "50", "60", "70", "80"), 
                     limits = c(-90, 90)) + 
  scale_color_manual(values = c("Red", "Blue")) + 
  ggtitle("True vs False Positives, by Launch Angle") + 
  theme(legend.position = c(.15, .90))

pos_predictions_by_launch_angle_data <- layer_data(pos_predictions_by_launch_angle)

launch_angle_FalsePos_1quartile <- pos_predictions_by_launch_angle_data[1, 2]
launch_angle_FalsePos_median <- pos_predictions_by_launch_angle_data[1, 3]
launch_angle_FalsePos_3quartile <- pos_predictions_by_launch_angle_data[1, 4]
launch_angle_TruePos_1quartile <- pos_predictions_by_launch_angle_data[2, 2]
launch_angle_TruePos_median <- pos_predictions_by_launch_angle_data[2, 3]
launch_angle_TruePos_3quartile <- pos_predictions_by_launch_angle_data[2, 4]

(pos_predictions_by_launch_angle <- ggplot(dplyr::filter(predictions_6174, 
                                                        eval_type %in% c("FalsePos", "TruePos")), 
                                          aes(x = eval_type, y = launch_angle, color = events)) + 
  geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
  geom_point(position = "jitter", alpha = .7, size = 0.8) + 
  scale_y_continuous(breaks = seq(-80, 80, by = 10), 
                     labels = c("-80", "-70", "-60", "-50", "-40", "30",  "20", "10", "0", 
                                "10", "20", "30", "40", "50", "60", "70", "80"), 
                     limits = c(-90, 90)) + 
  scale_color_manual(values = c("Red", "Blue")) + 
  ggtitle("True vs False Positives, by Launch Angle") + 
  theme(legend.position = c(.15, .90)) + 
  annotate("text", x = c(.58,.58, .58), size = 3, fontface = 2, hjust = 1, 
           y = c(launch_angle_FalsePos_1quartile, launch_angle_FalsePos_median, launch_angle_FalsePos_3quartile),
           label = c(launch_angle_FalsePos_1quartile, launch_angle_FalsePos_median, launch_angle_FalsePos_3quartile)) + 
  annotate("text", x = c(1.58, 1.58, 1.58), size = 3, fontface = 2, hjust = 1, 
           y = c(launch_angle_TruePos_1quartile, launch_angle_TruePos_median, launch_angle_TruePos_3quartile), 
           label = c(launch_angle_TruePos_1quartile, launch_angle_TruePos_median, launch_angle_TruePos_3quartile)))

# Compare the launch_angle densities of False Positives and True Positives.

x_scale <- scale_x_continuous(breaks = seq(-80, 80, by = 10), 
                              labels = c("-80", "-70", "-60", "-50", "-40", "-30",  "-20", "-10", "0", 
                                         "10", "20", "30", "40", "50", "60", "70", "80"), 
                              limits = c(-90, 90))
y_scale <- scale_y_continuous(limits = c(0, 0.045))
(la_density_pos <- ggplot(dplyr::filter(predictions_6174, 
                                        eval_type == "FalsePos" | eval_type == "TruePos"), 
                          aes(x = launch_angle, y = stat(density), color = eval_type)) + 
    geom_freqpoly(binwidth = 2) + 
    x_scale + 
    y_scale + 
    scale_color_manual(values = c(FalsePos = "royalblue3", TruePos = "red4")) + 
    labs(title = "Density Plots of Launch Angle by FalsePos and TruePos", 
         x = "Launch Angle (degrees)")) + 
  theme(legend.position = c(.15, .85))
  
############### launch speeds of false positives

# Visualize false positive and true positive predictions by launch_speed_cat.

pos_labels <- c("false positives", "true positives")
names(pos_labels) <- c("FalsePos", "TruePos")

ggplot(dplyr::filter(predictions_6174, eval_type %in% c("FalsePos", "TruePos")), 
       aes(hc_x_Kolp, hc_y_Kolp, color = eval_type)) + 
  scale_color_manual(values = c(FalsePos = "royalblue4", TruePos = "red3")) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = .7) + theme_linedraw() + 
  labs(title = "False Positives by launch_speed_cat") + 
  facet_grid(launch_speed_cat ~ eval_type, labeller = labeller(eval_type = pos_labels))

# Using boxplots, compare false positive and true positive predictions by launch_speed.

pos_predictions_by_launch_speed <- ggplot(dplyr::filter(predictions_6174, 
                                                        eval_type %in% c("FalsePos", "TruePos")), 
                                          aes(x = eval_type, y = launch_speed, color = events)) + 
  geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
  geom_point(position = "jitter", alpha = .7, size = 0.8) + 
  scale_y_continuous(breaks = seq(25, 115, by = 10), 
                     labels = c("25", "35", "45", "55", "65", "75",  "85", "95", "105", "115"), 
                     limits = c(25, 115)) + 
  scale_color_manual(values = c(field_out = "Blue", single = "Red")) + 
  ggtitle("True vs False Positives, by Launch Speed") + 
  theme(legend.position = c(.5, .04), legend.direction = "horizontal")

pos_predictions_by_launch_speed_data <- layer_data(pos_predictions_by_launch_speed)

launch_speed_FalsePos_1quartile <- pos_predictions_by_launch_speed_data[1, 2]
launch_speed_FalsePos_median <- pos_predictions_by_launch_speed_data[1, 3]
launch_speed_FalsePos_3quartile <- pos_predictions_by_launch_speed_data[1, 4]
launch_speed_TruePos_1quartile <- pos_predictions_by_launch_speed_data[2, 2]
launch_speed_TruePos_median <- pos_predictions_by_launch_speed_data[2, 3]
launch_speed_TruePos_3quartile <- pos_predictions_by_launch_speed_data[2, 4]

(pos_predictions_by_launch_speed <- ggplot(dplyr::filter(predictions_6174, 
                                                         eval_type %in% c("FalsePos", "TruePos")), 
                                           aes(x = eval_type, y = launch_speed, color = events)) + 
    geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
    geom_point(position = "jitter", alpha = .7, size = 0.8) + 
    scale_y_continuous(breaks = seq(25, 115, by = 10), 
                       labels = c("25", "35", "45", "55", "65", "75",  "85", "95", "105", "115"), 
                       limits = c(25, 115)) + 
    scale_color_manual(values = c(field_out = "Blue", single = "Red")) + 
    ggtitle("True vs False Positives, by Launch Speed") + 
    theme(legend.position = c(.5, .04), legend.direction = "horizontal") + 
    annotate("text", x = c(.59, .59, .59), size = 2.5, fontface = 2, hjust = 1, 
             y = c(launch_speed_FalsePos_1quartile, launch_speed_FalsePos_median, launch_speed_FalsePos_3quartile),
             label = c(launch_speed_FalsePos_1quartile, launch_speed_FalsePos_median, launch_speed_FalsePos_3quartile)) + 
    annotate("text", x = c(1.59, 1.59, 1.59), size = 2.5, fontface = 2, hjust = 1, 
             y = c(launch_speed_TruePos_1quartile, launch_speed_TruePos_median, launch_speed_TruePos_3quartile), 
             label = c(launch_speed_TruePos_1quartile, launch_speed_TruePos_median, launch_speed_TruePos_3quartile)))

# Compare the launch_speed densities of False Positives and True Positives.

x_scale <- scale_x_continuous(breaks = seq(25, 115, by = 10), 
                              labels = c("25", "35", "45", "55", "65", "75",  "85", "95", "105", "115"), 
                              limits = c(25, 115))
y_scale <- scale_y_continuous(limits = c(0, 0.04))
(ls_density_pos <- ggplot(dplyr::filter(predictions_6174, eval_type == "FalsePos" | eval_type == "TruePos"), 
                          aes(x = launch_speed, y = stat(density), color = eval_type)) + 
    geom_freqpoly(binwidth = 2.5) + 
    x_scale + 
    y_scale + 
    scale_color_manual(values = c(FalsePos = "red4", TruePos = "royalblue3")) + 
    labs(title = "Density Plots of Exit Velocity by FalsePos and TruePos", 
         x = "Exit Velocity (mph)")) + 
  theme(legend.position = c(.15, .8))

############### spray angles (Kolp) of false positives

# Visualize false positive and true positive predictions by rev_spray_angle_Kolp_cat.

predictions_6174_std_gb <- 
  dplyr::filter(predictions_6174, if_fielding_alignment == "Standard" & eval_type %in% c("FalsePos", "TruePos"))
predictions_6174_std_gb <- predictions_6174_std_gb %>% 
  dplyr::filter(adv_bb_type == "low_ground_ball" | 
                  adv_bb_type == "high_ground_ball")

pos_labels <- c("false positives", "true positives")
names(pos_labels) <- c("FalsePos", "TruePos")

(falsePos_predictions_by_rev_spray_angle_Kolp_cat <- ggplot(dplyr::filter(predictions_6174_std_gb, 
                                                                      eval_type %in% c("FalsePos", "TruePos")), 
                                                        aes(hc_x_Kolp, hc_y_Kolp, color = eval_type)) + 
    scale_color_manual(values = c(FalsePos = "royalblue4", TruePos = "red3")) + 
    geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
    geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
    geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
    geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
    geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
    geom_point(size = .7) + theme_linedraw() + 
    labs(title = "False Positives by spray_angle_Kolp (Standard infield)") + 
    #facet_grid(rev_spray_angle_Kolp_cat ~ eval_type, labeller = labeller(eval_type = pos_labels)))
    facet_grid(eval_type ~ rev_spray_angle_Kolp_cat, labeller = labeller(eval_type = pos_labels)))

# Using boxplots, compare false positive and true positive predictions by spray_angle_Kolp.

predictions_6174_std_gb <- 
  dplyr::filter(predictions_6174, if_fielding_alignment == "Standard" & eval_type %in% c("FalsePos", "TruePos"))
predictions_6174_std_gb <- predictions_6174_std_gb %>% 
  dplyr::filter(adv_bb_type == "low_ground_ball" | 
                  adv_bb_type == "high_ground_ball")

pos_predictions_by_spray_angle_Kolp <- ggplot(predictions_6174_std_gb, 
                                              aes(x = eval_type, y = spray_angle_Kolp, color = events)) + 
  geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
  geom_point(position = "jitter", alpha = .7, size = 0.85) + 
  scale_y_continuous(breaks = seq(-50, 50, by = 5), 
                     labels = c("-50", "-45", "-40", "-35", "-30", "-25", "-20", "-15", "-10", "-5",  "0", 
                                "5", "10", "15", "20", "25", "30", "35", "40", "45", "50"), 
                     limits = c(-50, 50)) + 
  scale_color_manual(values = c(single = "Red", field_out = "Blue")) + 
  ggtitle("True vs False Positives, by spray_angle_Kolp (Inf Standard)") +
  theme(legend.position = c(.5, .96), legend.direction = "horizontal")

pos_predictions_by_spray_angle_Kolp_data <- layer_data(pos_predictions_by_spray_angle_Kolp)

spray_angle_Kolp_FalsePos_1quartile <- round(pos_predictions_by_spray_angle_Kolp_data[1, 2], 1)
spray_angle_Kolp_FalsePos_median <- round(pos_predictions_by_spray_angle_Kolp_data[1, 3], 1)
spray_angle_Kolp_FalsePos_3quartile <- round(pos_predictions_by_spray_angle_Kolp_data[1, 4], 1)
spray_angle_Kolp_TruePos_1quartile <- round(pos_predictions_by_spray_angle_Kolp_data[2, 2], 1)
spray_angle_Kolp_TruePos_median <- round(pos_predictions_by_spray_angle_Kolp_data[2, 3], 1)
spray_angle_Kolp_TruePos_3quartile <- round(pos_predictions_by_spray_angle_Kolp_data[2, 4], 1)

(pos_predictions_by_spray_angle_Kolp <- ggplot(predictions_6174_std_gb, 
                                               aes(x = eval_type, y = spray_angle_Kolp, color = events)) + 
    geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
    geom_point(position = "jitter", alpha = .7, size = 0.85) + 
    scale_y_continuous(breaks = seq(-50, 50, by = 5), 
                       labels = c("-50", "-45", "-40", "-35", "-30", "-25", "-20", "-15", "-10", "-5",  "0", 
                                  "5", "10", "15", "20", "25", "30", "35", "40", "45", "50"), 
                       limits = c(-50, 50)) + 
    scale_color_manual(values = c(single = "Red", field_out = "Blue")) + 
    ggtitle("True vs False Positives, by spray_angle_Kolp (Inf Standard)") +
    theme(legend.position = c(.5, .96), legend.direction = "horizontal") + 
    annotate("text", x = c(.60, .60, .60), size = 3, fontface = 2, hjust = 1, 
             y = c(spray_angle_Kolp_FalsePos_1quartile, spray_angle_Kolp_FalsePos_median, spray_angle_Kolp_FalsePos_3quartile),
             label = c(spray_angle_Kolp_FalsePos_1quartile, spray_angle_Kolp_FalsePos_median, spray_angle_Kolp_FalsePos_3quartile)) + 
    annotate("text", x = c(1.60, 1.60, 1.60), size = 3, fontface = 2, hjust = 1, 
             y = c(spray_angle_Kolp_TruePos_1quartile, spray_angle_Kolp_TruePos_median, spray_angle_Kolp_TruePos_3quartile), 
             label = c(spray_angle_Kolp_TruePos_1quartile, spray_angle_Kolp_TruePos_median, spray_angle_Kolp_TruePos_3quartile))) 

# Compare the spray_angle_Kolp densities of False Positives and True Positives.

predictions_6174_std_gb <- 
  dplyr::filter(predictions_6174, 
                if_fielding_alignment == "Standard" & eval_type %in% c("FalsePos", "TruePos"))
predictions_6174_std_gb <- predictions_6174_std_gb %>% 
  dplyr::filter(adv_bb_type == "low_ground_ball" | 
                  adv_bb_type == "high_ground_ball")

x_scale <- scale_x_continuous(breaks = seq(-50, 50, by = 5), 
                              labels = c("-50", "-45", "-40", "-35", "-30", "-25", "-20", "-15", "-10", "-5",  "0", 
                                         "5", "10", "15", "20", "25", "30", "35", "40", "45", "50"), 
                              limits = c(-50, 50))
y_scale <- scale_y_continuous(limits = c(0, 0.035)) 

(sa_Kolp_density_pos <- ggplot(predictions_6174_std_gb, 
                               aes(x = spray_angle_Kolp, y = stat(density), color = eval_type)) + 
    geom_freqpoly(binwidth = 2.5) + 
    x_scale + 
    y_scale + 
    scale_color_manual(values = c(FalsePos = "blue", TruePos = "red3")) + 
    labs(title = "Density Plots of spray_angle_Kolp by FalsePos and TruePos  
         (Standard If Alignment)", x = "Spray Angle Kolp (degrees)")) + 
  theme(legend.position = c(.12, .80))

############### spray angles (adjusted) of false positives

# Visualize false positive and true positive predictions by rev_spray_angle_adj_cat.

predictions_6174_shift_gb <- 
  dplyr::filter(predictions_6174, if_fielding_alignment == "Infield shift" & eval_type %in% c("FalsePos", "TruePos"))
predictions_6174_shift_gb <- predictions_6174_shift_gb %>% 
  dplyr::filter(adv_bb_type == "low_ground_ball" | 
                  adv_bb_type == "high_ground_ball")

pos_labels <- c("false positives", "true positives")
names(pos_labels) <- c("FalsePos", "TruePos")

(falsePos_predictions_by_rev_spray_angle_adj_cat <- ggplot(dplyr::filter(predictions_6174, 
                                                                     eval_type %in% c("FalsePos", "TruePos")), 
                                                       aes(hc_x_Kolp, hc_y_Kolp, color = eval_type)) + 
    scale_color_manual(values = c(FalsePos = "royalblue4", TruePos = "red3")) + 
    geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
    geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
    geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
    geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
    geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
    geom_point(size = .7) + theme_linedraw() + 
    labs(title = "False Positives by spray_angle_adj (Infield shift)") + 
    #facet_grid(rev_spray_angle_adj_cat ~ eval_type, labeller = labeller(eval_type = pos_labels)))
    facet_grid(eval_type ~ rev_spray_angle_adj_cat, labeller = labeller(eval_type = pos_labels)))

# Using boxplots, compare false positive and true positive predictions by spray_angle_adj.

predictions_6174_shift_gb <- 
  dplyr::filter(predictions_6174, if_fielding_alignment == "Infield shift" & eval_type %in% c("FalsePos", "TruePos"))
predictions_6174_shift_gb <- predictions_6174_shift_gb %>% 
  dplyr::filter(adv_bb_type == "low_ground_ball" | 
                  adv_bb_type == "high_ground_ball")

pos_predictions_by_spray_angle_adj <- ggplot(predictions_6174_shift_gb, 
                                             aes(x = eval_type, y = spray_angle_adj, color = events)) + 
  geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
  geom_point(position = "jitter", alpha = .7, size = 0.85) + 
  scale_y_continuous(breaks = seq(-50, 50, by = 5), 
                     labels = c("-50", "-45", "-40", "-35", "-30", "-25", "-20", "-15", "-10", "-5",  "0", 
                                "5", "10", "15", "20", "25", "30", "35", "40", "45", "50"), 
                     limits = c(-50, 50)) + 
  scale_color_manual(values = c(single = "Red", field_out = "Blue")) + 
  ggtitle("True vs False Positives, by spray_angle_adj (Infield shift)") +
  theme(legend.position = c(.5, .96), legend.direction = "horizontal")

pos_predictions_by_spray_angle_adj_data <- layer_data(pos_predictions_by_spray_angle_adj)

spray_angle_adj_FalsePos_1quartile <- round(pos_predictions_by_spray_angle_adj_data[1, 2], 1)
spray_angle_adj_FalsePos_median <- round(pos_predictions_by_spray_angle_adj_data[1, 3], 1)
spray_angle_adj_FalsePos_3quartile <- round(pos_predictions_by_spray_angle_adj_data[1, 4], 1)
spray_angle_adj_TruePos_1quartile <- round(pos_predictions_by_spray_angle_adj_data[2, 2], 1)
spray_angle_adj_TruePos_median <- round(pos_predictions_by_spray_angle_adj_data[2, 3], 1)
spray_angle_adj_TruePos_3quartile <- round(pos_predictions_by_spray_angle_adj_data[2, 4], 1)

(pos_predictions_by_spray_angle_adj <- ggplot(predictions_6174_shift_gb, 
                                              aes(x = eval_type, y = spray_angle_adj, color = events)) + 
    geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
    geom_point(position = "jitter", alpha = .7, size = 0.85) + 
    scale_y_continuous(breaks = seq(-50, 50, by = 5), 
                       labels = c("-50", "-45", "-40", "-35", "-30", "-25", "-20", "-15", "-10", "-5",  "0", 
                                  "5", "10", "15", "20", "25", "30", "35", "40", "45", "50"), 
                       limits = c(-50, 50)) + 
    scale_color_manual(values = c(single = "Red", field_out = "Blue")) + 
    ggtitle("True vs False Positives, by spray_angle_adj (Infield shift)") +
    theme(legend.position = c(.5, .96), legend.direction = "horizontal") + 
    annotate("text", x = c(.60, .60, .60), size = 3, fontface = 2, hjust = 1, 
             y = c(spray_angle_adj_FalsePos_1quartile, spray_angle_adj_FalsePos_median, spray_angle_adj_FalsePos_3quartile),
             label = c(spray_angle_adj_FalsePos_1quartile, spray_angle_adj_FalsePos_median, spray_angle_adj_FalsePos_3quartile)) + 
    annotate("text", x = c(1.60, 1.60, 1.60), size = 3, fontface = 2, hjust = 1, 
             y = c(spray_angle_adj_TruePos_1quartile, spray_angle_adj_TruePos_median, spray_angle_adj_TruePos_3quartile), 
             label = c(spray_angle_adj_TruePos_1quartile, spray_angle_adj_TruePos_median, spray_angle_adj_TruePos_3quartile))) 

# Compare the spray_angle_adj densities of False Positives and True Positives.

predictions_6174_shift_gb <- 
  dplyr::filter(predictions_6174, 
                if_fielding_alignment == "Infield shift" & eval_type %in% c("FalsePos", "TruePos"))
predictions_6174_shift_gb <- predictions_6174_shift_gb %>% 
  dplyr::filter(adv_bb_type == "low_ground_ball" | 
                  adv_bb_type == "high_ground_ball")

x_scale <- scale_x_continuous(breaks = seq(-50, 50, by = 5), 
                              labels = c("-50", "-45", "-40", "-35", "-30", "-25", "-20", "-15", "-10", "-5",  "0", 
                                         "5", "10", "15", "20", "25", "30", "35", "40", "45", "50"), 
                              limits = c(-50, 50))
y_scale <- scale_y_continuous(limits = c(0, 0.035)) 

(sa_adj_density_pos <- ggplot(predictions_6174_shift_gb, 
                              aes(x = spray_angle_adj, y = stat(density), color = eval_type)) + 
    geom_freqpoly(binwidth = 2.5) + 
    x_scale + 
    y_scale + 
    scale_color_manual(values = c(FalsePos = "blue", TruePos = "red3")) + 
    labs(title = "Density Plots of spray_angle_adj by FalsePos and TruePos  
         (If Shift)", x = "Spray Angle adj (degrees)")) + 
  theme(legend.position = c(.12, .80))

############### running times to first base (hp_to_1b) for false positives

# Visualize false positive and true positive predictions by hp_to_1b_cat.

predictions_6174_lowgb_topweak <- 
  dplyr::filter(predictions_6174, eval_type %in% c("FalsePos", "TruePos"))
predictions_6174_lowgb_topweak <- predictions_6174_lowgb_topweak %>% 
  dplyr::filter(adv_bb_type == "low_ground_ball")
predictions_6174_lowgb_topweak <- predictions_6174_lowgb_topweak %>% 
  dplyr::filter(launch_speed_angle == "1" | 
                  launch_speed_angle == "2")

pos_labels <- c("false positives", "true positives")
names(pos_labels) <- c("FalsePos", "TruePos")

(falsePos_predictions_by_hp_to_1b_cat <- ggplot(predictions_6174_lowgb_topweak, 
                                                aes(hc_x_Kolp, hc_y_Kolp, color = eval_type)) + 
    scale_color_manual(values = c(FalsePos = "royalblue4", TruePos = "red3")) + 
    geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
    geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
    geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
    geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
    geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
    geom_point(size = .7) + theme_linedraw() + 
    labs(title = "False Positives by hp_to_1b_cat") + 
    facet_grid(hp_to_1b_cat ~ eval_type, labeller = labeller(eval_type = pos_labels)))

# Using boxplots, compare false positive and true positive predictions by hp_to_1b.

predictions_6174_lowgb_topweak <- 
  dplyr::filter(predictions_6174, eval_type %in% c("FalsePos", "TruePos"))
predictions_6174_lowgb_topweak <- predictions_6174_lowgb_topweak %>% 
  dplyr::filter(adv_bb_type == "low_ground_ball")

pos_predictions_by_hp_to_1b <- ggplot(predictions_6174_lowgb_topweak, 
                                      aes(x = eval_type, y = hp_to_1b, color = eval_type)) + 
  geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
  geom_point(position = "jitter", alpha = .7, size = 0.85) + 
  scale_y_continuous(breaks = seq(3.90, 5.10, by = 0.1), 
                     labels = c("3.90", "4.00", "4.10", "4.20", "4.30", "4.40", "4.50", "4.60", "4.70", "4.80",  "4.90", "5.00", "5.10"), 
                     limits = c(3.90, 5.10)) + 
  scale_color_manual(values = c(FalsePos = "Blue", TruePos = "Red")) + 
  ggtitle("False vs True Positives, by hp_to_1b") +
  theme(legend.position = c(.90, .90))

pos_predictions_by_hp_to_1b_data <- layer_data(pos_predictions_by_hp_to_1b)

hp_to_1b_FalsePos_1quartile <- round(pos_predictions_by_hp_to_1b_data[1, 2], 2)
hp_to_1b_FalsePos_median <- round(pos_predictions_by_hp_to_1b_data[1, 3], 2)
hp_to_1b_FalsePos_3quartile <- round(pos_predictions_by_hp_to_1b_data[1, 4], 2)
hp_to_1b_TruePos_1quartile <- round(pos_predictions_by_hp_to_1b_data[2, 2], 2)
hp_to_1b_TruePos_median <- round(pos_predictions_by_hp_to_1b_data[2, 3], 2)
hp_to_1b_TruePos_3quartile <- round(pos_predictions_by_hp_to_1b_data[2, 4], 2)

(pos_predictions_by_hp_to_1b <- ggplot(predictions_6174_lowgb_topweak, 
                                       aes(x = eval_type, y = hp_to_1b, color = eval_type)) + 
    geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
    geom_point(position = "jitter", alpha = .7, size = 0.85) + 
    scale_y_continuous(breaks = seq(3.90, 5.10, by = 0.1), 
                       labels = c("3.90", "4.00", "4.10", "4.20", "4.30", "4.40", "4.50", "4.60", "4.70", "4.80",  "4.90", "5.00", "5.10"), 
                       limits = c(3.90, 5.10)) + 
    scale_color_manual(values = c(FalsePos = "Blue", TruePos = "Red")) + 
    ggtitle("False vs True Positives, by hp_to_1b") +
    theme(legend.position = c(.90, .90)) + 
    annotate("text", x = c(.60, .60, .60), size = 3, fontface = 2, hjust = 1, 
             y = c(hp_to_1b_FalsePos_1quartile, hp_to_1b_FalsePos_median, hp_to_1b_FalsePos_3quartile),
             label = c(hp_to_1b_FalsePos_1quartile, hp_to_1b_FalsePos_median, hp_to_1b_FalsePos_3quartile)) + 
    annotate("text", x = c(1.60, 1.60, 1.60), size = 3, fontface = 2, hjust = 1, 
             y = c(hp_to_1b_TruePos_1quartile, hp_to_1b_TruePos_median, hp_to_1b_TruePos_3quartile), 
             label = c(hp_to_1b_TruePos_1quartile, hp_to_1b_TruePos_median, hp_to_1b_TruePos_3quartile))) 

# Compare the hp_to_1b densities of False Positives and True Positives.

predictions_6174_lowgb_topweak <- 
  dplyr::filter(predictions_6174, eval_type %in% c("FalsePos", "TruePos"))
predictions_6174_lowgb_topweak <- predictions_6174_lowgb_topweak %>% 
  dplyr::filter(adv_bb_type == "low_ground_ball")

x_scale <- scale_x_continuous(breaks = seq(3.90, 5.10, by = 0.1), 
                              labels = c("3.90", "4.00", "4.10", "4.20", "4.30", "4.40", "4.50", "4.60", "4.70", "4.80",  "4.90", "5.00", "5.10"), 
                              limits = c(3.90, 5.10))
y_scale <- scale_y_continuous()

(hp_to_1b_density_pos <- ggplot(predictions_6174_lowgb_topweak, 
                                aes(x = hp_to_1b, y = stat(density), color = eval_type)) + 
    geom_freqpoly(binwidth = 0.2) + 
    x_scale + 
    y_scale + 
    scale_color_manual(values = c(FalsePos = "blue", TruePos = "red3")) + 
    labs(title = "Density Plots of hp_to_1b by FalsePos and TruePos", 
         x = "hp_to_1b (seconds)")) + 
  theme(legend.position = c(.80, .80))

################################################

# Use K-means clustering to identify groups of batted balls that tend to be misclassified 
# as singles or outs.

rf_predict_mistakes2 <- dplyr::select(rf_predict_mistakes, obs_nmbr, 
                                      prob_single, prob_out, pred, 
                                      obs, eval, eval_type, prob_diff, 
                                      launch_angle, launch_speed, spray_angle_Kolp, 
                                      spray_angle_adj, hp_to_1b, 
                                      if_fielding_alignment, everything())

rf_predict_FalsePos_std <- filter(rf_predict_mistakes2, eval_type == "FalsePos", 
                                  if_fielding_alignment == "Standard")
rf_predict_FalsePos_std_scaled <- scale(rf_predict_FalsePos_std[, 9:13])

rf_predict_FalsePos_shift <- filter(rf_predict_mistakes2, eval_type == "FalsePos", 
                                    if_fielding_alignment == "Infield shift")
rf_predict_FalsePos_shift_scaled <- scale(rf_predict_FalsePos_shift[, 9:13])

rf_predict_FalseNeg_std <- filter(rf_predict_mistakes2, eval_type == "FalseNeg", 
                                  if_fielding_alignment == "Standard")
rf_predict_FalseNeg_std_scaled <- scale(rf_predict_FalseNeg_std[, 9:13])

rf_predict_FalseNeg_shift <- filter(rf_predict_mistakes2, eval_type == "FalseNeg", 
                                    if_fielding_alignment == "Infield shift")
rf_predict_FalseNeg_shift_scaled <- scale(rf_predict_FalseNeg_shift[, 9:13])

######### FalsePos_std

# Use NbClust package to identify optimal number of clusters in 
# rf_predict_FalsePos_std_scaled.

library(NbClust)
set.seed(1234)
nc <- NbClust(rf_predict_FalsePos_std_scaled, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
table(nc$Best.nc[1,])

# plot(nc$All.index[,4], type="o", ylab="CCC",
#      xlab="Number of clusters", col="blue")

# Identify optimal number of clusters in rf_predict_FalsePos_std_scaled by 
# plotting the total within-groups sums of squares against the number of 
# clusters. (reference: Kabacoff)

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(rf_predict_FalsePos_std_scaled, nc = 10, seed = 1234)

# Use K-means clustering to identify the types of batted balls that tend to be 
# misclassified as singles against an infield in Standard alignment.

set.seed(1234)
km_fit <- kmeans(rf_predict_FalsePos_std_scaled, 6, iter.max = 10, nstart = 50)
round(aggregate(rf_predict_FalsePos_std[, 9:13], by = list(cluster = km_fit$cluster), median), 2)
round(aggregate(rf_predict_FalsePos_std[, 9:13], by = list(cluster = km_fit$cluster), mad), 2)
km_fit$size
km_clusters <- fitted(km_fit, method = "classes")
rf_predict_FalsePos_std <- add_column(rf_predict_FalsePos_std, km_clusters, .after = "obs_nmbr")

# Create a tibble for each cluster of false positives against an infield in 
# Standard alignment.

FalsePos_std_cluster1 <- rf_predict_FalsePos_std %>% 
  dplyr::select(adv_bb_type, launch_speed_cat, rev_spray_angle_Kolp_cat, 
                rev_spray_angle_adj_cat, hp_to_1b_cat) %>% filter(km_clusters == 1)
FalsePos_std_cluster2 <- rf_predict_FalsePos_std %>% 
  dplyr::select(adv_bb_type, launch_speed_cat, rev_spray_angle_Kolp_cat, 
                rev_spray_angle_adj_cat, hp_to_1b_cat) %>% filter(km_clusters == 2)
FalsePos_std_cluster3 <- rf_predict_FalsePos_std %>% 
  dplyr::select(adv_bb_type, launch_speed_cat, rev_spray_angle_Kolp_cat, 
                rev_spray_angle_adj_cat, hp_to_1b_cat) %>% filter(km_clusters == 3)
FalsePos_std_cluster4 <- rf_predict_FalsePos_std %>% 
  dplyr::select(adv_bb_type, launch_speed_cat, rev_spray_angle_Kolp_cat, 
                rev_spray_angle_adj_cat, hp_to_1b_cat) %>% filter(km_clusters == 4)
FalsePos_std_cluster5 <- rf_predict_FalsePos_std %>% 
  dplyr::select(adv_bb_type, launch_speed_cat, rev_spray_angle_Kolp_cat, 
                rev_spray_angle_adj_cat, hp_to_1b_cat) %>% filter(km_clusters == 5)
FalsePos_std_cluster6 <- rf_predict_FalsePos_std %>% 
  dplyr::select(adv_bb_type, launch_speed_cat, rev_spray_angle_Kolp_cat, 
                rev_spray_angle_adj_cat, hp_to_1b_cat) %>% filter(km_clusters == 6)

# Visualize false positive predictions by cluster, when the infield was in a 
# Standard alignment.

pos_labels <- c("false positives")
names(pos_labels) <- c("FalsePos")

ggplot(rf_predict_FalsePos_std, aes(hc_x_Kolp, hc_y_Kolp)) + 
    geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
    geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
    geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
    geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
    geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
    geom_point(size = 0.7, color = "royalblue4") + theme_linedraw() + 
    labs(title = "FalsePos_std Preds by Cluster") + 
    facet_grid(km_clusters ~ eval_type, labeller = labeller(eval_type = pos_labels))

# Determine the number of times each combination of features (feature set) was 
# represented by a batted ball in Cluster 1.

FalsePos_std_cluster1_combs <- as_tibble(aggregate(cbind(n = 1:nrow(FalsePos_std_cluster1))~., 
                                                   FalsePos_std_cluster1, length))
FalsePos_std_cluster1_combs <- FalsePos_std_cluster1_combs %>% arrange(desc(n))
FalsePos_std_cluster1_combs <- mutate(FalsePos_std_cluster1_combs, 
                                      n_pct = round(n/sum(n)*100, 2), 
                                      n_cum_pct = round(cumsum(n)/sum(n)*100, 2))
FalsePos_std_cluster1_combs <- add_column(FalsePos_std_cluster1_combs, 
                                          comb_rank = min_rank(desc(FalsePos_std_cluster1_combs$n)), 
                                          .before = "adv_bb_type")

# Determine the number of times each combination of features (feature set) was 
# represented by a batted ball in Cluster 3.

FalsePos_std_cluster3_combs <- as_tibble(aggregate(cbind(n = 1:nrow(FalsePos_std_cluster3))~., 
                                                   FalsePos_std_cluster3, length))
FalsePos_std_cluster3_combs <- FalsePos_std_cluster3_combs %>% arrange(desc(n))
FalsePos_std_cluster3_combs <- mutate(FalsePos_std_cluster3_combs, 
                                      n_pct = round(n/sum(n)*100, 2), 
                                      n_cum_pct = round(cumsum(n)/sum(n)*100, 2))
FalsePos_std_cluster3_combs <- add_column(FalsePos_std_cluster3_combs, 
                                          comb_rank = min_rank(desc(FalsePos_std_cluster3_combs$n)), 
                                          .before = "adv_bb_type")

# Determine the number of times each combination of features (feature set) was 
# represented by a batted ball in Cluster 2.

FalsePos_std_cluster2_combs <- as_tibble(aggregate(cbind(n = 1:nrow(FalsePos_std_cluster2))~., 
                                                   FalsePos_std_cluster2, length))
FalsePos_std_cluster2_combs <- FalsePos_std_cluster2_combs %>% arrange(desc(n))
FalsePos_std_cluster2_combs <- mutate(FalsePos_std_cluster2_combs, 
                                      n_pct = round(n/sum(n)*100, 2), 
                                      n_cum_pct = round(cumsum(n)/sum(n)*100, 2))
FalsePos_std_cluster2_combs <- add_column(FalsePos_std_cluster2_combs, 
                                          comb_rank = min_rank(desc(FalsePos_std_cluster2_combs$n)), 
                                          .before = "adv_bb_type")

# Determine the number of times each combination of features (feature set) was 
# represented by a batted ball in Cluster 4.

FalsePos_std_cluster4_combs <- as_tibble(aggregate(cbind(n = 1:nrow(FalsePos_std_cluster4))~., 
                                                   FalsePos_std_cluster4, length))
FalsePos_std_cluster4_combs <- FalsePos_std_cluster4_combs %>% arrange(desc(n))
FalsePos_std_cluster4_combs <- mutate(FalsePos_std_cluster4_combs, 
                                      n_pct = round(n/sum(n)*100, 2), 
                                      n_cum_pct = round(cumsum(n)/sum(n)*100, 2))
FalsePos_std_cluster4_combs <- add_column(FalsePos_std_cluster4_combs, 
                                          comb_rank = min_rank(desc(FalsePos_std_cluster4_combs$n)), 
                                          .before = "adv_bb_type")

# Determine the number of times each combination of features (feature set) was 
# represented by a batted ball in Cluster 5.

FalsePos_std_cluster5_combs <- as_tibble(aggregate(cbind(n = 1:nrow(FalsePos_std_cluster5))~., 
                                                   FalsePos_std_cluster5, length))
FalsePos_std_cluster5_combs <- FalsePos_std_cluster5_combs %>% arrange(desc(n))
FalsePos_std_cluster5_combs <- mutate(FalsePos_std_cluster5_combs, 
                                      n_pct = round(n/sum(n)*100, 2), 
                                      n_cum_pct = round(cumsum(n)/sum(n)*100, 2))
FalsePos_std_cluster5_combs <- add_column(FalsePos_std_cluster5_combs, 
                                          comb_rank = min_rank(desc(FalsePos_std_cluster5_combs$n)), 
                                          .before = "adv_bb_type")

# Determine the number of times each combination of features (feature set) was 
# represented by a batted ball in Cluster 6.

FalsePos_std_cluster6_combs <- as_tibble(aggregate(cbind(n = 1:nrow(FalsePos_std_cluster6))~., 
                                                   FalsePos_std_cluster6, length))
FalsePos_std_cluster6_combs <- FalsePos_std_cluster6_combs %>% arrange(desc(n))
FalsePos_std_cluster6_combs <- mutate(FalsePos_std_cluster6_combs, 
                                      n_pct = round(n/sum(n)*100, 2), 
                                      n_cum_pct = round(cumsum(n)/sum(n)*100, 2))
FalsePos_std_cluster6_combs <- add_column(FalsePos_std_cluster6_combs, 
                                          comb_rank = min_rank(desc(FalsePos_std_cluster6_combs$n)), 
                                          .before = "adv_bb_type")

########## FalsePos_shift

# Use NbClust package to identify optimal number of clusters in 
# rf_predict_FalsePos_shift_scaled.

library(NbClust)
set.seed(1234)
nc <- NbClust(rf_predict_FalsePos_shift_scaled, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
table(nc$Best.nc[1,])

# plot(nc$All.index[,4], type="o", ylab="CCC",
#      xlab="Number of clusters", col="blue")

# Identify optimal number of clusters in rf_predict_FalsePos_shift_scaled by 
# plotting the total within-groups sums of squares against the number of 
# clusters. (reference: Kabacoff)

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(rf_predict_FalsePos_shift_scaled, nc = 10, seed = 1234)

# Use K-means clustering to identify the types of batted balls that tend to be 
# misclassified as singles against a shifted infield.

set.seed(1234)
km_fit <- kmeans(rf_predict_FalsePos_shift_scaled, 6, iter.max = 10, nstart = 50)
round(aggregate(rf_predict_FalsePos_shift[, 9:13], by = list(cluster = km_fit$cluster), median), 2)
round(aggregate(rf_predict_FalsePos_shift[, 9:13], by = list(cluster = km_fit$cluster), mad), 2)
km_fit$size
km_clusters <- fitted(km_fit, method = "classes")
rf_predict_FalsePos_shift <- add_column(rf_predict_FalsePos_shift, km_clusters, .after = "obs_nmbr")

# Create a tibble for each cluster of false positives against a fully
# shifted infield.

FalsePos_shift_cluster1 <- rf_predict_FalsePos_shift %>% 
  dplyr::select(adv_bb_type, launch_speed_cat, rev_spray_angle_Kolp_cat, 
                rev_spray_angle_adj_cat, hp_to_1b_cat) %>% filter(km_clusters == 1)
FalsePos_shift_cluster2 <- rf_predict_FalsePos_shift %>% 
  dplyr::select(adv_bb_type, launch_speed_cat, rev_spray_angle_Kolp_cat, 
                rev_spray_angle_adj_cat, hp_to_1b_cat) %>% filter(km_clusters == 2)
FalsePos_shift_cluster3 <- rf_predict_FalsePos_shift %>% 
  dplyr::select(adv_bb_type, launch_speed_cat, rev_spray_angle_Kolp_cat, 
                rev_spray_angle_adj_cat, hp_to_1b_cat) %>% filter(km_clusters == 3)
FalsePos_shift_cluster4 <- rf_predict_FalsePos_shift %>% 
  dplyr::select(adv_bb_type, launch_speed_cat, rev_spray_angle_Kolp_cat, 
                rev_spray_angle_adj_cat, hp_to_1b_cat) %>% filter(km_clusters == 4)
FalsePos_shift_cluster5 <- rf_predict_FalsePos_shift %>% 
  dplyr::select(adv_bb_type, launch_speed_cat, rev_spray_angle_Kolp_cat, 
                rev_spray_angle_adj_cat, hp_to_1b_cat) %>% filter(km_clusters == 5)
FalsePos_shift_cluster6 <- rf_predict_FalsePos_shift %>% 
  dplyr::select(adv_bb_type, launch_speed_cat, rev_spray_angle_Kolp_cat, 
                rev_spray_angle_adj_cat, hp_to_1b_cat) %>% filter(km_clusters == 6)

# Visualize false positive predictions by cluster, when the infield was fully  
# shifted.

pos_labels <- c("false positives")
names(pos_labels) <- c("FalsePos")

ggplot(rf_predict_FalsePos_shift, aes(hc_x_Kolp, hc_y_Kolp)) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = 0.7, color = "royalblue4") + theme_linedraw() + 
  labs(title = "FalsePos_shift Preds by Cluster") + 
  facet_grid(km_clusters ~ eval_type, labeller = labeller(eval_type = pos_labels))

# Determine the number of times each combination of features (feature set) was 
# represented by a batted ball in Cluster 1.

FalsePos_shift_cluster1_combs <- as_tibble(aggregate(cbind(n = 1:nrow(FalsePos_shift_cluster1))~., 
                                                   FalsePos_shift_cluster1, length))
FalsePos_shift_cluster1_combs <- FalsePos_shift_cluster1_combs %>% arrange(desc(n))
FalsePos_shift_cluster1_combs <- mutate(FalsePos_shift_cluster1_combs, 
                                      n_pct = round(n/sum(n)*100, 2), 
                                      n_cum_pct = round(cumsum(n)/sum(n)*100, 2))
FalsePos_shift_cluster1_combs <- add_column(FalsePos_shift_cluster1_combs, 
                                          comb_rank = min_rank(desc(FalsePos_shift_cluster1_combs$n)), 
                                          .before = "adv_bb_type")

# Determine the number of times each combination of features (feature set) was 
# represented by a batted ball in Cluster 2.

FalsePos_shift_cluster2_combs <- as_tibble(aggregate(cbind(n = 1:nrow(FalsePos_shift_cluster2))~., 
                                                   FalsePos_shift_cluster2, length))
FalsePos_shift_cluster2_combs <- FalsePos_shift_cluster2_combs %>% arrange(desc(n))
FalsePos_shift_cluster2_combs <- mutate(FalsePos_shift_cluster2_combs, 
                                      n_pct = round(n/sum(n)*100, 2), 
                                      n_cum_pct = round(cumsum(n)/sum(n)*100, 2))
FalsePos_shift_cluster2_combs <- add_column(FalsePos_shift_cluster2_combs, 
                                          comb_rank = min_rank(desc(FalsePos_shift_cluster2_combs$n)), 
                                          .before = "adv_bb_type")

# Determine the number of times each combination of features (feature set) was 
# represented by a batted ball in Cluster 3.

FalsePos_shift_cluster3_combs <- as_tibble(aggregate(cbind(n = 1:nrow(FalsePos_shift_cluster3))~., 
                                                   FalsePos_shift_cluster3, length))
FalsePos_shift_cluster3_combs <- FalsePos_shift_cluster3_combs %>% arrange(desc(n))
FalsePos_shift_cluster3_combs <- mutate(FalsePos_shift_cluster3_combs, 
                                      n_pct = round(n/sum(n)*100, 2), 
                                      n_cum_pct = round(cumsum(n)/sum(n)*100, 2))
FalsePos_shift_cluster3_combs <- add_column(FalsePos_shift_cluster3_combs, 
                                          comb_rank = min_rank(desc(FalsePos_shift_cluster3_combs$n)), 
                                          .before = "adv_bb_type")

# Determine the number of times each combination of features (feature set) was 
# represented by a batted ball in Cluster 4.

FalsePos_shift_cluster4_combs <- as_tibble(aggregate(cbind(n = 1:nrow(FalsePos_shift_cluster4))~., 
                                                   FalsePos_shift_cluster4, length))
FalsePos_shift_cluster4_combs <- FalsePos_shift_cluster4_combs %>% arrange(desc(n))
FalsePos_shift_cluster4_combs <- mutate(FalsePos_shift_cluster4_combs, 
                                      n_pct = round(n/sum(n)*100, 2), 
                                      n_cum_pct = round(cumsum(n)/sum(n)*100, 2))
FalsePos_shift_cluster4_combs <- add_column(FalsePos_shift_cluster4_combs, 
                                          comb_rank = min_rank(desc(FalsePos_shift_cluster4_combs$n)), 
                                          .before = "adv_bb_type")

# Determine the number of times each combination of features (feature set) was 
# represented by a batted ball in Cluster 5.

FalsePos_shift_cluster5_combs <- as_tibble(aggregate(cbind(n = 1:nrow(FalsePos_shift_cluster5))~., 
                                                   FalsePos_shift_cluster5, length))
FalsePos_shift_cluster5_combs <- FalsePos_shift_cluster5_combs %>% arrange(desc(n))
FalsePos_shift_cluster5_combs <- mutate(FalsePos_shift_cluster5_combs, 
                                      n_pct = round(n/sum(n)*100, 2), 
                                      n_cum_pct = round(cumsum(n)/sum(n)*100, 2))
FalsePos_shift_cluster5_combs <- add_column(FalsePos_shift_cluster5_combs, 
                                          comb_rank = min_rank(desc(FalsePos_shift_cluster5_combs$n)), 
                                          .before = "adv_bb_type")

# Determine the number of times each combination of features (feature set) was 
# represented by a batted ball in Cluster 6.

FalsePos_shift_cluster6_combs <- as_tibble(aggregate(cbind(n = 1:nrow(FalsePos_shift_cluster6))~., 
                                                   FalsePos_shift_cluster6, length))
FalsePos_shift_cluster6_combs <- FalsePos_shift_cluster6_combs %>% arrange(desc(n))
FalsePos_shift_cluster6_combs <- mutate(FalsePos_shift_cluster6_combs, 
                                      n_pct = round(n/sum(n)*100, 2), 
                                      n_cum_pct = round(cumsum(n)/sum(n)*100, 2))
FalsePos_shift_cluster6_combs <- add_column(FalsePos_shift_cluster6_combs, 
                                          comb_rank = min_rank(desc(FalsePos_shift_cluster6_combs$n)), 
                                          .before = "adv_bb_type")

########## FalseNeg_std

# Use NbClust package to identify optimal number of clusters in 
# rf_predict_FalseNeg_std_scaled.

library(NbClust)
set.seed(1234)
nc <- NbClust(rf_predict_FalseNeg_std_scaled, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
table(nc$Best.nc[1,])

# plot(nc$All.index[,4], type="o", ylab="CCC",
#       xlab="Number of clusters", col="blue")

# Identify optimal number of clusters in rf_predict_FalseNeg_std_scaled by 
# plotting the total within-groups sums of squares against the number of 
# clusters. (reference: Kabacoff)

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(rf_predict_FalseNeg_std_scaled, nc = 10, seed = 1234)

# Use K-means clustering to identify the types of batted balls that tend to be 
# misclassified as outs against an infield in Standard alignment.

set.seed(1234)
km_fit <- kmeans(rf_predict_FalseNeg_std_scaled, 2, iter.max = 10, nstart = 50)
round(aggregate(rf_predict_FalseNeg_std[, 9:13], by = list(cluster = km_fit$cluster), median), 2)
round(aggregate(rf_predict_FalseNeg_std[, 9:13], by = list(cluster = km_fit$cluster), mad), 2)
km_fit$size
km_clusters <- fitted(km_fit, method = "classes")
rf_predict_FalseNeg_std <- add_column(rf_predict_FalseNeg_std, km_clusters, .after = "obs_nmbr")

# Create a tibble for each cluster of false negatives against an infield in 
# Standard alignment.

FalseNeg_std_cluster1 <- rf_predict_FalseNeg_std %>% 
  dplyr::select(adv_bb_type, launch_speed_cat, rev_spray_angle_Kolp_cat, 
                rev_spray_angle_adj_cat, hp_to_1b_cat) %>% filter(km_clusters == 1)
FalseNeg_std_cluster2 <- rf_predict_FalseNeg_std %>% 
  dplyr::select(adv_bb_type, launch_speed_cat, rev_spray_angle_Kolp_cat, 
                rev_spray_angle_adj_cat, hp_to_1b_cat) %>% filter(km_clusters == 2)

# Visualize false negative predictions by cluster, when the infield was in a 
# Standard alignment.

neg_labels <- c("false negatives")
names(neg_labels) <- c("FalseNeg")

ggplot(rf_predict_FalseNeg_std, aes(hc_x_Kolp, hc_y_Kolp)) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = 0.7, color = "red") + theme_linedraw() + 
  labs(title = "FalseNeg_std Preds by Cluster") + 
  facet_grid(km_clusters ~ eval_type, labeller = labeller(eval_type = neg_labels))

# Determine the number of times each combination of features (feature set) was 
# represented by a batted ball in Cluster 1.

FalseNeg_std_cluster1_combs <- as_tibble(aggregate(cbind(n = 1:nrow(FalseNeg_std_cluster1))~., 
                                                   FalseNeg_std_cluster1, length))
FalseNeg_std_cluster1_combs <- FalseNeg_std_cluster1_combs %>% arrange(desc(n))
FalseNeg_std_cluster1_combs <- mutate(FalseNeg_std_cluster1_combs, 
                                      n_pct = round(n/sum(n)*100, 2), 
                                      n_cum_pct = round(cumsum(n)/sum(n)*100, 2))
FalseNeg_std_cluster1_combs <- add_column(FalseNeg_std_cluster1_combs, 
                                          comb_rank = min_rank(desc(FalseNeg_std_cluster1_combs$n)), 
                                          .before = "adv_bb_type")

# Determine the number of times each combination of features (feature set) was 
# represented by a batted ball in Cluster 2.

FalseNeg_std_cluster2_combs <- as_tibble(aggregate(cbind(n = 1:nrow(FalseNeg_std_cluster2))~., 
                                                   FalseNeg_std_cluster2, length))
FalseNeg_std_cluster2_combs <- FalseNeg_std_cluster2_combs %>% arrange(desc(n))
FalseNeg_std_cluster2_combs <- mutate(FalseNeg_std_cluster2_combs, 
                                      n_pct = round(n/sum(n)*100, 2), 
                                      n_cum_pct = round(cumsum(n)/sum(n)*100, 2))
FalseNeg_std_cluster2_combs <- add_column(FalseNeg_std_cluster2_combs, 
                                          comb_rank = min_rank(desc(FalseNeg_std_cluster2_combs$n)), 
                                          .before = "adv_bb_type")

########## FalseNeg_shift

# Use NbClust package to identify optimal number of clusters in 
# rf_predict_FalseNeg_shift_scaled.

library(NbClust)
set.seed(1234)
nc <- NbClust(rf_predict_FalseNeg_shift_scaled, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
table(nc$Best.nc[1,])

plot(nc$All.index[,4], type="o", ylab="CCC",
     xlab="Number of clusters", col="blue")

# Identify optimal number of clusters in rf_predict_FalseNeg_shift_scaled by 
# plotting the total within-groups sums of squares against the number of 
# clusters. (reference: Kabacoff)

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(rf_predict_FalseNeg_shift_scaled, nc = 10, seed = 1234)

# Use K-means clustering to identify the types of batted balls that tend to be 
# misclassified as outs against a shifted infield.

set.seed(1234)
km_fit <- kmeans(rf_predict_FalseNeg_shift_scaled, 5, iter.max = 10, nstart = 50)
round(aggregate(rf_predict_FalseNeg_shift[, 9:13], by = list(cluster = km_fit$cluster), median), 2)
round(aggregate(rf_predict_FalseNeg_shift[, 9:13], by = list(cluster = km_fit$cluster), mad), 2)
km_fit$size
km_clusters <- fitted(km_fit, method = "classes")
rf_predict_FalseNeg_shift <- add_column(rf_predict_FalseNeg_shift, km_clusters, .after = "obs_nmbr")

# Create a tibble for each cluster of false negatives against a shifted infield.

FalseNeg_shift_cluster1 <- rf_predict_FalseNeg_shift %>% 
  dplyr::select(adv_bb_type, launch_speed_cat, rev_spray_angle_Kolp_cat, 
                rev_spray_angle_adj_cat, hp_to_1b_cat) %>% filter(km_clusters == 1)
FalseNeg_shift_cluster2 <- rf_predict_FalseNeg_shift %>% 
  dplyr::select(adv_bb_type, launch_speed_cat, rev_spray_angle_Kolp_cat, 
                rev_spray_angle_adj_cat, hp_to_1b_cat) %>% filter(km_clusters == 2)
FalseNeg_shift_cluster3 <- rf_predict_FalseNeg_shift %>%
  dplyr::select(adv_bb_type, launch_speed_cat, rev_spray_angle_Kolp_cat,
                rev_spray_angle_adj_cat, hp_to_1b_cat) %>% filter(km_clusters == 3)
FalseNeg_shift_cluster4 <- rf_predict_FalseNeg_shift %>%
  dplyr::select(adv_bb_type, launch_speed_cat, rev_spray_angle_Kolp_cat,
                rev_spray_angle_adj_cat, hp_to_1b_cat) %>% filter(km_clusters == 4)
FalseNeg_shift_cluster5 <- rf_predict_FalseNeg_shift %>%
  dplyr::select(adv_bb_type, launch_speed_cat, rev_spray_angle_Kolp_cat,
                rev_spray_angle_adj_cat, hp_to_1b_cat) %>% filter(km_clusters == 5)

# Visualize false negative predictions by cluster, when the infield was fully  
# shifted.

neg_labels <- c("false negatives")
names(neg_labels) <- c("FalseNeg")

ggplot(rf_predict_FalseNeg_shift, aes(hc_x_Kolp, hc_y_Kolp)) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = 0.7, color = "red") + theme_linedraw() + 
  labs(title = "FalseNeg_shift Preds by Cluster") + 
  facet_grid(km_clusters ~ eval_type, labeller = labeller(eval_type = neg_labels))

# Determine the number of times each combination of features (feature set) was 
# represented by a batted ball in Cluster 1.

FalseNeg_shift_cluster1_combs <- as_tibble(aggregate(cbind(n = 1:nrow(FalseNeg_shift_cluster1))~., 
                                                     FalseNeg_shift_cluster1, length))
FalseNeg_shift_cluster1_combs <- FalseNeg_shift_cluster1_combs %>% arrange(desc(n))
FalseNeg_shift_cluster1_combs <- mutate(FalseNeg_shift_cluster1_combs, 
                                        n_pct = round(n/sum(n)*100, 2), 
                                        n_cum_pct = round(cumsum(n)/sum(n)*100, 2))
FalseNeg_shift_cluster1_combs <- add_column(FalseNeg_shift_cluster1_combs, 
                                            comb_rank = min_rank(desc(FalseNeg_shift_cluster1_combs$n)), 
                                            .before = "adv_bb_type")

# Determine the number of times each combination of features (feature set) was 
# represented by a batted ball in Cluster 2.

FalseNeg_shift_cluster2_combs <- as_tibble(aggregate(cbind(n = 1:nrow(FalseNeg_shift_cluster2))~., 
                                                     FalseNeg_shift_cluster2, length))
FalseNeg_shift_cluster2_combs <- FalseNeg_shift_cluster2_combs %>% arrange(desc(n))
FalseNeg_shift_cluster2_combs <- mutate(FalseNeg_shift_cluster2_combs, 
                                        n_pct = round(n/sum(n)*100, 2), 
                                        n_cum_pct = round(cumsum(n)/sum(n)*100, 2))
FalseNeg_shift_cluster2_combs <- add_column(FalseNeg_shift_cluster2_combs, 
                                            comb_rank = min_rank(desc(FalseNeg_shift_cluster2_combs$n)), 
                                            .before = "adv_bb_type")

# Determine the number of times each combination of features (feature set) was 
# represented by a batted ball in Cluster 3.

FalseNeg_shift_cluster3_combs <- as_tibble(aggregate(cbind(n = 1:nrow(FalseNeg_shift_cluster3))~., 
                                                     FalseNeg_shift_cluster3, length))
FalseNeg_shift_cluster3_combs <- FalseNeg_shift_cluster3_combs %>% arrange(desc(n))
FalseNeg_shift_cluster3_combs <- mutate(FalseNeg_shift_cluster3_combs, 
                                        n_pct = round(n/sum(n)*100, 2), 
                                        n_cum_pct = round(cumsum(n)/sum(n)*100, 2))
FalseNeg_shift_cluster3_combs <- add_column(FalseNeg_shift_cluster3_combs, 
                                            comb_rank = min_rank(desc(FalseNeg_shift_cluster3_combs$n)), 
                                            .before = "adv_bb_type")

# Determine the number of times each combination of features (feature set) was 
# represented by a batted ball in Cluster 4.

FalseNeg_shift_cluster4_combs <- as_tibble(aggregate(cbind(n = 1:nrow(FalseNeg_shift_cluster4))~., 
                                                     FalseNeg_shift_cluster4, length))
FalseNeg_shift_cluster4_combs <- FalseNeg_shift_cluster4_combs %>% arrange(desc(n))
FalseNeg_shift_cluster4_combs <- mutate(FalseNeg_shift_cluster4_combs, 
                                        n_pct = round(n/sum(n)*100, 2), 
                                        n_cum_pct = round(cumsum(n)/sum(n)*100, 2))
FalseNeg_shift_cluster4_combs <- add_column(FalseNeg_shift_cluster4_combs, 
                                            comb_rank = min_rank(desc(FalseNeg_shift_cluster4_combs$n)), 
                                            .before = "adv_bb_type")

# Determine the number of times each combination of features (feature set) was 
# represented by a batted ball in Cluster 5.

FalseNeg_shift_cluster5_combs <- as_tibble(aggregate(cbind(n = 1:nrow(FalseNeg_shift_cluster5))~., 
                                                     FalseNeg_shift_cluster5, length))
FalseNeg_shift_cluster5_combs <- FalseNeg_shift_cluster5_combs %>% arrange(desc(n))
FalseNeg_shift_cluster5_combs <- mutate(FalseNeg_shift_cluster5_combs, 
                                        n_pct = round(n/sum(n)*100, 2), 
                                        n_cum_pct = round(cumsum(n)/sum(n)*100, 2))
FalseNeg_shift_cluster5_combs <- add_column(FalseNeg_shift_cluster5_combs, 
                                            comb_rank = min_rank(desc(FalseNeg_shift_cluster5_combs$n)), 
                                            .before = "adv_bb_type")

##################################################
# Conclusion
##################################################

# Import a data file into an R data frame.

singles_props <- 
  read_csv("https://raw.githubusercontent.com/jfmusso/HarvardX/master/Singles_Proportions.csv", 
           col_types = cols(
             Year = col_number(), 
             `1B` = col_integer(), 
             H = col_integer(), 
             BB = col_integer(), 
             HBP = col_integer()
          )
           
  )
           
class(singles_props[])

# Compute the average singles proportion for the period 1920 to 2019.

avg_singles_prop <- round(sum(singles_props$`1B`) / (sum(singles_props$H) + 
                                                 sum(singles_props$BB) + 
                                                 sum(singles_props$HBP)), 3)

# Plot the season-by-season singles proportions over the period 
# 1920 to 2019.

ggplot(data = singles_props, aes(x = Year, y = `1B/H+BB+HBP`)) + 
  geom_line(color = "royalblue3", size = 1.1) + 
  scale_x_continuous(breaks = seq(1920, 2020, by = 5), 
                   labels = c("1920", "1925", "1930", "1935", "1940", "1945",  "1950", "1955", "1960", 
                              "1965", "1970", "1975", "1980", "1985", "1990", "1995", "2000", "2005", 
                              "2010", "2015", "2020"), 
                   limits = c(1920, 2019)) + 
  scale_y_continuous(breaks = seq(0.42, 0.580, by = .02), 
                     labels = c("0.42", "0.44", "0.46", "0.48", "0.50", "0.52",  "0.54", "0.56", "0.58"), 
                     limits = c(0.43, 0.585)) + 
  theme(axis.text.x = element_text(angle = 45)) + 
  geom_hline(yintercept = avg_singles_prop) + 
  labs(title = "Singles as a Proportion of On-Base Events", x = "Year", 
       y = "Singles/(H+BB+HBP)") + 
  annotate("text", size = 4, fontface = 1, x = 1923, y = 0.506, 
           label = "Avg 0.503") + 
  annotate("text", size = 4, fontface = 1, x = 1930.5, y = 0.583, 
           label = "A") + 
  annotate("text", size = 4, fontface = 1, x = 1944, y = 0.553, 
           label = "B") + 
  annotate("text", size = 4, fontface = 1, x = 1955, y = 0.514, 
           label = "C") + 
  annotate("text", size = 4, fontface = 1, x = 1974, y = 0.542, 
           label = "D") + 
  annotate("text", size = 4, fontface = 1, x = 1989, y = 0.518, 
           label = "E") + 
  annotate("text", size = 4, fontface = 1, x = 2002.5, y = 0.482, 
           label = "F") + 
  annotate("text", size = 4, fontface = 1, x = 2013, y = 0.499, 
           label = "G") + 
  annotate("text", size = 4, fontface = 1, x = 2017.5, y = 0.470, 
           label = "H") + 
  annotate("rect", xmin = 1920, xmax = 1941, ymin = 0.516, ymax = 0.580,
           alpha = .1, color = "black", fill = "maroon") + 
  annotate("rect", xmin = 1942, xmax = 1946, ymin = 0.521, ymax = 0.550,
           alpha = .1, color = "black", fill = "olivedrab") + 
  annotate("rect", xmin = 1947, xmax = 1962, ymin = 0.485, ymax = 0.511,
           alpha = .1, color = "black", fill = "white") + 
  annotate("rect", xmin = 1963, xmax = 1984, ymin = 0.498, ymax = 0.539,
           alpha = .1, color = "black", fill = "yellow") + 
  annotate("rect", xmin = 1985, xmax = 1993, ymin = 0.485, ymax = 0.515,
           alpha = .1, color = "black", fill = "white") + 
  annotate("rect", xmin = 1994, xmax = 2010, ymin = 0.457, ymax = 0.479,
           alpha = .1, color = "black", fill = "green") + 
  annotate("rect", xmin = 2011, xmax = 2015, ymin = 0.480, ymax = 0.496,
           alpha = .1, color = "black", fill = "gray9") + 
  annotate("rect", xmin = 2016, xmax = 2019, ymin = 0.433, ymax = 0.467,
           alpha = .1, color = "black", fill = "maroon")

# Compute the average home run proportion for the period 1920 to 2019.

avg_hr_prop <- round(sum(singles_props$`HR`) / (sum(singles_props$H) + 
                                                  sum(singles_props$BB) + 
                                                  sum(singles_props$HBP)), 3)

# Compare the season-by-season singles proportions and home run proportions 
# over the period 1920 to 2019.

singles_prop_plot <- ggplot(data = singles_props, aes(x = Year, y = `1B/H+BB+HBP`)) + 
  geom_line(color = "royalblue3", size = 1.1) + 
  scale_x_continuous(breaks = seq(1920, 2020, by = 5), 
                     labels = c("1920", "1925", "1930", "1935", "1940", "1945",  "1950", "1955", "1960", 
                                "1965", "1970", "1975", "1980", "1985", "1990", "1995", "2000", "2005", 
                                "2010", "2015", "2020"), 
                     limits = c(1920, 2019)) + 
  scale_y_continuous(breaks = seq(0.42, 0.580, by = .02), 
                     labels = c("0.42", "0.44", "0.46", "0.48", "0.50", "0.52",  "0.54", "0.56", "0.58"), 
                     limits = c(0.43, 0.585)) + 
  theme(axis.text.x = element_text(angle = 45)) + 
  geom_hline(yintercept = avg_singles_prop) + 
  labs(title = "Singles as a Proportion of On-Base Events", x = "Year", 
       y = "Singles/(H+BB+HBP)") + 
  annotate("text", size = 4, fontface = 1, x = 1924, y = 0.508, 
           label = "Avg 0.503") + 
  annotate("text", size = 4, fontface = 1, x = 1930.5, y = 0.5845, 
           label = "A") + 
  annotate("text", size = 4, fontface = 1, x = 1944, y = 0.5545, 
           label = "B") + 
  annotate("text", size = 4, fontface = 1, x = 1955, y = 0.5155, 
           label = "C") + 
  annotate("text", size = 4, fontface = 1, x = 1974, y = 0.5435, 
           label = "D") + 
  annotate("text", size = 4, fontface = 1, x = 1989, y = 0.5195, 
           label = "E") + 
  annotate("text", size = 4, fontface = 1, x = 2002.5, y = 0.4835, 
           label = "F") + 
  annotate("text", size = 4, fontface = 1, x = 2013, y = 0.5005, 
           label = "G") + 
  annotate("text", size = 4, fontface = 1, x = 2017.5, y = 0.4715, 
           label = "H") + 
  annotate("rect", xmin = 1920, xmax = 1941, ymin = 0.516, ymax = 0.580,
           alpha = .1, color = "black", fill = "maroon") + 
  annotate("rect", xmin = 1942, xmax = 1946, ymin = 0.521, ymax = 0.550,
           alpha = .1, color = "black", fill = "olivedrab") + 
  annotate("rect", xmin = 1947, xmax = 1962, ymin = 0.485, ymax = 0.511,
           alpha = .1, color = "black", fill = "white") + 
  annotate("rect", xmin = 1963, xmax = 1984, ymin = 0.498, ymax = 0.539,
           alpha = .1, color = "black", fill = "yellow") + 
  annotate("rect", xmin = 1985, xmax = 1993, ymin = 0.485, ymax = 0.515,
           alpha = .1, color = "black", fill = "white") + 
  annotate("rect", xmin = 1994, xmax = 2010, ymin = 0.457, ymax = 0.479,
           alpha = .1, color = "black", fill = "green") + 
  annotate("rect", xmin = 2011, xmax = 2015, ymin = 0.480, ymax = 0.496,
           alpha = .1, color = "black", fill = "gray9") + 
  annotate("rect", xmin = 2016, xmax = 2019, ymin = 0.433, ymax = 0.467,
           alpha = .1, color = "black", fill = "maroon")

homers_prop_plot <- ggplot(data = singles_props, aes(x = Year, y = `HR/H+BB+HBP`)) + 
  geom_line(color = "red3", size = 1.1) + 
  scale_x_continuous(breaks = seq(1920, 2020, by = 5), 
                     labels = c("1920", "1925", "1930", "1935", "1940", "1945",  "1950", "1955", "1960", 
                                "1965", "1970", "1975", "1980", "1985", "1990", "1995", "2000", "2005", 
                                "2010", "2015", "2020"), 
                     limits = c(1920, 2019)) + 
  scale_y_continuous(breaks = seq(0.02, 0.12, by = .01), 
                     labels = c("0.02", "0.03", "0.04", "0.05", "0.06", "0.07",  
                                "0.08", "0.09", "0.10", "0.11", "0.12"), 
                     limits = c(0.02, 0.117)) + 
  theme(axis.text.x = element_text(angle = 45)) + 
  geom_hline(yintercept = avg_hr_prop) + 
  labs(title = "Home Runs as a Proportion of On-Base Events", x = "Year", 
       y = "Home Runs/(H+BB+HBP)") + 
  annotate("text", size = 4, fontface = 1, x = 1924, y = 0.069, 
           label = "Avg 0.066") + 
  annotate("text", size = 4, fontface = 1, x = 1930.5, y = 0.053, 
           label = "A") + 
  annotate("text", size = 4, fontface = 1, x = 1944, y = 0.043, 
           label = "B") + 
  annotate("text", size = 4, fontface = 1, x = 1955, y = 0.080, 
           label = "C") + 
  annotate("text", size = 4, fontface = 1, x = 1974, y = 0.077, 
           label = "D") + 
  annotate("text", size = 4, fontface = 1, x = 1989, y = 0.087, 
           label = "E") + 
  annotate("text", size = 4, fontface = 1, x = 2002.5, y = 0.092, 
           label = "F") + 
  annotate("text", size = 4, fontface = 1, x = 2013, y = 0.088, 
           label = "G") + 
  annotate("text", size = 4, fontface = 1, x = 2017.5, y = 0.116, 
           label = "H") + 
  annotate("rect", xmin = 1920, xmax = 1941, ymin = 0.021, ymax = 0.050,
           alpha = .1, color = "black", fill = "maroon") + 
  annotate("rect", xmin = 1942, xmax = 1946, ymin = 0.030, ymax = 0.040,
           alpha = .1, color = "black", fill = "olivedrab") + 
  annotate("rect", xmin = 1947, xmax = 1962, ymin = 0.048, ymax = 0.077,
           alpha = .1, color = "black", fill = "white") + 
  annotate("rect", xmin = 1963, xmax = 1984, ymin = 0.048, ymax = 0.074,
           alpha = .1, color = "black", fill = "yellow") + 
  annotate("rect", xmin = 1985, xmax = 1993, ymin = 0.059, ymax = 0.084,
           alpha = .1, color = "black", fill = "white") + 
  annotate("rect", xmin = 1994, xmax = 2010, ymin = 0.077, ymax = 0.089,
           alpha = .1, color = "black", fill = "green") + 
  annotate("rect", xmin = 2011, xmax = 2015, ymin = 0.073, ymax = 0.085,
           alpha = .1, color = "black", fill = "gray9") + 
  annotate("rect", xmin = 2016, xmax = 2019, ymin = 0.095, ymax = 0.113,
           alpha = .1, color = "black", fill = "maroon")

grid.arrange(singles_prop_plot, homers_prop_plot, ncol = 1)

# Compute the correlations between each on-base event's proportions.

cor_data <- dplyr::select(singles_props, Year, `1B/H+BB+HBP`, `2B/H+BB+HBP`, 
                          `3B/H+BB+HBP`, `HR/H+BB+HBP`, `BB/H+BB+HBP`, 
                          `HBP/H+BB+HBP`, `SB/G`, `SO/G`, `SH/G`)

cor_data <- cor_data %>% rename(Year = Year, `1B` = `1B/H+BB+HBP`, `2B` = `2B/H+BB+HBP`, 
                                `3B` = `3B/H+BB+HBP`, HR = `HR/H+BB+HBP`, BB = `BB/H+BB+HBP`, 
                                HBP = `HBP/H+BB+HBP`, SB = `SB/G`,  SO = `SO/G`, SH = `SH/G`)

props_cor <- round(cor(cor_data), 3)

# Create a correlogram to visualize the correlations and group them accordingly.

corrplot(props_cor, method = "color", type = "lower", 
         diag = FALSE, addCoef.col = "white", order = "hclust", 
         tl.col = "black", tl.srt = 0, tl.cex = 0.9, number.cex = 0.95, 
         number.font = 15)
title("Correlations Between On-Base Event Proportions", line = 2.5)

# Import a data file into an R data frame.

singles_props_leaderboard <- 
  read_csv("https://raw.githubusercontent.com/jfmusso/HarvardX/master/SinglesProp_Leaderboard2.csv", 
           col_types = cols(
             Player = col_character(), 
             Pos = col_character(), 
             From = col_number(), 
             To = col_number(), 
             Prop = col_double(), 
             `1B` = col_integer(), 
             `1B Rk` = col_integer(), 
             Honors = col_character()
           )
           
  )

class(singles_props_leaderboard[])
knitr::kable(singles_props_leaderboard, 
             caption = "Singles Proportion: Top 50 Career Leaders")
