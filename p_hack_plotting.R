#Lege Stichprobenumfang fest
n <- 10000

# Legee Freiheitsgrade der t-Verteilung fest
df <- 98


# ziehe t-Werte aus t-Verteilung
ts <- rt(n, df)


# betrachte Verteilung der gezogenen Werte
dens_t <- density(ts)
plot(dens_t)



# Finde p-Werte zu t-Werten
p_values <- pt(ts, df)


# plotte p-Werte
hist(p_values)


## Plotte beides zusammen ##
plot(ts, p_values)



# wenn H0 gilt: wie oft muss man optional stoppen (nachschauen), bis man mit 90% Wahrscheinlichkeit ein signifikantes Ergebnis findet?
p_find_sig <- 0.05

# Funktion, die ausrechnet, wie wahrscheinlich ein signifikantes Ergebnis ist nach einer bestimmten Anzahl an Versuchen (n) und einem alpha-Level von p_find_sig
wie_wschl_sig_nach_versuchen <- function(n, p_find_sig) {
  1 - ((1 - p_find_sig) ^ n)
}

df_sim_1 <- data.frame(
  "p_values" =  wie_wschl_sig_nach_versuchen (1:200, 0.05),
  "vers_num" =  1:200,
  "tran" = 1:200) # z column is needed for transition_states()
df_sim_2 <- data.frame(
  "p_values" =  wie_wschl_sig_nach_versuchen (1:200, 0.005),
  "vers_num" =  1:200,
  "tran" = 1:200) 

library(ggplot2)
library(gganimate)

sim_1_p <-  ggplot(df_sim_1, aes(y = p_values, x = vers_num)) +
  geom_point(color='black', fill='lightblue',
             alpha = .7, shape=21, size=4) +
  transition_states(tran) +
  geom_hline(yintercept=1.0, linetype="dashed", 
             color = "red") +
  shadow_mark() +
  xlab("No. of trials") +
  ylab("p-values") +
  ggtitle("p-value = .05") +
  theme_classic()

anim_1 <- animate(sim_1_p)
anim_save("sim_1_p.gif", anim_1)


sim_2_p <-  ggplot(df_sim_2, aes(y = p_values, x = vers_num)) +
  geom_point(color='black', fill='lightblue',
             alpha = .7, shape=21, size=4) +
  transition_states(tran) +
  geom_hline(yintercept=1.0, linetype="dashed", 
             color = "red") +
  shadow_mark() +
  xlab("No. of trials") +
  ylab("p-values") +
  ggtitle("p-value = .005") +
  theme_classic()

anim <- animate(sim_2_p)
anim_save("sim_2_p.gif", anim)

