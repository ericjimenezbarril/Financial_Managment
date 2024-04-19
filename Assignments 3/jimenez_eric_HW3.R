# HW 3 

library(ggplot2)

# PLOT 1.1.B #
V1.1 <- function(x){
  ifelse(x<=100, 0,
         x-100)
}

V1.2 <- function(x){
  -ifelse(x<=120, 0,
          x-120)
}

V1 <- function(x){
  V1.1(x)+V1.2(x)
}

#plot 100 Call
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, aes(color = "100 Call")) +
  geom_vline(xintercept = 100, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("100 Call" = "steelblue3"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()

# plot 120 Call
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, aes(color = "120 Call")) +
  geom_vline(xintercept = 120, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("120 Call" = "orange"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()

## plot total
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "100 Call")) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "120 Call")) +
  stat_function(fun = V1, geom = "line", linewidth = 1.3, aes(color = "Total")) +
  geom_vline(xintercept = 100, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  geom_vline(xintercept = 120, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("100 Call" = "steelblue3", "120 Call" = "orange", "Total" = "red3"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()

# PLOT 1.1.D #

P1 = 15
P2 = 10

V1.1 <- function(x){
  ifelse(x<=100, -P1,
         x-100-P1)
}

V1.2 <- function(x){
  -ifelse(x<=120, -P2,
          x-120-P2)
}

V1 <- function(x){
  ifelse(x <= 100, P2-P1,
         ifelse(x <= 120, x-100+P2-P1, 
                20+P2-P1))
}


# 100 call
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3,aes(color = "100 Call")) +
  geom_vline(xintercept = 100, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("100 Call" = "steelblue3"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  annotate("text", x = 0, y = -P1+3, label = "P1", color="steelblue3", size = 3)+
  theme_minimal()
# plot 120 call venta
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, aes(color = "120 Call")) +
  geom_vline(xintercept = 120, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("120 Call" = "orange"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  annotate("text", x = 0, y = P2+3, label = "P2", color="orange", size = 3) +
  theme_minimal()
# plot total
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "100 Call")) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "120 Call")) +
  stat_function(fun = V1, geom = "line", linewidth = 1.3, aes(color = "Total")) +
  geom_vline(xintercept = 100, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  geom_vline(xintercept = 120, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("100 Call" = "steelblue3", "120 Call" = "orange", "Total" = "red3"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  annotate("text", x = 0, y = -P1+5, label = "P1", color="steelblue3", size = 3)+
  annotate("text", x = 0, y = P2+5, label = "P2", color="orange", size = 3) +
  annotate("text", x = 5, y = P2-P1+5, label = "P2-P1", color="red3", size = 3) +
  theme_minimal()

# PLOT 1.2.B #

V1.1 <- function(x){
  -ifelse(x<=100, 100-x,
         0)
}

V1.2 <- function(x){
  ifelse(x<=120, 120-x,
          0)
}

V1 <- function(x){
  V1.1(x)+V1.2(x)
}

# 100 Put venta
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3,aes(color = "100 Put")) +
  geom_vline(xintercept = 100, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("100 Put" = "steelblue3"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()

# 120 Put compra 
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, aes(color = "120 Put")) +
  geom_vline(xintercept = 120, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("120 Put" = "orange"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
# plot total
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "100 Put")) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "120 Put")) +
  stat_function(fun = V1, geom = "line", linewidth = 1.3, aes(color = "Total")) +
  geom_vline(xintercept = 100, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  geom_vline(xintercept = 120, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("100 Put" = "steelblue3", "120 Put" = "orange", "Total" = "red3"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()

# PLOT 1.2.D #

P1 = 10
P2 = 15

V1.1 <- function(x){
  -ifelse(x<=100, 100-x-P1,
          -P1)
}

V1.2 <- function(x){
  ifelse(x<=120, 120-x-P2,
         -P2)
}

V1 <- function(x){
  V1.1(x)+V1.2(x)
}


# plot 100 Put venta primes
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, aes(color = "100 Put")) +
  geom_vline(xintercept = 100, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("100 Put" = "steelblue3"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  annotate("text", x = 190, y = P1+3, label = "P1", color="steelblue3", size = 3)+
  theme_minimal()


# plot 120 put compra primes
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, aes(color = "120 Put")) +
  geom_vline(xintercept = 120, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("120 Put" = "orange"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  annotate("text", x = 200, y = -P2+3, label = "P2", color="orange", size = 3) +
  theme_minimal()
# Plot total
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "100 Put")) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "120 Put")) +
  stat_function(fun = V1, geom = "line", linewidth = 1.3, aes(color = "Total")) +
  geom_vline(xintercept = 100, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  geom_vline(xintercept = 120, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("100 Put" = "steelblue3", "120 Put" = "orange", "Total" = "red3"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  annotate("text", x = 190, y = P1+7, label = "P1", color="steelblue3", size = 3)+
  annotate("text", x = 190, y = -P2+6, label = "P2", color="orange", size = 3) +
  annotate("text", x = 190, y = P1-P2+7, label = "P1-P2", color="red3", size = 3) +
  theme_minimal()


# PLOT 1.3.B #

V1.1 <- function(x){
  x-100
}

V1.2 <- function(x){
  -ifelse(x<=110,0, x-110)
}

V1 <- function(x){
  V1.1(x)+V1.2(x)
}

# plot anar en llarg
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3,  aes(color = "Llarg")) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("Llarg" = "steelblue3"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
# plot vendre call 110
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, aes(color = "110 Call")) +
  geom_vline(xintercept = 110, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("110 Call" = "orange"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
# plot total
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "Llarg")) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "110 Call")) +
  stat_function(fun = V1, geom = "line", linewidth = 1.3, aes(color = "Total")) +
  geom_vline(xintercept = 110, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("Llarg" = "steelblue3", "110 Call" = "orange", "Total" = "red3"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()

# PLOT 1.3.D
P = 10

V1.1 <- function(x){
  x-100
}

V1.2 <- function(x){
  P - ifelse(x<=110,0, x-110)
}

V1 <- function(x){
  V1.1(x)+V1.2(x)
}

# anar en llarg
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, aes(color = "Llarg")) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("Llarg" = "steelblue3"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
# 110 call vendre
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, aes(color = "110 Call")) +
  geom_vline(xintercept = 110, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("110 Call" = "orange"), name = "Llegenda") + 
  annotate("text", x = 10, y = P+5, label = "P", color="orange", size = 3) +
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()

# plot total primes
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "Llarg")) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "110 Call")) +
  stat_function(fun = V1, geom = "line", linewidth = 1.3, aes(color = "Total")) +
  geom_vline(xintercept = 110, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("Llarg" = "steelblue3", "110 Call" = "orange", "Total" = "red3"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()



# PLOT 1.4.B #

V1.1 <- function(x){
  100 - x
}

V1.2 <- function(x){
  -ifelse(x<=90,90-x,0)
}

V1 <- function(x){
  V1.1(x)+V1.2(x)
}

# anar en curt
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, aes(color = "Curt")) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("Curt" = "steelblue3"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()

# vendre 90 put
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, aes(color = "90 Put")) +
  geom_vline(xintercept = 90, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("90 Put" = "orange"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()

# plot total
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "Curt")) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "90 Put")) +
  stat_function(fun = V1, geom = "line", linewidth = 1.3, aes(color = "Total")) +
  geom_vline(xintercept = 90, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("Curt" = "steelblue3", "90 Put" = "orange", "Total" = "red3"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()

# PLOT 1.4.D
P = 10

V1.1 <- function(x){
  100 - x
}

V1.2 <- function(x){
  P-ifelse(x<=90,90-x,0)
}

V1 <- function(x){
  V1.1(x)+V1.2(x)
}


# anar en curt
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, aes(color = "Curt")) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("Curt" = "steelblue3"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
#vendre 90 put
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3,   aes(color = "90 Put")) +
  geom_vline(xintercept = 90, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("90 Put" = "orange"), name = "Llegenda") + 
  annotate("text", x = 200, y = P+5, label = "P", color="orange", size = 3) +
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
#plot total
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "Curt")) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "90 Put")) +
  stat_function(fun = V1, geom = "line", linewidth = 1.3, aes(color = "Total")) +
  geom_vline(xintercept = 90, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("Curt" = "steelblue3", "90 Put" = "orange", "Total" = "red3"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()



# PLOT 1.5.B #

V1.1 <- function(x){
  x-100
}

V1.2 <- function(x){
  -ifelse(x<=110,0, x-110)
}

V1.3 <- function(x){
  ifelse(x<=90,90-x,0)
}

V1 <- function(x){
  V1.1(x)+V1.2(x)+V1.3(x)
}

# plot anar en llarg
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, aes(color = "Llarg")) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("Llarg" = "steelblue3"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
#venere 110 call
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, aes(color = "110 Call")) +
  geom_vline(xintercept = 110, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("110 Call"="chartreuse"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
#vendre 90 put
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.3, geom = "line", linewidth = 1.3,  aes(color = "90 Put")) +
  geom_vline(xintercept = 90, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("90 Put" = "orange"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
#plot total
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "Llarg")) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "110 Call")) +
  stat_function(fun = V1.3, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "90 Put")) +
  stat_function(fun = V1, geom = "line", linewidth = 1.3, aes(color = "Total")) +
  geom_vline(xintercept = 90, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  geom_vline(xintercept = 110, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("Llarg" = "steelblue3","110 Call"="chartreuse", "90 Put" = "orange", "Total" = "red3"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()

# PLOT 1.5.D
P2 = 15
P3 = 10

V1.1 <- function(x){
  x-100
}

V1.2 <- function(x){
  P2-ifelse(x<=110,0, x-110)
}

V1.3 <- function(x){
  ifelse(x<=90,90-x,0)-P3
}

V1 <- function(x){
  V1.1(x)+V1.2(x)+V1.3(x)
}


# plot anar en llarg
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, aes(color = "Llarg")) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("Llarg" = "steelblue3"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
#venere 110 call
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3,  aes(color = "110 Call")) +
  geom_vline(xintercept = 110, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("110 Call"="chartreuse"), name = "Llegenda") + 
  annotate("text", x = 20, y = P2+5, label = "P2", color="chartreuse", size = 3) +
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
#vendre 90 put
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.3, geom = "line", linewidth = 1.3,  aes(color = "90 Put")) +
  geom_vline(xintercept = 90, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("90 Put" = "orange"), name = "Llegenda") + 
  annotate("text", x = 200, y = P3-15, label = "P3", color="orange", size = 3) +
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()

# plot total amb primes
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "Llarg")) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "110 Call")) +
  stat_function(fun = V1.3, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "90 Put")) +
  stat_function(fun = V1, geom = "line", linewidth = 1.3, aes(color = "Total")) +
  geom_vline(xintercept = 90, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  geom_vline(xintercept = 110, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("Llarg" = "steelblue3","110 Call"="chartreuse", "90 Put" = "orange", "Total" = "red3"), name = "Llegenda") + 
  annotate("text", x = 10, y = P2+10, label = "P2", color="chartreuse", size = 3) +
  annotate("text", x = 200, y = P3-15, label = "P3", color="orange", size = 3) +
  annotate("text", x = 200, y = P2-P3+3, label = "10+P2-P3", color="red3", size = 3) +
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()




# PLOT 1.6.B #

V1.1 <- function(x){
  ifelse(x<=90, 0, x-90)
}

V1.2 <- function(x){
  -2*ifelse(x<=100,0, x-100)
}

V1.3 <- function(x){
  ifelse(x<=110,0,x-110)
}

V1 <- function(x){
  V1.1(x)+V1.2(x)+V1.3(x)
}

#plot compra 90 call
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3,  aes(color = "90 Call")) +
  geom_vline(xintercept = 90, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("90 Call" = "steelblue3"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
#plot venta 100 call
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, aes(color = "100 Call")) +
  geom_vline(xintercept = 100, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("100 Call"="chartreuse"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
#plot compra 110 call
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.3, geom = "line", linewidth = 1.3, aes(color = "110 Call")) +
  geom_vline(xintercept = 110, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("110 Call" = "orange"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
# plot total
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "90 Call")) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "Dos 100 Call")) +
  stat_function(fun = V1.3, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "110 Call")) +
  stat_function(fun = V1, geom = "line", linewidth = 1.3, aes(color = "Total")) +
  geom_vline(xintercept = 100, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  geom_vline(xintercept = 90, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  geom_vline(xintercept = 110, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("90 Call" = "steelblue3","Dos 100 Call"="chartreuse", "110 Call" = "orange", "Total" = "red3"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()

# PLOT 1.6.D
P1= 22
P2 = 14
P3 = 10


V1.1 <- function(x){
  ifelse(x<=90, 0, x-90)-P1
}

V1.2 <- function(x){
  2*P2-2*ifelse(x<=100,0, x-100)
}

V1.3 <- function(x){
  ifelse(x<=110,0,x-110)-P3
}

V1 <- function(x){
  V1.1(x)+V1.2(x)+V1.3(x)
}

# compra 90 call
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, aes(color = "90 Call")) +
  geom_vline(xintercept = 90, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("90 Call" = "steelblue3"), name = "Llegenda") + 
  annotate("text", x = 5, y = -P1+5, label = "P1", color="steelblue3", size = 3)+
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
# venta 100 call primes
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3,  aes(color = "100 Call")) +
  geom_vline(xintercept = 100, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("100 Call"="chartreuse"), name = "Llegenda") + 
  annotate("text", x = 20, y = P2+5, label = "P2", color="chartreuse", size = 3) +
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
#compra 110 call
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.3, geom = "line", linewidth = 1.3,  aes(color = "110 Call")) +
  geom_vline(xintercept = 110, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("110 Call" = "orange"), name = "Llegenda") + 
  annotate("text", x = 5, y = -P3+5, label = "P3", color="orange", size = 3)+
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()

#plot total
ggplot(data.frame(x = c(50, 150)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "90 Call")) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "Dos 100 Call")) +
  stat_function(fun = V1.3, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "110 Call")) +
  stat_function(fun = V1, geom = "line", linewidth = 1.3, aes(color = "Total")) +
  geom_vline(xintercept = 100, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  geom_vline(xintercept = 90, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  geom_vline(xintercept = 110, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(50, 150, by = 20), limits = c(50, 150)) +
  scale_color_manual(values = c("90 Call" = "steelblue3","Dos 100 Call"="chartreuse", "110 Call" = "orange", "Total" = "red3"), name = "Llegenda") + 
  annotate("text", x = 50, y = -P1+4, label = "P1", color="steelblue3", size = 3)+
  annotate("text", x = 50, y = -P3+4, label = "P3", color="orange", size = 3)+
  annotate("text", x = 50, y = P2+18, label = "P2", color="chartreuse", size = 3) +
  annotate("text", x=150, y=2*P2-P1-P3 + 10, label="2P2-P1-P3", col="red", size=3)+
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()




# PLOT 1.7.B #

V1.1 <- function(x){
  ifelse(x<=90, 0, x-90)
}

V1.2 <- function(x){
  -ifelse(x<=100,0, x-100)
}

V1.3 <- function(x){
  -ifelse(x<=110,0,x-110)
}

V1.4 <- function(x){
  ifelse(x<=120,0,x-120)
}


V1 <- function(x){
  V1.1(x)+V1.2(x)+V1.3(x)+V1.4(x)
}


#plot compra 90 call
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, aes(color = "90 Call")) +
  geom_vline(xintercept = 90, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("90 Call" = "steelblue3"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
# venta 100 call
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3,  aes(color = "100 Call")) +
  geom_vline(xintercept = 100, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("100 Call"="chartreuse"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
# venta 110 call
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.3, geom = "line", linewidth = 1.3, aes(color = "110 Call")) +
  geom_vline(xintercept = 110, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("110 Call"="violet"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
# compra 120 call
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.4, geom = "line", linewidth = 1.3,  aes(color = "120 Call")) +
  geom_vline(xintercept = 120, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("120 Call" = "orange"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
#plot total
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "90 Call")) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "100 Call")) +
  stat_function(fun = V1.3, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "110 Call")) +
  stat_function(fun = V1.4, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "120 Call")) +
  stat_function(fun = V1, geom = "line", linewidth = 1.3, aes(color = "Total")) +
  geom_vline(xintercept = 100, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  geom_vline(xintercept = 90, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  geom_vline(xintercept = 110, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  geom_vline(xintercept = 120, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("90 Call" = "steelblue3","100 Call"="chartreuse","110 Call"="violet", "120 Call" = "orange", "Total" = "red3"), name = "Llegenda") + 
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()

# PLOT 1.7.D #
P1= 22
P2= 14
P3= 12
P4= 6

V1.1 <- function(x){
  ifelse(x<=90, 0, x-90) - P1
}

V1.2 <- function(x){
  P2-ifelse(x<=100,0, x-100)
}

V1.3 <- function(x){
P3-ifelse(x<=110,0,x-110)
}

V1.4 <- function(x){
  ifelse(x<=120,0,x-120) - P4
}


V1 <- function(x){
  V1.1(x)+V1.2(x)+V1.3(x)+V1.4(x)
}

#plot compra 90 call
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, aes(color = "90 Call")) +
  geom_vline(xintercept = 90, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("90 Call" = "steelblue3"), name = "Llegenda") + 
  annotate("text", x = 5, y = -P1+5, label = "P1", color="steelblue3", size = 3)+
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
# venta 100 call
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, aes(color = "100 Call")) +
  geom_vline(xintercept = 100, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("100 Call"="chartreuse"), name = "Llegenda") + 
  annotate("text", x = 20, y = P2+5, label = "P2", color="chartreuse", size = 3) +
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
# venta 110 call
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.3, geom = "line", linewidth = 1.3, aes(color = "110 Call")) +
  geom_vline(xintercept = 110, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("110 Call"="violet"), name = "Llegenda") + 
  annotate("text", x = 20, y = P3+5, label = "P3", color="violet", size = 3) +
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
# compra 120 call
ggplot(data.frame(x = c(0, 200)), aes(x)) +
  stat_function(fun = V1.4, geom = "line", linewidth = 1.3,  aes(color = "120 Call")) +
  geom_vline(xintercept = 120, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200)) +
  scale_color_manual(values = c("120 Call" = "orange"), name = "Llegenda") + 
  annotate("text", x = 5, y = -P4+5, label = "P4", color="orange", size = 3)+
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()
#plot total
ggplot(data.frame(x = c(50, 150)), aes(x)) +
  stat_function(fun = V1.1, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "90 Call")) +
  stat_function(fun = V1.2, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "100 Call")) +
  stat_function(fun = V1.3, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "110 Call")) +
  stat_function(fun = V1.4, geom = "line", linewidth = 1.3, alpha = 0.5, linetype = "dashed", aes(color = "120 Call")) +
  stat_function(fun = V1, geom = "line", linewidth = 1.3, aes(color = "Total")) +
  geom_vline(xintercept = 100, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  geom_vline(xintercept = 90, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  geom_vline(xintercept = 110, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  geom_vline(xintercept = 120, linetype = "dashed", size = 0.7, color = "black", alpha = 0.2) +
  scale_x_continuous(breaks = seq(50, 150, by = 20), limits = c(50, 150)) +
  scale_color_manual(values = c("90 Call" = "steelblue3","100 Call"="chartreuse","110 Call"="violet", "120 Call" = "orange", "Total" = "red3"), name = "Llegenda") + 
  annotate("text", x = 50, y = -P1+3, label = "P1", color="steelblue3", size = 3)+
  annotate("text", x = 50, y = -P4-3, label = "P4", color="orange", size = 3)+
  annotate("text", x = 50, y = P2+2, label = "P2", color="chartreuse", size = 3) +
  annotate("text", x = 50, y = P3-3, label = "P3", color="violet", size = 3) +
  annotate("text", x=55, y=P3+P2-P1-P4 +2, label="P2+P3-P1-P4", col="red", size=3)+
  labs(title = "Funcio de pagament", x = "S(T)", y = "Benficis/Perdues", color = "Función") +
  theme_minimal()

