# SIRS-Model-in-R
The SIRS Model is a mathematical model used to study the spread of infectious diseases in a population. 
# Define the initial conditions
N <- 1000 # Total population
S0 <- 995 # Initial number of susceptible individuals
I0 <- 5 # Initial number of infected individuals
R0 <- 0 # Initial number of recovered individuals
beta <- 0.4 # Infectious rate (1/day)
gamma <- 0.2 # Recovery rate (1/day)
lambda <- 0.005 # Loss of immunity rate (1/day)

# Define the time interval and number of steps
t_start <- 0
t_end <- 100
steps <- 1000
dt <- (t_end - t_start)/steps

# Create a matrix to store the results
out <- matrix(nrow = steps + 1, ncol = 4)
colnames(out) <- c("time", "S", "I", "R")

# Initialize the first row of the matrix
out[1,] <- c(t_start, S0, I0, R0)

# Define the SIRS model
for (i in 2:(steps + 1)) {
  out[i, "time"] <- out[i - 1, "time"] + dt
  out[i, "S"] <- out[i - 1, "S"] - (beta * out[i - 1, "S"] * out[i - 1, "I"])/N + lambda * out[i - 1, "R"]
  out[i, "I"] <- out[i - 1, "I"] + (beta * out[i - 1, "S"] * out[i - 1, "I"])/N - gamma * out[i - 1, "I"]
  out[i, "R"] <- out[i - 1, "R"] + gamma * out[i - 1, "I"] - lambda * out[i - 1, "R"]
}

# Plot the results
par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1, mgp = c(3, 0.5, 0), font.lab = 2, cex.axis = 1.5, cex.lab = 1.5)
plot(out[, "time"], out[, "S"], type = "l", xlab = "Time (days)", ylab = "Number of Individuals", ylim = c(0, 1000), col = "black", lwd = 2, main = "SIRS Model")
lines(out[, "time"], out[, "I"], col = "red", lwd = 2)
lines(out[, "time"], out[, "R"], col = "blue", lwd = 2)
legend("topright", c("Susceptible", "Infected", "Recovered"), lty = c(1, 1, 1), col = c("black", "red", "blue"), bty = "n", cex = 1.5)

# Show the plot
show(plot)
