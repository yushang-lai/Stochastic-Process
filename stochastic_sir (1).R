## Let's write a function that simulates one realization of the stochastic SIR model.
## We are going to assume that the number of recovered individuals is 0 at time 0.

simulateSIR = function(beta, alpha, init_susceptible, init_infectious, total_time){

  current_susceptible = my_susceptible = init_susceptible
  current_infectious = my_infectious = init_infectious
  current_time = my_times = 0


  waiting_time = rexp(1, beta*current_infectious*current_susceptible + alpha*current_infectious)

  current_time = current_time + waiting_time

  while ((current_time < total_time) & (current_infectious !=0)){

    my_move = sample(1:2, size=1, prob = c(beta*current_infectious*current_susceptible, alpha*current_infectious))

    if (my_move == 1){
      current_infectious = current_infectious + 1
      current_susceptible = current_susceptible - 1

    }else{
      current_infectious = current_infectious - 1
    }

    my_susceptible = c(my_susceptible, current_susceptible)
    my_infectious = c(my_infectious, current_infectious)
    my_times = c(my_times, current_time)

    if (current_infectious !=0){
      waiting_time = rexp(1, beta*current_infectious*current_susceptible + alpha*current_infectious)
    }

    current_time = current_time + waiting_time
  }

  my_susceptible = c(my_susceptible, current_susceptible)
  my_infectious = c(my_infectious, current_infectious)
  my_times = c(my_times, total_time)

  return_matrix = cbind(my_susceptible, my_infectious, my_times)
  colnames(return_matrix) = c("S", "I", "times")

  return(return_matrix)
}



sir_sim = list()

set.seed(2020)
num_realizations = 3
population_size = 10000

for (i in 1:num_realizations){
  sir_sim[[i]] = simulateSIR(beta=8/population_size, alpha=4.0, init_susceptible = population_size-1, init_infectious=1, total_time = 4)
}

my_line_type = c(1,2,3)

par(mar=c(5,5,4,1))

plot(1,1, xlim=c(0,4), ylim = c(0,population_size), type="n", xlab="Time", ylab="Number of Individuals", cex.lab = 2.0, cex.main = 2.0, cex.axis=2.0, main="N=1,000")
for (i in 1:num_realizations){
  lines(sir_sim[[i]][,"times"], sir_sim[[i]][,"S"], type="s", col="blue", lwd=3, lty=my_line_type[i])
}

for (i in 1:num_realizations){
  lines(sir_sim[[i]][,"times"], sir_sim[[i]][,"I"], type="s", col="red", lty=my_line_type[i], lwd=3)
}

for (i in 1:num_realizations){
  lines(sir_sim[[i]][,"times"], population_size-sir_sim[[i]][,"I"]-sir_sim[[i]][,"S"], type="s", col="black", lty=my_line_type[i], lwd=3)
}

legend(2.5,0.75*population_size, legend=c("Susceptible", "Infectious", "Removed"), col=c("blue", "red", "black"), lwd=3, cex=1.8, bty="n")
