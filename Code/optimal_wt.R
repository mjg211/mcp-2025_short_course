##### README ###################################################################
# Filename:      optimal_wt.R                                                  #
# Author(s):     Michael Grayling (mgraylin@its.jnj.com)                       #
#                Yevgen Tymofyeyev (ytymofye@its.jnj.com)                      #
# Description:   Provides functions for Exercise 5 in Practical 1 of the       #
#                MCP-2025 Short Course 'An introduction to graphical testing   #
#                procedures for group-sequential designs', relating to optimal #
#                (Wang-Tsiatis) group-sequential design.                       #
# Last modified: 2025/07/11                                                    #

##### FUNCTIONS ################################################################

# Function that returns the ESS of a particular Wang-Tsiatis design; first
# derives the design for desired operating characteristics. Used in optimizing
# below
opt_fun <- function(
  x,                    # Value of the Wang-Tsiatis boundary shape param Delta
  timing   = c(0.5, 1), # Timing of IAs on IF scale
  delta    = -log(0.7), # Effect for power calculation
  theta    = delta,     # Effect vector for which to get expected value(s)
  thetawgt = 1,         # Corresponding weights for above effects in theta
  alpha    = 0.025,     # Desired type I error rate
  beta     = 0.2,       # Desired type II error rate
  sfu      = "WT"       # Spending function
) {
  # Derive design
  d    <- gsDesign::gsDesign(
    k         = length(timing),
    timing    = timing,
    test.type = 1,
    alpha     = alpha,
    beta      = beta,
    delta     = delta/2,
    sfu       = "WT",
    sfupar    = x
  )
  # Compute boundary crossing probabilities for input theta
  y    <- gsDesign::gsProbability(theta = theta/2, d = d)
  # Compute weighted average of expected values
  sp_1 <- head(y$upper$prob,-1)
  en   <- sum(y$n.I * c(sp_1, 1 - sum(sp_1)))
  en   <- sum(as.vector(en)*as.vector(thetawgt))
  en
}

# Function to return the ESS of a specified group-sequential design at a
# particular effect
en      <- function(
  d,                # Design object
  theta = -log(0.7) # Effect for which to get stopping probabilities
) {
  # Compute boundary crossing probabilities for input theta
  y    <- gsDesign::gsProbability(theta = theta/2, d = d)
  # Compute weighted average of expected values
  sp_1 <- head(y$upper$prob, -1)
  en   <- sum(y$n.I * c(sp_1, 1 - sum(sp_1)))
  en   <- sum(as.vector(en))
  en
}
en      <- Vectorize(en, "theta")

##### EXAMPLE ##################################################################
##### Set-up to return optimal Wang-Tsiatis boundary shape parameter for 2.5%
##### one-sided alpha and 80% power

# Call optimizer to get optimal WT parameter
x       <- stats::nlminb(
  start    = 0.15, # Starting value for opt_fun
  opt_fun,         # Function to optimize
  # Other fixed parameters
  timing   = (1:3)/3,
  alpha    = 0.025,
  beta     = 0.2,
  delta    = -log(0.7),
  theta    = -log(0.7), # Compute ESS under H1
  thetawgt = 1
)
# Optimal value of parameter for WT boundary
x$par # ~0.389 as seen in the slide deck
x$objective
# Compare the optimal design to that for O'Brien-Fleming and Pocock boundaries,
# over a range of effects
tibble::tibble(
  # Vector of HRs to consider
  theta  = seq(-log(0.5), -log(1), length.out = 500),
  HR     = exp(-theta),
  # O'Brien-Fleming design
  gsd_OF = list(gsDesign::gsDesign(
    k         = 3,
    timing    = (1:3)/3,
    test.type = 1,
    alpha     = 0.025,
    beta      = 0.2,
    delta     = -log(0.7)/2,
    sfu       = "OF"
  )),
  # Pocock design
  gsd_P = list(gsDesign::gsDesign(
    k         = 3,
    timing    = (1:3)/3,
    test.type = 1,
    alpha     = 0.025,
    beta      = 0.2,
    delta     = -log(0.7)/2,
    sfu       = "Pocock"
  )),
  # Optimal Wang-Tsiatis design
  gsd_WT = list(gsDesign::gsDesign(
    k         = 3,
    timing    = (1:3)/3,
    test.type = 1,
    alpha     = 0.025,
    beta      = 0.2,
    delta     = -log(0.7)/2,
    sfu       = "WT",
    sfupar    = x$par
  )),
  # Set ESSs for each design
  OF          = en(gsd_OF[[1]], theta),
  Pocock      = en(gsd_P[[1]], theta),
  `WT(0.389)` = en(gsd_WT[[1]], theta)
) -> store
# Plot the results
store |>
  tidyr::pivot_longer(OF:`WT(0.389)`) |>
  ggplot2::ggplot(ggplot2::aes(HR, value, colour = name)) +
  ggplot2::geom_line() +
  ggplot2::theme_bw() +
  ggplot2::theme(aspect.ratio = 1,
                 text = ggplot2::element_text(size = 16),
                 legend.position = "bottom") +
  ggplot2::labs(x      = expression(paste("Hazard ratio, ", italic(HR))),
                y      = "Expected events",
                colour = "") -> plot_optimal
ggplot2::ggsave("optimal_wt.pdf", plot_optimal, "pdf", width = 5, height = 5,
                units = "in")
