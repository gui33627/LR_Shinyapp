
create_sigma_formula <- function(x1, x2, x3, p, x4){
  sigma <- sprintf("$\\sigma = \\sqrt{\\frac{%s + \\cdots +%s }{%d - %d}} = %.2f$", x1, x2, x3, p, x4)
  return(sigma)
}