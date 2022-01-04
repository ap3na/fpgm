# 5.2 fun_nlr.lgm 
# 
nlr.lgm <- function(data) {
  minpack.lm::nlsLM(
    Q ~ lgm(t, K, n, a),
    start = c(K = max(data$Q, na.rm = T),
              a = max(data$Q, na.rm = T),
              n = 1),
    control = c(nls.control(maxiter = 1024)),
    trace = F,
    model = TRUE,
    data = data)
}
