# Useful functions


# estimate Beta function parameters from mean and variance

estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}


# Create categorical variable

Categorical <- function(outcomes, p = NULL) {
  if (!is.null(p) && length(outcomes) != length(p)) {
    stop("`outcomes` and `p` must be the same length.", call. = FALSE)
  }
  
  if (is.null(p)) {
    p <- rep(1 / length(outcomes), length(outcomes))
  }
  
  p <- p / sum(p)
  
  d <- list(outcomes = outcomes, p = p)
  class(d) <- c("Categorical", "distribution")
  d
}

#' @export
print.Categorical <- function(x, ...) {
  num_categories <- length(x$outcomes)
  
  if (num_categories > 3) {
    outcomes <- paste(
      c(x$outcomes[1:2], "...", x$outcomes[num_categories]),
      collapse = ", "
    )
    
    p <- paste(
      c(round(x$p, 3)[1:2], "...", round(x$p, 3)[num_categories]),
      collapse = ", "
    )
  } else {
    outcomes <- paste(x$outcomes, collapse = ", ")
    p <- paste(round(x$p, 3), collapse = ", ")
  }
  
  cat(
    glue(
      "Categorical distribution\n  outcomes = [{outcomes}]\n  p = [{p}]",
      .trim = FALSE
    ),
    "\n"
  )
}

# Sample from categorical distribution

random.Categorical <- function(x, n = 1L, ...) {
  sample(x = x$outcomes, size = n, prob = x$p, replace = TRUE)
}


# PERt distribution
rpert <- function( n, x.min, x.max, x.mode, lambda = 4 ){
  
  if( x.min > x.max || x.mode > x.max || x.mode < x.min ) stop( "invalid parameters" );
  
  x.range <- x.max - x.min;
  if( x.range == 0 ) return( rep( x.min, n ));
  
  mu <- ( x.min + x.max + lambda * x.mode ) / ( lambda + 2 );
  
  # special case if mu == mode
  if( mu == x.mode ){
    v <- ( lambda / 2 ) + 1
  }
  else {
    v <- (( mu - x.min ) * ( 2 * x.mode - x.min - x.max )) /
      (( x.mode - mu ) * ( x.max - x.min ));
  }
  
  w <- ( v * ( x.max - mu )) / ( mu - x.min );
  return ( rbeta( n, v, w ) * x.range + x.min );
}
