push_to_end <- function(vec, element)
{
  pos <- which(vec == element)
  if (length(pos) != 0) {
    vec <- append(vec, vec[pos])
    vec <- vec[-pos]
  }
  vec
}