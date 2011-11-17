## wrapper to make eapply search the current env
eapply2 <- function(FUN, env=topenv(), ..., all.names = FALSE) {
	base::eapply(env=env, FUN=FUN, ..., all.names=all.names)
}
