.onAttach <- function(...) {
    packageStartupMessage(
        emoji::emoji("beach_with_umbrella"),
        crayon::white(" survivor "),
        crayon::cyan(utils::packageVersion("survivor"))
    )
}
