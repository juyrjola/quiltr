#' Parse JSON specification to find info about a file
#'
#' @param pkg_name which package the file belongs to
#' @param file path to the file, see examples
#'
#' @return dataframe with info about a particular file
#' @export
#'
#' @import dplyr magrittr tidyjson reticulate
#' @examples
#' qparse("examples/wine", "quality/red")
#' qparse("akarve/seattle_911", "responses")
qparse <- function(pkg_name, file) {
    tools <- reticulate::import(module = "quilt.tools")
    pkg_pythonic <- stringr::str_replace_all(pkg_name, "/", "\\.")
    stripped_pkg <- stringr::str_split(pkg_pythonic, "\\.")[[1]]
    pkg <- tools$store$PackageStore$find_package(NULL, stripped_pkg[1], stripped_pkg[2])
    contents <- pkg[[2]]

    # strip first layer of 'children'
    children <- contents$children
    # split the path
    nodes <- magrittr::extract2(stringr::str_split(file, "\\."), 1)

    # traverse to second-lowest level
    if (length(nodes > 1)) {
        for (i in nodes[1:length(nodes) - 1]) {
            children <- children %>%
                magrittr::extract2(i) %>%
                magrittr::extract2("children")
        }
    }

    # find the final subtable
    final <- magrittr::extract2(children, nodes[length(nodes)])

    if(paste(final) == "<quilt.tools.core.GroupNode>") {
        info <- dplyr::data_frame(
            name = paste(pkg_name, file),
            children = final$children,
            json_type = final$json_type,
            # preorder_tablenodes = final$preorder_tablenodes,
            class = "GroupNode"
        )
    } else if(paste(final) == "<quilt.tools.core.FileNode>") {
        info <- final
    } else {
        info <- dplyr::data_frame(
            name = paste(pkg_name, file),
            format = paste(final$format),
            hashes = final$hashes,
            json_type = final$json_type,
            class = "TableNode"
        )
    }

    info
}
