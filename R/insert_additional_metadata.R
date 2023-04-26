#' Insert additional metadata into an EML document
#' @description Sometime, for various reasons, additional metadata just needs to be inserted into EML documents manually. This is for quickly doing that in R. The function adds a snippet of plain text EML after a specified line number in the EML file. A pattern can also be defined for grep-ing the line number to append to (e.g. additional metadata needs to be inserted after a specific line). The defaults reflect BLE's need to add a snippet to aid replication of metadata to the Arctic Data Center that for a certain reason cannot be added via our normal means (the EML R package).
#' @param file (character) Path to EML file. Function will also write to this same file.
#' @param pattern (character) Grep pattern. \code{grep()} will be called on the contents of the file, and the first match will be used. Defaults to "<\dataset>".
#' @param addition (character) Plain text of EML snippet to insert. Defaults to an "additionalMetadata" node pertaining to replication I won't paste here.
#' @param after (numeric) A line number to append after. Only comes into play if pattern is not found. No defaults.
#' @return Nothing in the R environment, but a file with the snippet inserted where specified. File will remain the same
#' @export
insert_additional_metadata <- function(file,
                                       pattern = NULL,
                                       addition = NULL,
                                       after) {
  stopifnot(file.exists(file))
  lines <- readLines(file)
  if (is.null(pattern))
    pattern <- "</dataset>"
  if (is.null(addition))
    addition <- "<additionalMetadata>
  <metadata>
  <d1v1:replicationPolicy xmlns:d1v1=\"http://ns.dataone.org/service/types/v1\" numberReplicas=\"1\" replicationAllowed=\"true\">
<preferredMemberNode>urn:node:ARCTIC</preferredMemberNode>
</d1v1:replicationPolicy>
</metadata>
</additionalMetadata>
"
  after2 <- grep(pattern = pattern,
                 x = lines)
  if (length(after2) > 0) # only if grep yielded at least one match
    after <- after2[1] # only the first result
  if (!is.null(after)) { #
    lines <- append(lines, addition, after)
  writeLines(lines, file)
  } else
    stop(
      "Pattern was not found in the file text and the after argument was not specified. Please check the supplied pattern or specify a line number after which the additional text should be appended."
    )
}
