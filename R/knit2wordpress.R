
knit2wordpress <- function(file,syntax.highlighter=NULL) {
  if (!(toupper(syntax.highlighter) %in% c('WP-SYNTAX','WP-CODEBOX','SYNTAX HIGHLIGHTER EVOLVED') || is.null(syntax.highlighter))){
    stop("Need valid Syntax Highlighter.")
  }

  require(XML)
  post.content <- readLines(file)

  ## Operations common to all highlighters
  post.content <- gsub(" <", "&nbsp;<", post.content)
  post.content <- gsub("> ", ">&nbsp;", post.content)
  post.content <- gsub("<br/>", "&nbsp;", post.content)

  post.content <- htmlTreeParse(post.content)
  post.content <- paste(capture.output(print(post.content$children$html$children$body,
                                             indent = FALSE, tagSeparator = "")), collapse = "\n")
  ## Change to actual apostrophes
  post.content <- gsub('&apos;',"'",post.content)
  post.content <- gsub('&quot;',"'",post.content)
  post.content <- gsub('&lt;',"<",post.content)
  post.content <- gsub('&gt;',">",post.content)
  post.content <- gsub('&amp;',"&",post.content)
#   post.content <- gsub('&gt;',">",post.content)
#   post.content <- gsub('&gt;',">",post.content)

  ## Somewhere this tag is getting inserted and is uninterpretable
  post.content <- gsub("<br/>", "&nbsp;", post.content)

  if(!is.null(syntax.highlighter)){

    # Default syntax.highlighter is SYNTAX HIGHLIGHTER EVOLVED
    # These substitutions prepare the .html file assuming that will be the interpreter
    post.content <- gsub("<pre><code>","\\[code]",post.content)
    post.content <- gsub("<pre><code class=\"r\">","\\[code lang=\'r\'\\]",post.content)
    post.content <- gsub("<pre><code class=\"python\">","\\[code lang=\'python\'\\]",post.content)

    post.content <- gsub("</code></pre>","\\[/code\\]",post.content)

    ## Sometimes you get [code] tags put around each line in a code block.
    ## This makes them all part of one code block
    post.content <- gsub("\\[/code\\]\n\\[code\\]", "\n", post.content)
    post.content <- gsub("\\[/code\\]\n\\[code lang=\'r\'\\]", "\n", post.content)
    post.content <- gsub("\\[/code\\]\n\\[code lang=\'python\'\\]", "\n", post.content)

    # Prepare .html for WP-SYTNAX or WP-CODEBOX
    if (toupper(syntax.highlighter) %in% c('WP-SYNTAX','WP-CODEBOX')){
      # Translate [code] blocks to <pre> blocks
      post.content <- gsub("\\[code\\]", "<pre lang=\"R\" line=\"1\">", post.content)
      post.content <- gsub("\\[code lang=\'r\'\\]", "<pre lang=\"rsplus\" line=\"1\">", post.content)
      post.content <- gsub("\\[/code]", "</pre>", post.content)
    }
  }
  return(post.content)
}
