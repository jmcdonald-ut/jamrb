def foo
  standard = <<INDENTED_TEXT
Identents are considered part of the text like so.
  HEREDOC
INDENTED_TEXT

  indented = <<-INDENTED_HEREDOC
  This actually ignores the first ident, this may mean I should capture how
  deep we are idented?
  What happens with this tab?
  INDENTED_HEREDOC

  least_indented = <<~SQUIGGLY_HEREDOC
This will treat the least indented text as the beginning left column.
  why?
    SQUIGGLY_HEREDOC
end
