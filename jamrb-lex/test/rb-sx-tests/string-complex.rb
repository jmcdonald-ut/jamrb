%Q(1 + 1 is #{1 + 1}) # => "1 + 1 is 2"
%q(1 + 1 is #{1 + 1}) # => '1 + 1 is #{1 + 1}'
expected = <<HEREDOC
This is long text that spans so many freaking lines.

Note the line above is blank.

Also odd, is that the identifiers for these "heredoc" do not heredoc have to be 'uppercase'.

HEREDOC

heredoc2 = <<HEREDOC2
Note how #{1 + 1} shows as 2.
HEREDOC2

heredoc3 = <<'NONESCAPE'
Note how #{1 + 1} is not escaped.
NONESCAPE

heredoc4 = <<f
This is kind of weird?
f

indenteddoc = <<-GOGOGO
This still expects left flush
  that tab will prob get captured
GOGOGO

backticks = <<-`LONGSHELLCODE`
cat #{__FILE__}
LONGSHELLCODE

str = "note" " how" " these" " string" " are" " concatenated!"
puts str

str2 = %q{see how } "this is also " 'valid?'
puts str2
