<<`HEREDOC`
cd ~/projects && echo "Get to work!"
HEREDOC

<<~`H`
cd ~/projects && #{foo}
H

<<-`SHELLCODE`
cd ~/projects && #{foo}
SHELLCODE
