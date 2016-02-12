alive = 21
dying = 25

<<SIMPLE
#{alive} and well, full of optimism.
SIMPLE

<<"DOUBLEQUOTES"
#{dying} and well, dead.
DOUBLEQUOTES

<<'SINGLEHEREDOC'
Not escaped, interpolated, so on...#{alive} is simply alive.
SINGLEHEREDOC

<<`BACKTICK`
cd ~/projects && echo "I am #{dying}"
BACKTICK
