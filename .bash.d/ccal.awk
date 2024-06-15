#!/usr/bin/awk

function floor(x) {y=int(x); return(x<y?y-1:y)}
function math_max(x, y) {return(x>y?x:y)}

BEGIN {
    max=0
    sub(/^0/,"",month)
    months[max] = int(month)
    days[max]   = int(day)
    max++
}

NR == FNR {
    months[max] = int($1)
    days[max]   = int($2)
    max++
    next
}

{
    index_at_start_of_row = math_max(1,floor((FNR-5) / 8) * 3 + 1)
    for (i=0; i<3; i++) {
        t[i] = " " substr($0,1+i*22,20) " "
         for(j=0; j < max; j++) {
            if (index_at_start_of_row + i == months[j]) {
                sub(" "days[j]" ",":"days[j]"@",t[i])
            }
        }
        gsub(/:/," \033[0;31m",t[i])
        gsub(/@/,"\033[0m ",t[i])
    }
    print t[0],t[1],t[2]
}
