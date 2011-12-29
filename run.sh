rm -f prgolog.[^m] prgolog.??* prgolog?*.* main.[^m] main.??* main?*.* && mmc -i prgolog.m && mmc main.m prgolog.m && echo "ok" && ./main
