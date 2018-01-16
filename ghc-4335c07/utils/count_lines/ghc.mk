utils/count_lines_PERL_SRC  = count_lines.pl
utils/count_lines_dist_PROGNAME = count_lines

$(eval $(call build-perl,utils/count_lines,dist))
