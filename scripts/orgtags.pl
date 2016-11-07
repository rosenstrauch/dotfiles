# https://lists.gnu.org/archive/html/emacs-orgmode/2010-11/msg01130.html
# simply run orgtags.pl on the desired org files:
# % perl orgtags.pl ~/org/*.org

#!/usr/bin/perl
use strict;
use warnings;

my %tags;

while (<>) {
  next unless (/^\*+\s/);
  if (/\s:([\w:]+):\s/) {
    for my $tag (split(/:/, $1)) {
      $tags{$tag} += 1;
    }
  }
}

for my $tag (sort {$tags{$b} <=> $tags{$a}} keys %tags) {
  print "$tag ($tags{$tag})\n";
}
