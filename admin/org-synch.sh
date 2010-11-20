#!/bin/sh

# this script expects $1 to be the download directory and $2 to have org-synch.el

pkgname=`/usr/bin/curl -s http://orgmode.org/pkg/daily/|/usr/bin/perl -ne 'push @f, $1 if m/(org-\d{8}\.tar)/; END { @f = sort @f; print "$f[-1]\n"}'`

cd $1
/usr/bin/wget -q http://orgmode.org/pkg/daily/$pkgname -O ${pkgname}-tmp
if [ -f ${pkgname}-tmp ]; then
    /bin/rm -f org*.tar
    /bin/mv ${pkgname}-tmp $pkgname && \
    /usr/bin/emacs -batch -l $2/org-synch.el -f org-synch
fi
