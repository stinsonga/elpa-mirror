#!/bin/sh

pkgname=`/usr/bin/curl -s http://orgmode.org/pkg/daily/|/usr/bin/perl -ne 'push @f, $1 if m/(org-\d{8}\.tar)/; END { @f = sort @f; print "$f[-1]\n"}'`

cd /home/elpa/
/usr/bin/wget -q http://orgmode.org/pkg/daily/$pkgname
if [ -f $pkgname ]; then
    /bin/rm -f packages/org*.tar && \
    /bin/mv $pkgname packages-new && \
    /usr/bin/emacs -batch -l /home/elpa/bin/org-synch.el -f org-synch &> /home/elpa/update-log
fi
