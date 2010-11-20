#/bin/sh

/bin/rm /home/elpa/update-log

## Create emacs-packages-latest.tgz
/usr/bin/bzr export --format=tgz --root=emacs-24.1-packages-`/bin/date +'%F'` /home/elpa/emacs-packages-latest.tgz /home/elpa/package-repo

## Create the world-facing copy
rm -rf /home/elpa/packages-new
/usr/bin/bzr export /home/elpa/packages-new /home/elpa/package-repo

# Copy the org daily package from orgmode.org
/home/elpa/bin/org-synch.sh

/bin/mv /home/elpa/packages /home/elpa/packages-old
/bin/mv /home/elpa/packages-new /home/elpa/packages
/bin/rm -rf /home/elpa/packages-old

/bin/chmod -R a+rX /home/elpa/packages

/bin/echo "Update complete at" `/bin/date` >> /home/elpa/update-log
