#/bin/sh

ROOT=$1
LOG=$ROOT/update-log
PKGROOT=$ROOT/packages
ADMINROOT=$ROOT/admin

TARBALL=$PKGROOT/emacs-packages-latest.tgz
TARBALL_ROOT="emacs-24.1-packages-`/bin/date +'%F'`"

REPO=bzr://bzr.savannah.gnu.org/emacs/elpa
REPO_PACKAGES=$REPO/packages
REPO_ADMIN=$REPO/admin

if [ -z $ROOT ]; then
    echo "Syntax: $0 HOMEDIR"
    exit 1
elif [ -d $ROOT ]; then
    echo "Installing into '$ROOT', log is '$LOG'"
    echo "Installing into '$ROOT'" > $LOG
else
    echo "Sorry but $ROOT is not a directory, aborting."
    exit 1
fi


## Create the world-facing copy
echo "[$TMPROOT -> $PKGROOT] Creating the world-facing package repository copy in $PKGROOT" >> $LOG
TMPROOT=$PKGROOT-new
rm -rf $TMPROOT
/usr/bin/bzr export $TMPROOT $REPO_PACKAGES

echo "[$TMPROOT -> $PKGROOT] Running the post-export fetchers in $ADMINROOT against $TMPROOT" >> $LOG
rm -rf $ADMINROOT
bzr export $ADMINROOT $REPO_ADMIN

# Copy the org daily package from orgmode.org
echo "[$TMPROOT -> $PKGROOT] Running the post-export org-mode fetcher as '$ADMINROOT/org-synch.sh $TMPROOT $ADMINROOT'" >> $LOG
$ADMINROOT/org-synch.sh $TMPROOT $ADMINROOT >> $LOG 2>&1

echo "[$TMPROOT -> $PKGROOT] Moving $TMPROOT to $PKGROOT" >> $LOG
/bin/mv $PKGROOT $PKGROOT-old
/bin/mv $TMPROOT $PKGROOT
/bin/rm -rf $PKGROOT-old

echo "Exporting packages into $TARBALL (root = $TARBALL_ROOT)" >> $LOG
/usr/bin/bzr export --format=tgz --root=$TARBALL_ROOT $TARBALL $REPO_PACKAGES

/bin/chmod -R a+rX $PKGROOT

echo "Update complete at" `/bin/date` >> $LOG
