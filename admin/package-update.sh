#/bin/sh

PATH=/bin:/usr/bin:/usr/local/bin
ROOT=$1
FETCHEXTRAS=$2

LOG=$ROOT/update-log
PKGROOT=$ROOT/packages
ADMINROOT=$ROOT/admin

TARBALL=$PKGROOT/emacs-packages-latest.tgz
TARBALL_ROOT="emacs-24.1-packages-`/bin/date +'%F'`"

REPO=bzr://bzr.savannah.gnu.org/emacs/elpa
REPO_PACKAGES=$REPO/packages
REPO_ADMIN=$REPO/admin

PACKAGE_TARBALLS="auctex-11.86 company-0.5 muse-3.20"

if [ -z $ROOT ]; then
    echo "Syntax: $0 HOMEDIR [fetch-extras-boolean]"
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
bzr export $TMPROOT $REPO_PACKAGES

echo "[$TMPROOT -> $PKGROOT] Running the post-export fetchers in $ADMINROOT against $TMPROOT" >> $LOG
rm -rf $ADMINROOT
bzr export $ADMINROOT $REPO_ADMIN

if [ -z $FETCHEXTRAS ]; then
    echo "Skipping the post-export fetchers" >> $LOG
    echo "(pass 1 as the second parameter to get them with $0 or just run them manually)" >> $LOG
else
    # Copy the org daily package from orgmode.org
    echo "[$TMPROOT -> $PKGROOT] Running the post-export org-mode fetcher as '$ADMINROOT/org-synch.sh $TMPROOT $ADMINROOT'" >> $LOG
    $ADMINROOT/org-synch.sh $TMPROOT $ADMINROOT >> $LOG 2>&1
fi

echo "Creating tarballs from unpacked packages $PACKAGE_TARBALLS in $TMPROOT" >> $LOG
cd $TMPROOT
for pt in $PACKAGE_TARBALLS; do
    echo "Creating tarball of $pt: tar of $TMPROOT/$pt into $TMPROOT/$pt.tar" >> $LOG
    tar -cf $pt.tar $pt
    echo "Removing $TMPROOT/$pt" >> $LOG
    rm -rf $pt
done
cd ..

echo "[$TMPROOT -> $PKGROOT] Moving $TMPROOT to $PKGROOT" >> $LOG
mv $PKGROOT $PKGROOT-old
mv $TMPROOT $PKGROOT
rm -rf $PKGROOT-old

echo "Exporting packages into $TARBALL (root = $TARBALL_ROOT)" >> $LOG
bzr export --format=tgz --root=$TARBALL_ROOT $TARBALL $REPO_PACKAGES

chmod -R a+rX $PKGROOT

echo "Update complete at" `/bin/date` >> $LOG
